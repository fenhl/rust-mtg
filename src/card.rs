//! Contains the `Card` type, which represents a *Magic* card.

use {
    std::{
        cmp::Ordering,
        collections::{
            HashMap,
            HashSet,
            btree_map::{
                self,
                BTreeMap
            }
        },
        ffi::OsString,
        fmt,
        fs::{
            self,
            File
        },
        hash::{
            Hash,
            Hasher
        },
        io::{
            self,
            prelude::*,
            stderr
        },
        path::Path,
        str::FromStr,
        sync::{
            Arc,
            RwLock
        },
        thread
    },
    caseless::default_case_fold_str,
    derive_more::From,
    itertools::Itertools as _,
    lazy_static::lazy_static,
    num::{
        BigInt,
        BigUint
    },
    regex::Regex,
    serde::{
        Deserialize,
        Serialize
    },
    serde_json::{
        Value as Json,
        json
    },
    topological_sort::TopologicalSort,
    crate::{
        cardtype::{
            CardType,
            LandType,
            Subtype,
            Supertype,
            TypeLine
        },
        color::{
            Color,
            ColorSet
        },
        cost::{
            Cost,
            ManaCost
        },
        util::StrExt as _
    }
};

macro_rules! verbose_eprint {
    ($verbose:expr, $($fmt:tt)+) => {
        if $verbose {
            eprint!($($fmt)+);
            stderr().flush()?;
        }
    };
}

lazy_static! {
    static ref PARENS_REGEX: Regex = Regex::new(r" ?\(.*?\)").expect("failed to build parens regex");
}

type Obj = ::serde_json::Map<String, Json>;

/// Used in `DbError::ParseSet` as additional debugging information.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum ParseStep {
    CardBorder,
    CardLayout,
    CardName,
    CardObject,
    OtherName,
    SetCards,
    SetCode,
    SetObject,
    SetReleaseDate
}

/// The JSON was not a valid MTG JSON 4 database.
#[derive(Debug, From)]
pub enum DbError {
    /// The MTG JSON download failed
    Download(::reqwest::Error),
    /// The filename of a set file could not be read
    Filename(OsString),
    /// An I/O error occurred while trying to read a database
    Io(io::Error),
    /// The downloaded file was not valid JSON
    Json(::serde_json::Error),
    /// The JSON root element was not an object
    NotAnObject,
    /// The set data was not in the expected format
    ParseSet {
        /// The parsing step during which the error occurred
        step: ParseStep,
        /// The set code of the set where the error occurred
        set_code: Option<String>
    }
}

/// A database of cards.
#[derive(Debug, Clone)]
pub struct Db {
    cards: BTreeMap<String, Card>,
    cards_uncased: BTreeMap<String, Card>,
    set_codes: HashSet<String>
}

impl Db {
    /// Returns a database without any cards in it.
    pub fn empty() -> Db {
        Db {
            cards: BTreeMap::default(),
            cards_uncased: BTreeMap::default(),
            set_codes: HashSet::default()
        }
    }

    /// Downloads the current version of MTG JSON from their website and converts it into a `Db`.
    ///
    /// If `verbose` is true, a progress bar is shown on stderr.
    pub fn download(verbose: bool) -> Result<Db, DbError> {
        verbose_eprint!(verbose, "\r[....] downloading card database");
        let response = ::reqwest::get("https://mtgjson.com/json/AllSets.json")?;
        verbose_eprint!(verbose, "\r[=...] decoding card database   ");
        let json = ::serde_json::from_reader(response)?;
        Db::from_mtg_json_inner(verbose, 2, json)
    }

    /// Parses a JSON object structured like AllSets.json into a card database.
    ///
    /// If `verbose` is true, a progress bar is shown on stderr.
    pub fn from_mtg_json(json: Json, verbose: bool) -> Result<Db, DbError> {
        Db::from_mtg_json_inner(verbose, 0, json)
    }

    fn from_mtg_json_inner(verbose: bool, starting_progress: usize, json: Json) -> Result<Db, DbError> {
        let mut db = Db::empty();
        let sets = if let Some(sets) = json.as_object() { sets } else { return Err(DbError::NotAnObject); };
        for (i, set) in sets.values().enumerate() {
            let progress = starting_progress + (4 - starting_progress).min((5 - starting_progress) * i / sets.len());
            verbose_eprint!(verbose, "\r[{}{}] adding sets to database: {} of {}", "=".repeat(progress), ".".repeat(4 - progress), i, sets.len());
            db.register_set(set)?;
        }
        verbose_eprint!(verbose, "\r[====] generating case-insensitive card name index");
        db.gen_uncased();
        db.start_parser_thread();
        verbose_eprint!(verbose, "\r[ ok ] cards loaded                               \n");
        Ok(db)
    }

    /// Creates a card database from a directory of MTG JSON 4 set files.
    ///
    /// If `verbose` is true, a progress bar is shown on stderr.
    pub fn from_sets_dir<P: AsRef<Path>>(path: P, verbose: bool) -> Result<Db, DbError> {
        let mut db = Db::empty();
        let sets = fs::read_dir(path)?.collect::<Result<Vec<_>, _>>()?;
        for (i, entry) in sets.iter().enumerate() {
            let progress = 4.min(5 * i / sets.len());
            verbose_eprint!(verbose, "\r[{}{}] adding sets to database: {} of {}", "=".repeat(progress), ".".repeat(4 - progress), i, sets.len());
            let mut set_file = File::open(entry.path())?;
            let set = ::serde_json::from_reader(&mut set_file)?;
            db.register_set(&set)?;
        }
        verbose_eprint!(verbose, "\r[====] generating case-insensitive card name index");
        db.gen_uncased();
        db.start_parser_thread();
        verbose_eprint!(verbose, "\r[ ok ] cards loaded                               \n");
        Ok(db)
    }

    /// Returns a card when given its exact name.
    ///
    /// Combined names like `Wear // Tear` are not accepted.
    pub fn card(&self, card_name: &str) -> Option<Card> {
        self.cards.get(card_name).cloned()
    }

    /// Returns a card whose name matches the given string when compared case-insensitively.
    pub fn card_fuzzy(&self, card_name: &str) -> Option<Card> {
        self.cards_uncased.get(&default_case_fold_str(card_name)).cloned()
    }

    fn gen_uncased(&mut self) {
        for (card_name, card) in &self.cards {
            self.cards_uncased.insert(default_case_fold_str(card_name), card.clone());
        }
    }

    fn register_set(&mut self, set: &Json) -> Result<(), DbError> {
        use crate::card::ParseStep::*;

        let set = if let Some(set) = set.as_object() { set } else { return Err(DbError::ParseSet { step: SetObject, set_code: None }); };
        // flatten MTG JSON 5 set files to MTG JSON 4-style for now, should fix on the async-json branch
        let set = if let Some(data) = set.get("data").and_then(|data| data.as_object()) {
            let mut flattened = data.clone();
            if let Some(meta) = set.get("meta") {
                flattened.insert(format!("meta"), meta.clone());
            }
            flattened
        } else {
            set.clone()
        };
        let set_code = if let Some(code) = set.get("code").and_then(|code| code.as_str()) { code } else { return Err(DbError::ParseSet { step: SetCode, set_code: None }); };
        match set_code {
            "THP3" => { return Ok(()); } //HACK because THP3 is a "promo" set for some reason, not "memorabilia"
            _ => {}
        }
        match set.get("type").and_then(|set_type| set_type.as_str()) {
            Some("errata") => { return Ok(()); } // ignore errata sets //TODO apply errata according to set priorities
            Some("funny") => { return Ok(()); } // ignore funny sets even if the cards aren't silver-bordered (e.g. Heroes of the Realm) //TODO figure out a better way to do this to get black-bordered cards in unsets back (basics, Steamflogger Boss)
            Some("memorabilia") => { return Ok(()); } // memorabilia sets either have oversized cards or nonstandard card backs, and include some non-silver-bordered uncards (e.g. Heroes of the Realm 2017)
            Some("token") => { return Ok(()); } // ignore token sets and Hero's Path
            _ => {}
        }
        let set_border = set.get("border").and_then(|border| border.as_str()).unwrap_or("black");
        let set_cards = if let Some(cards) = set.get("cards").and_then(|cards| cards.as_array()) { cards } else { return Err(DbError::ParseSet { step: SetCards, set_code: Some(set_code.into()) }); };
        let set_release_date = if let Some(date) = set.get("releaseDate") { date } else { return Err(DbError::ParseSet { step: SetReleaseDate, set_code: Some(set_code.into()) }); };
        for mut card in set_cards.into_iter().cloned() {
            let card = if let Some(card) = card.as_object_mut() { card } else { return Err(DbError::ParseSet { step: CardObject, set_code: Some(set_code.into()) }); };
            match card.get("borderColor").or_else(|| card.get("border")).map_or(Some(set_border), |border| border.as_str()) {
                Some("silver") => { continue; } // silver-bordered card
                Some(_) => {}
                None => { return Err(DbError::ParseSet { step: CardBorder, set_code: Some(set_code.into()) }); }
            }
            match card.get("layout").and_then(|layout| layout.as_str()) {
                Some("token") => { continue; } // token card, don't include
                Some(_) => {}
                None => { return Err(DbError::ParseSet { step: CardLayout, set_code: Some(set_code.into()) }); }
            }
            card.entry("releaseDate".to_owned()).or_insert(set_release_date.clone());
            card.insert("setCode".into(), json!(set_code));
            let card_name = if let Some(name) = card.get("name").and_then(|name| name.as_str()) { name.to_owned() } else { return Err(DbError::ParseSet { step: CardName, set_code: Some(set_code.into()) }); };
            self.cards.entry(card_name.clone())
                .or_insert_with(|| Card::new(&card_name))
                .push_printing(card.to_owned());
            // add cross-references to other parts of the card
            if let Some(names) = card.get("names").and_then(|names| names.as_array()) {
                for name_json in names {
                    let other_name = name_json.as_str().ok_or(DbError::ParseSet { step: OtherName, set_code: Some(set_code.into()) })?;
                    if other_name != card_name {
                        let other_card = self.cards.entry(other_name.to_owned())
                            .or_insert_with(|| Card::new(other_name)).clone();
                        other_card.push_other(self.cards[&card_name].clone());
                        self.cards[&card_name].push_other(other_card);
                    }
                }
            }
        }
        self.set_codes.insert(set_code.into());
        Ok(())
    }

    /// Returns the set codes of all sets in the database.
    pub fn set_codes(&self) -> &HashSet<String> {
        &self.set_codes
    }

    fn start_parser_thread(&self) {
        let db = self.clone();
        //TODO lower thread priority
        let _ = thread::Builder::new().name("MTG db parser".to_owned()).spawn(move || {
            for card in &db {
                let _ = card.parse(); // parsing is an optimization, ignore errors
            }
            //TODO sort printings
        }); // parsing is an optimization, ignore errors
    }
}

impl<'a> IntoIterator for &'a Db {
    type Item = Card;
    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Iter<'a> {
        Iter(self.cards.values())
    }
}

/// Iterates over all cards in a `Db` alphabetically.
#[derive(Debug)]
pub struct Iter<'a>(btree_map::Values<'a, String, Card>);

impl<'a> Iterator for Iter<'a> {
    type Item = Card;

    fn next(&mut self) -> Option<Card> {
        self.0.next().cloned()
    }
}

/// The known portion of a card's release date.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ReleaseDate {
    /// The year this printing of the card was released.
    pub year: u32,
    /// The month this printing of the card was released, if known. January is `1`.
    pub month: Option<u8>,
    /// The day of month this printing of the card was released, if known. `1...31`.
    pub day: Option<u8>
}

impl PartialOrd for ReleaseDate {
    fn partial_cmp(&self, other: &ReleaseDate) -> Option<Ordering> {
        if self.year == other.year {
            match (self.month, other.month) {
                (None, None) => Some(Ordering::Equal),
                (None, Some(_)) | (Some(_), None) => None,
                (Some(m1), Some(m2)) => {
                    if m1 == m2 {
                        match (self.day, other.day) {
                            (None, None) => Some(Ordering::Equal),
                            (None, Some(_)) | (Some(_), None) => None,
                            (Some(d1), Some(d2)) => Some(d1.cmp(&d2))
                        }
                    } else {
                        Some(m1.cmp(&m2))
                    }
                }
            }
        } else {
            Some(self.year.cmp(&other.year))
        }
    }
}

/// The [rarity](https://mtg.gamepedia.com/Rarity) of a card printing.
#[derive(Debug, Deserialize, Serialize, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Rarity {
    #[serde(rename = "basic", alias = "Basic Land")]
    /// Basic land rarity. Not to be confused with the “basic” supertype, the “land” card type, or the `Card::is_basic` property.
    Land,
    #[serde(rename = "common", alias = "Common")]
    #[allow(missing_docs)]
    Common,
    #[serde(rename = "uncommon", alias = "Uncommon")]
    #[allow(missing_docs)]
    Uncommon,
    #[serde(rename = "rare", alias = "Rare")]
    #[allow(missing_docs)]
    Rare,
    #[serde(rename = "mythic", alias = "Mythic Rare")]
    #[allow(missing_docs)]
    Mythic,
    #[serde(rename = "special", alias = "Special")]
    /// Special rarity, such as timeshifted cards in *Time Spiral*.
    Special
}

impl Rarity {
    /// Returns an approximation of the color an expansion symbol of that rarity would be, as a (red, green, blue) tuple.
    pub fn color(&self) -> (u8, u8, u8) {
        match *self {
            Rarity::Land | Rarity::Common => (0, 0, 0),
            Rarity::Uncommon => (140, 159, 172),
            Rarity::Rare => (178, 152, 81),
            Rarity::Mythic => (217, 97, 33),
            Rarity::Special => (144, 99, 156)
        }
    }

    /// Returns a one-letter rarity abbreviation.
    pub fn short(&self) -> char {
        match *self {
            Rarity::Land => 'L',
            Rarity::Common => 'C',
            Rarity::Uncommon => 'U',
            Rarity::Rare => 'R',
            Rarity::Mythic => 'M',
            Rarity::Special => 'S'
        }
    }
}

/// A printed (or Magic-online-only “printed”) version of a card.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Printing {
    /// The card. Use this to get Oracle information like card name, cost, or types.
    pub card: Card,
    /// The set code according to MTG JSON.
    pub set: String,
    /// The date the card was released.
    pub release_date: ReleaseDate,
    /// The official rarity of the printing, as defined on Gatherer.
    pub rarity: Rarity
} //TODO image info, see also https://github.com/mtgjson/mtgjson/issues/55

impl PartialOrd for Printing {
    fn partial_cmp(&self, other: &Printing) -> Option<Ordering> {
        match self.release_date.partial_cmp(&other.release_date) {
            Some(Ordering::Equal) => {
                if self == other {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            }
            e => e
        }
    }
}

/// A pair of symbols used to indicate the faces of a double-faced card.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DfcSymbol {
    /// The sun/moon symbols used in *Innistrad*, *Dark Ascension*, *Shadows over Innistrad*, and on [Ulrich of the Krallenhorde](https://mtg.wtf/card/emn/191a).
    Sun,
    #[cfg(not(feature = "custom"))]
    /// The spark/planeswalker symbols used in *Magic Origins*.
    Spark,
    #[cfg(feature = "custom")]
    /// The spark/planeswalker symbols used in *Magic Origins* and *Magic Villains*.
    Spark,
    /// The full moon/Emrakul symbols used in *Eldritch Moon*, with the exception of Ulrich.
    Emrakul,
    /// The compass/hill symbols used in *Ixalan*.
    Compass,
    #[cfg(feature = "custom")]
    /// The chalice/flame symbols used in *Tesla*.
    Chalice,
    #[cfg(feature = "custom")]
    /// The mortal/god symbols used in *Scriptures of Urshad*.
    MortalGod
}

/// The layout of a card, including other parts if any.
#[derive(Debug, Clone)]
pub enum Layout {
    /// The regular card layout, including variations like planeswalkers, levelers, full-art lands, planes, or schemes.
    Normal,
    /// A split spell, with or without fuse.
    Split {
        /// The left half of the split spell.
        left: Card,
        /// The right half of the split spell.
        right: Card
    },
    /// A Kamigawa-style flip card.
    Flip {
        /// The default half of the flip card.
        unflipped: Card,
        /// The alternate half of the flip card.
        flipped: Card
    },
    /// An Innistrad-style double-faced card.
    DoubleFaced {
        /// The pair of symbols used for this card.
        symbol: DfcSymbol,
        /// The front face of the card.
        front: Card,
        /// The back face of the card.
        back: Card
    },
    /// A meld card.
    Meld {
        /// The card with the meld ability that forms the top half of the melded permanent.
        top: Card,
        /// The card with the reminder text that forms the bottom half of the melded permanent.
        bottom: Card,
        /// The melded permanent.
        back: Card
    },
    /// An adventurer card.
    Adventure {
        /// The main part, usually a creature.
        creature: Card,
        /// The Adventure spell part.
        adventure: Card
    }
}

impl Default for Layout {
    fn default() -> Layout {
        Layout::Normal
    }
}

/// A number used in an ability.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Number {
    /// A constant number.
    Const(BigUint),
    /// The value of X.
    X,
    /// The number of colors spent to cast this object.
    Sunburst
}

impl Default for Number {
    fn default() -> Number {
        Number::Const(BigUint::default())
    }
}

impl<T: Into<BigUint>> From<T> for Number {
    fn from(n: T) -> Number {
        Number::Const(n.into())
    }
}

impl FromStr for Number {
    type Err = ();

    fn from_str(s: &str) -> Result<Number, ()> {
        if let Ok(n) = BigUint::from_str(s) { return Ok(Number::Const(n)); }
        if s == "X" { return Ok(Number::X); }
        if s.to_lowercase() == "sunburst" { return Ok(Number::Sunburst); }
        Err(())
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Number::Const(ref n) => n.fmt(f),
            Number::X => write!(f, "X"),
            Number::Sunburst => write!(f, "Sunburst")
        }
    }
}

/// A keyword ability, including parameters (e.g. `absorb 1`).
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KeywordAbility {
    Deathtouch,
    Defender,
    /// double strike
    DoubleStrike,
    /// enchant [object or player]
    Enchant(String),
    /// equip [cost] (regular equip)
    Equip(Cost),
    /// equip [quality] creature [cost]
    EquipQuality(String, Cost),
    /// first strike
    FirstStrike,
    Flash,
    Flying,
    Haste,
    // regular hexproof
    Hexproof,
    // hexproof from [quality]
    HexproofFrom(String),
    Indestructible,
    Intimidate,
    /// [type]walk (landwalk)
    Landwalk {
        /// The required parts of the land's type line. Always includes the card type “land”.
        pos: TypeLine,
        /// These types may not appear on the land's type line, e.g. for “nonbasic landwalk”, this is “Basic”.
        neg: TypeLine
    },
    Lifelink,
    /// protection from [quality]
    Protection(String),
    Reach,
    Shroud,
    Trample,
    Vigilance,
    /// regular banding
    Banding,
    /// bands with other [quality]
    BandsWithOther(String),
    /// rampage N
    Rampage(Number),
    /// cumulative upkeep [cost]
    CumulativeUpkeep(Cost),
    Flanking,
    Phasing,
    /// buyback [cost]
    Buyback(Cost),
    Shadow,
    /// cycling [cost] (regular cycling)
    Cycling(Cost),
    /// [type]cycling [cost]
    Typecycling(TypeLine, Cost),
    /// echo [cost]
    Echo(Cost),
    Horsemanship,
    /// fading N
    Fading(Number),
    /// kicker [cost] (regular kicker)
    Kicker(Cost),
    /// multikicker [cost]
    Multikicker(Cost),
    /// flashback [cost]
    Flashback(Cost),
    /// madness [cost]
    Madness(Cost),
    Fear,
    /// morph [cost] (regular morph)
    Morph(Cost),
    /// megamorph [cost]
    Megamorph(Cost),
    /// amplify N
    Amplify(Number),
    Provoke,
    Storm,
    /// affinity for [text]
    Affinity(String),
    /// entwine [cost]
    Entwine(Cost),
    /// modular N
    Modular(Number),
    Sunburst,
    /// bushido N
    Bushido(Number),
    /// soulshift N
    Soulshift(Number),
    /// splice onto [subtype] [cost]
    Splice(Subtype, Cost),
    /// [subtype] offering
    Offering(Subtype),
    /// ninjutsu [cost] (regular ninjutsu)
    Ninjutsu(Cost),
    /// commander ninjutsu [cost]
    CommanderNinjutsu(Cost),
    Epic,
    Convoke,
    /// dredge N
    Dredge(Number),
    /// transmute [cost]
    Transmute(Cost),
    /// bloodthirst N
    Bloodthirst(Number),
    Haunt,
    /// replicate [cost]
    Replicate(Cost),
    /// forecast—[activated ability]
    Forecast(Box<Ability>),
    /// graft N
    Graft(Number),
    /// recover [cost]
    Recover(Cost),
    /// ripple N
    Ripple(Number),
    /// split second
    SplitSecond,
    /// suspend N—[cost]
    Suspend(Number, Cost),
    /// vanishing N
    Vanishing(Number),
    /// absorb N
    Absorb(Number),
    /// aura swap [cost]
    AuraSwap(Cost),
    Delve,
    /// fortify [cost]
    Fortify(Cost),
    /// frenzy N
    Frenzy(Number),
    Gravestorm,
    /// poisonous N
    Poisonous(Number),
    /// transfigure [cost]
    Transfigure(Cost),
    /// champion an [object]
    Champion(String),
    Changeling,
    /// evoke [cost]
    Evoke(Cost),
    Hideaway,
    /// prowl [cost]
    Prowl(Cost),
    /// reinforce N—[cost]
    Reinforce(Number, Cost),
    Conspire,
    Persist,
    Wither,
    Retrace,
    /// devour N
    Devour(Number),
    Exalted,
    /// unearth [cost]
    Unearth(Cost),
    Cascade,
    /// annihilator N
    Annihilator(Number),
    /// level up [cost]
    LevelUp(Cost),
    Rebound,
    /// totem armor
    TotemArmor,
    Infect,
    /// battle cry
    BattleCry,
    /// living weapon
    LivingWeapon,
    Undying,
    /// miracle [cost]
    Miracle(Cost),
    #[cfg(not(feature = "custom"))]
    Soulbond,
    #[cfg(feature = "custom")]
    /// regular soulbond
    Soulbond,
    #[cfg(feature = "custom")]
    Warband,
    /// overload [cost]
    Overload(Cost),
    /// scavenge [cost]
    Scavenge(Cost),
    Unleash,
    Cipher,
    Evolve,
    Extort,
    Fuse,
    /// bestow [cost]
    Bestow(Cost),
    /// tribute N
    Tribute(Number),
    Dethrone,
    /// regular hidden agenda
    HiddenAgenda,
    /// double agenda
    DoubleAgenda,
    /// outlast [cost]
    Outlast(Cost),
    Prowess,
    /// dash [cost]
    Dash(Cost),
    Exploit,
    Menace,
    /// renown N
    Renown(Number),
    /// awaken N—[cost]
    Awaken(Number, Cost),
    Devoid,
    Ingest,
    Myriad,
    /// surge [cost]
    Surge(Cost),
    Skulk,
    /// emerge [cost]
    Emerge(Cost),
    /// escalate [cost]
    Escalate(Cost),
    Melee,
    /// crew N
    Crew(Number),
    /// fabricate N
    Fabricate(Number),
    Partner,
    /// partner with [name]
    PartnerWith(String),
    Undaunted,
    Improvise,
    Aftermath,
    /// embalm [cost]
    Embalm(Cost),
    /// eternalize [cost]
    Eternalize(Cost),
    /// afflict N
    Afflict(Number),
    Ascend,
    Assist,
    /// jump-start
    JumpStart,
    Mentor,
    /// afterlife N
    Afterlife(Number),
    Riot,
    /// spectacle [cost]
    Spectacle(Cost),
    #[cfg(feature = "custom")]
    /// resonance [cost]
    Resonance(Cost),
    #[cfg(feature = "custom")]
    Dreamwalk,
    #[cfg(feature = "custom")]
    /// premonition N—[cost]
    Premonition(Number, Cost),
    #[cfg(feature = "custom")]
    /// decipher [cost]
    Decipher(Cost),
    #[cfg(feature = "custom")]
    /// worship [cost]
    Worship(Cost),
    #[cfg(feature = "custom")]
    /// loaded N
    Loaded(Number),
    #[cfg(feature = "custom")]
    /// reflex [cost]
    Reflex(Cost),
    #[cfg(feature = "custom")]
    Revolution,
    #[cfg(feature = "custom")]
    Possess,
    #[cfg(feature = "custom")]
    Torture,
    #[cfg(feature = "custom")]
    /// focus [cost]
    Focus(Cost),
    #[cfg(feature = "custom")]
    Invocation,
    #[cfg(feature = "custom")]
    /// resurrect [cost]
    Resurrect(Cost),
    #[cfg(feature = "custom")]
    Desperation,
    #[cfg(feature = "custom")]
    Hollow,
    #[cfg(feature = "custom")]
    /// salvage [cost]
    Salvage(Cost),
    #[cfg(feature = "custom")]
    /// seal [cost]
    Seal(Cost),
    #[cfg(feature = "custom")]
    /// secure N—[cost]
    Secure(Number, Cost),
    #[cfg(feature = "custom")]
    /// prestige N
    Prestige(Number),
    #[cfg(feature = "custom")]
    Voyage,
    #[cfg(feature = "custom")]
    Enspirit,
    #[cfg(feature = "custom")]
    /// harmony — [Effect]
    Harmony(String),
    #[cfg(feature = "custom")]
    Tithe,
    #[cfg(feature = "custom")]
    /// assault [cost]
    Assault(Cost)
}

macro_rules! keyword_from_str {
    (@$s:expr, (plain $str:expr => $variant:ident)) => {
        if $s.to_lowercase() == $str {
            return Some($variant);
        }
    };
    (@$s:expr, (cost $str:expr => $variant:ident)) => {
        if $s.to_lowercase().starts_with(concat!($str, ' ')) {
            if let Ok(cost) = Cost::from_str(&$s[$str.len() + 1..]) {
                return Some($variant(cost));
            }
        }
        if $s.to_lowercase().starts_with(concat!($str, '—')) {
            if let Ok(cost) = Cost::from_str(&$s[$str.len() + "—".len()..]) {
                return Some($variant(cost));
            }
        }
    };
    (@$s:expr, (dash_text $str:expr => $variant:ident)) => {
        if $s.to_lowercase().starts_with(concat!($str, " — ")) {
            return Some($variant($s[$str.len() + " — ".len()..].to_owned()));
        }
    };
    (@$s:expr, (landwalk => $variant:ident)) => {
        if $s.ends_with("walk") {
            let mut pos = TypeLine::default();
            let mut neg = TypeLine::default();
            let mut error = false;
            for mut word in $s[..$s.len() - 4].split_whitespace() {
                let target_line = if word.to_lowercase().starts_with("non") {
                    word = &word[3..];
                    &mut neg
                } else {
                    &mut pos
                };
                if let Ok(supertype) = Supertype::from_str(word) {
                    *target_line |= supertype;
                    continue;
                }
                if let Ok(card_type) = CardType::from_str(word) {
                    *target_line |= card_type;
                    continue;
                }
                if let Ok(subtype) = Subtype::from_str(word) {
                    *target_line |= subtype;
                    continue;
                }
                error = true;
                break;
            }
            if !error && (pos >= CardType::Land || LandType::iter_variants().any(|land_type| pos >= land_type)) {
                return Some($variant { pos, neg });
            }
        }
    };
    (@$s:expr, (number $str:expr => $variant:ident)) => {
        if $s.to_lowercase().starts_with(concat!($str, ' ')) {
            if let Ok(number) = Number::from_str(&$s[$str.len() + 1..]) {
                return Some($variant(number));
            }
        }
    };
    (@$s:expr, (number_cost $str:expr => $variant:ident)) => {
        if $s.to_lowercase().starts_with(concat!($str, ' ')) {
            if let Some(dash_idx) = $s.find('—') {
                if let Ok(number) = Number::from_str(&$s[$str.len() + 1..dash_idx]) {
                    if let Ok(cost) = Cost::from_str(&$s[dash_idx + "—".len()..]) {
                        return Some($variant(number, cost));
                    }
                }
            }
        }
    };
    (@$s:expr, (prefix_subtype $str:expr => $variant:ident)) => {
        if $s.ends_with($str) {
            if let Ok(subtype) = Subtype::from_str(&$s[..$s.len() - $str.len()]) {
                return Some($variant(subtype));
            }
        }
    };
    (@$s:expr, (subtype_cost $str:expr => $variant:ident)) => {
        if $s.to_lowercase().starts_with(concat!($str, ' ')) {
            if let Some(space_idx) = $s[$str.len() + 1..].find(' ') {
                if let Ok(subtype) = Subtype::from_str(&$s[$str.len() + 1..$str.len() + 1 + space_idx]) {
                    if let Ok(cost) = Cost::from_str(&$s[$str.len() + 1 + space_idx + 1..]) {
                        return Some($variant(subtype, cost));
                    }
                }
            }
            if let Some(dash_idx) = $s.find('—') {
                if let Ok(subtype) = Subtype::from_str(&$s[$str.len() + 1..dash_idx]) {
                    if let Ok(cost) = Cost::from_str(&$s[dash_idx + "—".len()..]) {
                        return Some($variant(subtype, cost));
                    }
                }
            }
        }
    };
    (@$s:expr, (text $str:expr => $variant:ident)) => {
        if $s.to_lowercase().starts_with(concat!($str, ' ')) {
            return Some($variant($s[$str.len() + 1..].to_owned()));
        }
    };
    (@$s:expr, (text_cost $str:expr => $variant:ident)) => {
        if $s.to_lowercase().starts_with(concat!($str, ' ')) {
            if let Some(dash_idx) = $s.find('—') {
                let text = $s[$str.len() + 1..dash_idx].to_owned();
                if let Ok(cost) = Cost::from_str(&$s[dash_idx + "—".len()..]) {
                    return Some($variant(text, cost));
                }
            }
            if let Some(space_idx) = $s[$str.len() + 1..].rfind(' ') {
                let text = $s[$str.len() + 1..$str.len() + 1 + space_idx].to_owned();
                if let Ok(cost) = Cost::from_str(&$s[$str.len() + 1 + space_idx + 1..]) {
                    return Some($variant(text, cost));
                }
            }
        }
    };
    (@$s:expr, (typecycling => $variant:ident)) => {
        if let Some(cycling_idx) = $s.find("cycling ") {
            if let Ok(type_line) = TypeLine::from_str(&$s[..cycling_idx]) {
                if let Ok(cost) = Cost::from_str(&$s[cycling_idx + "cycling ".len()..]) {
                    return Some($variant(type_line, cost));
                }
            }
        }
        if let Some(cycling_idx) = $s.find("cycling—") {
            if let Ok(type_line) = TypeLine::from_str(&$s[..cycling_idx]) {
                if let Ok(cost) = Cost::from_str(&$s[cycling_idx + "cycling—".len()..]) {
                    return Some($variant(type_line, cost));
                }
            }
        }
    };
    (@$s:expr, (wrapper $str:expr => $variant:ident)) => {
        if $s.to_lowercase().starts_with(concat!($str, " — ")) {
            if let Ok(ability) = Ability::from_str(&$s[$str.len() + " — ".len()..]) {
                return Some($variant(Box::new(ability)));
            }
        }
    };
    ($s:expr, { $($inner:tt),* }) => {
        $(keyword_from_str!(@$s, $inner);)*
    };
}

impl KeywordAbility {
    fn from_str_base(s: &str) -> Option<KeywordAbility> {
        use self::KeywordAbility::*;

        keyword_from_str!(s, {
            (plain "deathtouch" => Deathtouch),
            (plain "defender" => Defender),
            (plain "double strike" => DoubleStrike),
            (text "enchant" => Enchant),
            (text_cost "equip" => EquipQuality),
            (cost "equip" => Equip),
            (plain "first strike" => FirstStrike),
            (plain "flash" => Flash),
            (plain "flying" => Flying),
            (plain "haste" => Haste),
            (text "hexproof from" => HexproofFrom),
            (plain "hexproof" => Hexproof),
            (plain "indestructible" => Indestructible),
            (plain "intimidate" => Intimidate),
            (landwalk => Landwalk),
            (plain "lifelink" => Lifelink),
            (text "protection from" => Protection), //TODO how to handle “protection from A and from B”?
            (plain "reach" => Reach),
            (plain "shroud" => Shroud),
            (plain "trample" => Trample),
            (plain "vigilance" => Vigilance),
            (plain "banding" => Banding),
            (text "bands with other" => BandsWithOther),
            (number "rampage" => Rampage),
            (cost "cumulative upkeep" => CumulativeUpkeep),
            (plain "flanking" => Flanking),
            (plain "phasing" => Phasing),
            (cost "buyback" => Buyback),
            (plain "shadow" => Shadow),
            (cost "cycling" => Cycling),
            (typecycling => Typecycling),
            (cost "echo" => Echo),
            (plain "horsemanship" => Horsemanship),
            (number "fading" => Fading),
            (cost "kicker" => Kicker),
            (cost "multikicker" => Multikicker),
            (cost "flashback" => Flashback),
            (cost "madness" => Madness),
            (plain "fear" => Fear),
            (cost "morph" => Morph),
            (cost "megamorph" => Megamorph),
            (number "amplify" => Amplify),
            (plain "provoke" => Provoke),
            (plain "storm" => Storm),
            (text "affinity for" => Affinity),
            (cost "entwine" => Entwine),
            (number "modular" => Modular),
            (plain "sunburst" => Sunburst),
            (number "bushido" => Bushido),
            (number "soulshift" => Soulshift),
            (subtype_cost "splice onto" => Splice),
            (prefix_subtype " offering" => Offering),
            (cost "ninjutsu" => Ninjutsu),
            (cost "commander ninjutsu" => CommanderNinjutsu),
            (plain "epic" => Epic),
            (plain "convoke" => Convoke),
            (number "dredge" => Dredge),
            (cost "transmute" => Transmute),
            (number "bloodthirst" => Bloodthirst),
            (plain "haunt" => Haunt),
            (cost "replicate" => Replicate),
            (wrapper "forecast" => Forecast),
            (number "graft" => Graft),
            (cost "recover" => Recover),
            (number "ripple" => Ripple),
            (plain "split second" => SplitSecond),
            (number_cost "suspend" => Suspend),
            (number "vanishing" => Vanishing),
            (number "absorb" => Absorb),
            (cost "aura swap" => AuraSwap),
            (plain "delve" => Delve),
            (cost "fortify" => Fortify),
            (number "frenzy" => Frenzy),
            (plain "gravestorm" => Gravestorm),
            (number "poisonous" => Poisonous),
            (cost "transfigure" => Transfigure),
            (text "champion a" => Champion),
            (text "champion an" => Champion),
            (plain "changeling" => Changeling),
            (cost "evoke" => Evoke),
            (plain "hideaway" => Hideaway),
            (cost "prowl" => Prowl),
            (number_cost "reinforce" => Reinforce),
            (plain "conspire" => Conspire),
            (plain "persist" => Persist),
            (plain "wither" => Wither),
            (plain "retrace" => Retrace),
            (number "devour" => Devour),
            (plain "exalted" => Exalted),
            (cost "unearth" => Unearth),
            (plain "cascade" => Cascade),
            (number "annihilator" => Annihilator),
            (cost "level up" => LevelUp),
            (plain "rebound" => Rebound),
            (plain "totem armor" => TotemArmor),
            (plain "infect" => Infect),
            (plain "battle cry" => BattleCry),
            (plain "living weapon" => LivingWeapon),
            (plain "undying" => Undying),
            (cost "miracle" => Miracle),
            (plain "soulbond" => Soulbond),
            (cost "overload" => Overload),
            (cost "scavenge" => Scavenge),
            (plain "unleash" => Unleash),
            (plain "cipher" => Cipher),
            (plain "evolve" => Evolve),
            (plain "extort" => Extort),
            (plain "fuse" => Fuse),
            (cost "bestow" => Bestow),
            (number "tribute" => Tribute),
            (plain "dethrone" => Dethrone),
            (plain "hidden agenda" => HiddenAgenda),
            (plain "double agenda" => DoubleAgenda),
            (cost "outlast" => Outlast),
            (plain "prowess" => Prowess),
            (cost "dash" => Dash),
            (plain "exploit" => Exploit),
            (plain "menace" => Menace),
            (number "renown" => Renown),
            (number_cost "awaken" => Awaken),
            (plain "devoid" => Devoid),
            (plain "ingest" => Ingest),
            (plain "myriad" => Myriad),
            (cost "surge" => Surge),
            (plain "skulk" => Skulk),
            (cost "emerge" => Emerge),
            (cost "escalate" => Escalate),
            (plain "melee" => Melee),
            (number "crew" => Crew),
            (number "fabricate" => Fabricate),
            (plain "partner" => Partner),
            (text "partner with" => PartnerWith),
            (plain "undaunted" => Undaunted),
            (plain "improvise" => Improvise),
            (plain "aftermath" => Aftermath),
            (cost "embalm" => Embalm),
            (cost "eternalize" => Eternalize),
            (number "afflict" => Afflict),
            (plain "assist" => Assist),
            (plain "jump-start" => JumpStart),
            (plain "mentor" => Mentor),
            (number "afterlife" => Afterlife),
            (plain "riot" => Riot),
            (cost "spectacle" => Spectacle)
        });
        None
    }

    #[cfg(feature = "custom")]
    fn from_str_custom(s: &str) -> Option<KeywordAbility> {
        use self::KeywordAbility::*;

        keyword_from_str!(s, {
            (plain "warband" => Warband),
            (cost "resonance" => Resonance),
            (plain "dreamwalk" => Dreamwalk),
            (number_cost "premonition" => Premonition),
            (cost "decipher" => Decipher),
            (cost "worship" => Worship),
            (number "loaded" => Loaded),
            (cost "reflex" => Reflex),
            (plain "revolution" => Revolution),
            (plain "possess" => Possess),
            (plain "torture" => Torture),
            (cost "focus" => Focus),
            (plain "invocation" => Invocation),
            (cost "resurrect" => Resurrect),
            (plain "desperation" => Desperation),
            (plain "hollow" => Hollow),
            (cost "salvage" => Salvage),
            (cost "seal" => Seal),
            (number_cost "secure" => Secure),
            (number "prestige" => Prestige),
            (plain "voyage" => Voyage),
            (plain "enspirit" => Enspirit),
            (dash_text "harmony" => Harmony),
            (plain "tithe" => Tithe),
            (cost "assault" => Assault)
        });
        KeywordAbility::from_str_base(s)
    }

    fn name(&self) -> &'static str {
        use self::KeywordAbility::*;

        match self {
            Deathtouch => "deathtouch",
            Defender => "defender",
            DoubleStrike => "double strike",
            Enchant(_) => "enchant",
            EquipQuality(_, _) => "equip",
            Equip(_) => "equip",
            FirstStrike => "first strike",
            Flash => "flash",
            Flying => "flying",
            Haste => "haste",
            HexproofFrom(_) => "hexproof from",
            Hexproof => "hexproof",
            Indestructible => "indestructible",
            Intimidate => "intimidate",
            Landwalk { .. } => "landwalk",
            Lifelink => "lifelink",
            Protection(_) => "protection from",
            Reach => "reach",
            Shroud => "shroud",
            Trample => "trample",
            Vigilance => "vigilance",
            Banding => "banding",
            BandsWithOther(_) => "bands with other",
            Rampage(_) => "rampage",
            CumulativeUpkeep(_) => "cumulative upkeep",
            Flanking => "flanking",
            Phasing => "phasing",
            Buyback(_) => "buyback",
            Shadow => "shadow",
            Cycling(_) => "cycling",
            Typecycling(_, _) => "typecycling",
            Echo(_) => "echo",
            Horsemanship => "horsemanship",
            Fading(_) => "fading",
            Kicker(_) => "kicker",
            Multikicker(_) => "multikicker",
            Flashback(_) => "flashback",
            Madness(_) => "madness",
            Fear => "fear",
            Morph(_) => "morph",
            Megamorph(_) => "megamorph",
            Amplify(_) => "amplify",
            Provoke => "provoke",
            Storm => "storm",
            Affinity(_) => "affinity for",
            Entwine(_) => "entwine",
            Modular(_) => "modular",
            Sunburst => "sunburst",
            Bushido(_) => "bushido",
            Soulshift(_) => "soulshift",
            Splice(_, _) => "splice onto",
            Offering(_) => " offering",
            Ninjutsu(_) => "ninjutsu",
            CommanderNinjutsu(_) => "commander ninjutsu",
            Epic => "epic",
            Convoke => "convoke",
            Dredge(_) => "dredge",
            Transmute(_) => "transmute",
            Bloodthirst(_) => "bloodthirst",
            Haunt => "haunt",
            Replicate(_) => "replicate",
            Forecast(_) => "forecast",
            Graft(_) => "graft",
            Recover(_) => "recover",
            Ripple(_) => "ripple",
            SplitSecond => "split second",
            Suspend(_, _) => "suspend",
            Vanishing(_) => "vanishing",
            Absorb(_) => "absorb",
            AuraSwap(_) => "aura swap",
            Delve => "delve",
            Fortify(_) => "fortify",
            Frenzy(_) => "frenzy",
            Gravestorm => "gravestorm",
            Poisonous(_) => "poisonous",
            Transfigure(_) => "transfigure",
            Champion(_) => "champion a",
            Changeling => "changeling",
            Evoke(_) => "evoke",
            Hideaway => "hideaway",
            Prowl(_) => "prowl",
            Reinforce(_, _) => "reinforce",
            Conspire => "conspire",
            Persist => "persist",
            Wither => "wither",
            Retrace => "retrace",
            Devour(_) => "devour",
            Exalted => "exalted",
            Unearth(_) => "unearth",
            Cascade => "cascade",
            Annihilator(_) => "annihilator",
            LevelUp(_) => "level up",
            Rebound => "rebound",
            TotemArmor => "totem armor",
            Infect => "infect",
            BattleCry => "battle cry",
            LivingWeapon => "living weapon",
            Undying => "undying",
            Miracle(_) => "miracle",
            Soulbond => "soulbond",
            Overload(_) => "overload",
            Scavenge(_) => "scavenge",
            Unleash => "unleash",
            Cipher => "cipher",
            Evolve => "evolve",
            Extort => "extort",
            Fuse => "fuse",
            Bestow(_) => "bestow",
            Tribute(_) => "tribute",
            Dethrone => "dethrone",
            HiddenAgenda => "hidden agenda",
            DoubleAgenda => "double agenda",
            Outlast(_) => "outlast",
            Prowess => "prowess",
            Dash(_) => "dash",
            Exploit => "exploit",
            Menace => "menace",
            Renown(_) => "renown",
            Awaken(_, _) => "awaken",
            Devoid => "devoid",
            Ingest => "ingest",
            Myriad => "myriad",
            Surge(_) => "surge",
            Skulk => "skulk",
            Emerge(_) => "emerge",
            Escalate(_) => "escalate",
            Melee => "melee",
            Crew(_) => "crew",
            Fabricate(_) => "fabricate",
            Partner => "partner",
            PartnerWith(_) => "partner with",
            Undaunted => "undaunted",
            Improvise => "improvise",
            Aftermath => "aftermath",
            Embalm(_) => "embalm",
            Eternalize(_) => "eternalize",
            Afflict(_) => "afflict",
            Ascend => "ascend",
            Assist => "assist",
            JumpStart => "jump-start",
            Mentor => "mentor",
            Afterlife(_) => "afterlife",
            Riot => "riot",
            Spectacle(_) => "spectacle",
            #[cfg(feature = "custom")] Warband => "warband",
            #[cfg(feature = "custom")] Resonance(_) => "resonance",
            #[cfg(feature = "custom")] Dreamwalk => "dreamwalk",
            #[cfg(feature = "custom")] Premonition(_, _) => "premonition",
            #[cfg(feature = "custom")] Decipher(_) => "decipher",
            #[cfg(feature = "custom")] Worship(_) => "worship",
            #[cfg(feature = "custom")] Loaded(_) => "loaded",
            #[cfg(feature = "custom")] Reflex(_) => "reflex",
            #[cfg(feature = "custom")] Revolution => "revolution",
            #[cfg(feature = "custom")] Possess => "possess",
            #[cfg(feature = "custom")] Torture => "torture",
            #[cfg(feature = "custom")] Focus(_) => "focus",
            #[cfg(feature = "custom")] Invocation => "invocation",
            #[cfg(feature = "custom")] Resurrect(_) => "resurrect",
            #[cfg(feature = "custom")] Desperation => "desperation",
            #[cfg(feature = "custom")] Hollow => "hollow",
            #[cfg(feature = "custom")] Salvage(_) => "salvage",
            #[cfg(feature = "custom")] Seal(_) => "seal",
            #[cfg(feature = "custom")] Secure(_, _) => "secure",
            #[cfg(feature = "custom")] Prestige(_) => "prestige",
            #[cfg(feature = "custom")] Voyage => "voyage",
            #[cfg(feature = "custom")] Enspirit => "enspirit",
            #[cfg(feature = "custom")] Harmony(_) => "harmony",
            #[cfg(feature = "custom")] Tithe => "tithe",
            #[cfg(feature = "custom")] Assault(_) => "assault"
        }
    }
}

impl FromStr for KeywordAbility {
    type Err = ();

    #[cfg(not(feature = "custom"))]
    fn from_str(s: &str) -> Result<KeywordAbility, ()> {
        KeywordAbility::from_str_base(s).ok_or(())
    }

    #[cfg(feature = "custom")]
    fn from_str(s: &str) -> Result<KeywordAbility, ()> {
        KeywordAbility::from_str_custom(s).ok_or(())
    }
}

impl fmt::Display for KeywordAbility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::KeywordAbility::*;

        match self {
            AuraSwap(cost) |
            Bestow(cost) |
            Buyback(cost) |
            CommanderNinjutsu(cost) |
            CumulativeUpkeep(cost) |
            Cycling(cost) |
            Dash(cost) |
            Echo(cost) |
            Embalm(cost) |
            Emerge(cost) |
            Entwine(cost) |
            Equip(cost) |
            Escalate(cost) |
            Eternalize(cost) |
            Evoke(cost) |
            Flashback(cost) |
            Fortify(cost) |
            Kicker(cost) |
            LevelUp(cost) |
            Madness(cost) |
            Megamorph(cost) |
            Miracle(cost) |
            Morph(cost) |
            Multikicker(cost) |
            Ninjutsu(cost) |
            Outlast(cost) |
            Overload(cost) |
            Prowl(cost) |
            Recover(cost) |
            Replicate(cost) |
            Scavenge(cost) |
            Spectacle(cost) |
            Surge(cost) |
            Transfigure(cost) |
            Transmute(cost) |
            Unearth(cost) => if cost.other.is_some() {
                write!(f, "{}—{}", self.name(), cost)
            } else {
                write!(f, "{} {}", self.name(), cost)
            },
            #[cfg(feature = "custom")]
            Assault(cost) |
            Decipher(cost) |
            Focus(cost) |
            Reflex(cost) |
            Resonance(cost) |
            Resurrect(cost) |
            Salvage(cost) |
            Seal(cost) |
            Worship(cost) => if cost.other.is_some() {
                write!(f, "{}—{}", self.name(), cost)
            } else {
                write!(f, "{} {}", self.name(), cost)
            },
            #[cfg(feature = "custom")]
            Harmony(text) => write!(f, "harmony — {}", text),
            Landwalk { pos, neg } => if neg.is_empty() {
                write!(f, "{}walk", pos.to_string().to_lowercase())
            } else {
                unimplemented!(); //TODO
            },
            Absorb(n) |
            Afflict(n) |
            Afterlife(n) |
            Amplify(n) |
            Annihilator(n) |
            Bloodthirst(n) |
            Bushido(n) |
            Crew(n) |
            Devour(n) |
            Dredge(n) |
            Fabricate(n) |
            Fading(n) |
            Frenzy(n) |
            Graft(n) |
            Modular(n) |
            Poisonous(n) |
            Rampage(n) |
            Renown(n) |
            Ripple(n) |
            Soulshift(n) |
            Tribute(n) |
            Vanishing(n) => write!(f, "{} {}", self.name(), n),
            #[cfg(feature = "custom")]
            Loaded(n) |
            Prestige(n) => write!(f, "{} {}", self.name(), n),
            Awaken(n, cost) |
            Reinforce(n, cost) |
            Suspend(n, cost) => if cost.other.is_some() {
                write!(f, "{} {}—{}", self.name(), n, cost)
            } else {
                write!(f, "{} {} {}", self.name(), n, cost)
            },
            #[cfg(feature = "custom")]
            Premonition(n, cost) |
            Secure(n, cost) => if cost.other.is_some() {
                write!(f, "{} {}—{}", self.name(), n, cost)
            } else {
                write!(f, "{} {} {}", self.name(), n, cost)
            },
            Aftermath |
            Ascend |
            Assist |
            Banding |
            BattleCry |
            Cascade |
            Changeling |
            Cipher |
            Conspire |
            Convoke |
            Deathtouch |
            Defender |
            Delve |
            Dethrone |
            Devoid |
            DoubleAgenda |
            DoubleStrike |
            Epic |
            Evolve |
            Exalted |
            Exploit |
            Extort |
            Fear |
            FirstStrike |
            Flanking |
            Flash |
            Flying |
            Fuse |
            Gravestorm |
            Haste |
            Haunt |
            Hexproof |
            HiddenAgenda |
            Hideaway |
            Horsemanship |
            Improvise |
            Indestructible |
            Infect |
            Ingest |
            Intimidate |
            JumpStart |
            Lifelink |
            LivingWeapon |
            Melee |
            Menace |
            Mentor |
            Myriad |
            Partner |
            Persist |
            Phasing |
            Provoke |
            Prowess |
            Reach |
            Rebound |
            Retrace |
            Riot |
            Shadow |
            Shroud |
            Skulk |
            Soulbond |
            SplitSecond |
            Storm |
            Sunburst |
            TotemArmor |
            Trample |
            Undaunted |
            Undying |
            Unleash |
            Vigilance |
            Wither => write!(f, "{}", self.name()),
            #[cfg(feature = "custom")]
            Desperation |
            Dreamwalk |
            Enspirit |
            Hollow |
            Invocation |
            Possess |
            Revolution |
            Tithe |
            Torture |
            Voyage |
            Warband => write!(f, "{}", self.name()),
            Offering(subtype) => write!(f, "{} offering", subtype),
            Splice(subtype, cost) => if cost.other.is_some() {
                write!(f, "splice onto {}—{}", subtype, cost)
            } else {
                write!(f, "splice onto {} {}", subtype, cost)
            },
            Affinity(text) |
            BandsWithOther(text) |
            Champion(text) |
            Enchant(text) |
            HexproofFrom(text) |
            PartnerWith(text) |
            Protection(text) => write!(f, "{} {}", self.name(), text),
            EquipQuality(text, cost) => if cost.other.is_some() {
                write!(f, "equip {}—{}", text, cost)
            } else {
                write!(f, "equip {} {}", text, cost)
            },
            Typecycling(type_line, cost) => if cost.other.is_some() {
                write!(f, "{}cycling—{}", type_line.to_string().to_lowercase(), cost)
            } else {
                write!(f, "{}cycling {}", type_line.to_string().to_lowercase(), cost)
            },
            Forecast(ability) => write!(f, "forecast — {}", ability)
        }
    }
}

impl PartialEq<Ability> for KeywordAbility {
    fn eq(&self, other: &Ability) -> bool {
        if let Ability::Keyword(keyword) = other {
            self == keyword
        } else {
            false
        }
    }
}

impl PartialEq<KeywordAbility> for Ability {
    fn eq(&self, other: &KeywordAbility) -> bool {
        if let Ability::Keyword(keyword) = self {
            keyword == other
        } else {
            false
        }
    }
}

#[cfg(feature = "custom")]
/// A chapter symbol, as seen on Sagas and Discoveries.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Chapter {
    /// A regular chapter number symbol `{rN}`.
    Number(u16),
    /// The `{DISCOVER}` symbol.
    Discover
}

#[cfg(feature = "custom")]
impl fmt::Display for Chapter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Chapter::Number(n) => write!(f, "{{r{}}}", n),
            Chapter::Discover => write!(f, "{{DISCOVER}}")
        }
    }
}

/// An ability printed on a card.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ability {
    /// A keyword ability, with optional additional text.
    Keyword(KeywordAbility),
    /// A chapter ability, typically found on a Saga card.
    Chapter {
        #[cfg(not(feature = "custom"))]
        /// The chapter numbers that trigger this ability.
        chapters: HashSet<u16>,
        #[cfg(feature = "custom")]
        /// The chapter numbers that trigger this ability.
        chapters: HashSet<Chapter>,
        /// The trigger effect.
        text: String
    },
    /// The keyword ability represented by a level symbol.
    Level {
        /// The minimum level for this ability to be active.
        min: BigUint,
        /// The maximum level, if any, for this ability to be active.
        max: Option<BigUint>,
        /// The base power set by this ability.
        power: BigInt,
        /// The base toughness set by this ability.
        toughness: BigInt,
        /// Additional abilities granted by this ability.
        abilities: Vec<Ability>
    },
    /// A modal ability.
    Modal {
        /// The string introducing the modal ability, e.g. “Choose one—”
        choose: String,
        /// The modes of the ability.
        modes: Vec<String>
    },
    /// A non-keyword, non-modal ability.
    Other(String)
}

impl FromStr for Ability {
    type Err = ();

    fn from_str(s: &str) -> Result<Ability, ()> {
        if let Ok(keyword) = KeywordAbility::from_str(s) { return Ok(Ability::Keyword(keyword)); }
        //TODO chapter keyword, level keyword, modal ability
        Ok(Ability::Other(s.into()))
    }
}

impl fmt::Display for Ability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ability::Keyword(keyword) => write!(f, "{}", keyword.to_string().to_uppercase_first()),
            Ability::Chapter { chapters, text } => write!(f, "{}—{}", chapters.iter().sorted().map(|chapter| {
                #[cfg(not(feature = "custom"))] { format!("{{r{}}}", chapter) }
                #[cfg(feature = "custom")] { format!("{}", chapter) }
            }).join(", "), text),
            Ability::Level { min, max, power, toughness, abilities } => write!(f, "{{LEVEL {}}}{}{}/{}",
                if let Some(max) = max { format!("{}-{}", min, max) } else { format!("{}+", min) },
                if abilities.is_empty() { format!(" ") } else { format!("\n{}\n", abilities.iter().map(ToString::to_string).join("\n")) },
                power,
                toughness
            ),
            Ability::Modal { choose, modes } => write!(f, "{}\n{}", choose, modes.join("\n• ")),
            Ability::Other(text) => write!(f, "{}", text)
        }
    }
}

/// Represents a card in the game.
///
/// All information returned by this struct's methods matches the Oracle data, and not necessarily what was originally printed on the card.
#[derive(Debug, Clone)]
pub struct Card {
    name: String,
    data: Arc<RwLock<CardData>>,
    other: Arc<RwLock<HashMap<String, Card>>>
}

#[derive(Debug, Clone)]
enum CardData {
    RawJson { printings: Vec<Obj> },
    Parsed {
        color_identity: ColorSet,
        color_indicator: Option<ColorSet>,
        colors: ColorSet,
        layout: Layout,
        loyalty: Option<Number>,
        mana_cost: Option<ManaCost>,
        printings: Vec<Printing>,
        pt: Option<(String, String)>,
        #[cfg(feature = "custom")]
        stability: Option<Number>,
        text: String,
        type_line: TypeLine,
        vanguard_modifiers: Option<(i64, i64)>
    }
}

impl Default for CardData {
    fn default() -> CardData {
        CardData::RawJson { printings: Vec::default() }
    }
}

impl Card {
    fn new(name: impl ToString) -> Card {
        Card {
            name: name.to_string(),
            data: Arc::default(),
            other: Arc::default()
        }
    }

    /// Returns each ability printed on the card as a string.
    ///
    /// Reminder text is removed, and multiple keyword abilities on one line are split into separate abilities. However, ability words will be retained.
    pub fn abilities(&self) -> Vec<Ability> {
        let mut result = Vec::<Ability>::default();
        let text = self.text();
        let mut lines = text.split('\n').filter(|line| !line.is_empty());
        while let Some(line) = lines.next() {
            if line.starts_with('•') && !result.is_empty() {
                match result[result.len() - 1] {
                    Ability::Modal { .. } => {
                        // mode, part of previous ability
                        let idx = result.len() - 1;
                        let modes = if let Ability::Modal { ref mut modes, .. } = result[idx] { modes } else { panic!("failed to convert ability to modal"); };
                        modes.push(PARENS_REGEX.replace_all(&line["• ".len()..], "").into());
                        continue;
                    }
                    Ability::Other(_) => {
                        let choose = if let Some(Ability::Other(text)) = result.pop() { text } else { panic!("failed to convert ability to modal"); };
                        result.push(Ability::Modal {
                            choose,
                            modes: vec![PARENS_REGEX.replace_all(&line["• ".len()..], "").into()]
                        });
                        continue;
                    }
                    _ => {}
                }
            }
            if line.starts_with("LEVEL ") {
                let level_range = &line["LEVEL ".len()..];
                let (min, max) = if level_range.ends_with('+') {
                    (level_range[..level_range.len() - 1].parse().expect("failed to parse level range"), None)
                } else {
                    let (min, max) = level_range.split('-')
                        .map(|n| n.parse().expect("failed to parse level range"))
                        .collect_tuple()
                        .expect("failed to parse level range");
                    (min, Some(max))
                };
                let (power, toughness) = lines.next().expect("missing P/T for level keyword")
                    .split('/')
                    .map(|n| n.parse().expect("failed to parse level P/T"))
                    .collect_tuple()
                    .expect("failed to parse level P/T");
                result.push(Ability::Level {
                    max, min, power, toughness,
                    abilities: Vec::default()
                });
                continue;
            }
            if line.starts_with('(') && line.ends_with(')') {
                continue; // skip reminder text
            }
            let line = PARENS_REGEX.replace_all(line, "");
            let mut is_keywords = true;
            let mut keywords = Vec::default();
            for line_part in line.split(&[',', ';', '\u{2014}'][..]).map(|s| s.trim()) {
                if let Ok(keyword) = KeywordAbility::from_str(line_part) {
                    keywords.push(Ability::Keyword(keyword));
                } else {
                    is_keywords = false;
                    break;
                }
            }
            if is_keywords {
                if let Some(Ability::Level { abilities, .. }) = result.last_mut() {
                    abilities.append(&mut keywords);
                } else {
                    result.append(&mut keywords);
                }
            } else {
                // new ability
                if let Some(Ability::Level { abilities, .. }) = result.last_mut() {
                    abilities.push(Ability::Other(line.into()));
                } else {
                    result.push(Ability::Other(line.into()));
                }
            }
        }
        result
    }

    /// Returns the converted mana cost the card has while it is not on the stack.
    ///
    /// To calculate the converted mana cost on the stack for a given value of X, use the method `mana_cost`.
    pub fn cmc(&self) -> BigUint {
        self.mana_cost().map(|cost| cost.converted(BigUint::default())).unwrap_or_default()
    }

    /// The card's color identity. Not to be confused with the card's colors.
    ///
    /// Note that the return value considers basic land types part of the color identity, in accordance with the Commander Rules Committee's definition, not the Comprehensive Rules.
    pub fn color_identity(&self) -> ColorSet {
        match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => {
                if printings[0].contains_key("colorIdentity") {
                    printings[0]["colorIdentity"]
                        .as_array()
                        .expect("colorIdentity field is not an array")
                        .into_iter()
                        .map(|color_str| color_str.as_str().expect("colorIdentity member is not a string").parse::<Color>().expect("colorIdentity member is not a color letter"))
                        .collect()
                } else {
                    ColorSet::default()
                }
            }
            CardData::Parsed { color_identity, .. } => color_identity
        }
    }

    /// If the card has a color indicator, returns it.
    ///
    /// Since older versions of MTG JSON did not store color indicator information, this may return incorrect values if the database is MTG JSON 3 or older.
    pub fn color_indicator(&self) -> Option<ColorSet> {
        match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => {
                if printings[0].contains_key("colorIndicator") {
                    Some(
                        printings[0]["colorIndicator"]
                            .as_array()
                            .expect("colorIndicator field is not an array")
                            .into_iter()
                            .map(|color_str| color_str.as_str().expect("colorIndicator member is not a string").parse::<Color>().expect("colorIndicator member is not a color letter"))
                            .collect()
                    )
                } else {
                    None
                }
            }
            CardData::Parsed { color_indicator, .. } => color_indicator
        }
    }

    /// The card's colors. Not to be confused with the card's color identity.
    pub fn colors(&self) -> ColorSet {
        match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => {
                if printings[0].contains_key("colors") {
                    printings[0]["colors"]
                        .as_array()
                        .expect("colors field is not an array")
                        .into_iter()
                        .map(|color_str| color_str.as_str().expect("colors member is not a string").parse::<Color>().expect("colorIdentity member is not a color word"))
                        .collect()
                } else {
                    ColorSet::default()
                }
            }
            CardData::Parsed { colors, .. } => colors
        }
    }

    fn dfc_symbol(&self) -> DfcSymbol {
        let set_code = match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => printings[0]["setCode"].as_str().expect("setCode is not a string").to_owned(),
            CardData::Parsed { ref printings, .. } => printings[0].set.clone()
        };
        match &set_code[..] {
            "ISD" | "PISD" | "DKA" | "PDKA" | "SOI" | "PSOI" | "PRM" => DfcSymbol::Sun,
            "ORI" | "PORI" | "PS15" | "M19" | "PM19" => DfcSymbol::Spark,
            "EMN" | "PEMN" => match &self.name[..] {
                "Ulrich of the Krallenhorde" | "Ulrich, Uncontested Alpha" => DfcSymbol::Sun,
                _ => DfcSymbol::Emrakul
            },
            "XLN" | "PXLN" | "RIX" | "PRIX" | "PXTC" => DfcSymbol::Compass,
            "V17" => match &self.name[..] {
                "Archangel Avacyn" |
                "Arlinn Kord" |
                "Arlinn, Embraced by the Moon" |
                "Avacyn, the Purifier" |
                "Bloodline Keeper" |
                "Delver of Secrets" |
                "Elbrus, the Binding Blade" |
                "Garruk Relentless" |
                "Garruk, the Veil-Cursed" |
                "Huntmaster of the Fells" |
                "Insectile Aberration" |
                "Lord of Lineage" |
                "Ravager of the Fells" |
                "Withengar Unbound" => DfcSymbol::Sun,
                "Chandra, Fire of Kaladesh" |
                "Chandra, Roaring Flame" |
                "Gideon, Battle-Forged" |
                "Jace, Telepath Unbound" |
                "Jace, Vryn's Prodigy" |
                "Kytheon, Hero of Akros" |
                "Liliana, Defiant Necromancer" |
                "Liliana, Heretical Healer" |
                "Nissa, Sage Animist" |
                "Nissa, Vastwood Seer" => DfcSymbol::Spark,
                "Brisela, Voice of Nightmares" |
                "Bruna, the Fading Light" |
                "Gisela, the Broken Blade" => DfcSymbol::Emrakul,
                "Arguel's Blood Fast" |
                "Temple of Aclazotz" => DfcSymbol::Compass,
                name => { panic!("unexpected V17 DFC: {}", name); }
            },
            "PLPA" => match &self.name[..] {
                "Ludevic's Abomination" |
                "Ludevic's Test Subject" |
                "Mondronen Shaman" |
                "Tovolar's Magehunter" => DfcSymbol::Sun,
                name => { panic!("unexpected pLPA DFC: {}", name); }
            },
            "PPRE" => match &self.name[..] {
                "Archdemon of Greed" |
                "Howlpack Alpha" |
                "Mayor of Avabruck" |
                "Ravenous Demon" => DfcSymbol::Sun,
                "Chandra, Fire of Kaladesh" |
                "Chandra, Roaring Flame" |
                "Gideon, Battle-Forged" |
                "Jace, Telepath Unbound" |
                "Jace, Vryn's Prodigy" |
                "Kytheon, Hero of Akros" |
                "Liliana, Defiant Necromancer" |
                "Liliana, Heretical Healer" |
                "Nissa, Sage Animist" |
                "Nissa, Vastwood Seer" => DfcSymbol::Spark,
                name => { panic!("unexpected pPRE DFC: {}", name); }
            },
            set => { panic!("unexpected DFC in set {}", set); }
        }
    }

    #[cfg(not(feature = "custom"))]
    fn dfc_symbol_custom(&self) -> DfcSymbol {
        self.dfc_symbol()
    }

    #[cfg(feature = "custom")]
    fn dfc_symbol_custom(&self) -> DfcSymbol {
        let set_code = match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => printings[0]["setCode"].as_str().expect("setCode is not a string").to_owned(),
            CardData::Parsed { ref printings, .. } => printings[0].set.clone()
        };
        match &set_code[..] {
            "TSL" => DfcSymbol::Chalice,
            "VLN" => DfcSymbol::Spark,
            "RAK" | "EAU" => DfcSymbol::Sun,
            "SOU" => DfcSymbol::MortalGod,
            _ => self.dfc_symbol()
        }
    }

    /// Returns `true` for:
    ///
    /// *   The right half of a split card,
    /// *   The flipped version of a flip card,
    /// *   The Adventure part of an adventurer card, and
    /// *   The back face of a double-faced or meld card.
    pub fn is_alt(&self) -> bool {
        match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => {
                let names = self.names(printings);
                match printings[0]["layout"].as_str().expect("card layout is not a string") {
                    "split" | "flip" | "double-faced" | "transform" | "meld" | "adventure" => self.name == names[1].name,
                    _ => false
                }
            }
            CardData::Parsed { ref layout, .. } => match *layout {
                Layout::Normal => false,
                Layout::Split { ref right, .. } => self == right,
                Layout::Flip { ref flipped, .. } => self == flipped,
                Layout::DoubleFaced { ref back, .. } => self == back,
                Layout::Meld { ref back, .. } => self == back,
                Layout::Adventure { ref adventure, .. } => self == adventure
            }
        }
    }

    /// Returns `true` for the five cards [Plains](https://mtg.wtf/card?q=%21Plains), [Island](https://mtg.wtf/card?q=%21Island), [Swamp](https://mtg.wtf/card?q=%21Swamp), [Mountain](https://mtg.wtf/card?q=%21Mountain), and [Forest](https://mtg.wtf/card?q=%21Forest), and false for all other cards, including snow basics and [Wastes](https://mtg.wtf/card?q=%21Wastes).
    ///
    /// This method is intended for limited formats where any number of these cards may be added to a player's card pool, and not for e.g. determining deck legality with respect to the 4-copy limit.
    pub fn is_basic(&self) -> bool {
        match &self.name[..] {
            "Plains" | "Island" | "Swamp" | "Mountain" | "Forest" => true,
            _ => false
        }
    }

    /// Returns `true` if the card has at least one level keyword ability.
    pub fn is_leveler(&self) -> bool {
        self.abilities()
            .into_iter()
            .any(|ability| match ability { Ability::Level { .. } => true, _ => false })
    }

    /// Returns the layout of the card.
    pub fn layout(&self) -> Layout {
        match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => {
                let names = self.names(printings);
                match printings[0]["layout"].as_str().expect("card layout is not a string") {
                    "split" | "aftermath" => Layout::Split { left: names[0].clone(), right: names[1].clone() },
                    "flip" => Layout::Flip { unflipped: names[0].clone(), flipped: names[1].clone() },
                    "double-faced" | "transform" => {
                        Layout::DoubleFaced {
                            symbol: self.dfc_symbol_custom(),
                            front: names[0].clone(),
                            back: names[1].clone()
                        }
                    }
                    "meld" => Layout::Meld {
                        top: names[0].clone(),
                        back: names[1].clone(),
                        bottom: names[2].clone()
                    },
                    "adventure" => Layout::Adventure {
                        creature: names[0].clone(),
                        adventure: names[1].clone()
                    },
                    _ => Layout::Normal
                }
            }
            CardData::Parsed { ref layout, .. } => layout.clone()
        }
    }

    /// Returns the card's starting loyalty, if any.
    pub fn loyalty(&self) -> Option<Number> {
        match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => printings[0].get("loyalty").map(|loy| match *loy {
                Json::Null => Number::X,
                Json::Number(ref n) => Number::from(n.as_u64().expect(&format!("invalid starting loyalty: {}", n))),
                Json::String(ref s) => if s == "X" { Number::X } else { s.parse().expect(&format!("invalid starting loyalty: {}", s)) },
                ref v => { panic!("invalid starting loyalty: {}", v); }
            }),
            CardData::Parsed { ref loyalty, .. } => loyalty.clone()
        }
    }

    /// Returns the card's mana cost, or `None` if it has an unpayable cost.
    pub fn mana_cost(&self) -> Option<ManaCost> {
        match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => printings[0].get("manaCost").map(|mana_cost| mana_cost.as_str().expect("mana cost is not a string").parse().expect("invalid mana cost")),
            CardData::Parsed { ref mana_cost, .. } => mana_cost.clone()
        }
    }

    fn names(&self, printings: &[Obj]) -> Vec<Card> {
        if printings[0].contains_key("names") {
            printings[0]["names"]
                .as_array()
                .expect("names field is not an array")
                .into_iter()
                .map(|name| {
                    let name = name.as_str().expect("alt name is not a string");
                    if name == self.name {
                        self.clone()
                    } else {
                        self.other
                            .read()
                            .unwrap()
                            .get(name)
                            .expect(&format!("Card {} missing from `other` map in {}", name, self))
                            .clone()
                    }
                })
                .collect()
        } else {
            vec![self.clone()]
        }
    }

    /// Returns the number of different printings of the card. Faster than `card.printings_unsorted().len()`.
    pub fn num_printings(&self) -> usize {
        match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => printings.len(),
            CardData::Parsed { ref printings, .. } => printings.len()
        }
    }

    fn parse(&self) -> Option<()> {
        match *self.data.read().ok()? {
            CardData::RawJson { .. } => (),
            CardData::Parsed { .. } => { return Some(()); } // already parsed
        }
        let parsed = CardData::Parsed {
            color_identity: self.color_identity(),
            color_indicator: self.color_indicator(),
            colors: self.colors(),
            layout: self.layout(),
            loyalty: self.loyalty(),
            mana_cost: self.mana_cost(),
            printings: self.printings_unsorted(),
            pt: self.pt(),
            #[cfg(feature = "custom")]
            stability: self.stability(),
            text: self.text(),
            type_line: self.type_line(),
            vanguard_modifiers: self.vanguard_modifiers()
        };
        *self.data.write().ok()? = parsed;
        Some(())
    }

    /// Returns the following card, depending on layout:
    ///
    /// * Split: The left half
    /// * Flip: The unflipped version
    /// * Adventure: the creature
    /// * Double-faced: the front face
    /// * Meld: The card with the meld ability
    /// * Other: The card itself
    pub fn primary(&self) -> Card {
        match self.layout() {
            Layout::Normal => self.clone(),
            Layout::Split { left, .. } => left,
            Layout::Flip { unflipped, .. } => unflipped,
            Layout::DoubleFaced { front, .. } => front,
            Layout::Meld { top, .. } => top,
            Layout::Adventure { creature, .. } => creature
        }
    }

    /// Returns all the different printings of the card, sorted chronologically if possible.
    ///
    /// Warning: this method may take a very long time to return for basic lands.
    pub fn printings(&self) -> impl IntoIterator<Item = Printing> {
        //TODO add a CardData variant or flag for caching the sort order
        self.printings_unsorted().into_iter().collect::<TopologicalSort<_>>()
    }

    /// Returns all the different printings of the card, in no particular order.
    pub fn printings_unsorted(&self) -> Vec<Printing> {
        match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => {
                printings.iter().map(|printing_json| {
                    let release_date_parts = printing_json["releaseDate"].as_str().expect("releaseDate is not a string").split('-').collect::<Vec<_>>();
                    let (year, month, day) = match release_date_parts.len() {
                        1 => (u32::from_str(release_date_parts[0]).expect("invalid year in release date"), None, None),
                        2 => (u32::from_str(release_date_parts[0]).expect("invalid year in release date"), Some(u8::from_str(release_date_parts[1]).expect("invalid month in release date")), None),
                        3 => (u32::from_str(release_date_parts[0]).expect("invalid year in release date"), Some(u8::from_str(release_date_parts[1]).expect("invalid month in release date")), Some(u8::from_str(release_date_parts[2]).expect("invalid day in release date"))),
                        _ => { panic!("invalid release date"); }
                    };
                    Printing {
                        card: self.clone(),
                        set: printing_json["setCode"].as_str().expect("setCode is not a string").to_owned(),
                        release_date: ReleaseDate {
                            year: year,
                            month: month,
                            day: day
                        },
                        rarity: ::serde_json::from_value(printing_json["rarity"].clone()).expect("failed to parse printing rarity")
                    }
                }).collect()
            }
            CardData::Parsed { ref printings, .. } => printings.clone()
        }
    }

    /// Returns the card's printed power and toughness, if any.
    pub fn pt(&self) -> Option<(String, String)> {
        match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => match (printings[0].get("power").and_then(Json::as_str), printings[0].get("toughness").and_then(Json::as_str)) {
                (Some(pow), Some(tou)) => Some((pow.to_owned(), tou.to_owned())),
                _ => None
            },
            CardData::Parsed { ref pt, .. } => pt.clone()
        }
    }

    fn push_other(&self, other: Card) {
        self.other.write().unwrap().insert(other.name.clone(), other);
    }

    fn push_printing(&self, printing: Obj) {
        match *self.data.write().unwrap() {
            CardData::RawJson { ref mut printings } => { printings.push(printing); }
            _ => { panic!("tried to add a printing to card data that's not in raw JSON form"); }
        }
    }

    /// Returns the minimal printed rarity of the card.
    pub fn rarity(&self) -> Rarity {
        self.printings_unsorted()
            .into_iter()
            .map(|printing| printing.rarity)
            .min()
            .expect("tried to get rarity of card without printings")
    }

    /// Returns the card's starting stability, if any.
    #[cfg(feature = "custom")]
    pub fn stability(&self) -> Option<Number> {
        match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => printings[0].get("stability").map(|sta| match *sta {
                Json::Null => Number::X,
                Json::Number(ref n) => Number::from(n.as_u64().expect(&format!("invalid starting stability: {}", n))),
                Json::String(ref s) => if s == "X" { Number::X } else { s.parse().expect(&format!("invalid starting stability: {}", s)) },
                ref v => { panic!("invalid starting stability: {}", v); }
            }),
            CardData::Parsed { ref stability, .. } => stability.clone()
        }
    }

    /// Returns the contents of the card's Oracle text box.
    pub fn text(&self) -> String {
        match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => printings[0].get("text").and_then(Json::as_str).unwrap_or("").to_owned(),
            CardData::Parsed { ref text, .. } => text.to_owned()
        }
    }

    /// Returns the card's type line.
    pub fn type_line(&self) -> TypeLine {
        match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => TypeLine::from_str(printings[0]["type"].as_str().expect("type line is not a string")).expect("failed to parse type line"),
            CardData::Parsed { ref type_line, .. } => type_line.clone()
        }
    }

    /// Returns the card's hand and life modifier, if any.
    pub fn vanguard_modifiers(&self) -> Option<(i64, i64)> {
        match *self.data.read().unwrap() {
            CardData::RawJson { ref printings } => match (printings[0].get("hand").and_then(Json::as_i64), printings[0].get("life").and_then(Json::as_i64)) {
                (Some(hand), Some(life)) => Some((hand, life)),
                _ => None
            },
            CardData::Parsed { vanguard_modifiers, .. } => vanguard_modifiers
        }
    }
}

impl PartialEq for Card {
    fn eq(&self, other: &Card) -> bool {
        self.name == other.name
    }
}

impl Eq for Card {}

impl Hash for Card {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl PartialOrd for Card {
    fn partial_cmp(&self, other: &Card) -> Option<Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

impl Ord for Card {
    fn cmp(&self, other: &Card) -> Ordering {
        self.name.cmp(&other.name)
    }
}

/// Displays the card name.
impl fmt::Display for Card {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        card::{
            Db,
            DbError
        },
        color::ColorSet
    };

    fn test_indicator(db: &Db, card_name: &str, indicator: ColorSet) -> Option<()> {
        let card = db.card(card_name)?;
        assert_eq!(card.color_indicator()?, indicator);
        Some(())
    }

    #[test]
    fn test_indicators() -> Result<(), DbError> {
        let db = Db::download(false)?;
        test_indicator(&db, "Dryad Arbor", ColorSet::green());
        test_indicator(&db, "Nicol Bolas, the Arisen", ColorSet::grixis());
        test_indicator(&db, "Transguild Courier", ColorSet::rainbow());
        Ok(())
    }

    #[cfg(feature = "custom")]
    #[test]
    fn test_lore_seeker_db() -> Result<(), DbError> {
        let db = Db::from_sets_dir("/opt/git/github.com/fenhl/lore-seeker/master/data/sets")?;
        test_indicator(&db, "Ancient Elemental", ColorSet::rainbow());
        test_indicator(&db, "Awaken the Nameless", ColorSet::blue());
        Ok(())
    }
}
