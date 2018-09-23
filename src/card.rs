//! Contains the `Card` type, which represents a *Magic* card.

use std::{
    cmp::Ordering,
    collections::{
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
    io,
    path::Path,
    str::FromStr
};

use num::{
    BigInt,
    BigUint
};

use regex::Regex;

use serde_json::Value as Json;

use topological_sort::TopologicalSort;

use cardtype::{
    CardType,
    LandType,
    Subtype,
    Supertype,
    TypeLine
};
use color::{
    Color,
    ColorSet
};
use cost::{
    Cost,
    ManaCost
};

type Obj = ::serde_json::Map<String, Json>;

/// A database of cards.
pub struct Db {
    cards: BTreeMap<String, Vec<Obj>>,
    set_codes: HashSet<String>
}

/// The JSON was not a valid MTG JSON database.
#[derive(Debug)]
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
        /// The set code of the set where the error occurred
        set_code: String
    }
}

impl From<OsString> for DbError {
    fn from(e: OsString) -> DbError {
        DbError::Filename(e)
    }
}

impl From<io::Error> for DbError {
    fn from(e: io::Error) -> DbError {
        DbError::Io(e)
    }
}

impl From<::reqwest::Error> for DbError {
    fn from(e: ::reqwest::Error) -> DbError {
        DbError::Download(e)
    }
}

impl From<::serde_json::Error> for DbError {
    fn from(e: ::serde_json::Error) -> DbError {
        DbError::Json(e)
    }
}

impl Db {
    /// Downloads the current version of MTG JSON from their website and converts it into a `Db`.
    pub fn download() -> Result<Db, DbError> {
        let response = ::reqwest::get("http://mtgjson.com/json/AllSets.json")?;
        Db::from_mtg_json(::serde_json::from_reader(response)?)
    }

    /// Returns a database without any cards in it.
    pub fn empty() -> Db {
        Db {
            cards: BTreeMap::default(),
            set_codes: HashSet::default()
        }
    }

    /// Parses a JSON object structured like AllSets.json into a card database.
    pub fn from_mtg_json(json: Json) -> Result<Db, DbError> {
        let mut db = Db::empty();
        let sets = if let Some(sets) = json.as_object() { sets } else { return Err(DbError::NotAnObject); };
        for (set_code, set) in sets {
            db.register_set(set_code, set)?;
        }
        Ok(db)
    }

    /// Creates a card database from a directory of MTG JSON set files.
    pub fn from_sets_dir<P: AsRef<Path>>(path: P) -> Result<Db, DbError> {
        let mut db = Db::empty();
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let set_code = entry.file_name().into_string()?;
            if !set_code.ends_with(".json") { continue; }
            let mut set_file = File::open(entry.path())?;
            let set = ::serde_json::from_reader(&mut set_file)?;
            db.register_set(&set_code[..set_code.len() - 5], &set)?;
        }
        Ok(db)
    }

    /// Returns a card when given its exact name.
    ///
    /// Combined names like `Wear // Tear` are not accepted.
    pub fn card(&self, card_name: &str) -> Option<Card> {
        self.cards.get(card_name).map(|printings| Card { printings: printings.clone() }) //TODO use a reference instead?
    }

    /// Returns cards whose name is similar to the `search_term` parameter. This tries the following matching algorithms, in order:
    ///
    /// * Exact match
    /// * **(not implemented)** Initials (e.g. BoP for Birds of Paradise)
    /// * **(not implemented)** Card names starting with the search term, with a preference for cards with fewer apostrophes in their names (so that a legend is preferred over cards using the legend's name)
    /// * **(not implemented)** Card names ending with the search term
    /// * **(not implemented)** Card names containing the search term
    /// * **(not implemented)** Card names containing each word in the search term
    /// * **(not implemented)** Card names with the lowest [Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance) to the search term
    ///
    /// All matching algorithms are case-sensitive. This may be changed in the future.
    ///
    /// If `set_code` is given, the search is restricted to that set.
    pub fn card_fuzzy(&self, search_term: &str, set_code: Option<&str>) -> Vec<Card> {
        //TODO make all algorithms case-insensitive
        // exact match
        if let Some(card) = self.card(search_term) {
            let matches_set = set_code.map_or(true, |set_code| card.printings_unsorted().into_iter().any(|printing| &printing.set == set_code));
            if matches_set { return vec![card]; }
        }
        //TODO initials
        //TODO starts_with
        //TODO ends_with
        //TODO contains
        //TODO contains all words
        //TODO Levenshtein
        Vec::default()
    }

    fn register_set(&mut self, set_code: &str, set: &Json) -> Result<(), DbError> {
        let set = if let Some(set) = set.as_object() { set } else { return Err(DbError::ParseSet { set_code: set_code.into() }); };
        if set.get("type").and_then(|set_type| set_type.as_str()) == Some("errata") { return Ok(()); } // ignore errata sets //TODO apply errata according to set priorities
        let set_border = if let Some(border) = set.get("border").and_then(|border| border.as_str()) { border } else { return Err(DbError::ParseSet { set_code: set_code.into() }); };
        let set_cards = if let Some(cards) = set.get("cards").and_then(|cards| cards.as_array()) { cards } else { return Err(DbError::ParseSet { set_code: set_code.into() }); };
        let set_release_date = if let Some(date) = set.get("releaseDate") { date } else { return Err(DbError::ParseSet { set_code: set_code.into() }); };
        for mut card in set_cards.into_iter().cloned() {
            let card = if let Some(card) = card.as_object_mut() { card } else { return Err(DbError::ParseSet { set_code: set_code.into() }); };
            match card.get("border").map_or(Some(set_border), |border| border.as_str()) {
                Some("silver") => { continue; } // silver-bordered card
                Some(_) => {}
                None => { return Err(DbError::ParseSet { set_code: set_code.into() }); }
            }
            match card.get("layout").and_then(|layout| layout.as_str()) {
                Some("token") => { continue; } // token card, don't include
                Some(_) => {}
                None => { return Err(DbError::ParseSet { set_code: set_code.into() }); }
            }
            match card.get("name").and_then(|name| name.as_str()) {
                Some(name) => if ["1996 World Champion", "Fraternal Exaltation", "Proposal", "Robot Chicken", "Shichifukujin Dragon", "Splendid Genesis"].contains(&name) { continue; } // black-bordered, but not legal in any format
                None => { return Err(DbError::ParseSet { set_code: set_code.into() }); }
            }
            card.entry("releaseDate".to_owned()).or_insert(set_release_date.clone());
            card.insert("setCode".into(), json!(set_code));
            let card_name = if let Some(name) = card.get("name").and_then(|name| name.as_str()) { name.to_owned() } else { return Err(DbError::ParseSet { set_code: set_code.into() }); };
            self.cards.entry(card_name)
                .or_insert_with(Vec::default)
                .push(card.to_owned());
        }
        self.set_codes.insert(set_code.into());
        Ok(())
    }

    /// Returns the set codes of all sets in the database.
    pub fn set_codes(&self) -> &HashSet<String> {
        &self.set_codes
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
pub struct Iter<'a>(btree_map::Values<'a, String, Vec<Obj>>);

impl<'a> Iterator for Iter<'a> {
    type Item = Card;

    fn next(&mut self) -> Option<Card> {
        self.0.next().map(|printings| Card { printings: printings.clone() })
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Deserialize)]
pub enum Rarity {
    /// Basic land rarity. Not to be confused with the “basic” supertype, the “land” card type, or the `Card::is_basic` property.
    #[serde(rename = "Basic Land")]
    Land,
    #[allow(missing_docs)]
    Common,
    #[allow(missing_docs)]
    Uncommon,
    #[allow(missing_docs)]
    Rare,
    #[allow(missing_docs)]
    #[serde(rename = "Mythic Rare")]
    Mythic,
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
#[derive(Debug)]
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
    Chalice
}

/// The layout of a card, including other parts if any.
#[derive(Debug)]
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
    }
}

impl Default for Layout {
    fn default() -> Layout {
        Layout::Normal
    }
}

/// A number used in an ability.
#[derive(Debug, Clone)]
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Number::Const(ref n) => n.fmt(f),
            Number::X => write!(f, "X"),
            Number::Sunburst => write!(f, "Sunburst")
        }
    }
}

/// A keyword ability, including parameters (e.g. `absorb 1`).
#[allow(missing_docs)]
#[derive(Debug, Clone)]
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
    /// hollow [cost]
    Hollow(Cost),
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
    Voyage
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
            (plain "assist" => Assist)
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
            (cost "hollow" => Hollow),
            (cost "salvage" => Salvage),
            (cost "seal" => Seal),
            (number_cost "secure" => Secure),
            (number "prestige" => Prestige),
            (plain "voyage" => Voyage)
        });
        KeywordAbility::from_str_base(s)
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

/// An ability printed on a card.
#[derive(Debug, Clone)]
pub enum Ability {
    /// A keyword ability, with optional additional text.
    Keyword(KeywordAbility),
    /// A chapter ability, typically found on a Saga card.
    Chapter {
        /// The chapter numbers that trigger this ability.
        chapters: HashSet<u16>,
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

/// Represents a card in the game.
///
/// All information returned by this struct's methods matches the Oracle data, and not necessarily what was originally printed on the card.
#[derive(Debug, Clone)]
pub struct Card {
    printings: Vec<Obj>
}

impl Card {
    /// Returns each ability printed on the card as a string.
    ///
    /// Reminder text is removed, and multiple keyword abilities on one line are split into separate abilities. However, ability words will be retained.
    pub fn abilities(&self) -> Vec<Ability> {
        let mut result = Vec::<Ability>::default();
        for line in self.text().split('\n').filter(|line| !line.is_empty()) {
            if line.chars().next().map_or(false, |c| c == '•') && !result.is_empty() {
                match result[result.len() - 1] {
                    Ability::Modal { .. } => {
                        // mode, part of previous ability
                        let re = Regex::new(r" ?\(.*?\)").expect("failed to build parens regex");
                        let idx = result.len() - 1;
                        let modes = if let Ability::Modal { ref mut modes, .. } = result[idx] { modes } else { panic!("failed to convert ability to modal"); };
                        modes.push(re.replace_all(line, "").into());
                        continue;
                    }
                    Ability::Other(_) => {
                        let choose = if let Some(Ability::Other(text)) = result.pop() { text } else { panic!("failed to convert ability to modal"); };
                        let re = Regex::new(r" ?\(.*?\)").expect("failed to build parens regex");
                        result.push(Ability::Modal {
                            choose,
                            modes: vec![re.replace_all(line, "").into()]
                        });
                        continue;
                    }
                    _ => {}
                }
            }
            if line.chars().next().map_or(false, |c| c == '(') && line.chars().next_back().map_or(false, |c| c == ')') {
                continue; // skip reminder text
            }
            let line = Regex::new(r" ?\(.*?\)").expect("failed to build parens regex").replace_all(line, "");
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
                result.append(&mut keywords);
            } else {
                // new ability
                result.push(Ability::Other(line.into()));
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
        if self.json_data().contains_key("colorIdentity") {
            self.json_data()["colorIdentity"]
                .as_array()
                .expect("colorIdentity field is not an array")
                .into_iter()
                .map(|color_str| color_str.as_str().expect("colorIdentity member is not a string").parse::<Color>().expect("colorIdentity member is not a color letter"))
                .collect()
        } else {
            ColorSet::default()
        }
    }

    /// If the card has a color indicator, returns it.
    ///
    /// Since MTG JSON does not store color indicator information, this is computed from the card's mana cost and colors.
    pub fn color_indicator(&self) -> Option<ColorSet> {
        let intrinsic_colors = self.mana_cost().map(|cost| ColorSet::from(cost)).unwrap_or_default();
        if intrinsic_colors != ColorSet::default() {
            unimplemented!(); //TODO
        }
        None
    }

    /// The card's colors. Not to be confused with the card's color identity.
    pub fn colors(&self) -> ColorSet {
        if self.json_data().contains_key("colors") {
            self.json_data()["colors"]
                .as_array()
                .expect("colors field is not an array")
                .into_iter()
                .map(|color_str| color_str.as_str().expect("colors member is not a string").parse::<Color>().expect("colorIdentity member is not a color word"))
                .collect()
        } else {
            ColorSet::default()
        }
    }

    fn dfc_symbol(&self) -> DfcSymbol {
        match self.json_data()["setCode"].as_str().expect("setCode is not a string") {
            "ISD" | "DKA" | "SOI" => DfcSymbol::Sun,
            "ORI" => DfcSymbol::Spark,
            "EMN" => match &self.to_string()[..] {
                "Ulrich of the Krallenhorde" | "Ulrich, Uncontested Alpha" => DfcSymbol::Sun,
                _ => DfcSymbol::Emrakul
            },
            "XLN" => DfcSymbol::Compass,
            "V17" => match &self.to_string()[..] {
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
                "Temple of Aclatzotz" => DfcSymbol::Compass,
                name => { panic!("unexpected V17 DFC: {}", name); }
            }
            set => { panic!("unexpected DFC in set {}", set); }
        }
    }

    #[cfg(not(feature = "custom"))]
    fn dfc_symbol_custom(&self) -> DfcSymbol {
        self.dfc_symbol()
    }

    #[cfg(feature = "custom")]
    fn dfc_symbol_custom(&self) -> DfcSymbol {
        match self.json_data()["setCode"].as_str().expect("setCode is not a string") {
            "VLN" => DfcSymbol::Spark,
            "TSL" => DfcSymbol::Chalice,
            set => self.dfc_symbol()
        }
    }

    /// Returns `true` for:
    ///
    /// *   The right half of a split card,
    /// *   The flipped version of a flip card, and
    /// *   The back face of a double-faced or meld card.
    pub fn is_alt(&self) -> bool {
        let names = if self.json_data().contains_key("names") {
            self.json_data()["names"]
                .as_array()
                .expect("names field is not an array")
                .into_iter()
                .map(|name| name.as_str().expect("alt name is not a string"))
                .collect()
        } else {
            Vec::default()
        };
        match self.json_data()["layout"].as_str().expect("card layout is not a string") {
            "split" | "flip" | "double-faced" => self.to_string() == names[1],
            "meld" => self.to_string() == names[2],
            _ => false
        }
    }

    /// Returns `true` for the five cards [Plains](https://mtg.wtf/card?q=%21Plains), [Island](https://mtg.wtf/card?q=%21Island), [Swamp](https://mtg.wtf/card?q=%21Swamp), [Mountain](https://mtg.wtf/card?q=%21Mountain), and [Forest](https://mtg.wtf/card?q=%21Forest), and false for all other cards, including snow basics and [Wastes](https://mtg.wtf/card?q=%21Wastes).
    ///
    /// This method is intended for limited formats where any number of these cards may be added to a player's card pool, and not for e.g. determining deck legality with respect to the 4-copy limit.
    pub fn is_basic(&self) -> bool {
        match &self.to_string()[..] {
            "Plains" | "Island" | "Swamp" | "Mountain" | "Forest" => true,
            _ => false
        }
    }

    fn json_data(&self) -> &Obj {
        &self.printings[0]
    }

    /// Returns the layout of the card.
    pub fn layout(&self, db: &Db) -> Layout {
        let names = if self.json_data().contains_key("names") {
            self.json_data()["names"]
                .as_array()
                .expect("names field is not an array")
                .into_iter()
                .map(|name| db.card(name.as_str().expect("alt name is not a string")).expect("alt card name not found in database"))
                .collect()
        } else {
            Vec::default()
        };
        match self.json_data()["layout"].as_str().expect("card layout is not a string") {
            "split" | "aftermath" => Layout::Split { left: names[0].clone(), right: names[1].clone() },
            "flip" => Layout::Flip { unflipped: names[0].clone(), flipped: names[1].clone() },
            "double-faced" => {
                Layout::DoubleFaced {
                    symbol: self.dfc_symbol_custom(),
                    front: names[0].clone(),
                    back: names[1].clone()
                }
            }
            "meld" => Layout::Meld {
                top: names[0].clone(),
                bottom: names[1].clone(),
                back: names[2].clone()
            },
            _ => Layout::Normal
        }
    }

    /// Returns the card's starting loyalty, if any.
    pub fn loyalty(&self) -> Option<Number> {
        self.json_data().get("loyalty").map(|loy| match *loy {
            Json::Null => Number::X,
            Json::Number(ref n) => Number::from(n.as_u64().expect(&format!("invalid starting loyalty: {}", n))),
            ref v => { panic!("invalid starting loyalty: {}", v); }
        })
    }

    /// Returns the card's mana cost, or `None` if it has an unpayable cost.
    pub fn mana_cost(&self) -> Option<ManaCost> {
        self.json_data().get("manaCost").map(|mana_cost| mana_cost.as_str().expect("mana cost is not a string").parse().expect("invalid mana cost"))
    }

    /// Returns the number of different printings of the card. Faster than `card.printings_unsorted().len()`.
    pub fn num_printings(&self) -> usize {
        self.printings.len()
    }

    /// Returns all the different printings of the card, sorted chronologically if possible.
    ///
    /// Warning: this method may take a very long time to return for basic lands.
    pub fn printings(&self) -> impl IntoIterator<Item = Printing> {
        self.printings_unsorted().into_iter().collect::<TopologicalSort<_>>()
    }

    /// Returns all the different printings of the card, in no particular order.
    pub fn printings_unsorted(&self) -> Vec<Printing> {
        self.printings.iter().map(|printing_json| {
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

    /// Returns the card's printed power and toughness, if any.
    pub fn pt(&self) -> Option<(&str, &str)> {
        match (self.json_data().get("power").and_then(Json::as_str), self.json_data().get("toughness").and_then(Json::as_str)) {
            (Some(pow), Some(tou)) => Some((pow, tou)),
            _ => None
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

    /// Returns the contents of the card's Oracle text box.
    pub fn text(&self) -> &str {
        self.json_data().get("text").and_then(Json::as_str).unwrap_or("")
    }

    /// Returns the card's type line.
    pub fn type_line(&self) -> TypeLine {
        TypeLine::from_str(self.json_data()["type"].as_str().expect("type line is not a string")).expect("failed to parse type line")
    }

    /// Returns the card's hand and life modifier, if any.
    pub fn vanguard_modifiers(&self) -> Option<(i64, i64)> {
        match (self.json_data().get("hand").and_then(Json::as_i64), self.json_data().get("life").and_then(Json::as_i64)) {
            (Some(hand), Some(life)) => Some((hand, life)),
            _ => None
        }
    }
}

impl PartialEq for Card {
    fn eq(&self, other: &Card) -> bool {
        self.to_string() == other.to_string()
    }
}

impl Eq for Card {}

impl Hash for Card {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

impl fmt::Display for Card {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.json_data()["name"].as_str().expect("card name is not a string"))
    }
}
