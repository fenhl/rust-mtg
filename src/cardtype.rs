//! This module contains Rust types representing Magic card supertypes, types, and subtypes.

use {
    std::{
        cmp::Ordering,
        collections::HashSet,
        fmt,
        iter,
        ops::{
            BitOr,
            BitOrAssign
        },
        str::FromStr
    },
    derive_more::From,
    enum_iterator::IntoEnumIterator,
    itertools::Itertools,
    crate::color::{
        Color,
        ColorSet
    }
};

/// An error encountered while parsing a string into a `TypeLine`.
#[derive(Debug, From)]
pub enum ParseError {
    /// A word to the left of the dash was not recognized as a supertype or card type.
    #[from(ignore)]
    UnknownSupertypeOrCardType(String),
    /// The type line contained both Plane and another card type. Subtypes for this combination cannot be parsed.
    MultitypePlane,
    /// The right side of the type line was not recognized as a planar type.
    #[from(ignore)]
    UnknownPlanarType(String),
    /// A word to the right of the dash was not recognized as a subtype.
    #[from(ignore)]
    UnknownSubtype(String),
    /// See `InvalidTypeLineError`
    InvalidComponents(InvalidTypeLineError)
}

/// An error encountered while creating a `TypeLine` from its components.
#[derive(Debug)]
pub enum InvalidTypeLineError {
    /// Attempted to create a type line with artifact types but without the type “Artifact”.
    Artifact,
    /// Attempted to create a type line with enchantment types but without the type “Enchantment”.
    Enchantment,
    /// Attempted to create a type line with land types but without the type “Land”.
    Land,
    /// Attempted to create a type line with planeswalker types but without the type “Planeswalker”.
    Planeswalker,
    /// Attempted to create a type line with spell types but without the types “Instant” or “Sorcery”.
    Spell,
    /// Attempted to create a type line with creature types but without the types “Tribal” or “Creature”.
    Creature,
    /// Attempted to create a type line with planar types but without the type “Plane”.
    Planar
}

/// A type line, consisting of supertypes, types, and/or subtypes.
///
/// A safely created type line is guaranteed to only contain subtypes that each correspond to at least one of its types.
#[derive(Debug, Default, Clone)]
pub struct TypeLine {
    supertypes: HashSet<Supertype>,
    types: HashSet<CardType>,
    artifact_types: HashSet<ArtifactType>,
    enchantment_types: HashSet<EnchantmentType>,
    land_types: HashSet<LandType>,
    planeswalker_types: HashSet<PlaneswalkerType>,
    spell_types: HashSet<SpellType>,
    creature_types: Vec<CreatureType>,
    planar_types: HashSet<PlanarType>
}

impl TypeLine {
    /// Create a new type line from its components.
    ///
    /// The result will be valid in the sense that subtypes can only be present if a corresponding card type is present. If this invariant is violated, an error will be returned.
    pub fn new<S, T, A, E, L, W, I, C, P>(
        supertypes: S,
        types: T,
        artifact_types: A,
        enchantment_types: E,
        land_types: L,
        planeswalker_types: W,
        spell_types: I,
        creature_types: C,
        planar_types: P
    ) -> Result<TypeLine, InvalidTypeLineError>
    where
    S: IntoIterator<Item = Supertype>,
    T: IntoIterator<Item = CardType>,
    A: IntoIterator<Item = ArtifactType>,
    E: IntoIterator<Item = EnchantmentType>,
    L: IntoIterator<Item = LandType>,
    W: IntoIterator<Item = PlaneswalkerType>,
    I: IntoIterator<Item = SpellType>,
    C: IntoIterator<Item = CreatureType>,
    P: IntoIterator<Item = PlanarType> {
        let types = types.into_iter().collect::<HashSet<_>>();
        let artifact_types = artifact_types.into_iter().collect::<HashSet<_>>();
        if !artifact_types.is_empty() && !types.contains(&CardType::Artifact) {
            return Err(InvalidTypeLineError::Artifact);
        }
        let enchantment_types = enchantment_types.into_iter().collect::<HashSet<_>>();
        if !enchantment_types.is_empty() && !types.contains(&CardType::Enchantment) {
            return Err(InvalidTypeLineError::Enchantment);
        }
        let land_types = land_types.into_iter().collect::<HashSet<_>>();
        if !land_types.is_empty() && !types.contains(&CardType::Land) {
            return Err(InvalidTypeLineError::Land);
        }
        let planeswalker_types = planeswalker_types.into_iter().collect::<HashSet<_>>();
        if !planeswalker_types.is_empty() && !types.contains(&CardType::Planeswalker) {
            return Err(InvalidTypeLineError::Planeswalker);
        }
        let spell_types = spell_types.into_iter().collect::<HashSet<_>>();
        if !spell_types.is_empty() && !types.contains(&CardType::Instant) && !types.contains(&CardType::Sorcery) {
            return Err(InvalidTypeLineError::Spell);
        }
        let creature_types = creature_types.into_iter().collect::<Vec<_>>();
        if !creature_types.is_empty() && !types.contains(&CardType::Tribal) && !types.contains(&CardType::Creature) {
            return Err(InvalidTypeLineError::Creature);
        }
        let planar_types = planar_types.into_iter().collect::<HashSet<_>>();
        if !planar_types.is_empty() && !types.contains(&CardType::Plane) {
            return Err(InvalidTypeLineError::Planar);
        }
        Ok(TypeLine {
            supertypes: supertypes.into_iter().collect::<HashSet<_>>(),
            types: types,
            artifact_types: artifact_types,
            enchantment_types: enchantment_types,
            land_types: land_types,
            planeswalker_types: planeswalker_types,
            spell_types: spell_types,
            creature_types: crate::util::full_dedup(creature_types),
            planar_types: planar_types
        })
    }

    /// Returns all supertypes in this type line.
    pub fn supertypes(&self) -> &HashSet<Supertype> {
        &self.supertypes
    }

    /// Returns all card types in this type line.
    pub fn types(&self) -> &HashSet<CardType> {
        &self.types
    }

    /// Returns all artifact types in this type line.
    pub fn artifact_types(&self) -> &HashSet<ArtifactType> {
        &self.artifact_types
    }

    /// Returns all enchantment types in this type line.
    pub fn enchantment_types(&self) -> &HashSet<EnchantmentType> {
        &self.enchantment_types
    }

    /// Returns all land types in this type line.
    pub fn land_types(&self) -> &HashSet<LandType> {
        &self.land_types
    }

    /// Returns all planeswalker types in this type line.
    pub fn planeswalker_types(&self) -> &HashSet<PlaneswalkerType> {
        &self.planeswalker_types
    }

    /// Returns all spell types in this type line.
    pub fn spell_types(&self) -> &HashSet<SpellType> {
        &self.spell_types
    }

    /// Returns all creature types in this type line.
    pub fn creature_types(&self) -> &Vec<CreatureType> {
        &self.creature_types
    }

    /// Returns all planar types in this type line.
    pub fn planar_types(&self) -> &HashSet<PlanarType> {
        &self.planar_types
    }

    /// Returns true if the type line contains no supertypes, card types, or subtypes of any kind.
    pub fn is_empty(&self) -> bool {
        self.supertypes.is_empty()
        && self.types.is_empty()
        && self.artifact_types.is_empty()
        && self.enchantment_types.is_empty()
        && self.land_types.is_empty()
        && self.planeswalker_types.is_empty()
        && self.spell_types.is_empty()
        && self.creature_types.is_empty()
        && self.planar_types.is_empty()
    }

    /// Returns all the parts (supertypes, card types, and subtypes) of this type line, in the order they would be displayed on a card.
    pub fn parts(&self) -> (Vec<Supertype>, Vec<CardType>, Vec<Subtype>) {
        use self::CardType::*;
        use self::LandType::*;

        let supertypes = Supertype::iter_variants()
            .filter(|supertype| self.supertypes.contains(&supertype))
            .collect();
        let card_types = [Tribal, Instant, Sorcery, Enchantment, Artifact, Land, Creature, Planeswalker, Conspiracy, Scheme, Vanguard, Phenomenon, Plane]
            .iter()
            .filter(|card_type| self.types.contains(card_type))
            .copied()
            .collect();
        let mut subtypes = Vec::default();
        // creature types (for unanimated tribals)
        if !self.types.contains(&Creature) {
            subtypes.extend(self.creature_types.iter().copied().map(Subtype::from));
        }
        // spell types
        subtypes.extend(
            SpellType::iter_variants()
                .filter(|spell_type| self.spell_types.contains(spell_type))
                .map(Subtype::from)
        );
        // enchantment types
        subtypes.extend(
            EnchantmentType::iter_variants()
                .filter(|enchantment_type| self.enchantment_types.contains(enchantment_type))
                .map(Subtype::from)
        );
        // artifact types
        subtypes.extend(
            ArtifactType::iter_variants()
                .filter(|artifact_type| self.artifact_types.contains(artifact_type))
                .map(Subtype::from)
        );
        // Urza's
        if self.land_types.contains(&Urzas) {
            subtypes.push(Subtype::from(Urzas));
        }
        // basic land types
        let land_types = ColorSet::from([
            self.land_types.contains(&Plains),
            self.land_types.contains(&Island),
            self.land_types.contains(&Swamp),
            self.land_types.contains(&Mountain),
            self.land_types.contains(&Forest)
        ]);
        subtypes.extend(
            land_types.canonical_order()
                .into_iter()
                .map(|color| Subtype::from(LandType::from(color)))
        );
        // nonbasic land types (other than Urza's)
        let nonbasic_land_types = LandType::iter_variants().filter(|land_type| ![Urzas, Plains, Island, Swamp, Mountain, Forest].contains(land_type));
        subtypes.extend(
            nonbasic_land_types
                .filter(|land_type| self.land_types.contains(land_type))
                .map(Subtype::from)
        );
        // creature types (for actual creatures)
        if self.types.contains(&Creature) {
            subtypes.extend(self.creature_types.iter().copied().map(Subtype::from));
        }
        // planeswalker types
        subtypes.extend(
            PlaneswalkerType::iter_variants()
                .filter(|planeswalker_type| self.planeswalker_types.contains(planeswalker_type))
                .map(Subtype::from)
        );
        // planar types
        subtypes.extend(
            PlanarType::iter_variants()
                .filter(|planar_type| self.planar_types.contains(planar_type))
                .map(Subtype::from)
        );
        (supertypes, card_types, subtypes)
    }
}

/// Parses a type line printed on a card.
///
/// As such, strings returned by `TypeLine::to_string` may not be accepted if they do not resemble type lines printed on cards. For example, the current implementation rejects combinations of planar types and other subtypes.
impl FromStr for TypeLine {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<TypeLine, ParseError> {
        let (supertypes_and_types, subtypes) = if s.contains(" \u{2014} ") {
            let mut splitn = s.splitn(2, " \u{2014} ");
            (splitn.next().unwrap(), splitn.next().unwrap())
        } else {
            (s, "")
        };
        let mut supertypes = HashSet::<Supertype>::default();
        let mut types = HashSet::<CardType>::default();
        let mut artifact_types = HashSet::<ArtifactType>::default();
        let mut enchantment_types = HashSet::<EnchantmentType>::default();
        let mut land_types = HashSet::<LandType>::default();
        let mut planeswalker_types = HashSet::<PlaneswalkerType>::default();
        let mut spell_types = HashSet::<SpellType>::default();
        let mut creature_types = Vec::<CreatureType>::default();
        let mut planar_types = HashSet::<PlanarType>::default();
        if !supertypes_and_types.is_empty() {
            for supertype_or_type in supertypes_and_types.split(' ') {
                if let Ok(supertype) = Supertype::from_str(supertype_or_type) {
                    supertypes.insert(supertype);
                } else if let Ok(card_type) = CardType::from_str(supertype_or_type) {
                    types.insert(card_type);
                } else {
                    return Err(ParseError::UnknownSupertypeOrCardType(supertype_or_type.to_owned()));
                }
            }
        }
        if !subtypes.is_empty() {
            if types.contains(&CardType::Plane) && types.len() > 1 {
                return Err(ParseError::MultitypePlane);
            } else if types.contains(&CardType::Plane) {
                if let Ok(planar_type) = PlanarType::from_str(subtypes) {
                    planar_types.insert(planar_type);
                } else {
                    return Err(ParseError::UnknownPlanarType(subtypes.to_owned()));
                }
            } else {
                for subtype in subtypes.split(' ') {
                    if let Ok(artifact_type) = ArtifactType::from_str(subtype) {
                        artifact_types.insert(artifact_type);
                    } else if let Ok(ench_type) = EnchantmentType::from_str(subtype) {
                        enchantment_types.insert(ench_type);
                    } else if let Ok(land_type) = LandType::from_str(subtype) {
                        land_types.insert(land_type);
                    } else if let Ok(planeswalker_type) = PlaneswalkerType::from_str(subtype) {
                        planeswalker_types.insert(planeswalker_type);
                    } else if let Ok(spell_type) = SpellType::from_str(subtype) {
                        spell_types.insert(spell_type);
                    } else if let Ok(creature_type) = CreatureType::from_str(subtype) {
                        creature_types.push(creature_type);
                    } else {
                        return Err(ParseError::UnknownSubtype(subtype.to_owned()));
                    }
                }
            }
        }
        Ok(TypeLine::new(supertypes, types, artifact_types, enchantment_types, land_types, planeswalker_types, spell_types, creature_types, planar_types)?)
    }
}

impl fmt::Display for TypeLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (supertypes, card_types, subtypes) = self.parts();
        write!(f, "{}", supertypes.into_iter().map(|supertype| supertype.to_string()).chain(card_types.into_iter().map(|card_type| card_type.to_string())).join(" "))?;
        if !subtypes.is_empty() {
            write!(f, " — {}", subtypes.into_iter().map(|subtype| subtype.to_string()).join(" "))?;
        }
        Ok(())
    }
}

impl<T: Clone + Into<TypeLine>> PartialEq<T> for TypeLine {
    fn eq(&self, other: &T) -> bool {
        let other = other.clone().into();
        self.supertypes == other.supertypes &&
        self.types == other.types &&
        self.artifact_types == other.artifact_types &&
        self.enchantment_types == other.enchantment_types &&
        self.land_types == other.land_types &&
        self.planeswalker_types == other.planeswalker_types &&
        self.spell_types == other.spell_types &&
        self.creature_types.iter().collect::<HashSet<_>>() == other.creature_types.iter().collect::<HashSet<_>>() &&
        self.planar_types == other.planar_types
    }
}

impl Eq for TypeLine {}

impl<T: Clone + Into<TypeLine>> PartialOrd<T> for TypeLine {
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        let other = other.clone().into();
        let mut less = false;
        let mut greater = false;

        macro_rules! cmp_type {
            ($attr:ident) => {
                match (self.$attr.is_subset(&other.$attr), self.$attr.is_superset(&other.$attr)) {
                    (false, false) => { return None; }
                    (false, true) => { greater = true; }
                    (true, false) => { less = true; }
                    (true, true) => ()
                }
            };
        }

        cmp_type!(supertypes);
        cmp_type!(types);
        cmp_type!(artifact_types);
        cmp_type!(enchantment_types);
        cmp_type!(land_types);
        cmp_type!(planeswalker_types);
        cmp_type!(spell_types);
        cmp_type!(planar_types);

        match (self.creature_types.iter().collect::<HashSet<_>>().is_subset(&other.creature_types.iter().collect::<HashSet<_>>()), self.creature_types.iter().collect::<HashSet<_>>().is_superset(&other.creature_types.iter().collect::<HashSet<_>>())) {
            (false, false) => { return None; }
            (false, true) => { greater = true; }
            (true, false) => { less = true; }
            (true, true) => ()
        }

        match (less, greater) {
            (true, false) => Some(Ordering::Less),
            (false, false) => Some(Ordering::Equal),
            (false, true) => Some(Ordering::Greater),
            (true, true) => None
        }
    }
}

impl<'a, T: Into<TypeLine>> BitOr<T> for TypeLine {
    type Output = TypeLine;

    fn bitor(self, rhs: T) -> TypeLine {
        &self | rhs
    }
}

impl<'a, T: Into<TypeLine>> BitOr<T> for &'a TypeLine {
    type Output = TypeLine;

    fn bitor(self, rhs: T) -> TypeLine {
        let rhs = rhs.into();
        TypeLine {
            supertypes: &self.supertypes | &rhs.supertypes,
            types: &self.types | &rhs.types,
            artifact_types: &self.artifact_types | &rhs.artifact_types,
            enchantment_types: &self.enchantment_types | &rhs.enchantment_types,
            land_types: &self.land_types | &rhs.land_types,
            planeswalker_types: &self.planeswalker_types | &rhs.planeswalker_types,
            spell_types: &self.spell_types | &rhs.spell_types,
            creature_types: crate::util::full_dedup(self.creature_types.iter().cloned().chain(rhs.creature_types).collect()),
            planar_types: &self.planar_types | &rhs.planar_types
        }
    }
}

impl<T: Into<TypeLine>> BitOrAssign<T> for TypeLine {
    fn bitor_assign(&mut self, rhs: T) {
        *self = &*self | rhs;
    }
}

macro_rules! type_enum {
    (#[$outer_doc:meta] pub enum $ty:ident { $(#[$doc:meta] $variant:ident($name:expr $(, $alt:pat)*)),* } part $part_id:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IntoEnumIterator)]
        #[$outer_doc]
        pub enum $ty { $(#[$doc] $variant),* }
        impl $ty {
            /// Returns an iterator over all variants of this enum.
            pub fn iter_variants() -> impl Iterator<Item = Self> {
                $ty::into_enum_iter()
            }
        }
        impl FromStr for $ty {
            type Err = ();
            fn from_str(s: &str) -> Result<$ty, ()> {
                match s {
                    $($name $(| $alt)* => Ok($ty::$variant),)*
                    _ => Err(())
                }
            }
        }
        impl fmt::Display for $ty {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", match *self { $($ty::$variant => $name),* })
            }
        }
        impl From<$ty> for TypeLine {
            fn from(part: $ty) -> TypeLine {
                let part_iter = iter::once(part);
                TypeLine {
                    $part_id: part_iter.collect(),
                    ..TypeLine::default()
                }
            }
        }
    };
    (#[$outer_doc:meta] pub enum $ty:ident { $(#[$doc:meta] $variant:ident($name:expr $(, $alt:pat)*)),* custom $(#[$custom_doc:meta] $custom_variant:ident($custom_name:expr $(, $custom_alt:pat)*)),* } part $part_id:ident) => {
        #[cfg(not(feature = "custom"))]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IntoEnumIterator)]
        #[$outer_doc]
        pub enum $ty { $(#[$doc] $variant),* }
        #[cfg(feature = "custom")]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IntoEnumIterator)]
        #[$outer_doc]
        pub enum $ty { $(#[$doc] $variant,)* $(#[$custom_doc] $custom_variant),* }
        impl $ty {
            /// Returns an iterator over all variants of this enum.
            pub fn iter_variants() -> impl Iterator<Item = Self> {
                $ty::into_enum_iter()
            }
        }
        impl FromStr for $ty {
            type Err = ();
            #[cfg(not(feature = "custom"))]
            fn from_str(s: &str) -> Result<$ty, ()> {
                match s {
                    $($name $(| $alt)* => Ok($ty::$variant),)*
                    _ => Err(())
                }
            }
            #[cfg(feature = "custom")]
            fn from_str(s: &str) -> Result<$ty, ()> {
                match s {
                    $($name $(| $alt)* => Ok($ty::$variant),)*
                    $($custom_name $(| $custom_alt)* => Ok($ty::$custom_variant),)*
                    _ => Err(())
                }
            }
        }
        impl fmt::Display for $ty {
            #[cfg(not(feature = "custom"))]
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", match *self { $($ty::$variant => $name),* })
            }
            #[cfg(feature = "custom")]
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", match *self { $($ty::$variant => $name,)* $($ty::$custom_variant => $custom_name),* })
            }
        }
        impl From<$ty> for TypeLine {
            fn from(part: $ty) -> TypeLine {
                let part_iter = iter::once(part);
                TypeLine {
                    $part_id: part_iter.collect(),
                    ..TypeLine::default()
                }
            }
        }
    };
}

type_enum! {
    /// A [card type](https://mtg.gamepedia.com/Card_type).
    pub enum CardType {
        /// Artifact
        Artifact("Artifact"),
        /// Conspiracy
        Conspiracy("Conspiracy"),
        /// Creature
        Creature("Creature"),
        /// Enchantment
        Enchantment("Enchantment"),
        /// Instant
        Instant("Instant"),
        /// Land
        Land("Land"),
        /// Phenomenon
        Phenomenon("Phenomenon"),
        /// Plane
        Plane("Plane"),
        /// Planeswalker
        Planeswalker("Planeswalker"),
        /// Scheme
        Scheme("Scheme"),
        /// Sorcery
        Sorcery("Sorcery"),
        /// Tribal
        Tribal("Tribal"),
        /// Vanguard
        Vanguard("Vanguard")
    } part types
}

type_enum! {
    /// A [supertype](https://mtg.gamepedia.com/Supertype).
    pub enum Supertype {
        /// Basic
        Basic("Basic"),
        /// Legendary
        Legendary("Legendary"),
        /// Ongoing
        Ongoing("Ongoing"),
        /// Snow
        Snow("Snow"),
        /// World
        World("World")
    } part supertypes
}

type_enum! {
    /// An [artifact](https://mtg.gamepedia.com/Artifact) subtype.
    pub enum ArtifactType {
        /// Clue
        Clue("Clue"),
        /// Contraption
        Contraption("Contraption"),
        /// Equipment
        Equipment("Equipment"),
        /// Food
        Food("Food"),
        /// Fortification
        Fortification("Fortification"),
        /// Gold
        Gold("Gold"),
        /// Treasure
        Treasure("Treasure"),
        /// Vehicle
        Vehicle("Vehicle")

        custom

        /// Bullet
        Bullet("Bullet"),
        /// Canister
        Canister("Canister"),
        /// Monument
        Monument("Monument"),
        /// Relic
        Relic("Relic"),
        /// Structure
        Structure("Structure")
    } part artifact_types
}

type_enum! {
    /// An [enchantment](https://mtg.gamepedia.com/Enchantment) subtype.
    pub enum EnchantmentType {
        /// Aura
        Aura("Aura"),
        /// Cartouche
        Cartouche("Cartouche"),
        /// Curse
        Curse("Curse"),
        /// Saga
        Saga("Saga"),
        /// Shrine
        Shrine("Shrine")

        custom

        /// Cloister
        Cloister("Cloister"),
        /// Discovery
        Discovery("Discovery")
    } part enchantment_types
}

type_enum! {
    /// A [land](https://mtg.gamepedia.com/Land) subtype.
    pub enum LandType {
        /// Desert
        Desert("Desert"),
        /// Forest, a basic land type corresponding to the color green.
        Forest("Forest"),
        /// Gate
        Gate("Gate"),
        /// Island, a basic land type corresponding to the color blue.
        Island("Island"),
        /// Lair
        Lair("Lair"),
        /// Locus
        Locus("Locus"),
        /// Mine
        Mine("Mine"),
        /// Mountain, a basic land type corresponding to the color red.
        Mountain("Mountain"),
        /// Plains, a basic land type corresponding to the color white.
        Plains("Plains"),
        /// Power-Plant
        PowerPlant("Power-Plant"),
        /// Swamp, a basic land type corresponding to the color black.
        Swamp("Swamp"),
        /// Tower
        Tower("Tower"),
        /// Urza's
        Urzas("Urza's", "Urza\u{2019}s")
    } part land_types
}

/// Converts a color to the corresponding basic land type.
impl From<Color> for LandType {
    fn from(color: Color) -> LandType {
        match color {
            Color::White => LandType::Plains,
            Color::Blue => LandType::Island,
            Color::Black => LandType::Swamp,
            Color::Red => LandType::Mountain,
            Color::Green => LandType::Forest
        }
    }
}

type_enum! {
    /// A [planeswalker](https://mtg.gamepedia.com/Planeswalker) subtype.
    pub enum PlaneswalkerType {
        /// Ajani
        Ajani("Ajani"),
        /// Aminatou
        Aminatou("Aminatou"),
        /// Angrath
        Angrath("Angrath"),
        /// Arlinn
        Arlinn("Arlinn"),
        /// Ashiok
        Ashiok("Ashiok"),
        /// Basri
        Basri("Basri"),
        /// Bolas
        Bolas("Bolas"),
        /// Calix
        Calix("Calix"),
        /// Chandra
        Chandra("Chandra"),
        /// Dack
        Dack("Dack"),
        /// Daretti
        Daretti("Daretti"),
        /// Davriel
        Davriel("Davriel"),
        /// Domri
        Domri("Domri"),
        /// Dovin
        Dovin("Dovin"),
        /// Elspeth
        Elspeth("Elspeth"),
        /// Estrid
        Estrid("Estrid"),
        /// Freyalise
        Freyalise("Freyalise"),
        /// Garruk
        Garruk("Garruk"),
        /// Gideon
        Gideon("Gideon"),
        /// Huatli
        Huatli("Huatli"),
        /// Jace
        Jace("Jace"),
        /// Jaya
        Jaya("Jaya"),
        /// Karn
        Karn("Karn"),
        /// Kasmina
        Kasmina("Kasmina"),
        /// Kaya
        Kaya("Kaya"),
        /// Kiora
        Kiora("Kiora"),
        /// Koth
        Koth("Koth"),
        /// Liliana
        Liliana("Liliana"),
        /// Lukka
        Lukka("Lukka"),
        /// Nahiri
        Nahiri("Nahiri"),
        /// Narset
        Narset("Narset"),
        /// Nissa
        Nissa("Nissa"),
        /// Nixilis
        Nixilis("Nixilis"),
        /// Oko
        Oko("Oko"),
        /// Ral
        Ral("Ral"),
        /// Rowan
        Rowan("Rowan"),
        /// Saheeli
        Saheeli("Saheeli"),
        /// Samut
        Samut("Samut"),
        /// Sarkhan
        Sarkhan("Sarkhan"),
        /// Serra
        Serra("Serra"),
        /// Sorin
        Sorin("Sorin"),
        /// Tamiyo
        Tamiyo("Tamiyo"),
        /// Teferi
        Teferi("Teferi"),
        /// Teyo
        Teyo("Teyo"),
        /// Tezzeret
        Tezzeret("Tezzeret"),
        /// Tibalt
        Tibalt("Tibalt"),
        /// Ugin
        Ugin("Ugin"),
        /// Vaen
        Vaen("Vaen"),
        /// Venser
        Venser("Venser"),
        /// Vivien
        Vivien("Vivien"),
        /// Vraska
        Vraska("Vraska"),
        /// Will
        Will("Will"),
        /// Windgrace
        Windgrace("Windgrace"),
        /// Wrenn
        Wrenn("Wrenn"),
        /// Xenagos
        Xenagos("Xenagos"),
        /// Yanggu
        Yanggu("Yanggu"),
        /// Yanling
        Yanling("Yanling")

        custom

        /// Ameret
        Ameret("Ameret"),
        /// Cypress
        Cypress("Cypress"),
        /// Eli
        Eli("Eli"),
        /// Evis
        Evis("Evis"),
        /// Farajo
        Farajo("Farajo"),
        /// Faralyn
        Faralyn("Faralyn"),
        /// Foug
        Foug("Foug"),
        /// Gaile
        Gaile("Gaile"),
        /// Gozani
        Gozani("Gozani"),
        /// Helena
        Helena("Helena"),
        /// Hohoja
        Hohoja("Hohoja"),
        /// Jericho
        Jericho("Jericho"),
        /// L3-T
        L3T("L3-T"),
        /// Lazaire
        Lazaire("Lazaire"),
        /// Lhosei
        Lhosei("Lhosei"),
        /// Luma
        Luma("Luma"),
        /// Nibinem
        Nibinem("Nibinem"),
        /// Nic
        Nic("Nic"),
        /// Patches
        Patches("Patches"),
        /// Roselyn
        Roselyn("Roselyn"),
        /// Sarasa
        Sarasa("Sarasa"),
        /// Shala
        Shala("Shala"),
        /// Sobki
        Sobki("Sobki"),
        /// Talya
        Talya("Talya"),
        /// Toahanga
        Toahanga("Toahanga"),
        /// Veila
        Veila("Veila"),
        /// Wulani
        Wulani("Wulani"),
        /// Yemma
        Yemma("Yemma")
    } part planeswalker_types
}

type_enum! {
    /// An [instant](https://mtg.gamepedia.com/Instant) or [sorcery](https://mtg.gamepedia.com/Sorcery) subtype.
    pub enum SpellType {
        /// Adventure
        Adventure("Adventure"),
        /// Arcane
        Arcane("Arcane"),
        /// Trap
        Trap("Trap")
    } part spell_types
}

type_enum! {
    /// A [creature](https://mtg.gamepedia.com/Creature) or [tribal](https://mtg.gamepedia.com/Tribal) subtype.
    pub enum CreatureType {
        /// Advisor
        Advisor("Advisor"),
        /// Aetherborn
        Aetherborn("Aetherborn"),
        /// Ally
        Ally("Ally"),
        /// Angel
        Angel("Angel"),
        /// Antelope
        Antelope("Antelope"),
        /// Ape
        Ape("Ape"),
        /// Archer
        Archer("Archer"),
        /// Archon
        Archon("Archon"),
        /// Army
        Army("Army"),
        /// Artificer
        Artificer("Artificer"),
        /// Assassin
        Assassin("Assassin"),
        /// Assembly-Worker
        AssemblyWorker("Assembly-Worker"),
        /// Atog
        Atog("Atog"),
        /// Aurochs
        Aurochs("Aurochs"),
        /// Avatar
        Avatar("Avatar"),
        /// Azra
        Azra("Azra"),
        /// Badger
        Badger("Badger"),
        /// Barbarian
        Barbarian("Barbarian"),
        /// Basilisk
        Basilisk("Basilisk"),
        /// Bat
        Bat("Bat"),
        /// Bear
        Bear("Bear"),
        /// Beast
        Beast("Beast"),
        /// Beeble
        Beeble("Beeble"),
        /// Berserker
        Berserker("Berserker"),
        /// Bird
        Bird("Bird"),
        /// Blinkmoth
        Blinkmoth("Blinkmoth"),
        /// Boar
        Boar("Boar"),
        /// Bringer
        Bringer("Bringer"),
        /// Brushwagg
        Brushwagg("Brushwagg"),
        /// Camarid
        Camarid("Camarid"),
        /// Camel
        Camel("Camel"),
        /// Caribou
        Caribou("Caribou"),
        /// Carrier
        Carrier("Carrier"),
        /// Cat
        Cat("Cat"),
        /// Centaur
        Centaur("Centaur"),
        /// Cephalid
        Cephalid("Cephalid"),
        /// Chimera
        Chimera("Chimera"),
        /// Citizen
        Citizen("Citizen"),
        /// Cleric
        Cleric("Cleric"),
        /// Cockatrice
        Cockatrice("Cockatrice"),
        /// Construct
        Construct("Construct"),
        /// Coward
        Coward("Coward"),
        /// Crab
        Crab("Crab"),
        /// Crocodile
        Crocodile("Crocodile"),
        /// Cyclops
        Cyclops("Cyclops"),
        /// Dauthi
        Dauthi("Dauthi"),
        /// Demigod
        Demigod("Demigod"),
        /// Demon
        Demon("Demon"),
        /// Deserter
        Deserter("Deserter"),
        /// Devil
        Devil("Devil"),
        /// Dinosaur
        Dinosaur("Dinosaur"),
        /// Djinn
        Djinn("Djinn"),
        /// Dog
        Dog("Dog"),
        /// Dragon
        Dragon("Dragon"),
        /// Drake
        Drake("Drake"),
        /// Dreadnought
        Dreadnought("Dreadnought"),
        /// Drone
        Drone("Drone"),
        /// Druid
        Druid("Druid"),
        /// Dryad
        Dryad("Dryad"),
        /// Dwarf
        Dwarf("Dwarf"),
        /// Efreet
        Efreet("Efreet"),
        /// Egg
        Egg("Egg"),
        /// Elder
        Elder("Elder"),
        /// Eldrazi
        Eldrazi("Eldrazi"),
        /// Elemental
        Elemental("Elemental"),
        /// Elephant
        Elephant("Elephant"),
        /// Elf
        Elf("Elf"),
        /// Elk
        Elk("Elk"),
        /// Eye
        Eye("Eye"),
        /// Faerie
        Faerie("Faerie"),
        /// Ferret
        Ferret("Ferret"),
        /// Fish
        Fish("Fish"),
        /// Flagbearer
        Flagbearer("Flagbearer"),
        /// Fox
        Fox("Fox"),
        /// Frog
        Frog("Frog"),
        /// Fungus
        Fungus("Fungus"),
        /// Gargoyle
        Gargoyle("Gargoyle"),
        /// Germ
        Germ("Germ"),
        /// Giant
        Giant("Giant"),
        /// Gnome
        Gnome("Gnome"),
        /// Goat
        Goat("Goat"),
        /// Goblin
        Goblin("Goblin"),
        /// God
        God("God"),
        /// Golem
        Golem("Golem"),
        /// Gorgon
        Gorgon("Gorgon"),
        /// Graveborn
        Graveborn("Graveborn"),
        /// Gremlin
        Gremlin("Gremlin"),
        /// Griffin
        Griffin("Griffin"),
        /// Hag
        Hag("Hag"),
        /// Harpy
        Harpy("Harpy"),
        /// Hellion
        Hellion("Hellion"),
        /// Hippo
        Hippo("Hippo"),
        /// Hippogriff
        Hippogriff("Hippogriff"),
        /// Homarid
        Homarid("Homarid"),
        /// Homunculus
        Homunculus("Homunculus"),
        /// Horror
        Horror("Horror"),
        /// Horse
        Horse("Horse"),
        /// Human
        Human("Human"),
        /// Hydra
        Hydra("Hydra"),
        /// Hyena
        Hyena("Hyena"),
        /// Illusion
        Illusion("Illusion"),
        /// Imp
        Imp("Imp"),
        /// Incarnation
        Incarnation("Incarnation"),
        /// Insect
        Insect("Insect"),
        /// Jackal
        Jackal("Jackal"),
        /// Jellyfish
        Jellyfish("Jellyfish"),
        /// Juggernaut
        Juggernaut("Juggernaut"),
        /// Kavu
        Kavu("Kavu"),
        /// Kirin
        Kirin("Kirin"),
        /// Kithkin
        Kithkin("Kithkin"),
        /// Knight
        Knight("Knight"),
        /// Kobold
        Kobold("Kobold"),
        /// Kor
        Kor("Kor"),
        /// Kraken
        Kraken("Kraken"),
        /// Lamia
        Lamia("Lamia"),
        /// Lammasu
        Lammasu("Lammasu"),
        /// Leech
        Leech("Leech"),
        /// Leviathan
        Leviathan("Leviathan"),
        /// Lhurgoyf
        Lhurgoyf("Lhurgoyf"),
        /// Licid
        Licid("Licid"),
        /// Lizard
        Lizard("Lizard"),
        /// Manticore
        Manticore("Manticore"),
        /// Masticore
        Masticore("Masticore"),
        /// Mercenary
        Mercenary("Mercenary"),
        /// Merfolk
        Merfolk("Merfolk"),
        /// Metathran
        Metathran("Metathran"),
        /// Minion
        Minion("Minion"),
        /// Minotaur
        Minotaur("Minotaur"),
        /// Mole
        Mole("Mole"),
        /// Monger
        Monger("Monger"),
        /// Mongoose
        Mongoose("Mongoose"),
        /// Monk
        Monk("Monk"),
        /// Monkey
        Monkey("Monkey"),
        /// Moonfolk
        Moonfolk("Moonfolk"),
        /// Mouse
        Mouse("Mouse"),
        /// Mutant
        Mutant("Mutant"),
        /// Myr
        Myr("Myr"),
        /// Mystic
        Mystic("Mystic"),
        /// Naga
        Naga("Naga"),
        /// Nautilus
        Nautilus("Nautilus"),
        /// Nephilim
        Nephilim("Nephilim"),
        /// Nightmare
        Nightmare("Nightmare"),
        /// Nightstalker
        Nightstalker("Nightstalker"),
        /// Ninja
        Ninja("Ninja"),
        /// Noble
        Noble("Noble"),
        /// Noggle
        Noggle("Noggle"),
        /// Nomad
        Nomad("Nomad"),
        /// Nymph
        Nymph("Nymph"),
        /// Octopus
        Octopus("Octopus"),
        /// Ogre
        Ogre("Ogre"),
        /// Ooze
        Ooze("Ooze"),
        /// Orb
        Orb("Orb"),
        /// Orc
        Orc("Orc"),
        /// Orgg
        Orgg("Orgg"),
        /// Otter
        Otter("Otter"),
        /// Ouphe
        Ouphe("Ouphe"),
        /// Ox
        Ox("Ox"),
        /// Oyster
        Oyster("Oyster"),
        /// Pangolin
        Pangolin("Pangolin"),
        /// Peasant
        Peasant("Peasant"),
        /// Pegasus
        Pegasus("Pegasus"),
        /// Pentavite
        Pentavite("Pentavite"),
        /// Pest
        Pest("Pest"),
        /// Phelddagrif
        Phelddagrif("Phelddagrif"),
        /// Phoenix
        Phoenix("Phoenix"),
        /// Pilot
        Pilot("Pilot"),
        /// Pincher
        Pincher("Pincher"),
        /// Pirate
        Pirate("Pirate"),
        /// Plant
        Plant("Plant"),
        /// Praetor
        Praetor("Praetor"),
        /// Prism
        Prism("Prism"),
        /// Processor
        Processor("Processor"),
        /// Rabbit
        Rabbit("Rabbit"),
        /// Rat
        Rat("Rat"),
        /// Rebel
        Rebel("Rebel"),
        /// Reflection
        Reflection("Reflection"),
        /// Rhino
        Rhino("Rhino"),
        /// Rigger
        Rigger("Rigger"),
        /// Rogue
        Rogue("Rogue"),
        /// Sable
        Sable("Sable"),
        /// Salamander
        Salamander("Salamander"),
        /// Samurai
        Samurai("Samurai"),
        /// Sand
        Sand("Sand"),
        /// Saproling
        Saproling("Saproling"),
        /// Satyr
        Satyr("Satyr"),
        /// Scarecrow
        Scarecrow("Scarecrow"),
        /// Scion
        Scion("Scion"),
        /// Scorpion
        Scorpion("Scorpion"),
        /// Scout
        Scout("Scout"),
        /// Serf
        Serf("Serf"),
        /// Serpent
        Serpent("Serpent"),
        /// Servo
        Servo("Servo"),
        /// Shade
        Shade("Shade"),
        /// Shaman
        Shaman("Shaman"),
        /// Shapeshifter
        Shapeshifter("Shapeshifter"),
        /// Shark
        Shark("Shark"),
        /// Sheep
        Sheep("Sheep"),
        /// Siren
        Siren("Siren"),
        /// Skeleton
        Skeleton("Skeleton"),
        /// Slith
        Slith("Slith"),
        /// Sliver
        Sliver("Sliver"),
        /// Slug
        Slug("Slug"),
        /// Snake
        Snake("Snake"),
        /// Soldier
        Soldier("Soldier"),
        /// Soltari
        Soltari("Soltari"),
        /// Spawn
        Spawn("Spawn"),
        /// Specter
        Specter("Specter"),
        /// Spellshaper
        Spellshaper("Spellshaper"),
        /// Sphinx
        Sphinx("Sphinx"),
        /// Spider
        Spider("Spider"),
        /// Spike
        Spike("Spike"),
        /// Spirit
        Spirit("Spirit"),
        /// Splinter
        Splinter("Splinter"),
        /// Sponge
        Sponge("Sponge"),
        /// Squid
        Squid("Squid"),
        /// Squirrel
        Squirrel("Squirrel"),
        /// Starfish
        Starfish("Starfish"),
        /// Surrakar
        Surrakar("Surrakar"),
        /// Survivor
        Survivor("Survivor"),
        /// Tentacle
        Tentacle("Tentacle"),
        /// Tetravite
        Tetravite("Tetravite"),
        /// Thalakos
        Thalakos("Thalakos"),
        /// Thopter
        Thopter("Thopter"),
        /// Thrull
        Thrull("Thrull"),
        /// Treefolk
        Treefolk("Treefolk"),
        /// Trilobite
        Trilobite("Trilobite"),
        /// Triskelavite
        Triskelavite("Triskelavite"),
        /// Troll
        Troll("Troll"),
        /// Turtle
        Turtle("Turtle"),
        /// Unicorn
        Unicorn("Unicorn"),
        /// Vampire
        Vampire("Vampire"),
        /// Vedalken
        Vedalken("Vedalken"),
        /// Viashino
        Viashino("Viashino"),
        /// Volver
        Volver("Volver"),
        /// Wall
        Wall("Wall"),
        /// Warlock
        Warlock("Warlock"),
        /// Warrior
        Warrior("Warrior"),
        /// Weird
        Weird("Weird"),
        /// Werewolf
        Werewolf("Werewolf"),
        /// Whale
        Whale("Whale"),
        /// Wizard
        Wizard("Wizard"),
        /// Wolf
        Wolf("Wolf"),
        /// Wolverine
        Wolverine("Wolverine"),
        /// Wombat
        Wombat("Wombat"),
        /// Worm
        Worm("Worm"),
        /// Wraith
        Wraith("Wraith"),
        /// Wurm
        Wurm("Wurm"),
        /// Yeti
        Yeti("Yeti"),
        /// Zombie
        Zombie("Zombie"),
        /// Zubera
        Zubera("Zubera")

        custom

        /// Bard
        Bard("Bard"),
        /// Coral
        Coral("Coral"),
        /// Dolphin
        Dolphin("Dolphin"),
        /// Farmer
        Farmer("Farmer"),
        /// Giraffe
        Giraffe("Giraffe"),
        //TODO remove once all custom sets are updated
        /// Hound
        Hound("Hound"),
        /// Mech
        Mech("Mech"),
        /// Nanobot
        Nanobot("Nanobot"),
        /// Ship
        Ship("Ship")
    } part creature_types
}

type_enum! {
    /// A [plane](https://mtg.gamepedia.com/Plane) subtype.
    pub enum PlanarType {
        /// Alara
        Alara("Alara"),
        /// Arkhos
        Arkhos("Arkhos"),
        /// Azgol
        Azgol("Azgol"),
        /// Belenon
        Belenon("Belenon"),
        /// Bolas's Meditation Realm
        BolassMeditationRealm("Bolas's Meditation Realm", "Bolas\u{2019}s Meditation Realm"),
        /// Dominaria
        Dominaria("Dominaria"),
        /// Equilor
        Equilor("Equilor"),
        /// Ergamon
        Ergamon("Ergamon"),
        /// Fabacin
        Fabacin("Fabacin"),
        /// Innistrad
        Innistrad("Innistrad"),
        /// Iquatana
        Iquatana("Iquatana"),
        /// Ir
        Ir("Ir"),
        /// Kaldheim
        Kaldheim("Kaldheim"),
        /// Kamigawa
        Kamigawa("Kamigawa"),
        /// Karsus
        Karsus("Karsus"),
        /// Kephalai
        Kephalai("Kephalai"),
        /// Kinshala
        Kinshala("Kinshala"),
        /// Kolbahan
        Kolbahan("Kolbahan"),
        /// Kyneth
        Kyneth("Kyneth"),
        /// Lorwyn
        Lorwyn("Lorwyn"),
        /// Luvion
        Luvion("Luvion"),
        /// Mercadia
        Mercadia("Mercadia"),
        /// Mirrodin
        Mirrodin("Mirrodin"),
        /// Moag
        Moag("Moag"),
        /// Mongseng
        Mongseng("Mongseng"),
        /// Muraganda
        Muraganda("Muraganda"),
        /// New Phyrexia
        NewPhyrexia("New Phyrexia"),
        /// Phyrexia
        Phyrexia("Phyrexia"),
        /// Pyrulea
        Pyrulea("Pyrulea"),
        /// Rabiah
        Rabiah("Rabiah"),
        /// Rath
        Rath("Rath"),
        /// Ravnica
        Ravnica("Ravnica"),
        /// Regatha
        Regatha("Regatha"),
        /// Segovia
        Segovia("Segovia"),
        /// Serra's Realm
        SerrasRealm("Serra's Realm", "Serra\u{2019}s Realm"),
        /// Shadowmoor
        Shadowmoor("Shadowmoor"),
        /// Shandalar
        Shandalar("Shandalar"),
        /// Ulgrotha
        Ulgrotha("Ulgrotha"),
        /// Valla
        Valla("Valla"),
        /// Vryn
        Vryn("Vryn"),
        /// Wildfire
        Wildfire("Wildfire"),
        /// Xerex
        Xerex("Xerex"),
        /// Zendikar
        Zendikar("Zendikar")
    } part planar_types
}

/// A subtype of any card type.
#[derive(Debug, From, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(missing_docs)]
pub enum Subtype {
    Artifact(ArtifactType),
    Enchantment(EnchantmentType),
    Land(LandType),
    Planeswalker(PlaneswalkerType),
    Spell(SpellType),
    Creature(CreatureType),
    Planar(PlanarType)
}

impl FromStr for Subtype {
    type Err = ();

    fn from_str(s: &str) -> Result<Subtype, ()> {
        if let Ok(artifact_type) = ArtifactType::from_str(s) { return Ok(Subtype::Artifact(artifact_type)); }
        if let Ok(enchantment_type) = EnchantmentType::from_str(s) { return Ok(Subtype::Enchantment(enchantment_type)); }
        if let Ok(land_type) = LandType::from_str(s) { return Ok(Subtype::Land(land_type)); }
        if let Ok(planeswalker_type) = PlaneswalkerType::from_str(s) { return Ok(Subtype::Planeswalker(planeswalker_type)); }
        if let Ok(spell_type) = SpellType::from_str(s) { return Ok(Subtype::Spell(spell_type)); }
        if let Ok(creature_type) = CreatureType::from_str(s) { return Ok(Subtype::Creature(creature_type)); }
        if let Ok(planar_type) = PlanarType::from_str(s) { return Ok(Subtype::Planar(planar_type)); }
        Err(())
    }
}

impl From<Subtype> for TypeLine {
    fn from(subtype: Subtype) -> TypeLine {
        match subtype {
            Subtype::Artifact(artifact_type) => artifact_type.into(),
            Subtype::Enchantment(enchantment_type) => enchantment_type.into(),
            Subtype::Land(land_type) => land_type.into(),
            Subtype::Planeswalker(planeswalker_type) => planeswalker_type.into(),
            Subtype::Spell(spell_type) => spell_type.into(),
            Subtype::Creature(creature_type) => creature_type.into(),
            Subtype::Planar(planar_type) => planar_type.into()
        }
    }
}

impl fmt::Display for Subtype {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Subtype::Artifact(artifact_type) => artifact_type.fmt(f),
            Subtype::Enchantment(enchantment_type) => enchantment_type.fmt(f),
            Subtype::Land(land_type) => land_type.fmt(f),
            Subtype::Planeswalker(planeswalker_type) => planeswalker_type.fmt(f),
            Subtype::Spell(spell_type) => spell_type.fmt(f),
            Subtype::Creature(creature_type) => creature_type.fmt(f),
            Subtype::Planar(planar_type) => planar_type.fmt(f)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::card::Db;

    fn test_type_line(db: &Db, card_name: &str, type_line: &str) {
        let card = db.card(card_name).expect(&format!("failed to find card by name {:?}", card_name));
        assert_eq!(&card.type_line().to_string(), type_line);
    }

    #[test]
    fn test_type_lines() {
        let db = Db::download(false).expect("failed to download test card db");
        test_type_line(&db, "Dark Depths", "Legendary Snow Land");
        test_type_line(&db, "Hammer of Purphoros", "Legendary Enchantment Artifact");
        test_type_line(&db, "My Laughter Echoes", "Ongoing Scheme");
        test_type_line(&db, "Plains", "Basic Land — Plains");
        test_type_line(&db, "Pools of Becoming", "Plane — Bolas's Meditation Realm");
        test_type_line(&db, "Reaper King", "Legendary Artifact Creature — Scarecrow");
        test_type_line(&db, "Sliver Queen Avatar", "Vanguard");
        test_type_line(&db, "Snow-Covered Mountain", "Basic Snow Land — Mountain");
        test_type_line(&db, "Taiga", "Land — Mountain Forest");
        test_type_line(&db, "Urza's Power Plant", "Land — Urza's Power-Plant");
    }
}
