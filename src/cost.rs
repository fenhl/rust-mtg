//! Mana costs and other costs.
//!
//! This module contains the `ManaCost` type which represents a mana cost. Other costs are currently simply represented as strings.

use std::{
    fmt,
    iter,
    ops::{
        Add,
        AddAssign
    },
    str::FromStr
};
use num::{
    BigUint,
    One,
    ToPrimitive
};
use crate::color::{
    Color,
    ColorSet
};

/// An error encountered while parsing a string into a `ManaCost`.
#[derive(Debug)]
pub enum ParseError {
    /// The string did not start with a `{`.
    MissingOpenBrace,
    /// The string did not end with a `}`.
    MissingCloseBrace,
    /// A component of the cost was not recognized.
    UnknownComponent(String)
}

/// A cost that may include a mana cost and/or other costs.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Cost {
    /// The mana part of the cost.
    pub mana: ManaCost,
    /// Any other parts of the cost.
    pub other: Option<String>
}

impl FromStr for Cost {
    type Err = ();

    fn from_str(s: &str) -> Result<Cost, ()> {
        if s == "" {
            return Ok(Cost::default());
        }
        let mut prefix_len = 0;
        let mut mana_cost = None;
        for len in s.char_indices().map(|(len, _)| len).chain(iter::once(s.len())) {
            if let Ok(cost_prefix) = ManaCost::from_str(&s[..len]) {
                prefix_len = len;
                mana_cost = Some(cost_prefix);
            }
        }
        if prefix_len == 0 {
            Ok(Cost {
                mana: ManaCost::default(),
                other: Some(s.into())
            })
        } else if s.get(prefix_len..prefix_len + 2) == Some(", ") {
            Ok(Cost {
                mana: mana_cost.expect("failed to parse mana part of a cost"),
                other: Some(s[prefix_len + 2..].into())
            })
        } else if prefix_len == s.len() {
            Ok(Cost {
                mana: mana_cost.expect("failed to parse mana part of a cost"),
                other: None
            })
        } else {
            Err(())
        }
    }
}

/// A mana cost like `{1}{G}`.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ManaCost {
    generic: BigUint,
    colorless: BigUint,
    white: BigUint,
    blue: BigUint,
    black: BigUint,
    red: BigUint,
    green: BigUint,
    snow: BigUint,
    phyrexian_white: BigUint,
    phyrexian_blue: BigUint,
    phyrexian_black: BigUint,
    phyrexian_red: BigUint,
    phyrexian_green: BigUint,
    hybrid_2_white: BigUint,
    hybrid_2_blue: BigUint,
    hybrid_2_black: BigUint,
    hybrid_2_red: BigUint,
    hybrid_2_green: BigUint,
    hybrid_white_blue: BigUint,
    hybrid_blue_black: BigUint,
    hybrid_black_red: BigUint,
    hybrid_red_green: BigUint,
    hybrid_green_white: BigUint,
    hybrid_white_black: BigUint,
    hybrid_blue_red: BigUint,
    hybrid_black_green: BigUint,
    hybrid_red_white: BigUint,
    hybrid_green_blue: BigUint,
    variable: BigUint
}

impl ManaCost {
    /// Returns the converted mana cost of an object with this mana cost.
    pub fn converted<N: Into<BigUint>>(&self, x: N) -> BigUint {
        &self.generic +
        &self.colorless +
        &self.white +
        &self.blue +
        &self.black +
        &self.red +
        &self.green +
        &self.snow +
        &self.phyrexian_white +
        &self.phyrexian_blue +
        &self.phyrexian_black +
        &self.phyrexian_red +
        &self.phyrexian_green +
        BigUint::from(2u8) * (
            &self.hybrid_2_white +
            &self.hybrid_2_blue +
            &self.hybrid_2_black +
            &self.hybrid_2_red +
            &self.hybrid_2_green
        ) +
        &self.hybrid_white_blue +
        &self.hybrid_blue_black +
        &self.hybrid_black_red +
        &self.hybrid_red_green +
        &self.hybrid_green_white +
        &self.hybrid_white_black +
        &self.hybrid_blue_red +
        &self.hybrid_black_green +
        &self.hybrid_red_white +
        &self.hybrid_green_blue +
        x.into() * &self.variable
    }

    /// Returns the amount a permanent with this mana cost contributes to its controller's devotion to the given colors.
    ///
    /// Note that the comprehensive rules only define devotion to a single color and to two colors. This function uses a more general definition, which behaves the same for single-color and two-color:
    ///
    /// 1.  The devotion to a non-empty set of colors is the number of mana symbols that are any of these colors.
    /// 2.  The devotion to colorless is the number of colorless mana symbols. Generic mana symbols, including variable, hybrid, and snow, are not colorless mana symbols.
    pub fn devotion(&self, colors: ColorSet) -> BigUint {
        let zero = BigUint::default();
        &zero + // add zero to make Rust parse this as an expression
        if colors == ColorSet::colorless() { &self.colorless } else { &zero } +
        if colors >= ColorSet::white() { &self.white + &self.phyrexian_white + &self.hybrid_2_white } else { BigUint::default() } +
        if colors >= ColorSet::blue() { &self.blue + &self.phyrexian_blue + &self.hybrid_2_blue } else { BigUint::default() } +
        if colors >= ColorSet::black() { &self.black + &self.phyrexian_black + &self.hybrid_2_black } else { BigUint::default() } +
        if colors >= ColorSet::red() { &self.red + &self.phyrexian_red + &self.hybrid_2_red } else { BigUint::default() } +
        if colors >= ColorSet::green() { &self.green + &self.phyrexian_green + &self.hybrid_2_green } else { BigUint::default() } +
        if colors >= ColorSet::white() || colors >= ColorSet::blue() { &self.hybrid_white_blue } else { &zero } +
        if colors >= ColorSet::blue() || colors >= ColorSet::black() { &self.hybrid_blue_black } else { &zero } +
        if colors >= ColorSet::black() || colors >= ColorSet::red() { &self.hybrid_black_red } else { &zero } +
        if colors >= ColorSet::red() || colors >= ColorSet::green() { &self.hybrid_red_green } else { &zero } +
        if colors >= ColorSet::green() || colors >= ColorSet::white() { &self.hybrid_green_white } else { &zero } +
        if colors >= ColorSet::white() || colors >= ColorSet::black() { &self.hybrid_white_black } else { &zero } +
        if colors >= ColorSet::blue() || colors >= ColorSet::red() { &self.hybrid_blue_red } else { &zero } +
        if colors >= ColorSet::black() || colors >= ColorSet::green() { &self.hybrid_black_green } else { &zero } +
        if colors >= ColorSet::red() || colors >= ColorSet::white() { &self.hybrid_red_white } else { &zero } +
        if colors >= ColorSet::green() || colors >= ColorSet::blue() { &self.hybrid_green_blue } else { &zero }
    }
}

impl FromStr for ManaCost {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<ManaCost, ParseError> {
        if s == "" {
            return Ok(ManaCost::default());
        }
        let mut result = ManaCost::default();
        let mut chars = s.chars();
        if !chars.next().map_or(false, |c| c == '{') {
            return Err(ParseError::MissingOpenBrace);
        }
        if !chars.next_back().map_or(false, |c| c == '}') {
            return Err(ParseError::MissingCloseBrace);
        }
        for component in chars.collect::<String>().split("}{") {
            result += match component {
                "C" => ManaCost { colorless: BigUint::one(), ..ManaCost::default() },
                "W" => ManaCost { white: BigUint::one(), ..ManaCost::default() },
                "U" => ManaCost { blue: BigUint::one(), ..ManaCost::default() },
                "B" => ManaCost { black: BigUint::one(), ..ManaCost::default() },
                "R" => ManaCost { red: BigUint::one(), ..ManaCost::default() },
                "G" => ManaCost { green: BigUint::one(), ..ManaCost::default() },
                "S" => ManaCost { snow: BigUint::one(), ..ManaCost::default() },
                "W/P" => ManaCost { phyrexian_white: BigUint::one(), ..ManaCost::default() },
                "U/P" => ManaCost { phyrexian_blue: BigUint::one(), ..ManaCost::default() },
                "B/P" => ManaCost { phyrexian_black: BigUint::one(), ..ManaCost::default() },
                "R/P" => ManaCost { phyrexian_red: BigUint::one(), ..ManaCost::default() },
                "G/P" => ManaCost { phyrexian_green: BigUint::one(), ..ManaCost::default() },
                "2/W" => ManaCost { hybrid_2_white: BigUint::one(), ..ManaCost::default() },
                "2/U" => ManaCost { hybrid_2_blue: BigUint::one(), ..ManaCost::default() },
                "2/B" => ManaCost { hybrid_2_black: BigUint::one(), ..ManaCost::default() },
                "2/R" => ManaCost { hybrid_2_red: BigUint::one(), ..ManaCost::default() },
                "2/G" => ManaCost { hybrid_2_green: BigUint::one(), ..ManaCost::default() },
                "W/U" => ManaCost { hybrid_white_blue: BigUint::one(), ..ManaCost::default() },
                "U/B" => ManaCost { hybrid_blue_black: BigUint::one(), ..ManaCost::default() },
                "B/R" => ManaCost { hybrid_black_red: BigUint::one(), ..ManaCost::default() },
                "R/G" => ManaCost { hybrid_red_green: BigUint::one(), ..ManaCost::default() },
                "G/W" => ManaCost { hybrid_green_white: BigUint::one(), ..ManaCost::default() },
                "W/B" => ManaCost { hybrid_white_black: BigUint::one(), ..ManaCost::default() },
                "U/R" => ManaCost { hybrid_blue_red: BigUint::one(), ..ManaCost::default() },
                "B/G" => ManaCost { hybrid_black_green: BigUint::one(), ..ManaCost::default() },
                "R/W" => ManaCost { hybrid_red_white: BigUint::one(), ..ManaCost::default() },
                "G/U" => ManaCost { hybrid_green_blue: BigUint::one(), ..ManaCost::default() },
                "X" => ManaCost { variable: BigUint::one(), ..ManaCost::default() },
                comp => if let Ok(generic) = comp.parse() {
                    ManaCost { generic: generic, ..ManaCost::default() }
                } else {
                    return Err(ParseError::UnknownComponent(comp.to_owned()));
                }
            };
        }
        Ok(result)
    }
}

impl fmt::Display for ManaCost {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if *self == ManaCost::default() { return "{0}".fmt(f); }
        // generic and colorless costs
        "{X}".repeat(self.variable.to_usize().ok_or(fmt::Error)?).fmt(f)?;
        if self.generic > BigUint::default() {
            write!(f, "{{{}}}", self.generic)?;
        }
        "{S}".repeat(self.snow.to_usize().ok_or(fmt::Error)?).fmt(f)?;
        "{C}".repeat(self.colorless.to_usize().ok_or(fmt::Error)?).fmt(f)?;
        // twobrid
        let twobrids = ColorSet::from([
            self.hybrid_2_white > BigUint::default(),
            self.hybrid_2_blue > BigUint::default(),
            self.hybrid_2_black > BigUint::default(),
            self.hybrid_2_red > BigUint::default(),
            self.hybrid_2_green > BigUint::default()
        ]);
        for color in twobrids.canonical_order() {
            match color {
                Color::White => { "{2/W}".repeat(self.hybrid_2_white.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
                Color::Blue => { "{2/U}".repeat(self.hybrid_2_blue.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
                Color::Black => { "{2/B}".repeat(self.hybrid_2_black.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
                Color::Red => { "{2/R}".repeat(self.hybrid_2_red.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
                Color::Green => { "{2/G}".repeat(self.hybrid_2_green.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
            }
        }
        // hybrid
        "{W/U}".repeat(self.hybrid_white_blue.to_usize().ok_or(fmt::Error)?).fmt(f)?;
        "{U/B}".repeat(self.hybrid_blue_black.to_usize().ok_or(fmt::Error)?).fmt(f)?;
        "{B/R}".repeat(self.hybrid_black_red.to_usize().ok_or(fmt::Error)?).fmt(f)?;
        "{R/G}".repeat(self.hybrid_red_green.to_usize().ok_or(fmt::Error)?).fmt(f)?;
        "{G/W}".repeat(self.hybrid_green_white.to_usize().ok_or(fmt::Error)?).fmt(f)?;
        "{W/B}".repeat(self.hybrid_white_black.to_usize().ok_or(fmt::Error)?).fmt(f)?;
        "{U/R}".repeat(self.hybrid_blue_red.to_usize().ok_or(fmt::Error)?).fmt(f)?;
        "{B/G}".repeat(self.hybrid_black_green.to_usize().ok_or(fmt::Error)?).fmt(f)?;
        "{R/W}".repeat(self.hybrid_red_white.to_usize().ok_or(fmt::Error)?).fmt(f)?;
        "{G/U}".repeat(self.hybrid_green_blue.to_usize().ok_or(fmt::Error)?).fmt(f)?;
        // Phyrexian
        let phyrexian = ColorSet::from([
            self.phyrexian_white > BigUint::default(),
            self.phyrexian_blue > BigUint::default(),
            self.phyrexian_black > BigUint::default(),
            self.phyrexian_red > BigUint::default(),
            self.phyrexian_green > BigUint::default()
        ]);
        for color in phyrexian.canonical_order() {
            match color {
                Color::White => { "{W/P}".repeat(self.phyrexian_white.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
                Color::Blue => { "{U/P}".repeat(self.phyrexian_blue.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
                Color::Black => { "{B/P}".repeat(self.phyrexian_black.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
                Color::Red => { "{R/P}".repeat(self.phyrexian_red.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
                Color::Green => { "{G/P}".repeat(self.phyrexian_green.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
            }
        }
        // colored
        let colored = ColorSet::from([
            self.white > BigUint::default(),
            self.blue > BigUint::default(),
            self.black > BigUint::default(),
            self.red > BigUint::default(),
            self.green > BigUint::default()
        ]);
        for color in colored.canonical_order() {
            match color {
                Color::White => { "{W}".repeat(self.white.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
                Color::Blue => { "{U}".repeat(self.blue.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
                Color::Black => { "{B}".repeat(self.black.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
                Color::Red => { "{R}".repeat(self.red.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
                Color::Green => { "{G}".repeat(self.green.to_usize().ok_or(fmt::Error)?).fmt(f)?; }
            }
        }
        Ok(())
    }
}

serde_plain::derive_deserialize_from_str!(ManaCost, "valid mana cost");
serde_plain::derive_serialize_from_display!(ManaCost);

impl<'a> Add<ManaCost> for &'a ManaCost {
    type Output = ManaCost;

    fn add(self, rhs: ManaCost) -> ManaCost {
        ManaCost {
            generic: &self.generic + rhs.generic,
            colorless: &self.colorless + rhs.colorless,
            white: &self.white + rhs.white,
            blue: &self.blue + rhs.blue,
            black: &self.black + rhs.black,
            red: &self.red + rhs.red,
            green: &self.green + rhs.green,
            snow: &self.snow + rhs.snow,
            phyrexian_white: &self.phyrexian_white + rhs.phyrexian_white,
            phyrexian_blue: &self.phyrexian_blue + rhs.phyrexian_blue,
            phyrexian_black: &self.phyrexian_black + rhs.phyrexian_black,
            phyrexian_red: &self.phyrexian_red + rhs.phyrexian_red,
            phyrexian_green: &self.phyrexian_green + rhs.phyrexian_green,
            hybrid_2_white: &self.hybrid_2_white + rhs.hybrid_2_white,
            hybrid_2_blue: &self.hybrid_2_blue + rhs.hybrid_2_blue,
            hybrid_2_black: &self.hybrid_2_black + rhs.hybrid_2_black,
            hybrid_2_red: &self.hybrid_2_red + rhs.hybrid_2_red,
            hybrid_2_green: &self.hybrid_2_green + rhs.hybrid_2_green,
            hybrid_white_blue: &self.hybrid_white_blue + rhs.hybrid_white_blue,
            hybrid_blue_black: &self.hybrid_blue_black + rhs.hybrid_blue_black,
            hybrid_black_red: &self.hybrid_black_red + rhs.hybrid_black_red,
            hybrid_red_green: &self.hybrid_red_green + rhs.hybrid_red_green,
            hybrid_green_white: &self.hybrid_green_white + rhs.hybrid_green_white,
            hybrid_white_black: &self.hybrid_white_black + rhs.hybrid_white_black,
            hybrid_blue_red: &self.hybrid_blue_red + rhs.hybrid_blue_red,
            hybrid_black_green: &self.hybrid_black_green + rhs.hybrid_black_green,
            hybrid_red_white: &self.hybrid_red_white + rhs.hybrid_red_white,
            hybrid_green_blue: &self.hybrid_green_blue + rhs.hybrid_green_blue,
            variable: &self.variable + rhs.variable
        }
    }
}

impl AddAssign for ManaCost {
    fn add_assign(&mut self, rhs: ManaCost) {
        *self = &*self + rhs;
    }
}

impl From<ManaCost> for ColorSet {
    fn from(mana_cost: ManaCost) -> ColorSet {
        let zero = BigUint::default();
        ColorSet::default() |
        if mana_cost.white > zero || mana_cost.phyrexian_white > zero || mana_cost.hybrid_2_white > zero || mana_cost.hybrid_white_blue > zero || mana_cost.hybrid_white_black > zero || mana_cost.hybrid_red_white > zero || mana_cost.hybrid_green_white > zero { ColorSet::white() } else { ColorSet::default() } |
        if mana_cost.blue > zero || mana_cost.phyrexian_blue > zero || mana_cost.hybrid_2_blue > zero || mana_cost.hybrid_blue_black > zero || mana_cost.hybrid_blue_red > zero || mana_cost.hybrid_green_blue > zero || mana_cost.hybrid_white_blue > zero { ColorSet::blue() } else { ColorSet::default() } |
        if mana_cost.black > zero || mana_cost.phyrexian_black > zero || mana_cost.hybrid_2_black > zero || mana_cost.hybrid_black_red > zero || mana_cost.hybrid_black_green > zero || mana_cost.hybrid_white_black > zero || mana_cost.hybrid_blue_black > zero { ColorSet::black() } else { ColorSet::default() } |
        if mana_cost.red > zero || mana_cost.phyrexian_red > zero || mana_cost.hybrid_2_red > zero || mana_cost.hybrid_red_green > zero || mana_cost.hybrid_red_white > zero || mana_cost.hybrid_blue_red > zero || mana_cost.hybrid_black_red > zero { ColorSet::red() } else { ColorSet::default() } |
        if mana_cost.green > zero || mana_cost.phyrexian_green > zero || mana_cost.hybrid_2_green > zero || mana_cost.hybrid_green_white > zero || mana_cost.hybrid_green_blue > zero || mana_cost.hybrid_black_green > zero || mana_cost.hybrid_red_green > zero { ColorSet::green() } else { ColorSet::default() }
    }
}

#[cfg(test)]
mod tests {
    use num::ToPrimitive;
    use crate::card::Db;

    fn test_cmc(db: &Db, card_name: &str, cmc: u64) {
        let card = db.card(card_name).expect(&format!("failed to find card by name {:?}", card_name));
        assert_eq!(card.cmc().to_u64().expect(&format!("failed to convert CMC of {:?} to u64", card_name)), cmc);
    }

    #[test]
    fn test_cmcs() {
        let db = Db::download(false).expect("failed to download test card db");
        test_cmc(&db, "Beseech the Queen", 6);
        test_cmc(&db, "Emrakul, the Aeons Torn", 15);
        test_cmc(&db, "Evermind", 0);
        test_cmc(&db, "Hangarback Walker", 0);
    }

    fn test_mana_cost(db: &Db, card_name: &str, cost_text: &str) {
        let card = db.card(card_name).expect(&format!("failed to find card by name {:?}", card_name));
        assert_eq!(&card.mana_cost().expect(&format!("card {:?} has no mana cost", card_name)).to_string(), cost_text);
    }

    #[test]
    fn test_mana_costs() {
        let db = Db::download(false).expect("failed to download test card db");
        test_mana_cost(&db, "Altered Ego", "{X}{2}{G}{U}");
        test_mana_cost(&db, "Angus Mackenzie", "{G}{W}{U}");
        test_mana_cost(&db, "Bone Saw", "{0}");
        test_mana_cost(&db, "Decree of Justice", "{X}{X}{2}{W}{W}");
        test_mana_cost(&db, "Emrakul, the Promised End", "{13}");
        test_mana_cost(&db, "Kozilek, the Great Distortion", "{8}{C}{C}");
        test_mana_cost(&db, "Progenitus", "{W}{W}{U}{U}{B}{B}{R}{R}{G}{G}");
        test_mana_cost(&db, "Queen Marchesa", "{1}{R}{W}{B}");
        test_mana_cost(&db, "Reaper King", "{2/W}{2/U}{2/B}{2/R}{2/G}");
        test_mana_cost(&db, "Slippery Bogle", "{G/U}");
        test_mana_cost(&db, "Spectral Procession", "{2/W}{2/W}{2/W}");
        test_mana_cost(&db, "O-Kagachi, Vengeful Kami", "{1}{W}{U}{B}{R}{G}");
        test_mana_cost(&db, "Thundering Tanadon", "{4}{G/P}{G/P}");
    }
}
