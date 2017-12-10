//! This module contains two types: `Color`, a simple enum which represents a single color, and `ColorSet`, which represents a set of zero or more colors.

use std::cmp::Ordering;

/// This enum represents a single color.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Color {
    White,
    Blue,
    Black,
    Red,
    Green
}

/// This struct represents a set of colors.
///
/// For each possible set of colors, there is one function provided that creates this color combination.
///
/// `PartialOrd` is also implemented, and is particularly useful for the Commander format: if `card_a.color_identity() <= card_b.color_identity()`, then `card_a` can be included in a deck which runs `card_b` as its commander.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ColorSet {
    pub white: bool,
    pub blue: bool,
    pub black: bool,
    pub red: bool,
    pub green: bool
}

impl ColorSet {
    /// Returns the `ColorSet` representing colorless.
    pub fn colorless() -> ColorSet {
        ColorSet::default()
    }

    /// Returns the `ColorSet` representing white.
    pub fn white() -> ColorSet {
        ColorSet {
            white: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing blue.
    pub fn blue() -> ColorSet {
        ColorSet {
            blue: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing black.
    pub fn black() -> ColorSet {
        ColorSet {
            black: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing red.
    pub fn red() -> ColorSet {
        ColorSet {
            red: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing green.
    pub fn green() -> ColorSet {
        ColorSet {
            green: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the two-color combination white-blue, also known as the guild colors of the Azorius Senate.
    pub fn azorius() -> ColorSet {
        ColorSet {
            white: true,
            blue: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the two-color combination blue-black, also known as the guild colors of House Dimir.
    pub fn dimir() -> ColorSet {
        ColorSet {
            blue: true,
            black: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the two-color combination black-red, also known as the guild colors of the Cult of Rakdos.
    pub fn rakdos() -> ColorSet {
        ColorSet {
            black: true,
            red: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the two-color combination red-green, also known as the guild colors of the Gruul Clans.
    pub fn gruul() -> ColorSet {
        ColorSet {
            red: true,
            green: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the two-color combination green-white, also known as the guild colors of the Selesnya Conclave.
    pub fn selesnya() -> ColorSet {
        ColorSet {
            green: true,
            white: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the two-color combination white-black, also known as the guild colors of the Orzhov Syndicate.
    pub fn orzhov() -> ColorSet {
        ColorSet {
            white: true,
            black: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the two-color combination blue-red, also known as the guild colors of the Izzet League.
    pub fn izzet() -> ColorSet {
        ColorSet {
            blue: true,
            red: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the two-color combination black-green, also known as the guild colors of the Golgari Swarm.
    pub fn golgari() -> ColorSet {
        ColorSet {
            black: true,
            green: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the two-color combination red-white, also known as the guild colors of the Boros Legion.
    pub fn boros() -> ColorSet {
        ColorSet {
            red: true,
            white: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the two-color combination green-blue, also known as the guild colors of the Simic Combine.
    pub fn simic() -> ColorSet {
        ColorSet {
            green: true,
            blue: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the three-color combination green-white-blue, also known as the shard colors of Bant.
    pub fn bant() -> ColorSet {
        ColorSet {
            green: true,
            white: true,
            blue: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the three-color combination white-blue-black, also known as the shard colors of Esper.
    pub fn esper() -> ColorSet {
        ColorSet {
            white: true,
            blue: true,
            black: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the three-color combination blue-black-red, also known as the shard colors of Grixis.
    pub fn grixis() -> ColorSet {
        ColorSet {
            blue: true,
            black: true,
            red: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the three-color combination black-red-green, also known as the shard colors of Jund.
    pub fn jund() -> ColorSet {
        ColorSet {
            black: true,
            red: true,
            green: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the three-color combination red-green-white, also known as the shard colors of Naya.
    pub fn naya() -> ColorSet {
        ColorSet {
            red: true,
            green: true,
            white: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the three-color combination white-black-green, also known as the clan colors of the Abzan Houses.
    pub fn abzan() -> ColorSet {
        ColorSet {
            white: true,
            black: true,
            green: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the three-color combination blue-red-white, also known as the clan colors of the Jeskai Way.
    pub fn jeskai() -> ColorSet {
        ColorSet {
            blue: true,
            red: true,
            white: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the three-color combination black-green-blue, also known as the clan colors of the Sultai Brood.
    pub fn sultai() -> ColorSet {
        ColorSet {
            black: true,
            green: true,
            blue: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the three-color combination red-white-black, also known as the clan colors of the Mardu Horde.
    pub fn mardu() -> ColorSet {
        ColorSet {
            red: true,
            white: true,
            black: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the three-color combination green-blue-red, also known as the clan colors of the Temur Frontier.
    pub fn temur() -> ColorSet {
        ColorSet {
            green: true,
            blue: true,
            red: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the four-color combination white-blue-black-red, also known as the colors of the Yore-Tiller Nephilim.
    pub fn yore_tiller() -> ColorSet {
        ColorSet {
            white: true,
            blue: true,
            black: true,
            red: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the four-color combination blue-black-red-green, also known as the colors of the Glint-Eye Nephilim.
    pub fn glint_eye() -> ColorSet {
        ColorSet {
            blue: true,
            black: true,
            red: true,
            green: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the four-color combination black-red-green-white, also known as the colors of the Dune-Brood Nephilim.
    pub fn dune_brood() -> ColorSet {
        ColorSet {
            black: true,
            red: true,
            green: true,
            white: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the four-color combination red-green-white-blue, also known as the colors of the Ink-Treader Nephilim.
    pub fn ink_treader() -> ColorSet {
        ColorSet {
            red: true,
            green: true,
            white: true,
            blue: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing the four-color combination green-white-blue-black, also known as the colors of the Witch-Maw Nephilim.
    pub fn witch_maw() -> ColorSet {
        ColorSet {
            green: true,
            white: true,
            blue: true,
            black: true,
            ..ColorSet::default()
        }
    }

    /// Returns the `ColorSet` representing all five colors, also known as WUBRG or rainbow.
    pub fn rainbow() -> ColorSet {
        ColorSet {
            white: true,
            blue: true,
            black: true,
            red: true,
            green: true
        }
    }
}

/// Returns the colorless `ColorSet`.
impl Default for ColorSet {
    fn default() -> ColorSet {
        ColorSet {
            white: false,
            blue: false,
            black: false,
            red: false,
            green: false
        }
    }
}

impl From<Color> for ColorSet {
    fn from(color: Color) -> ColorSet {
        match color {
            Color::White => ColorSet::white(),
            Color::Blue => ColorSet::blue(),
            Color::Black => ColorSet::black(),
            Color::Red => ColorSet::red(),
            Color::Green => ColorSet::green()
        }
    }
}

impl From<(bool, bool, bool, bool, bool)> for ColorSet {
    fn from((white, blue, black, red, green): (bool, bool, bool, bool, bool)) -> ColorSet {
        ColorSet { white, blue, black, red, green }
    }
}

impl From<[bool; 5]> for ColorSet {
    fn from(flags: [bool; 5]) -> ColorSet {
        ColorSet::from((flags[0], flags[1], flags[2], flags[3], flags[4]))
    }
}

impl ColorSet {
    /// Returns a total ordering of color sets without the properties of the `PartialOrd` implementation, but which can be used to sort a list of e.g. cards.
    ///
    /// This should *not* be used to sort the component colors of a `ColorSet`, use `ColorSet::canonical_order` for that.
    pub fn canonical_cmp(&self, other: ColorSet) -> Ordering {
        self.canonical_idx().cmp(&other.canonical_idx())
    }

    fn canonical_idx(&self) -> u8 {
        match (self.white, self.blue, self.black, self.red, self.green) {
            //colorless
            (false, false, false, false, false) => 0,
            // single colors
            (true, false, false, false, false) => 1,
            (false, true, false, false, false) => 2,
            (false, false, true, false, false) => 3,
            (false, false, false, true, false) => 4,
            (false, false, false, false, true) => 5,
            // allied pairs
            (true, true, false, false, false) => 6,
            (false, true, true, false, false) => 7,
            (false, false, true, true, false) => 8,
            (false, false, false, true, true) => 9,
            (true, false, false, false, true) => 10,
            // enemy pairs
            (true, false, true, false, false) => 11,
            (false, true, false, true, false) => 12,
            (false, false, true, false, true) => 13,
            (true, false, false, true, false) => 14,
            (false, true, false, false, true) => 15,
            // shards
            (true, true, false, false, true) => 16,
            (true, true, true, false, false) => 17,
            (false, true, true, true, false) => 18,
            (false, false, true, true, true) => 19,
            (true, false, false, true, true) => 20,
            // wedges
            (true, false, true, false, true) => 21,
            (true, true, false, true, false) => 22,
            (false, true, true, false, true) => 23,
            (true, false, true, true, false) => 24,
            (false, true, false, true, true) => 25,
            // nephilim
            (true, true, true, true, false) => 26,
            (false, true, true, true, true) => 27,
            (true, false, true, true, true) => 28,
            (true, true, false, true, true) => 29,
            (true, true, true, false, true) => 30,
            // rainbow
            (true, true, true, true, true) => 31
        }
    }

    /// Returns the component colors in their canonical order (e.g. “green, blue” or “red, white, black”).
    pub fn canonical_order(&self) -> Vec<Color> {
        match (self.white, self.blue, self.black, self.red, self.green) {
            //colorless
            (false, false, false, false, false) => Vec::default(),
            // single colors
            (true, false, false, false, false) => vec![Color::White],
            (false, true, false, false, false) => vec![Color::Blue],
            (false, false, true, false, false) => vec![Color::Black],
            (false, false, false, true, false) => vec![Color::Red],
            (false, false, false, false, true) => vec![Color::Green],
            // allied pairs
            (true, true, false, false, false) => vec![Color::White, Color::Blue],
            (false, true, true, false, false) => vec![Color::Blue, Color::Black],
            (false, false, true, true, false) => vec![Color::Black, Color::Red],
            (false, false, false, true, true) => vec![Color::Red, Color::Green],
            (true, false, false, false, true) => vec![Color::Green, Color::White],
            // enemy pairs
            (true, false, true, false, false) => vec![Color::White, Color::Black],
            (false, true, false, true, false) => vec![Color::Blue, Color::Red],
            (false, false, true, false, true) => vec![Color::Black, Color::Green],
            (true, false, false, true, false) => vec![Color::Red, Color::White],
            (false, true, false, false, true) => vec![Color::Green, Color::Blue],
            // shards
            (true, true, false, false, true) => vec![Color::Green, Color::White, Color::Blue],
            (true, true, true, false, false) => vec![Color::White, Color::Blue, Color::Black],
            (false, true, true, true, false) => vec![Color::Blue, Color::Black, Color::Red],
            (false, false, true, true, true) => vec![Color::Black, Color::Red, Color::Green],
            (true, false, false, true, true) => vec![Color::Red, Color::Green, Color::White],
            // wedges
            (true, false, true, false, true) => vec![Color::White, Color::Black, Color::Green],
            (true, true, false, true, false) => vec![Color::Blue, Color::Red, Color::White],
            (false, true, true, false, true) => vec![Color::Black, Color::Green, Color::Blue],
            (true, false, true, true, false) => vec![Color::Red, Color::White, Color::Black],
            (false, true, false, true, true) => vec![Color::Green, Color::Blue, Color::Red],
            // nephilim
            (true, true, true, true, false) => vec![Color::White, Color::Blue, Color::Black, Color::Red],
            (false, true, true, true, true) => vec![Color::Blue, Color::Black, Color::Red, Color::Green],
            (true, false, true, true, true) => vec![Color::Black, Color::Red, Color::Green, Color::White],
            (true, true, false, true, true) => vec![Color::Red, Color::Green, Color::White, Color::Blue],
            (true, true, true, false, true) => vec![Color::Green, Color::White, Color::Blue, Color::Black],
            // rainbow
            (true, true, true, true, true) => vec![Color::White, Color::Blue, Color::Black, Color::Red, Color::Green]
        }
    }
}

/// For two `ColorSet`s `a` and `b`, `a <= b` means that all colors in `a` are also in `b`.
impl PartialOrd for ColorSet {
    fn partial_cmp(&self, other: &ColorSet) -> Option<Ordering> {
        let mut less = false;
        let mut greater = false;

        macro_rules! cmp_color {
            ($color:ident) => {
                match (self.$color, other.$color) {
                    (false, true) => { less = true; }
                    (true, false) => { greater = true; }
                    (_, _) => ()
                }
            };
        }

        cmp_color!(white);
        cmp_color!(blue);
        cmp_color!(black);
        cmp_color!(red);
        cmp_color!(green);

        match (less, greater) {
            (true, false) => Some(Ordering::Less),
            (false, false) => Some(Ordering::Equal),
            (false, true) => Some(Ordering::Greater),
            (true, true) => None
        }
    }
}
