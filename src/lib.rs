//! A library for writing *Magic: The Gathering*-related code in Rust.

#![cfg_attr(test, deny(warnings))]
#![warn(trivial_casts)]
#![deny(unused, missing_docs, unused_qualifications)]
#![forbid(unused_import_braces)]

pub mod card;
pub mod cardtype;
pub mod color;
pub mod cost;
mod util;
