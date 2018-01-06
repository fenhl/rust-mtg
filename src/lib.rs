//! A library for writing *Magic: The Gathering*-related code in Rust.

#![cfg_attr(test, deny(warnings))]
#![warn(trivial_casts)]
#![deny(unused, missing_docs, unused_qualifications)]
#![forbid(unused_import_braces)]

extern crate num;
extern crate regex;
extern crate reqwest;
#[macro_use] extern crate serde_derive;
#[macro_use] extern crate serde_json;
extern crate topological_sort;
#[macro_use] extern crate wrapped_enum;

pub mod card;
pub mod cardtype;
pub mod color;
pub mod cost;
mod util;
