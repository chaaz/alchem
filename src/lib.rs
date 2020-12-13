//! A simple http/json language.
//!
//! `main` sets up the modules and boilerplate, and runs `execute` to do the work.

#![recursion_limit = "1024"]

#[macro_use]
pub mod errors;
pub mod common;
pub mod value;
pub mod vm;
pub mod scanner;
pub mod compiler;

// #[allow(clippy::all)]
// mod grammar_ext {
//   use lalrpop_util::lalrpop_mod;
//   lalrpop_mod!(pub grammar);
// }

// pub mod grammar {
//   pub use super::grammar_ext::grammar::ScriptParser;
//   pub use super::grammar_ext::grammar::Token;
// }
