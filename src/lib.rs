//! A simple http/json language.
//!
//! `main` sets up the modules and boilerplate, and runs `execute` to do the work.

#![recursion_limit = "1024"]

#[macro_use]
pub mod errors;
#[macro_use]
pub(crate) mod common;
pub(crate) mod compiler;
pub(crate) mod scanner;
pub(crate) mod inline;
pub mod value;
pub mod vm;
