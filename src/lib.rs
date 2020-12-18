//! A simple http/json language.
//!
//! `main` sets up the modules and boilerplate, and runs `execute` to do the work.

#![recursion_limit = "1024"]

#[macro_use]
pub mod errors;
pub(crate) mod common;
pub(crate) mod compiler;
pub(crate) mod scanner;
pub mod value;
pub mod vm;
