//! A simple http/json language.
//!
//! `main` sets up the modules and boilerplate, and runs `execute` to do the work.

pub mod collapsed;
pub(crate) mod commas;
pub(crate) mod common;
pub(crate) mod compiler;
pub(crate) mod either;
pub mod errors;
pub(crate) mod inline;
pub(crate) mod natives;
pub(crate) mod scanner;
pub(crate) mod scope;
pub(crate) mod types;
pub mod value;
pub mod vm;
