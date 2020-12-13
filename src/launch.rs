//! A simple http/json language.
//!
//! `main` sets up the modules and boilerplate, and runs `execute` to do the work.

#![recursion_limit = "1024"]

mod main;
mod cli;

use env_logger::{Builder, Env};
use std::process;
use tokio::runtime::Runtime;

fn main() {
  Builder::from_env(Env::new().default_filter_or("alchem=warn")).try_init().expect("Can't init logging.");
  ctrlc::set_handler(graceful_exit_sigint).expect("Can't install interrupt handler.");

  if let Err(e) = Runtime::new().unwrap().block_on(crate::main::main()) {
    use std::io::Write;
    let stderr = &mut std::io::stderr();
    let errmsg = "Error writing to stderr.";

    writeln!(stderr, "Error: {}", e).expect(errmsg);

    for e in e.iter().skip(1) {
      writeln!(stderr, "  Caused by: {}", e).expect(errmsg);
    }

    // Try running with `RUST_BACKTRACE=1` for a backtrace
    if let Some(backtrace) = e.backtrace() {
      writeln!(stderr, "Backtrace:\n{:?}", backtrace).expect(errmsg);
    }

    process::exit(1);
  }
}

#[cfg(not(test))]
pub fn graceful_exit_sigint() {
  log::warn!("Received SIGINT: exiting");
  std::process::exit(0);
}

#[cfg(test)]
pub fn graceful_exit_sigint() {}
