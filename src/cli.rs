//! The command-line options for the executable.

use clap::{crate_version, App, AppSettings, Arg, ArgMatches};
use alchem::errors::Result;
use alchem::vm::interpret;
use alchem::value::Value;
use std::io::{self, BufRead};

/// Get the values from the expected command-line options.
pub async fn execute() -> Result<()> {
  let matches = App::new("alchem")
    .setting(AppSettings::UnifiedHelpMessage)
    .author("Charlie Ozinga, ozchaz@gmail.com")
    .version(concat!(crate_version!(), " (", env!("GIT_SHORT_HASH"), " ", env!("DATE_DASH"), ")"))
    .about("Run an alchemy interpreter")
    .arg(
      Arg::with_name("input")
        .short("i")
        .long("input")
        .takes_value(true)
        .value_name("file")
        .display_order(1)
        .help("The alchemy file to read")
    )
    .get_matches();

  parse_matches(matches).await
}

async fn parse_matches(m: ArgMatches<'_>) -> Result<()> {
  match m.value_of("input") {
    Some(input) => run_file(input).await.map(|_value| ()),
    None => repl().await
  }
}

async fn run_file(input: &str) -> Result<Value> {
  let val = std::fs::read_to_string(input)?;
  interpret(&val)
}

async fn repl() -> Result<()> {
  let stdin = io::stdin();
  for line in stdin.lock().lines() {
    let line = line?;
    let _value = interpret(&line)?;
  }
  println!("done");
  Ok(())
}
