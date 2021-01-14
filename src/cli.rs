//! The command-line options for the executable.

use alchem::errors::Result;
use alchem::vm::Vm;
use clap::{crate_version, App, AppSettings, Arg, ArgMatches};
// use macro_rules_attribute::macro_rules_attribute;
// use std::io::{self, BufRead};

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
    Some(input) => run_file(input).await,
    None => run_file("fib.alc").await
  }
}

async fn run_file(input: &str) -> Result<()> {
  let val = std::fs::read_to_string(input)?;
  let vm = Vm::new();
  // vm.add_native("print", print);
  // vm.add_native("number", number);
  // vm.add_native("recall", recall);
  println!("{:?}", vm.interpret(&val).await);
  Ok(())
}

// #[macro_rules_attribute(native_fn!)]
// async fn number(_argv: Vec<Value>, _runner: &mut Runner) -> Value { Value::Int(42) }
// 
// #[macro_rules_attribute(native_fn!)]
// async fn print(vals: Vec<Value>, _runner: &mut Runner) -> Value {
//   println!("*** PRINT: {:?}", vals[0]);
//   Value::Int(1)
// }
// 
// #[macro_rules_attribute(native_fn!)]
// async fn recall(vals: Vec<Value>, runner: &mut Runner) -> Value {
//   let f = vals[0].as_closure();
//   runner.run_closure(f, Vec::new()).await
// }

// async fn repl() -> Result<()> {
//   let mut vm = Vm::new();
//   vm.add_native("print", print);
//
//   let stdin = io::stdin();
//   for line in stdin.lock().lines() {
//     let line = line?;
//     let value = vm.interpret(&line)?;
//     println!("\nResult:\n= {:?}\n", value);
//   }
//   println!("Done.");
//   Ok(())
// }
