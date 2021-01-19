//! The command-line options for the executable.

use alchem::errors::Result;
use alchem::native_fn;
use alchem::value::{add_native, new_globals, Globals, MorphStatus, NativeInfo, Type, NoCustom, Value};
use alchem::vm::{Runner, Vm};
use clap::{crate_version, App, AppSettings, Arg, ArgMatches};
use macro_rules_attribute::macro_rules_attribute;
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

  let mut globals = new_globals();
  add_native(&mut globals, "print", 1, ntv_print, ntvt_print);
  add_native(&mut globals, "number", 1, ntv_number, ntvt_number);
  add_native(&mut globals, "recall", 1, ntv_recall, ntvt_recall);

  println!("{:?}", vm.interpret(&val, globals).await);
  Ok(())
}

fn ntvt_print(_args: Vec<Type<NoCustom>>, _globals: &Globals<NoCustom>) -> MorphStatus<NoCustom> {
  let info = NativeInfo::new();
  MorphStatus::NativeCompleted(info, Type::Number)
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_print(vals: Vec<Value>, _info: NativeInfo, _runner: &mut Runner) -> Value {
  println!("*** PRINT: {:?}", vals[0]);
  Value::Int(1)
}

fn ntvt_number(_: Vec<Type<NoCustom>>, _: &Globals<NoCustom>) -> MorphStatus<NoCustom> {
  MorphStatus::NativeCompleted(NativeInfo::new(), Type::Number)
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_number(_argv: Vec<Value>, _info: NativeInfo, _runner: &mut Runner) -> Value { Value::Int(42) }

fn ntvt_recall(args: Vec<Type<NoCustom>>, globals: &Globals<NoCustom>) -> MorphStatus<NoCustom> {
  let mut info = NativeInfo::new();
  let func = args[0].as_function().upgrade().unwrap();
  let (inst_ind, ftype) = func.find_or_build(Vec::new(), globals);

  if let Some(ftype) = ftype {
    info.add_call_index(inst_ind);
    MorphStatus::NativeCompleted(info, ftype)
  } else {
    MorphStatus::Known(Type::<NoCustom>::depends(&func, inst_ind))
  }
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_recall(vals: Vec<Value>, info: NativeInfo, runner: &mut Runner) -> Value {
  let f = vals[0].as_closure();
  let inst_ind = info.call_indexes()[0];
  runner.run_closure(f, inst_ind, Vec::new()).await
}

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
