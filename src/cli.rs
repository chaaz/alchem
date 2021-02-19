//! The command-line options for the executable.

use alchem::collapsed::CollapsedInfo;
use alchem::errors::Result;
use alchem::value::{add_native, Globals, MorphStatus, NativeInfo, NoCustom, Type, Value};
use alchem::vm::{Runner, Vm};
use alchem::{native_fn, native_tfn};
use clap::{crate_version, App, AppSettings, Arg, ArgMatches};
use macro_rules_attribute::macro_rules_attribute;

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
  let vm = Vm::new(());

  let mut globals = new_globals();
  add_native(&mut globals, "print", 1, ntv_print, ntvt_print);
  add_native(&mut globals, "number", 1, ntv_number, ntvt_number);
  add_native(&mut globals, "recall", 1, ntv_recall, ntvt_recall);

  println!("{:?}", vm.interpret(&val, globals).await);
  Ok(())
}

type Val = Value<NoCustom>;
type CoInfo = CollapsedInfo<NoCustom>;
type Run = Runner<NoCustom>;
type Tp = Type<NoCustom>;
type Gl = Globals<NoCustom>;
type Status = MorphStatus<NoCustom>;

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_print(_args: Vec<Tp>, _globals: &Gl) -> Status {
  let info = NativeInfo::new();
  MorphStatus::NativeCompleted(info, Type::Number)
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_print(vals: Vec<Val>, _info: CoInfo, _runner: &mut Run) -> Val {
  println!("*** PRINT: {:?}", vals[0]);
  Value::Int(1)
}

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_number(_a: Vec<Tp>, _b: &Gl) -> Status { MorphStatus::NativeCompleted(NativeInfo::new(), Type::Number) }

#[macro_rules_attribute(native_fn!)]
async fn ntv_number(_argv: Vec<Val>, _info: CoInfo, _runner: &mut Run) -> Val { Value::Int(42) }

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_recall(args: Vec<Tp>, globals: &Gl) -> Status {
  let mut info = NativeInfo::new();
  let func = args[0].as_function().upgrade().unwrap();
  let (inst_ind, ftype) = func.clone().find_or_build(Vec::new(), globals).await;

  if let Some(ftype) = ftype {
    info.add_call_index(inst_ind);
    MorphStatus::NativeCompleted(info, ftype)
  } else {
    MorphStatus::Known(Type::<NoCustom>::depends(&func, inst_ind.index()))
  }
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_recall(vals: Vec<Val>, info: CoInfo, runner: &mut Run) -> Val {
  let f = vals[0].as_closure();
  let inst_ind = info.into_call_indexes().into_iter().next().unwrap();
  runner.run_closure(f, inst_ind, Vec::new()).await
}

pub fn new_globals() -> Globals<NoCustom> { Globals::new() }

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
