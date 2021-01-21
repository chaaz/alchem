//! Some native functions for alchem testing.

use alchem::native_fn;
use alchem::value::{Globals, MorphStatus, NativeInfo, NoCustom, Type, Value};
use alchem::vm::Runner;
use macro_rules_attribute::macro_rules_attribute;

type Val = Value<NoCustom>;
type Info = NativeInfo<NoCustom>;
type Run = Runner<NoCustom>;
type Tp = Type<NoCustom>;
type Gl = Globals<NoCustom>;
type Status = MorphStatus<NoCustom>;

pub fn ntvt_print(_args: Vec<Tp>, _globals: &Gl) -> Status {
  let info = NativeInfo::new();
  MorphStatus::NativeCompleted(info, Type::Number)
}

#[macro_rules_attribute(native_fn!)]
pub async fn ntv_print(vals: Vec<Val>, _info: Info, _runner: &mut Run) -> Val {
  println!("*** PRINT: {:?}", vals[0]);
  Value::Int(1)
}

pub fn ntvt_number(_: Vec<Tp>, _: &Gl) -> Status {
  MorphStatus::NativeCompleted(NativeInfo::new(), Type::Number)
}

#[macro_rules_attribute(native_fn!)]
pub async fn ntv_number(_argv: Vec<Val>, _info: Info, _runner: &mut Run) -> Val { Value::Int(42) }

pub fn ntvt_recall(args: Vec<Tp>, globals: &Gl) -> Status {
  assert_eq!(args.len(), 1);
  let func = args[0].as_function().upgrade().unwrap();
  assert_eq!(func.arity(), 0);
  let (inst_ind, ftype) = func.find_or_build(Vec::new(), globals);

  if let Some(ftype) = ftype {
    let mut info = NativeInfo::new();
    info.add_call_index(inst_ind);
    MorphStatus::NativeCompleted(info, ftype)
  } else {
    MorphStatus::Known(Type::depends(&func, inst_ind))
  }
}

#[macro_rules_attribute(native_fn!)]
pub async fn ntv_recall(vals: Vec<Val>, info: Info, runner: &mut Run) -> Val {
  let mut vals = vals;
  let f = vals.remove(0);
  let inst_ind = info.call_indexes()[0];
  runner.run_value(f, inst_ind, Vec::new()).await
}

pub fn ntvt_recall_1(args: Vec<Tp>, globals: &Gl) -> Status {
  let mut args = args.into_iter();
  let func = args.next().unwrap().as_function().upgrade().unwrap();
  assert_eq!(func.arity(), 1);
  let (inst_ind, ftype) = func.find_or_build(args.collect(), globals);

  if let Some(ftype) = ftype {
    let mut info = NativeInfo::new();
    info.add_call_index(inst_ind);
    MorphStatus::NativeCompleted(info, ftype)
  } else {
    MorphStatus::Known(Type::depends(&func, inst_ind))
  }
}

#[macro_rules_attribute(native_fn!)]
pub async fn ntv_recall_1(vals: Vec<Val>, info: Info, runner: &mut Run) -> Val {
  let mut vals = vals.into_iter();
  let f = vals.next().unwrap();
  let a = vals.next().unwrap();
  let inst_ind = info.call_indexes()[0];
  runner.run_value(f, inst_ind, vec![a]).await
}

pub fn ntvt_reloop(args: Vec<Tp>, globals: &Gl) -> Status {
  assert_eq!(args.len(), 1);
  let func = args[0].as_function().upgrade().unwrap();
  assert_eq!(func.arity(), 0);
  assert!(!func.is_single_use());
  let (inst_ind, ftype) = func.find_or_build(Vec::new(), globals);

  if let Some(ftype) = ftype {
    let mut info = NativeInfo::new();
    info.add_call_index(inst_ind);
    MorphStatus::NativeCompleted(info, ftype)
  } else {
    MorphStatus::Known(Type::depends(&func, inst_ind))
  }
}

#[macro_rules_attribute(native_fn!)]
pub async fn ntv_reloop(vals: Vec<Val>, info: Info, runner: &mut Run) -> Val {
  let mut vals = vals;
  let mut f = vals.remove(0);
  let inst_ind = info.call_indexes()[0];
  let _ = runner.run_value(f.shift(), inst_ind, Vec::new()).await;
  runner.run_value(f, inst_ind, Vec::new()).await
}
