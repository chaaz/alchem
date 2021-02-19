//! Some native functions for alchem testing.

use alchem::collapsed::CollapsedInfo;
use alchem::value::{add_native, add_std, Globals, MorphStatus, NativeInfo, NoCustom, Type, Value};
use alchem::vm::Runner;
use alchem::{native_fn, native_tfn};
use macro_rules_attribute::macro_rules_attribute;

type Val = Value<NoCustom>;
type Info = NativeInfo<NoCustom>;
type CoInfo = CollapsedInfo<NoCustom>;
type Run = Runner<NoCustom>;
type Tp = Type<NoCustom>;
type Gl = Globals<NoCustom>;
type Status = MorphStatus<NoCustom>;

pub fn add_all_natives(globals: &mut Gl) {
  add_std(globals);
  add_native(globals, "fourty_two", 0, ntv_number, ntvt_number);
  add_native(globals, "recall", 1, ntv_recall, ntvt_recall);
  add_native(globals, "recall_1", 2, ntv_recall_1, ntvt_recall_1);
  add_native(globals, "reloop", 1, ntv_reloop, ntvt_reloop);
}

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_number(_args: Vec<Tp>, _globals: &Gl) -> Status {
  MorphStatus::NativeCompleted(Info::new(), Type::Number)
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_number(_argv: Vec<Val>, _info: CoInfo, _runner: &mut Run) -> Val { Value::Int(42) }

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_recall(args: Vec<Tp>, globals: &Gl) -> Status {
  assert_eq!(args.len(), 1);
  let func = args[0].as_function().upgrade().unwrap();
  assert_eq!(func.arity(), 0);
  let (inst_ind, ftype) = func.clone().find_or_build(Vec::new(), globals).await;

  if let Some(ftype) = ftype {
    let mut info = Info::new();
    info.add_call_index(inst_ind);
    MorphStatus::NativeCompleted(info, ftype)
  } else {
    MorphStatus::Known(Type::depends(&func, inst_ind.index()))
  }
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_recall(vals: Vec<Val>, info: CoInfo, runner: &mut Run) -> Val {
  let mut vals = vals;
  let f = vals.remove(0);
  let inst_ind = info.into_call_indexes().into_iter().next().unwrap();
  runner.run_value(f, inst_ind, Vec::new()).await
}

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_recall_1(args: Vec<Tp>, globals: &Gl) -> Status {
  let mut args = args.into_iter();
  let func = args.next().unwrap().as_function().upgrade().unwrap();
  assert_eq!(func.arity(), 1);
  let (inst_ind, ftype) = func.clone().find_or_build(args.collect(), globals).await;

  if let Some(ftype) = ftype {
    let mut info = Info::new();
    info.add_call_index(inst_ind);
    MorphStatus::NativeCompleted(info, ftype)
  } else {
    MorphStatus::Known(Type::depends(&func, inst_ind.index()))
  }
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_recall_1(vals: Vec<Val>, info: CoInfo, runner: &mut Run) -> Val {
  let mut vals = vals.into_iter();
  let f = vals.next().unwrap();
  let a = vals.next().unwrap();
  let inst_ind = info.into_call_indexes().into_iter().next().unwrap();
  runner.run_value(f, inst_ind, vec![a]).await
}

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_reloop(args: Vec<Tp>, globals: &Gl) -> Status {
  assert_eq!(args.len(), 1);
  let f = args[0].as_function().upgrade().unwrap();
  assert_eq!(f.arity(), 0);
  assert!(!f.is_single_use());
  let (inst_ind, ftype) = f.clone().find_or_build(Vec::new(), globals).await;

  if let Some(ftype) = ftype {
    let mut info = Info::new();
    info.add_call_index(inst_ind);
    MorphStatus::NativeCompleted(info, ftype)
  } else {
    MorphStatus::Known(Type::depends(&f, inst_ind.index()))
  }
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_reloop(vals: Vec<Val>, info: CoInfo, runner: &mut Run) -> Val {
  let mut vals = vals;
  let mut f = vals.remove(0);
  let inst_ind = info.into_call_indexes().into_iter().next().unwrap();
  let _ = runner.run_value(f.shift(), inst_ind.clone(), Vec::new()).await;
  runner.run_value(f, inst_ind, Vec::new()).await
}
