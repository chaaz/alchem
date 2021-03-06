//! Some native functions for alchem testing.

use alchem::collapsed::{Captured, RunMeta};
use alchem::value::{add_native, add_std, Globals, MorphStatus, NativeInfo, NoCustom, Type, TypeCaptured, Value};
use alchem::vm::Runner;
use alchem_macros::{native_fn, native_tfn};

type Val = Value<NoCustom>;
type Info = NativeInfo<NoCustom>;
type Capd = Captured<NoCustom>;
type Run = Runner<NoCustom>;
type Tp = Type<NoCustom>;
type Gl = Globals<NoCustom>;
type Status = MorphStatus<NoCustom>;
type Meta = RunMeta<NoCustom>;
type Tc = TypeCaptured<NoCustom>;

pub fn add_all_natives(globals: &mut Gl) {
  add_std(globals);
  add_native(globals, "fourty_two", 0, ntv_number, ntvt_number);
  add_native(globals, "recall", 1, ntv_recall, ntvt_recall);
  add_native(globals, "recall_1", 2, ntv_recall_1, ntvt_recall_1);
  add_native(globals, "reloop", 1, ntv_reloop, ntvt_reloop);
}

#[native_tfn]
async fn ntvt_number(_args: Vec<Tp>, _tc: Tc, _globals: &Gl) -> Status {
  MorphStatus::NativeCompleted(Info::new(), Type::Number)
}

#[native_fn]
async fn ntv_number(_argv: Vec<Val>, _info: Capd, _meta: Meta, _runner: &mut Run) -> Val { Value::Int(42) }

#[native_tfn]
async fn ntvt_recall(args: Vec<Tp>, _tc: Tc, globals: &Gl) -> Status {
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

#[native_fn]
async fn ntv_recall(vals: Vec<Val>, info: Capd, meta: Meta, runner: &mut Run) -> Val {
  let mut vals = vals;
  let f = vals.remove(0);
  let inst_ind = info.into_call_indexes().into_iter().next().unwrap();
  runner.run(f, inst_ind, meta, Vec::new()).await
}

#[native_tfn]
async fn ntvt_recall_1(args: Vec<Tp>, _tc: Tc, globals: &Gl) -> Status {
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

#[native_fn]
async fn ntv_recall_1(vals: Vec<Val>, info: Capd, meta: Meta, runner: &mut Run) -> Val {
  let mut vals = vals.into_iter();
  let f = vals.next().unwrap();
  let a = vals.next().unwrap();
  let inst_ind = info.into_call_indexes().into_iter().next().unwrap();
  runner.run(f, inst_ind, meta, vec![a]).await
}

#[native_tfn]
async fn ntvt_reloop(args: Vec<Tp>, _tc: Tc, globals: &Gl) -> Status {
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

#[native_fn]
async fn ntv_reloop(vals: Vec<Val>, info: Capd, meta: Meta, runner: &mut Run) -> Val {
  let mut vals = vals;
  let mut f = vals.remove(0);
  let inst_ind = info.into_call_indexes().into_iter().next().unwrap();
  let _ = runner.run(f.shift(), inst_ind.clone(), meta.clone(), Vec::new()).await;
  runner.run(f, inst_ind, meta, Vec::new()).await
}
