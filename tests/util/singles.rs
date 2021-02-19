//! Some native functions for alchem testing.

use alchem::collapsed::CollapsedInfo;
use alchem::value::{add_native, add_std, CustomType, CustomValue, Globals, IsSingle, MorphStatus, NativeInfo, Type,
                    Value};
use alchem::vm::{Runner, Vm};
use alchem::{native_fn, native_tfn};
use macro_rules_attribute::macro_rules_attribute;

#[allow(dead_code)]
pub async fn expect_i32(script: &str, expected: i32) {
  assert_eq!(Vm::new(()).interpret(script, globals()).await.as_int(), expected);
}

#[allow(dead_code)]
pub async fn expect_str(script: &str, expected: &str) {
  assert_eq!(Vm::new(()).interpret(script, globals()).await.as_str(), expected);
}

#[allow(dead_code)]
pub async fn expect_u1(script: &str) {
  assert_eq!(Vm::new(()).interpret(script, globals()).await.as_custom(), &OneVal);
}

#[derive(Clone, Debug)]
pub struct OneUse;

impl CustomType for OneUse {
  type Collapsed = ();
  type Value = OneVal;
  type Runtime = ();

  fn collapse(&self) {}
}

impl PartialEq for OneUse {
  fn eq(&self, _other: &Self) -> bool { true }
}

impl Eq for OneUse {}

impl IsSingle for OneUse {
  fn is_single_use(&self) -> bool { true }
}

#[derive(Clone, Debug)]
pub struct OneVal;

impl CustomValue for OneVal {
  fn shift(&mut self) -> Option<Self> { Some(self.clone()) }
}

impl PartialEq for OneVal {
  fn eq(&self, _other: &Self) -> bool { true }
}

impl Eq for OneVal {}

type Val = Value<OneUse>;
type Info = NativeInfo<OneUse>;
type CoInfo = CollapsedInfo<OneUse>;
type Run = Runner<OneUse>;
type Tp = Type<OneUse>;
type Gl = Globals<OneUse>;
type Status = MorphStatus<OneUse>;

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_u1(args: Vec<Tp>, globals: &Gl) -> Status {
  MorphStatus::NativeCompleted(Info::new(), Type::Custom(OneUse))
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_u1(vals: Vec<Val>, info: CoInfo, runner: &mut Run) -> Val { Val::Custom(OneVal) }

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

fn add_all_natives(globals: &mut Gl) {
  add_std(globals);
  add_native(globals, "reloop", 1, ntv_reloop, ntvt_reloop);
  add_native(globals, "u1", 0, ntv_u1, ntvt_u1);
}

fn globals() -> Globals<OneUse> {
  let mut globals = Globals::new();
  add_all_natives(&mut globals);
  globals
}
