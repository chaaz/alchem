//! Some functions for testing some complex native function scenarios.

use alchem::collapsed::{Captured, RunMeta};
use alchem::value::{add_native, add_std, Closure, Function, FunctionIndex, Globals, MorphStatus, NativeInfo, NoCustom,
                    Type, TypeCaptured, Value};
use alchem::vm::{collapse_script, compile, Runner};
use alchem_macros::{native_fn, native_tfn};
use std::sync::Arc;

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
  add_native(globals, "make_run", 1, ntv_make_run, ntvt_make_run);
}

#[allow(dead_code)]
pub async fn expect_total_i64(script: &str, expected: i64) {
  // Globals
  let mut globals = Globals::new();
  add_all_natives(&mut globals);

  // Compile
  let (scope, stype) = compile(script, &globals).await;
  let func = stype.as_function().upgrade().unwrap();
  assert_eq!(func.arity(), 0);
  let (f_inst_ind, ftype) = func.find_or_build(vec![], &globals).await;
  assert_eq!(ftype, Some(Type::Number));

  // Collapse
  let (script, s_inst_ind, globals) = collapse_script(scope, stype, globals);
  let script = Arc::new(Closure::new(Arc::new(script), Vec::new()));

  // Run the script, then the returned function.
  let mut runner = Runner::fresh(globals, ());
  let meta = Meta::new(0);
  let func = runner.run(script.into(), FunctionIndex::empty(s_inst_ind), meta.clone(), Vec::new()).await;
  let out = runner.run(func, f_inst_ind, meta, Vec::new()).await;

  assert_eq!(out.as_int(), expected);
}

#[native_tfn]
async fn ntvt_make_run(args: Vec<Tp>, _tc: Tc, _globals: &Gl) -> Status {
  assert!(args.iter().all(|a| a.is_function()));

  let rfunc = make_run(args);
  let mut info = Info::new();
  info.add_function(rfunc.clone());
  Status::NativeCompleted(info, Type::FnSync(Arc::downgrade(&rfunc)))
}

fn make_run(fns: Vec<Tp>) -> Arc<Function<NoCustom>> {
  Arc::new(Function::capture_native(0, ntv_run, ntvt_run, fns, Vec::new()))
}

#[native_fn]
async fn ntv_make_run(vals: Vec<Val>, info: Capd, _meta: Meta, _runner: &mut Run) -> Val {
  let rfunc = info.into_functions().into_iter().next().unwrap();
  rfunc.set_native_captured(vals);
  rfunc.into_value()
}

#[native_tfn]
async fn ntvt_run(_args: Vec<Tp>, tc: Tc, globals: &Gl) -> Status {
  let mut tc = tc.into_iter();
  let rfunc = tc.next().unwrap().as_function().upgrade().unwrap();
  assert_eq!(rfunc.arity(), 0);
  assert!(!rfunc.is_single_use());
  let (rfunc_ind, rtype) = rfunc.clone().find_or_build(Vec::new(), globals).await;

  if let Some(rtype) = rtype {
    assert!(rtype.is_number());
    let mut info = Info::new();
    info.add_call_index(rfunc_ind);
    Status::NativeCompleted(info, rtype)
  } else {
    MorphStatus::Known(Type::depends(&rfunc, rfunc_ind.index()))
  }
}

#[native_fn]
async fn ntv_run(_vals: Vec<Val>, info: Capd, meta: Meta, runner: &mut Run) -> Val {
  let (inds, _, _, caps) = info.into_parts();
  let rfunc = caps.into_iter().next().unwrap();
  let rfunc_ind = inds.into_iter().next().unwrap();

  runner.run(rfunc, rfunc_ind, meta.clone(), Vec::new()).await
}
