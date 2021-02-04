//! Some natives to add to the standard Alchem compile/runtime.

use crate::collapsed::CollapsedInfo;
use crate::value::{add_native, match_native, CustomType, Function, Globals, MorphStatus, NativeInfo, Type, Value};
use crate::vm::{compile, Runner};
use crate::{native_fn, native_tfn};
use macro_rules_attribute::macro_rules_attribute;
use std::sync::Arc;

pub fn add_std<C: CustomType + 'static>(globals: &mut Globals<C>) {
  match_native(globals, "eval", 1, ntv_eval, ntvt_eval, ntvm_eval);
  add_native(globals, "print", 1, ntv_print, ntvt_print);
}

fn ntvm_eval<C: CustomType + 'static>(a1: &[Type<C>], a2: &[Type<C>]) -> bool {
  assert_eq!(a1.len(), 1);
  assert_eq!(a2.len(), 1);

  let c1 = a1[0].as_string().as_deref().expect("Eval arg must be a literal string.");
  let c2 = a2[0].as_string().as_deref().expect("Eval arg must be a literal string.");
  c1 == c2
}

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_eval<C: CustomType + 'static>(args: Vec<Type<C>>, globals: &Globals<C>) -> MorphStatus<C> {
  let code = args[0].as_string().as_deref().expect("Eval argument must be a literal string.");
  let (scope, stype) = compile(code, &globals).await;
  let chunk = scope.into_chunk();
  let function = Function::script(chunk, stype.clone());

  let mut info = NativeInfo::new();
  info.add_eval_function(Arc::new(function));
  MorphStatus::NativeCompleted(info, stype)
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_eval<C: CustomType + 'static>(
  _vals: Vec<Value<C>>, info: CollapsedInfo<C>, runner: &mut Runner<C>
) -> Value<C> {
  let closure = info.eval_functions()[0].clone();
  runner.run_closure(closure, 0, Vec::new()).await
}

#[macro_rules_attribute(native_tfn!)]
async fn ntvt_print<C: CustomType + 'static>(_args: Vec<Type<C>>, _globals: &Globals<C>) -> MorphStatus<C> {
  let info = NativeInfo::new();
  MorphStatus::NativeCompleted(info, Type::Number)
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_print<C: CustomType + 'static>(
  vals: Vec<Value<C>>, _info: CollapsedInfo<C>, _runner: &mut Runner<C>
) -> Value<C> {
  println!("*** PRINT: {:?}", vals[0]);
  Value::Int(1)
}
