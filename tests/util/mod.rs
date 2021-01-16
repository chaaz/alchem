//! Test utility.

use alchem::native_fn;
use alchem::value::{add_native, new_globals, Globals, MorphStatus, NativeInfo, Type, Value};
use alchem::vm::{Runner, Vm};
use macro_rules_attribute::macro_rules_attribute;

#[allow(dead_code)]
pub async fn expect<V: Into<Value>>(script: &str, expected: V) {
  let vm = Vm::new();
  assert_eq!(vm.interpret(script, new_globals()).await, expected.into());
}

#[allow(dead_code)]
pub async fn expectn<V: Into<Value>>(script: &str, expected: V) {
  let vm = Vm::new();

  let mut globals = new_globals();
  add_native(&mut globals, "print", 1, ntv_print, ntvt_print);
  add_native(&mut globals, "fourty_two", 0, ntv_number, ntvt_number);
  add_native(&mut globals, "recall", 1, ntv_recall, ntvt_recall);
  add_native(&mut globals, "recall_1", 2, ntv_recall_1, ntvt_recall_1);

  assert_eq!(vm.interpret(script, globals).await, expected.into());
}

fn ntvt_print(_args: Vec<Type>, _globals: &Globals) -> MorphStatus {
  let info = NativeInfo::new();
  MorphStatus::NativeCompleted(info, Type::Number)
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_print(vals: Vec<Value>, _info: NativeInfo, _runner: &mut Runner) -> Value {
  println!("*** PRINT: {:?}", vals[0]);
  Value::Int(1)
}

fn ntvt_number(_: Vec<Type>, _: &Globals) -> MorphStatus {
  MorphStatus::NativeCompleted(NativeInfo::new(), Type::Number)
}

#[macro_rules_attribute(native_fn!)]
async fn ntv_number(_argv: Vec<Value>, _info: NativeInfo, _runner: &mut Runner) -> Value { Value::Int(42) }

fn ntvt_recall(args: Vec<Type>, globals: &Globals) -> MorphStatus {
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
async fn ntv_recall(vals: Vec<Value>, info: NativeInfo, runner: &mut Runner) -> Value {
  let mut vals = vals;
  let f = vals.remove(0);
  let inst_ind = info.call_indexes()[0];
  runner.run_value(f, inst_ind, Vec::new()).await
}

fn ntvt_recall_1(args: Vec<Type>, globals: &Globals) -> MorphStatus {
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
async fn ntv_recall_1(vals: Vec<Value>, info: NativeInfo, runner: &mut Runner) -> Value {
  let mut vals = vals.into_iter();
  let f = vals.next().unwrap();
  let a = vals.next().unwrap();
  let inst_ind = info.call_indexes()[0];
  runner.run_value(f, inst_ind, vec![a]).await
}
