//! Test utility.

use alchem::native_fn;
use alchem::value::Value;
use alchem::vm::{Vm, Runner};
use macro_rules_attribute::macro_rules_attribute;
use std::convert::TryInto;

#[allow(dead_code)]
pub async fn expect_script(script: &str, expected: Value) {
  let vm = Vm::new();
  assert_eq!(vm.interpret(script).await, expected);
}

#[allow(dead_code)]
pub async fn expect_with_natives(script: &str, expected: Value) {
  let mut vm = Vm::new();
  vm.add_native("fourty_two", fourty_two);
  vm.add_native("recall", recall);
  vm.add_native("recall_a1", recall_a1);
  assert_eq!(vm.interpret(script).await, expected);
}

#[macro_rules_attribute(native_fn!)]
/// A native function that returns 42. Roughly equal to `fn() { = 42 }`
async fn fourty_two(_vals: Vec<Value>, _runner: &mut Runner) -> Value { Value::Int(42) }

#[macro_rules_attribute(native_fn!)]
/// A native function that calls its first argument, which must be a 0-arity function. Roughly `fn(f) { = f() }`
async fn recall(vals: Vec<Value>, runner: &mut Runner) -> Value {
  let f = vals[0].as_closure();
  runner.run_closure(f, Vec::new()).await
}

#[macro_rules_attribute(native_fn!)]
/// A native function that calls its first argument with its second, which must be a 1-arity function. Roughly
/// `fn(f, a) { = f(a) }`
async fn recall_a1(vals: Vec<Value>, runner: &mut Runner) -> Value {
  let [f, a]: [Value; 2] = vals.try_into().unwrap();
  runner.run_closure(f.as_closure(), vec![a]).await
}
