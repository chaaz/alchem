//! Test utility.

use alchem::value::Value;
use alchem::vm::Vm;

#[allow(dead_code)]
pub async fn expect_script<V: Into<Value>>(script: &str, expected: V) {
  let vm = Vm::new();
  assert_eq!(vm.interpret(script).await, expected.into());
}
