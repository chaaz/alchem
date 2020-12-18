//! Test utility.

use alchem::value::Value;
use alchem::vm::Vm;

pub fn expect_script(script: &str, expected: Value) {
  let mut vm = Vm::new();
  assert_eq!(vm.interpret(script).unwrap(), expected);
}
