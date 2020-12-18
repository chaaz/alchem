//! Test utility.

use alchem::vm::Vm;
use alchem::value::Value;

pub fn expect_script(script: &str, expected: Value) {
  let mut vm = Vm::new();
  assert_eq!(vm.interpret(script).unwrap(), expected);
}
