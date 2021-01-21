//! Test utility.

mod natives;
mod jsons;

use alchem::value::{add_native, new_globals, Value, NoCustom};
use alchem::vm::Vm;
use natives::{
  ntv_print, ntvt_print, ntv_number, ntvt_number, ntv_recall, ntvt_recall, ntv_recall_1, ntvt_recall_1, ntv_reloop,
  ntvt_reloop
};
use jsons::{ntv_to_json, ntvt_to_json};
use serde_json::{Number, Value as Json};

#[allow(dead_code)]
pub async fn expect<V: Into<Value<NoCustom>>>(script: &str, expected: V) {
  let vm = Vm::new();
  assert_eq!(vm.interpret(script, new_globals()).await, expected.into());
}

#[allow(dead_code)]
pub async fn expectn<V: Into<Value<NoCustom>>>(script: &str, expected: V) {
  let vm = Vm::new();

  let mut globals = new_globals();
  add_native(&mut globals, "print", 1, ntv_print, ntvt_print);
  add_native(&mut globals, "fourty_two", 0, ntv_number, ntvt_number);
  add_native(&mut globals, "recall", 1, ntv_recall, ntvt_recall);
  add_native(&mut globals, "recall_1", 2, ntv_recall_1, ntvt_recall_1);
  add_native(&mut globals, "reloop", 1, ntv_reloop, ntvt_reloop);

  assert_eq!(vm.interpret(script, globals).await, expected.into());
}

#[allow(dead_code)]
pub async fn expectj<V: Into<Value<NoCustom>>>(script: &str, expected: V) {
  let vm = Vm::new();

  let mut globals = new_globals();
  add_native(&mut globals, "to_json", 1, ntv_to_json, ntvt_to_json);

  let expc = expected.into();
  assert_eq!(vm.interpret(script, globals).await, expc);
}

#[allow(dead_code)]
pub async fn expectj_f64(script: &str, expected: f64) {
  let expected = Json::Number(Number::from_f64(expected).unwrap());
  expectj(script, expected).await
}
