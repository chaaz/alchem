//! Test utility.

mod jsons;
mod natives;

use alchem::value::{Globals, NoCustom, Value};
use alchem::vm::Vm;
use serde_json::{Number, Value as Json};

type Map = serde_json::Map<String, Json>;

#[allow(dead_code)]
pub async fn expect<V: Into<Value<NoCustom>>>(script: &str, expected: V) {
  let vm = Vm::new();
  assert_eq!(vm.interpret(script, new_globals()).await, expected.into());
}

#[allow(dead_code)]
pub async fn expectn<V: Into<Value<NoCustom>>>(script: &str, expected: V) {
  let vm = Vm::new();

  let mut globals = new_globals();
  natives::add_all_natives(&mut globals);

  assert_eq!(vm.interpret(script, globals).await, expected.into());
}

#[allow(dead_code)]
pub async fn expectj<V: Into<Value<NoCustom>>>(script: &str, expected: V) {
  let vm = Vm::new();

  let mut globals = new_globals();
  jsons::add_all_natives(&mut globals);

  let expc = expected.into();
  assert_eq!(vm.interpret(script, globals).await, expc);
}

#[allow(dead_code)]
pub async fn expectj_f64(script: &str, expected: f64) { expectj(script, n(expected)).await }

#[allow(dead_code)]
pub async fn expectj_bool(script: &str, expected: bool) { expectj(script, Json::Bool(expected)).await }

#[allow(dead_code)]
pub async fn expectj_str(script: &str, expected: &str) { expectj(script, Json::String(expected.to_string())).await }

#[allow(dead_code)]
pub async fn expectj_array(script: &str, expected: &[f64]) {
  expectj(script, Json::Array(expected.iter().copied().map(n).collect())).await
}

#[allow(dead_code)]
pub async fn expectj_vec(script: &str, expected: Vec<Json>) { expectj(script, Json::Array(expected)).await }

#[allow(dead_code)]
pub async fn expectj_obj(script: &str, expected: Map) { expectj(script, Json::Object(expected)).await }

pub fn n(v: f64) -> Json { Json::Number(Number::from_f64(v).unwrap()) }

pub fn new_globals() -> Globals<NoCustom> { Globals::new() }
