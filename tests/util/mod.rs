//! Test utility.

mod natives;
pub mod singles;
pub mod total;

use alchem::value::{Globals, NoCustom};
use alchem::vm::Vm;
use serde_json::{Number, Value as Json};

type Map = serde_json::Map<String, Json>;

#[allow(dead_code)]
pub async fn expect_bool(script: &str, expected: bool) {
  assert_eq!(Vm::new(()).interpret(script, globals()).await.as_bool(), expected);
}

#[allow(dead_code)]
pub async fn expect_i32(script: &str, expected: i32) {
  assert_eq!(Vm::new(()).interpret(script, globals()).await.as_int(), expected);
}

#[allow(dead_code)]
pub async fn expect_f64(script: &str, expected: f64) {
  assert_eq!(Vm::new(()).interpret(script, globals()).await.as_float(), expected);
}

#[allow(dead_code)]
pub async fn expect_str(script: &str, expected: &str) {
  assert_eq!(Vm::new(()).interpret(script, globals()).await.as_str(), expected);
}

#[allow(dead_code)]
pub async fn expect_json_bool(script: &str, expected: bool) {
  assert_eq!(Vm::new(()).interpret(script, globals()).await.as_json().as_bool().unwrap(), expected);
}

#[allow(dead_code)]
pub async fn expect_json_i64(script: &str, expected: i64) {
  assert_eq!(Vm::new(()).interpret(script, globals()).await.as_json().as_i64().unwrap(), expected);
}

#[allow(dead_code)]
pub async fn expect_json_f64(script: &str, expected: f64) {
  assert_eq!(Vm::new(()).interpret(script, globals()).await.as_json().as_f64().unwrap(), expected);
}

#[allow(dead_code)]
pub async fn expect_json_str(script: &str, expected: &str) {
  assert_eq!(Vm::new(()).interpret(script, globals()).await.as_json().as_str().unwrap(), expected);
}

#[allow(dead_code)]
pub async fn expect_array_f64(script: &str, expected: &[f64]) {
  assert_eq!(
    Vm::new(()).interpret(script, globals()).await.as_json(),
    &Json::Array(expected.iter().copied().map(n).collect())
  )
}

#[allow(dead_code)]
pub async fn expect_vec_json(script: &str, expected: Vec<Json>) {
  assert_eq!(Vm::new(()).interpret(script, globals()).await.as_json(), &Json::Array(expected))
}

#[allow(dead_code)]
pub async fn expect_map_json(script: &str, expected: Map) {
  assert_eq!(Vm::new(()).interpret(script, globals()).await.as_json(), &Json::Object(expected))
}

pub fn n(v: f64) -> Json { Json::Number(Number::from_f64(v).unwrap()) }

fn globals() -> Globals<NoCustom> {
  let mut globals = Globals::new();
  natives::add_all_natives(&mut globals);
  globals
}
