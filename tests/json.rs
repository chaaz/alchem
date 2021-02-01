//! A set of json-based tests for the alchemy vm.

#![recursion_limit = "1024"]

mod util;

use serde_json::Value as Json;
use util::{expectj_array, expectj_bool, expectj_f64, expectj_obj, expectj_str, expectj_vec, n};

type Map = serde_json::Map<String, serde_json::Value>;

#[tokio::test]
async fn convert_int() { expectj_f64("=to_json(1)", 1.0).await; }

#[tokio::test]
async fn convert_float() { expectj_f64("=to_json(1.0)", 1.0).await; }

#[tokio::test]
async fn convert_bool() { expectj_bool("=to_json(true)", true).await; }

#[tokio::test]
async fn convert_str() { expectj_str(r#"=to_json("a")"#, "a").await; }

#[tokio::test]
async fn convert_array() { expectj_array("=to_json([1, 2])", &[1.0, 2.0]).await; }

#[tokio::test]
async fn convert_obj() { expectj_obj("=to_json({a:1,b:2})", new_obj()).await; }

#[tokio::test]
async fn convert_obj_rev() { expectj_obj("=to_json({b:2,a:1})", new_obj()).await; }

#[tokio::test]
async fn convert_nest() { expectj_obj("=to_json({a:1,b:{c:{d:4,e:5},f:6}})", nest_obj()).await; }

#[tokio::test]
async fn convert_complex() { expectj_vec("=to_json([1,{c:[4,5],f:6}])", cplx_obj()).await; }

#[tokio::test]
async fn json_dot_array() { expectj_f64("=to_json([1,2]).0", 1.0).await; }

#[tokio::test]
async fn json_dot_object() { expectj_f64("=to_json({a:1,b:2}).a", 1.0).await; }

#[tokio::test]
async fn unify_sum_l() { expectj_f64("=to_json(1)+2", 3.0).await; }

#[tokio::test]
async fn unify_sum_r() { expectj_f64("=2+to_json(1)", 3.0).await; }

#[tokio::test]
async fn unify_diff_l() { expectj_f64("=to_json(1)-2", -1.0).await; }

#[tokio::test]
async fn unify_diff_r() { expectj_f64("=2-to_json(1)", 1.0).await; }

#[tokio::test]
#[should_panic]
async fn fn_to_json_fail() { expectj_f64("=to_json(fn(){=1})", 1.0).await; }

#[tokio::test]
#[should_panic]
async fn nest_to_json_fail() { expectj_f64("=to_json([fn(){=1}])", 1.0).await; }

#[tokio::test]
async fn double_convert() { expectj_f64("=to_json(to_json(1))", 1.0).await; }

fn new_obj() -> Map {
  let mut expected = Map::new();
  expected.insert("a".to_string(), n(1.0));
  expected.insert("b".to_string(), n(2.0));
  expected
}

fn cplx_obj() -> Vec<Json> {
  let d = vec![n(4.0), n(5.0)];

  let mut c = Map::new();
  c.insert("c".to_string(), Json::Array(d));
  c.insert("f".to_string(), n(6.0));

  vec![n(1.0), Json::Object(c)]
}

fn nest_obj() -> Map {
  let mut d = Map::new();
  d.insert("d".to_string(), n(4.0));
  d.insert("e".to_string(), n(5.0));

  let mut c = Map::new();
  c.insert("c".to_string(), Json::Object(d));
  c.insert("f".to_string(), n(6.0));

  let mut a = Map::new();
  a.insert("a".to_string(), n(1.0));
  a.insert("b".to_string(), Json::Object(c));

  a
}
