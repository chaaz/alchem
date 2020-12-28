//! A set of basic tests for the alchemy vm.

mod util;

use alchem::value::Value;
use util::expect_script;

#[tokio::test]
async fn simple_int() { expect_script("=1", Value::Int(1)).await; }

#[tokio::test]
async fn simple_float() { expect_script("=1.1", Value::Float(1.1)).await; }

#[tokio::test]
async fn simple_string() { expect_script(r#"="hello""#, "hello".into()).await; }

#[tokio::test]
async fn expr_math() { expect_script("= 1 + 1", 2.into()).await; }

#[tokio::test]
async fn expr_subt() { expect_script("= 2 - 1", 1.into()).await; }

#[tokio::test]
async fn expr_math_prec() { expect_script("= 1 + 1 * 2", 3.into()).await; }

#[tokio::test]
async fn expr_math_complex() { expect_script("= 2 * 1 + 1 * 2", 4.into()).await; }

#[tokio::test]
async fn expr_bool() { expect_script("= 1 == 1", true.into()).await; }

#[tokio::test]
async fn expr_bool_complex() { expect_script("= 1 + 2 == 2 * 1 + 1", true.into()).await; }

#[tokio::test]
async fn expr_grouping() { expect_script("= (1 + 1) * 2", 4.into()).await; }

#[tokio::test]
async fn expr_bool_grouping() { expect_script("= (1 + 1 == 2) == true", true.into()).await; }
