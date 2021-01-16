//! A set of basic tests for the alchemy vm.

mod util;

use util::expect;

#[tokio::test]
async fn simple_int() { expect("=1", 1).await; }

#[tokio::test]
async fn simple_float() { expect("=1.1", 1.1).await; }

#[tokio::test]
async fn simple_string() { expect(r#"="hello""#, "hello").await; }

#[tokio::test]
async fn expr_math() { expect("= 1 + 1", 2).await; }

#[tokio::test]
async fn expr_subt() { expect("= 2 - 1", 1).await; }

#[tokio::test]
async fn expr_math_prec() { expect("= 1 + 1 * 2", 3).await; }

#[tokio::test]
async fn expr_math_complex() { expect("= 2 * 1 + 1 * 2", 4).await; }

#[tokio::test]
async fn expr_bool() { expect("= 1 == 1", true).await; }

#[tokio::test]
async fn expr_bool_complex() { expect("= 1 + 2 == 2 * 1 + 1", true).await; }

#[tokio::test]
async fn expr_grouping() { expect("= (1 + 1) * 2", 4).await; }

#[tokio::test]
async fn expr_bool_grouping() { expect("= (1 + 1 == 2) == true", true).await; }
