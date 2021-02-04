//! A set of basic tests for the alchemy vm.

mod util;

use util::{expect_i32, expect_f64, expect_str, expect_bool};

#[tokio::test]
async fn simple_int() { expect_i32("=1", 1).await; }

#[tokio::test]
async fn simple_float() { expect_f64("=1.1", 1.1).await; }

#[tokio::test]
#[should_panic]
async fn wrong_float() { expect_f64("=1.1.1", 1.1).await; }

#[tokio::test]
async fn simple_string() { expect_str(r#"="hello""#, "hello").await; }

#[tokio::test]
async fn expr_math() { expect_i32("= 1 + 1", 2).await; }

#[tokio::test]
async fn expr_subt() { expect_i32("= 2 - 1", 1).await; }

#[tokio::test]
async fn expr_math_prec() { expect_i32("= 1 + 1 * 2", 3).await; }

#[tokio::test]
async fn expr_math_complex() { expect_i32("= 2 * 1 + 1 * 2", 4).await; }

#[tokio::test]
async fn expr_bool() { expect_bool("= 1 == 1", true).await; }

#[tokio::test]
async fn expr_bool_complex() { expect_bool("= 1 + 2 == 2 * 1 + 1", true).await; }

#[tokio::test]
async fn expr_grouping() { expect_i32("= (1 + 1) * 2", 4).await; }

#[tokio::test]
async fn expr_bool_grouping() { expect_bool("= (1 + 1 == 2) == true", true).await; }
