//! A set of tests for making sure functions work.

mod util;

use util::expect_i32;

// Closure tests are in their own 'closure.rs'

#[tokio::test]
async fn function_simple() { expect_i32("f=fn(){=1};=f()", 1).await; }

#[tokio::test]
async fn function_nested() { expect_i32("f=fn(){=fn(){=1}};f1=f();=f1()", 1).await; }

#[tokio::test]
async fn function_inline() { expect_i32("=fn(){=1}()", 1).await; }

#[tokio::test]
async fn function_nested_inline() { expect_i32("=fn(){=fn(){=1}}()()", 1).await; }

#[tokio::test]
async fn parameters() { expect_i32("f=fn(a){=a};=f(1)", 1).await; }

#[tokio::test]
async fn nested_parameters() { expect_i32("f=fn(){=fn(a){=a}};fx=f();=fx(1)", 1).await; }

#[tokio::test]
async fn return_op() { expect_i32("f=fn(){=1};=f()+1", 2).await; }
