//! A set of tests for making sure functions work.

mod util;

use util::expect_script;

// Closure tests are in their own 'closure.rs'

#[tokio::test]
async fn function_simple() { expect_script("f=fn(){=1};=f()", 1.into()).await; }

#[tokio::test]
async fn function_nested() { expect_script("f=fn(){=fn(){=1}};f1=f();=f1()", 1.into()).await; }

#[tokio::test]
async fn function_inline() { expect_script("=fn(){=1}()", 1.into()).await; }

#[tokio::test]
async fn function_nested_inline() { expect_script("=fn(){=fn(){=1}}()()", 1.into()).await; }

#[tokio::test]
async fn parameters() { expect_script("f=fn(a){=a};=f(1)", 1.into()).await; }

#[tokio::test]
async fn nested_parameters() { expect_script("f=fn(){=fn(a){=a}};fx=f();=fx(1)", 1.into()).await; }
