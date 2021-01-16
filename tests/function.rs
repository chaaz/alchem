//! A set of tests for making sure functions work.

mod util;

use util::expect;

// Closure tests are in their own 'closure.rs'

#[tokio::test]
async fn function_simple() { expect("f=fn(){=1};=f()", 1).await; }

#[tokio::test]
async fn function_nested() { expect("f=fn(){=fn(){=1}};f1=f();=f1()", 1).await; }

#[tokio::test]
async fn function_inline() { expect("=fn(){=1}()", 1).await; }

#[tokio::test]
async fn function_nested_inline() { expect("=fn(){=fn(){=1}}()()", 1).await; }

#[tokio::test]
async fn parameters() { expect("f=fn(a){=a};=f(1)", 1).await; }

#[tokio::test]
async fn nested_parameters() { expect("f=fn(){=fn(a){=a}};fx=f();=fx(1)", 1).await; }

#[tokio::test]
async fn return_op() { expect("f=fn(){=1};=f()+1", 2).await; }
