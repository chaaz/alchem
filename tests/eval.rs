//! A set of eval tests for the alchemy vm.
//!
//! The tests in this suite use some native functions, defined in `util`:
//! - `eval(string)`: evaluates a context-free inline script
//! - `show(string)`: re-outputs the string: its return type is not a literal string

mod util;

use util::{expect_i32, expect_str};

#[tokio::test]
async fn simple() { expect_i32(r#"=eval("=1")"#, 1).await; }

#[tokio::test]
async fn fn_call() { expect_i32(r#"=eval("=fn(a){=a}(1)")"#, 1).await; }

#[tokio::test]
async fn fn_out_0() { expect_i32(r#"=eval("=fn(){=1}")()"#, 1).await; }

#[tokio::test]
async fn fn_out_1() { expect_i32(r#"=eval("=fn(a){=a}")(1)"#, 1).await; }

#[tokio::test]
async fn nonliteral() { expect_str(r#"=show("=1")"#, "=1").await; }

#[tokio::test]
#[should_panic]
async fn nonliteral_fail() { expect_i32(r#"=eval(show("=1"))"#, 1).await; }

#[tokio::test]
async fn deferred_lit() { expect_i32(r#"=eval(fn(a){=a}("=1"))"#, 1).await; }

#[tokio::test]
#[should_panic]
async fn var_fail() { expect_i32(r#"a=1;=eval("=a")"#, 1).await; }

#[tokio::test]
#[should_panic]
async fn capture_fail() { expect_i32(r#"a=1;=eval("=fn(){=a}")()"#, 1).await; }

#[tokio::test]
async fn double_call() { expect_i32(r#"=eval("=1")+eval("=2")"#, 3).await; }

#[tokio::test]
async fn double_match() { expect_i32(r#"=eval("=1")+eval("=1")"#, 2).await; }
