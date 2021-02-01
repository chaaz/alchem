//! A set of eval tests for the alchemy vm.
//!
//! The tests in this suite use some native functions, defined in `util`:
//! - `eval(string)`: evaluates a context-free inline script
//! - `show(string)`: re-outputs the string: its return type is not a literal string

mod util;

use util::expectn;

#[tokio::test]
async fn simple() { expectn(r#"=eval("=1")"#, 1).await; }

#[tokio::test]
async fn fn_call() { expectn(r#"=eval("=fn(a){=a}(1)")"#, 1).await; }

#[tokio::test]
async fn fn_out_0() { expectn(r#"=eval("=fn(){=1}")()"#, 1).await; }

#[tokio::test]
async fn fn_out_1() { expectn(r#"=eval("=fn(a){=a}")(1)"#, 1).await; }

#[tokio::test]
async fn nonliteral() { expectn(r#"=show("=1")"#, "=1").await; }

#[tokio::test]
#[should_panic]
async fn nonliteral_fail() { expectn(r#"=eval(show("=1"))"#, 1).await; }

#[tokio::test]
async fn deferred_lit() { expectn(r#"=eval(fn(a){=a}("=1"))"#, 1).await; }

#[tokio::test]
#[should_panic]
async fn var_fail() { expectn(r#"a=1;=eval("=a")"#, 1).await; }

#[tokio::test]
#[should_panic]
async fn capture_fail() { expectn(r#"a=1;=eval("=fn(){=a}")()"#, 1).await; }

#[tokio::test]
async fn double_call() { expectn(r#"=eval("=1")+eval("=2")"#, 3).await; }

#[tokio::test]
async fn double_match() { expectn(r#"=eval("=1")+eval("=1")"#, 2).await; }
