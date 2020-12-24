//! A set of tests for making sure local variables work.

mod util;

use alchem::value::Value;
use util::expect_script;

#[tokio::test]
async fn local_int() { expect_script("a=1;=a", Value::Int(1)).await; }

#[tokio::test]
async fn local_float() { expect_script("a=1.1;=a", Value::Float(1.1)).await; }

#[tokio::test]
async fn local_string() { expect_script(r#"a="hello";=a"#, "hello".into()).await; }

#[tokio::test]
async fn local_nest() { expect_script("=if true {a=1;=a} else {=2}", 1.into()).await; }

#[tokio::test]
async fn local_nest_search() { expect_script("a=1;=if true {=a} else {=2}", 1.into()).await; }

#[tokio::test]
async fn deep_rotate() { expect_script("a=1;b=2;c=3;d=4;=d", 4.into()).await; }
