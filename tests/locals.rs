//! A set of tests for making sure local variables work.

mod util;

use util::expect_script;

#[tokio::test]
async fn local_int() { expect_script("a=1;=a", 1).await; }

#[tokio::test]
async fn local_float() { expect_script("a=1.1;=a", 1.1).await; }

#[tokio::test]
async fn local_string() { expect_script(r#"a="hello";=a"#, "hello").await; }

#[tokio::test]
async fn local_nest() { expect_script("=if true {a=1;=a} else {=2}", 1).await; }

#[tokio::test]
async fn local_nest_search() { expect_script("a=1;=if true {=a} else {=2}", 1).await; }

#[tokio::test]
async fn deep_rotate() { expect_script("a=1;b=2;c=3;d=4;=d", 4).await; }
