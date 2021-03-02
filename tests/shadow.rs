//! A set of basic tests for the alchemy vm.

mod util;

use util::{expect_i32, expect_str};

#[tokio::test]
async fn shadow_same_type() { expect_i32("a=1;a=2;=a", 2).await; }

#[tokio::test]
async fn shadow_diff_type() { expect_str(r#"a=1;a="one";=a"#, "one").await; }

#[tokio::test]
async fn build_on() { expect_i32("a=1;a={a:a};=a.a", 1).await; }

#[tokio::test]
async fn build_math() { expect_i32("a=1;a=a+1;=a", 2).await; }

#[tokio::test]
async fn capture() { expect_i32("a=1;f=fn(){=a};a=2;=f()", 1).await; }

#[tokio::test]
#[should_panic]
async fn non_exists_fail() { expect_i32("a=a+1;=a", 2).await; }
