//! A set of tests for making sure branches work.

mod util;

use util::expect_script;

#[tokio::test]
async fn branch_simple() { expect_script("=if true {=1} else {=2}", 1.into()).await; }

#[tokio::test]
async fn branch_else() { expect_script("=if false {=1} else {=2}", 2.into()).await; }

#[tokio::test]
async fn branch_local() { expect_script("=if true {a=1;=a} else {=2}", 1.into()).await; }

#[tokio::test]
async fn branch_on_local() { expect_script("a=1; =if a == 1 {=1} else {=2}", 1.into()).await; }

#[tokio::test]
async fn deep_branch_local() { expect_script("=if true {a=1;b=2;c=3;=c} else {=8}", 3.into()).await; }

#[tokio::test]
async fn branch_weird() {
  // You can use an 'if' statement as the expression to test on, as long as all consequents evaluate to a
  // boolean. But, you know, don't.
  expect_script("=if if 1 == 1 {=true} else {=false} {=1} else {=2}", 1.into()).await;
}
