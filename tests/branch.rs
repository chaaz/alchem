//! A set of tests for making sure branches work.

mod util;

use util::expect;

#[tokio::test]
async fn branch_simple() { expect("=if true {=1} else {=2}", 1).await; }

#[tokio::test]
async fn branch_else() { expect("=if false {=1} else {=2}", 2).await; }

#[tokio::test]
async fn branch_local() { expect("=if true {a=1;=a} else {=2}", 1).await; }

#[tokio::test]
async fn branch_on_local() { expect("a=1; =if a == 1 {=1} else {=2}", 1).await; }

#[tokio::test]
async fn deep_branch_local() { expect("=if true {a=1;b=2;c=3;=c} else {=8}", 3).await; }

#[tokio::test]
async fn branch_weird() {
  // You can use an 'if' statement as the expression to test on, as long as all consequents evaluate to a
  // boolean. But, you know, don't.
  expect("=if if 1 == 1 {=true} else {=false} {=1} else {=2}", 1).await;
}
