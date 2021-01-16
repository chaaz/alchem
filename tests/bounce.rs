//! A set of tests for testing nested and recursive functions, including bouncing.

mod util;

use util::expect;

#[tokio::test]
async fn bounce_ab() {
  expect("a=fn(b,n){=if n<1 {=0} else {=n+b(b,n-1)}}; b=fn(b,n){=a(b,n)}; =a(b,5)", 15).await;
}

#[tokio::test]
async fn bounce_ba() {
  expect("a=fn(b,n){=if n<1 {=0} else {=n+b(b,n-1)}}; b=fn(b,n){=a(b,n)}; =b(b,5)", 15).await;
}

#[tokio::test]
async fn finite() {
  expect("f=fn(f,n){=if n<1 {=0} else {=n+f(f,n-1)}}; =f(f,5)", 15).await;
}

#[tokio::test]
#[should_panic]
async fn infinite() {
  // Panic with "scope has dependencies" or similar, since the compiled scope is always dependent on itself.
  expect("f=fn(f,n){=n+f(f,n-1)}; =f(f,5)", 15).await;
}

#[tokio::test]
#[should_panic]
async fn infinite_bounce() {
  // Same as infinite, but bouncing.
  expect("a=fn(b,n){=n+b(b,n-1)}; b=fn(b,n){=a(b,n)}; =a(b,5)", 15).await;
}
