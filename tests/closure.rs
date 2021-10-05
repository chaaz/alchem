//! A set of tests for making sure functions work.

mod util;

use util::expect_i64;

#[tokio::test]
async fn simple_closure() { expect_i64("a=1;f=fn(){=a};=f()", 1).await; }

#[tokio::test]
async fn closure_inline() { expect_i64("a=1;=fn(){=a}()", 1).await; }

#[tokio::test]
async fn parameter_closed() { expect_i64("f=fn(a){=fn(){=a}};fx=f(1);=fx()", 1).await; }

#[tokio::test]
async fn partial_sum() { expect_i64("f=fn(a){=fn(b){=a+b}};fx=f(1);=fx(2)", 3).await; }

#[tokio::test]
async fn closure_fake_recurse() { expect_i64("f=fn(f,a){=if a < 1 {=a} else {=a + f(f,a-1)}}; =f(f,3)", 6).await; }

// This test will only pass if we somehow enable recursive closures, which are currently not available in the
// language.
//
// #[tokio::test]
// async fn closure_recurse() { expect_i64("f=fn(a){=if a < 1 {=a} else {=a + f(a - 1)}}; =f(3)", 6).await; }

#[tokio::test]
async fn fib_10() { expect_i64("fib=fn(f,n){=if n<2 {=n} else {=f(f,n-2)+f(f,n-1)}};=fib(fib,10)", 55).await; }
