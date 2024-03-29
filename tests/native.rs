//! A set of native function tests for the alchemy vm.
//!
//! The tests in this suite use some native functions, defined in `util`:
//! - `fourty_two()`: returns the number 42.
//! - `recall(f)`: calls the given function `f`, and returns its return.
//! - `recall_1(f, a)`: calls the given function as `f(a)`.

mod util;

use util::expect_i64;
use util::total::expect_total_i64;

#[tokio::test]
async fn fourty_two() { expect_i64("=fourty_two()", 42).await; }

#[tokio::test]
async fn native_to_alc() { expect_i64("f=fn(){=1}; =recall(f) + 2", 3).await; }

#[tokio::test]
async fn alc_to_native() { expect_i64("f=fn(){=fourty_two()}; =f() + 1", 43).await; }

#[tokio::test]
async fn native_to_native() { expect_i64("=recall(fourty_two) + 1", 43).await; }

#[tokio::test]
async fn assign_native() { expect_i64("n42 = fourty_two; =n42()", 42).await; }

#[tokio::test]
async fn full_ffi() { expect_i64("f=fn(){=fourty_two()}; f2=fn(){=recall(f)}; =f2() + 1", 43).await; }

#[tokio::test]
async fn native_args() { expect_i64("f=fn(a){=a+1}; =recall_1(f, 2)", 3).await; }

#[tokio::test]
async fn total_native() { expect_total_i64("f=fn(){=1};=make_run(f)", 1).await; }
