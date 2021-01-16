//! A set of native function tests for the alchemy vm.
//!
//! The tests in this suite use some native functions, defined in `util`:
//! - `fourty_two()`: returns the number 42.
//! - `recall(f)`: calls the given function `f`, and returns its return.
//! - `recall_1(f, a)`: calls the given function as `f(a)`.

mod util;

use util::expectn;

#[tokio::test]
async fn fourty_two() { expectn("=fourty_two()", 42).await; }

#[tokio::test]
async fn native_to_alc() { expectn("f=fn(){=1}; =recall(f) + 2", 3).await; }

#[tokio::test]
async fn alc_to_native() { expectn("f=fn(){=fourty_two()}; =f() + 1", 43).await; }

#[tokio::test]
async fn native_to_native() { expectn("=recall(fourty_two) + 1", 43).await; }

#[tokio::test]
async fn assign_native() { expectn("n42 = fourty_two; =n42()", 42).await; }

#[tokio::test]
async fn full_ffi() { expectn("f=fn(){=fourty_two()}; f2=fn(){=recall(f)}; =f2() + 1", 43).await; }

#[tokio::test]
async fn native_args() { expectn("f=fn(a){=a+1}; =recall_1(f, 2)", 3).await; }
