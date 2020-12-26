//! A set of native function tests for the alchemy vm.
//!
//! The tests in this suite use some native functions, defined in `util`:
//! - `fourty_two()`: returns the number 42.
//! - `recall(f)`: calls the given function `f`, and returns its return.
//! - `recall_a1(f, a)`: calls the given function as `f(a)`.

mod util;

use alchem::value::Value;
use util::expect_with_natives;

#[tokio::test]
async fn fourty_two() { expect_with_natives("=fourty_two()", Value::Int(42)).await; }

#[tokio::test]
async fn native_to_alc() { expect_with_natives("f=fn(){=1}; =recall(f) + 2", Value::Int(3)).await; }

#[tokio::test]
async fn alc_to_native() { expect_with_natives("f=fn(){=fourty_two()}; =f() + 1", Value::Int(43)).await; }

#[tokio::test]
async fn full_ffi() {
  expect_with_natives("f=fn(){=fourty_two()}; f2=fn(){=recall(f)}; =f2() + 1", Value::Int(43)).await;
}

#[tokio::test]
async fn native_args() { expect_with_natives("f=fn(a){=a+1}; =recall_a1(f, 2)", Value::Int(3)).await; }
