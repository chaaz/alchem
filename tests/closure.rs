//! A set of tests for making sure functions work.

mod util;

use util::expect_script;

#[test]
fn simple_closure() {
  expect_script("a=1;f=fn(){=a};=f()", 1.into());
}

#[test]
fn closure_inline() {
  expect_script("a=1;=fn(){=a}()", 1.into());
}

#[test]
fn parameter_closed() {
  expect_script("f=fn(a){=fn(){=a}};fx=f(1);=fx()", 1.into());
}

#[test]
fn partial_sum() {
  expect_script("f=fn(a){=fn(b){=a+b}};fx=f(1);=fx(2)", 3.into());
}
