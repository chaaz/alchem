//! A set of tests for making sure functions work.

mod util;

use util::expect_script;

#[test]
fn simple_closure() { expect_script("a=1;f=fn(){=a};=f()", 1.into()); }

#[test]
fn closure_inline() { expect_script("a=1;=fn(){=a}()", 1.into()); }

#[test]
fn parameter_closed() { expect_script("f=fn(a){=fn(){=a}};fx=f(1);=fx()", 1.into()); }

#[test]
fn partial_sum() { expect_script("f=fn(a){=fn(b){=a+b}};fx=f(1);=fx(2)", 3.into()); }

#[test]
fn closure_fake_recurse() { expect_script("f=fn(f,a){=if a < 1 {=a} else {=a + f(f,a-1)}}; =f(f,3)", 6.into()); }

#[test]
fn closure_recurse() { expect_script("f=fn(a){=if a < 1 {=a} else {=a + f(a - 1)}}; =f(3)", 6.into()); }

#[test]
fn fib_10() { expect_script("fib=fn(f,n){=if n<2 {=n} else {=f(f,n-2)+f(f,n-1)}};=fib(fib,10)", 55.into()); }
