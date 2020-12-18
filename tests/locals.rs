//! A set of tests for making sure local variables work.

mod util;

use alchem::value::Value;
use util::expect_script;

#[test]
fn local_int() { expect_script("a=1;=a", Value::Int(1)); }

#[test]
fn local_float() { expect_script("a=1.1;=a", Value::Float(1.1)); }

#[test]
fn local_string() { expect_script(r#"a="hello";=a"#, "hello".into()); }

#[test]
fn local_nest() { expect_script("=if true {a=1;=a} else {=2}", 1.into()); }

#[test]
fn local_nest_search() { expect_script("a=1;=if true {=a} else {=2}", 1.into()); }

#[test]
fn deep_rotate() { expect_script("a=1;b=2;c=3;d=4;=d", 4.into()); }
