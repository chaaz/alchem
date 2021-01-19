//! Some tests of the "single-use" feature of some types. These tests depend on the string type being "single
//! use", which shouldn't be true in the actual system (string types are easily copyable).

mod util;

use util::{expect, expectn};

#[tokio::test]
async fn single_succ() { expect(r#"a="a";b=a;=b"#, "a").await; }

#[tokio::test]
async fn double_succ() { expect("a=1;b=a;c=a;=c", 1).await; }

#[tokio::test]
#[should_panic]
async fn double_fail() { expect(r#"a="a";b=a;c=a;=c"#, "a").await; }

#[tokio::test]
async fn fn_param_succ() { expect(r#"a="a";b="b";f=fn(x){=x};g=f(a);=f(b)"#, "b").await; }

#[tokio::test]
async fn fn_reuse_succ() { expect("f=fn(x){a=x;=x};=f(1)", 1).await; }

#[tokio::test]
#[should_panic]
async fn fn_reuse_fail() { expect(r#"f=fn(x){a=x;=x};=f("a")"#, "a").await; }

#[tokio::test]
async fn capture_succ() { expect(r#"a="a";f=fn(){=a};g=f;=g()"#, "a").await; }

#[tokio::test]
#[should_panic]
async fn capture_fail() { expect(r#"a="a";f=fn(){=a};g=f;=f()"#, "a").await; }

#[tokio::test]
#[should_panic]
async fn capture_fail2() { expect(r#"a="a";f=fn(){=a};g=f();=f()"#, "a").await; }

#[tokio::test]
#[should_panic]
async fn object_fail() { expect(r#"a={a:"a"};b=a;=a.a"#, "a").await; }

#[tokio::test]
async fn array_succ() { expect("a=[1];b=a;=a.0", 1).await; }

#[tokio::test]
#[should_panic]
async fn array_fail() { expect(r#"a=["a"];b=a;=a.0"#, "a").await; }

#[tokio::test]
async fn native_succ() { expectn(r#"a=1;f=fn(){=a};=reloop(f)"#, 1).await; }

#[tokio::test]
#[should_panic]
async fn native_fail() { expectn(r#"a="a";f=fn(){=a};=reloop(f)"#, "a").await; }