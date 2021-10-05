//! Some tests of the "single-use" feature of some types. These tests depend on a non-viewable type that is
//! single-use, which is returned by the `u1()` function.

mod util;

use util::singles::{expect_i64, expect_u1};

#[tokio::test]
async fn single_succ() { expect_u1(r#"a=u1();b=a;=b"#).await; }

#[tokio::test]
async fn if_else() { expect_u1(r#"a=u1();=if true {=a} else {=a}"#).await; }

#[tokio::test]
async fn nest_if_else() { expect_u1(r#"a=u1();=if true {=if true {=a} else {=a}} else {=a}"#).await; }

#[tokio::test]
async fn double_succ() { expect_i64("a=1;b=a;c=a;=c", 1).await; }

#[tokio::test]
#[should_panic]
async fn double_fail() { expect_u1(r#"a=u1();b=a;c=a;=c"#).await; }

#[tokio::test]
async fn fn_param_succ() { expect_u1(r#"a=u1();b=u1();f=fn(x){=x};g=f(a);=f(b)"#).await; }

#[tokio::test]
async fn fn_reuse_succ() { expect_i64("f=fn(x){a=x;=x};=f(1)", 1).await; }

#[tokio::test]
#[should_panic]
async fn fn_reuse_fail() { expect_u1(r#"f=fn(x){a=x;=x};=f(u1())"#).await; }

#[tokio::test]
async fn capture_succ() { expect_u1(r#"a=u1();f=fn(){=a};g=f;=g()"#).await; }

#[tokio::test]
#[should_panic]
async fn capture_fail() { expect_u1(r#"a=u1();f=fn(){=a};g=f;=f()"#).await; }

#[tokio::test]
#[should_panic]
async fn capture_fail2() { expect_u1(r#"a=u1();f=fn(){=a};g=f();=f()"#).await; }

#[tokio::test]
#[should_panic]
async fn object_fail() { expect_u1(r#"a={a:u1()};b=a;=a.a"#).await; }

#[tokio::test]
async fn array_succ() { expect_i64("a=[1];b=a;=a.0", 1).await; }

#[tokio::test]
#[should_panic]
async fn array_fail() { expect_u1(r#"a=[u1()];b=a;=a.0"#).await; }

#[tokio::test]
async fn native_succ() { expect_i64(r#"a=1;f=fn(){=a};=reloop(f)"#, 1).await; }

#[tokio::test]
#[should_panic]
async fn native_fail() { expect_u1(r#"a=u1();f=fn(){=a};=reloop(f)"#).await; }
