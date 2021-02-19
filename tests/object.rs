//! A set of tests for the alchemy objects.

mod util;

use util::{expect_bool, expect_i32};

#[tokio::test]
async fn create_simple() { expect_i32("a={a:1};=a.a", 1).await; }

#[tokio::test]
async fn create_empty() { expect_i32("a={};=1", 1).await; }

#[tokio::test]
async fn create_multi() { expect_i32("a={a:1,b:2,c:3};=a.c", 3).await; }

#[tokio::test]
async fn extract_multi() { expect_i32("a={a:1,b:2,c:3};=a.a+a.c", 4).await; }

#[tokio::test]
async fn extract_compare() { expect_bool("a={a:3,b:2,c:3};=a.a==a.c", true).await; }

#[tokio::test]
async fn nested() { expect_i32("a={a:3,b:2,c:{x:1,y:5}};=a.c.y", 5).await; }

#[tokio::test]
async fn shorthand() { expect_i32("a=1;b=2;c=3;y={c};x={y,a,b};=x.y.c", 3).await; }

#[tokio::test]
async fn destructure() { expect_i32("{a:x}={a:1};=x", 1).await; }

#[tokio::test]
async fn destr_args() { expect_i32("f=fn({a:y,b:z},x){=y+z+x};=f({c:3,b:1,a:2},3)", 6).await; }

#[tokio::test]
async fn destr_args_short() { expect_i32("f=fn({a,b},x){=a+b+x};=f({c:3,b:1,a:2},3)", 6).await; }

#[tokio::test]
async fn destr_args_late() { expect_i32("f=fn(x,{a:a,b:b}){=a+b+x};=f(3,{c:3,b:1,a:2})", 6).await; }

#[tokio::test]
async fn destr_assigned() { expect_i32("a={a:1};{a:x}=a;=x", 1).await; }

#[tokio::test]
async fn destr_nested() { expect_i32("{a:{b:x}}={a:{b:1}};=x", 1).await; }

#[tokio::test]
async fn destr_nested_assgnd() { expect_i32("a={a:{b:1}};{a:{b:x}}=a;=x", 1).await; }

#[tokio::test]
async fn destr_multi() { expect_i32("{a:{c:x}}={a:{b:1,c:3},d:5,e:6};=x", 3).await; }

#[tokio::test]
async fn destr_assgn_multi() { expect_i32("a={a:{b:1,c:3},d:5,e:6};{a:{c:x}}=a;=x", 3).await; }

#[tokio::test]
async fn destr_short() { expect_i32("{x}={x:1};=x", 1).await; }

#[tokio::test]
async fn destr_assigned_short() { expect_i32("a={x:1};{x}=a;=x", 1).await; }

#[tokio::test]
async fn destr_nested_short() { expect_i32("{a:{x}}={a:{x:1}};=x", 1).await; }

#[tokio::test]
async fn destr_nested_assgnd_short() { expect_i32("a={a:{x:1}};{a:{x}}=a;=x", 1).await; }

#[tokio::test]
async fn destr_multi_short() { expect_i32("{a:{x}}={a:{b:1,x:3},d:5,e:6};=x", 3).await; }

#[tokio::test]
async fn destr_assgn_multi_short() { expect_i32("a={a:{b:1,x:3},d:5,e:6};{a:{x}}=a;=x", 3).await; }

#[tokio::test]
async fn dest_short_robust() { expect_i32("a={x:{b:1,x:3},d:5,e:6};{x}=a;=x.x", 3).await; }
