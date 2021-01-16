//! A set of tests for the alchemy objects.

mod util;

use util::expect;

#[tokio::test]
async fn create_simple() { expect("a={a:1};=a.a", 1).await; }

#[tokio::test]
async fn create_multi() { expect("a={a:1,b:2,c:3};=a.c", 3).await; }

#[tokio::test]
async fn extract_multi() { expect("a={a:1,b:2,c:3};=a.a+a.c", 4).await; }

#[tokio::test]
async fn extract_compare() { expect("a={a:3,b:2,c:3};=a.a==a.c", true).await; }

#[tokio::test]
async fn nested() { expect("a={a:3,b:2,c:{x:1,y:5}};=a.c.y", 5).await; }
