//! A set of tests for the alchemy arrays.

mod util;

use util::expect;

#[tokio::test]
async fn create_simple() { expect("a=[1];=a.0", 1).await; }

#[tokio::test]
async fn create_multi() { expect("a=[1,2,3];=a.2", 3).await; }

#[tokio::test]
async fn extract_multi() { expect("a=[1,2,3];=a.0+a.2", 4).await; }

#[tokio::test]
async fn extract_compare() { expect("a=[3,2,3];=a.0==a.2", true).await; }

#[tokio::test]
async fn dot_nested() { expect("a=[3,2,[1,5]];=a.2.1", 5).await; }

#[tokio::test]
#[should_panic]
async fn bad_index() { expect("a=[3,2,[1,5]];=a.2.3", 1).await; }

#[tokio::test]
#[should_panic]
async fn not_array() { expect("a=21.7;=a.3", 1).await; }

#[tokio::test]
async fn nested_mixed() { expect("a=[3,2,[1,5]];=a[2].1", 5).await; }

#[tokio::test]
async fn mixed_nested() { expect("a=[3,2,[1,5]];=a.2[1]", 5).await; }

#[tokio::test]
async fn nested_square() { expect("a=[3,2,[1,5]];=a[2][1]", 5).await; }
