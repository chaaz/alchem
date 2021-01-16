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
