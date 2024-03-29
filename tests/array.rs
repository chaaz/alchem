//! A set of tests for the alchemy arrays.

mod util;

use util::{expect_bool, expect_i64};

#[tokio::test]
async fn create_simple() { expect_i64("a=[1];=a.0", 1).await; }

#[tokio::test]
async fn create_empty() { expect_i64("a=[];=1", 1).await; }

#[tokio::test]
async fn create_multi() { expect_i64("a=[1,2,3];=a.2", 3).await; }

#[tokio::test]
async fn extract_multi() { expect_i64("a=[1,2,3];=a.0+a.2", 4).await; }

#[tokio::test]
async fn extract_compare() { expect_bool("a=[3,2,3];=a.0==a.2", true).await; }

#[tokio::test]
async fn dot_nested() { expect_i64("a=[3,2,[1,5]];=a.2.1", 5).await; }

#[tokio::test]
#[should_panic]
async fn bad_index() { expect_i64("a=[3,2,[1,5]];=a.2.3", 1).await; }

#[tokio::test]
#[should_panic]
async fn not_array() { expect_i64("a=21.7;=a.3", 1).await; }

#[tokio::test]
async fn nested_mixed() { expect_i64("a=[3,2,[1,5]];=a[2].1", 5).await; }

#[tokio::test]
async fn mixed_nested() { expect_i64("a=[3,2,[1,5]];=a.2[1]", 5).await; }

#[tokio::test]
async fn nested_square() { expect_i64("a=[3,2,[1,5]];=a[2][1]", 5).await; }

#[tokio::test]
async fn destructure() { expect_i64("[x]=[1];=x", 1).await; }

#[tokio::test]
async fn destr_in_fn() { expect_i64("f=fn(){[x]=[1];=x};=f()", 1).await; }

#[tokio::test]
async fn destr_args() { expect_i64("f=fn([a,b],x){=a+b+x};=f([1,2],3)", 6).await; }

#[tokio::test]
async fn destr_args_late() { expect_i64("f=fn(x,[a,b]){=a+b+x};=f(3,[1,2])", 6).await; }

#[tokio::test]
async fn destr_recall() { expect_i64("f=fn([a,b]){=a+b};=recall_1(f,[1,2])", 3).await; }

#[tokio::test]
#[should_panic]
async fn destr_recall_fail() { expect_i64("f=fn([a,b]){=a+b};=recall_1(f,1)", 1).await; }

#[tokio::test]
async fn destr_assigned() { expect_i64("a=[1];[x]=a;=x", 1).await; }

#[tokio::test]
async fn destr_nested() { expect_i64("[[x]]=[[1]];=x", 1).await; }

#[tokio::test]
async fn destr_nested_assgnd() { expect_i64("a=[[1]];[[x]]=a;=x", 1).await; }

#[tokio::test]
async fn destr_multi() { expect_i64("[[y,x]]=[[1,3],5,6];=x", 3).await; }

#[tokio::test]
async fn destr_assgn_multi() { expect_i64("a=[[1,3],5,6];[[y,x]]=a;=x", 3).await; }
