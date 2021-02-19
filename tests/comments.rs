//! A set of basic tests for the alchemy vm.

mod util;

use util::expect_i32;

#[tokio::test]
async fn double_slash() { expect_i32("// comment\n=1", 1).await; }
