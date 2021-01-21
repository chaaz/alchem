//! A set of json-based tests for the alchemy vm.

#![recursion_limit = "1024"]

mod util;

use util::expectj_f64;

#[tokio::test]
async fn convert_int() {
  expectj_f64("=to_json(1)", 1.0).await;
}
