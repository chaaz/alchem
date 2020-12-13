//! The execution for alchem.

// This isn't actually the [[bin]] entrypoint for alchem: See `launch.rs` instead.

use alchem::errors::Result;

pub async fn main() -> Result<()> {
  crate::cli::execute().await
}
