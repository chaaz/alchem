//! Compile-time type information

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
  Int,
  Bool,
  String,
  FnSync,
  Unset
}
