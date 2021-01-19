//! The values representable in our language.

use crate::collapsed::FuncNative;
use crate::common::{Closure, Native, TypeNative};
use crate::types::CustomType;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

pub use crate::common::{Function, Globals, MorphStatus, NativeInfo};
pub use crate::types::{Type, NoCustom};

pub fn new_globals() -> Globals<NoCustom> { HashMap::new() }

pub fn add_native<C>(globals: &mut Globals<C>, name: impl ToString, arity: u8, native: Native, typen: TypeNative<C>)
where
  C: CustomType + 'static
{
  let function = Function::new_native(arity, native, typen);
  globals.insert(name.to_string(), Arc::new(function));
}

pub enum Value {
  Float(f64),
  Int(i32),
  Bool(bool),
  String(Arc<str>),
  Array(Vec<Value>),
  Closure(Arc<Closure>),
  Native(Arc<FuncNative>),
  Void
}

impl PartialEq for Value {
  fn eq(&self, other: &Value) -> bool {
    match (self, other) {
      (Self::Float(v1), Self::Float(v2)) => v1 == v2,
      (Self::Int(v1), Self::Int(v2)) => v1 == v2,
      (Self::Bool(v1), Self::Bool(v2)) => v1 == v2,
      (Self::String(v1), Self::String(v2)) => v1 == v2,
      (Self::Closure(v1), Self::Closure(v2)) => Arc::ptr_eq(v1, v2),
      (Self::Native(v1), Self::Native(v2)) => Arc::ptr_eq(v1, v2),
      (Self::Array(v1), Self::Array(v2)) => v1 == v2,
      (Self::Void, Self::Void) => true,
      _ => false
    }
  }
}

impl fmt::Debug for Value {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Float(v) => write!(f, "{}", v),
      Self::Int(v) => write!(f, "{}", v),
      Self::Bool(v) => write!(f, "{}", v),
      Self::String(v) => write!(f, "\"{}\"", v),
      Self::Closure(v) => write!(f, "{:?}", v),
      Self::Native(v) => write!(f, "{:?}", v),
      Self::Array(_) => write!(f, "(array)"),
      Self::Void => write!(f, "(void)")
    }
  }
}

impl From<i32> for Value {
  fn from(v: i32) -> Value { Value::Int(v) }
}

impl From<f64> for Value {
  fn from(v: f64) -> Value { Value::Float(v) }
}

impl From<bool> for Value {
  fn from(v: bool) -> Value { Value::Bool(v) }
}

impl From<String> for Value {
  fn from(v: String) -> Value { Value::String(v.into()) }
}

impl From<&str> for Value {
  fn from(v: &str) -> Value { v.to_string().into() }
}

impl From<Closure> for Value {
  fn from(v: Closure) -> Value { Value::Closure(Arc::new(v)) }
}

impl From<Arc<Closure>> for Value {
  fn from(v: Arc<Closure>) -> Value { Value::Closure(v) }
}

impl Value {
  pub fn cloneable(&self) -> bool { true }

  pub fn as_int(&self) -> i32 {
    match self {
      Self::Int(v) => *v,
      _ => panic!("Not an int: {:?}", self)
    }
  }

  pub fn as_bool(&self) -> bool {
    match self {
      Self::Bool(v) => *v,
      _ => panic!("Not a boolean: {:?}", self)
    }
  }

  pub fn as_array_mut(&mut self) -> &mut Vec<Value> {
    match self {
      Self::Array(v) => v,
      _ => panic!("Not an array.")
    }
  }

  pub fn as_closure(&self) -> Arc<Closure> {
    match self {
      Self::Closure(v) => v.clone(),
      _ => panic!("Not a closure: {:?}", self)
    }
  }

  // `shift` acts like `clone` for most value types, but for mutable or otherwise un-cloneable values, it will
  // instead take the value, leaving a void in its place.
  pub fn shift(&mut self) -> Value {
    match self {
      Self::Float(v) => Self::Float(*v),
      Self::Int(v) => Self::Int(*v),
      Self::Bool(v) => Self::Bool(*v),
      Self::String(v) => Self::String(v.clone()),
      Self::Closure(v) => Self::Closure(v.clone()),
      Self::Native(v) => Self::Native(v.clone()),
      Self::Array(v) => Self::Array(v.iter_mut().map(|v| v.shift()).collect()),
      Self::Void => panic!("Cannot access an evaculated value.")
    }
  }

  pub fn op_negate(&self) -> Value {
    match self {
      Self::Float(v) => Self::Float(-*v),
      Self::Int(v) => Self::Int(-*v),
      _ => panic!("No negation for {:?}", self)
    }
  }

  pub fn op_not(&self) -> Value {
    match self {
      Self::Bool(v) => Self::Bool(!*v),
      _ => panic!("No negation for {:?}", self)
    }
  }

  pub fn op_add(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Float(v1 + v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Int(v1 + v2),
      (Self::String(v1), Value::String(v2)) => {
        let mut s = v1.to_string();
        s.push_str(&v2);
        Self::String(s.into())
      }
      (o1, o2) => panic!("Can't add mismatch types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_subtract(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Float(v1 - v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Int(v1 - v2),
      (o1, o2) => panic!("Can't subtract mismatch types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_multiply(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Float(v1 * v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Int(v1 * v2),
      (o1, o2) => panic!("Can't multiply mismatch types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_divide(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Float(v1 / v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Int(v1 / v2),
      (o1, o2) => panic!("Can't divide mismatch types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_mod(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Float(v1 % v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Int(v1 % v2),
      (o1, o2) => panic!("Can't divide mismatch types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_gt(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Bool(v1 > v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Bool(v1 > v2),
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_gte(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Bool(v1 >= v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Bool(v1 >= v2),
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_lt(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Bool(v1 < v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Bool(v1 < v2),
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_lte(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Bool(v1 <= v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Bool(v1 <= v2),
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_eq(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Bool((v1 - v2).abs() < f64::EPSILON),
      (Self::Int(v1), Value::Int(v2)) => Self::Bool(v1 == v2),
      (Self::Bool(v1), Value::Bool(v2)) => Self::Bool(v1 == v2),
      (Self::String(v1), Value::String(v2)) => Self::Bool(v1 == v2),
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_neq(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Bool((v1 - v2).abs() >= f64::EPSILON),
      (Self::Int(v1), Value::Int(v2)) => Self::Bool(v1 != v2),
      (Self::Bool(v1), Value::Bool(v2)) => Self::Bool(v1 != v2),
      (Self::String(v1), Value::String(v2)) => Self::Bool(v1 != v2),
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_and(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Bool(v1), Value::Bool(v2)) => Self::Bool(*v1 && *v2),
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_or(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Bool(v1), Value::Bool(v2)) => Self::Bool(*v1 || *v2),
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }
}

pub enum Declared<C: CustomType> {
  Float(f64),
  Int(i32),
  Bool(bool),
  String(Arc<str>),
  Function(Arc<Function<C>>)
}

impl<C: CustomType> fmt::Debug for Declared<C> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Float(v) => write!(f, "{}", v),
      Self::Int(v) => write!(f, "{}", v),
      Self::Bool(v) => write!(f, "{}", v),
      Self::String(v) => write!(f, "\"{}\"", v),
      Self::Function(v) => write!(f, "{:?}", v)
    }
  }
}

impl<C: CustomType> From<i32> for Declared<C> {
  fn from(v: i32) -> Declared<C> { Declared::Int(v) }
}

impl<C: CustomType> From<f64> for Declared<C> {
  fn from(v: f64) -> Declared<C> { Declared::Float(v) }
}

impl<C: CustomType> From<bool> for Declared<C> {
  fn from(v: bool) -> Declared<C> { Declared::Bool(v) }
}

impl<C: CustomType> From<String> for Declared<C> {
  fn from(v: String) -> Declared<C> { Declared::String(v.into()) }
}

impl<C: CustomType> From<&str> for Declared<C> {
  fn from(v: &str) -> Declared<C> { v.to_string().into() }
}

impl<C: CustomType> From<Function<C>> for Declared<C> {
  fn from(v: Function<C>) -> Declared<C> { Declared::Function(Arc::new(v)) }
}

impl<C: CustomType> From<Arc<Function<C>>> for Declared<C> {
  fn from(v: Arc<Function<C>>) -> Declared<C> { Declared::Function(v) }
}

impl<C: CustomType> Declared<C> {
  pub fn as_str(&self) -> Option<&str> {
    match self {
      Self::String(s) => Some(s),
      _ => None
    }
  }
}

// convert
//
// ```
// async fn print(
//   vals: Vec<Value>, info: NativeInfo, run: &mut Runner
// ) -> Value
// ```
//
// to
//
// ```
// fn print<'r>(
//   vals: Vec<Value>, info: NativeInfo, run: &'r mut Runner
// ) -> Pin<Box<dyn Future<Output = Value> + Send + 'r>>`
// ```

#[macro_export]
macro_rules! native_fn {(
  $( #[$attr:meta] )* // includes doc strings
  $pub:vis
  async
  fn $fname:ident (
    $arg1_i:ident : $arg1_t:ty, $arg2_i:ident : $arg2_t:ty, $arg3_i:ident : &mut $arg3_t:ty
  ) -> $Ret:ty
  {
      $($body:tt)*
  }
) => (
  $( #[$attr] )*
  $pub
  fn $fname<'r> (
    $arg1_i : $arg1_t, $arg2_i : $arg2_t, $arg3_i : &'r mut $arg3_t
  ) -> ::std::pin::Pin<::std::boxed::Box<dyn ::std::future::Future<Output = $Ret> + ::std::marker::Send + 'r>>
  {
      ::std::boxed::Box::pin(async move { $($body)* })
  }
)}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn value_clone() {
    assert_eq!(Value::Float(1.0).shift(), Value::Float(1.0));
  }
}
