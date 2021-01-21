//! The values representable in our language.

use crate::collapsed::FuncNative;
use crate::common::{Closure, Native, TypeNative};
use crate::types::CustomType;
use serde_json::{Number, Value as Json};
use std::cmp::PartialEq;
use std::fmt;
use std::sync::Arc;

pub use crate::common::{Function, Globals, MorphStatus, NativeInfo};
pub use crate::types::{NoCustom, Type};

pub fn add_native<C>(globals: &mut Globals<C>, name: impl ToString, arity: u8, native: Native<C>, typen: TypeNative<C>)
where
  C: CustomType + 'static
{
  let function = Function::new_native(arity, native, typen);
  globals.insert(name.to_string(), Arc::new(function));
}

pub enum Value<C: CustomType> {
  Float(f64),
  Int(i32),
  Bool(bool),
  String(Arc<str>),
  Array(Vec<Value<C>>),
  Closure(Arc<Closure<C>>),
  Native(Arc<FuncNative<C>>),
  Json(Json),
  Void
}

impl<C: CustomType> PartialEq for Value<C> {
  fn eq(&self, other: &Value<C>) -> bool {
    match (self, other) {
      (Self::Float(v1), Self::Float(v2)) => v1 == v2,
      (Self::Int(v1), Self::Int(v2)) => v1 == v2,
      (Self::Bool(v1), Self::Bool(v2)) => v1 == v2,
      (Self::String(v1), Self::String(v2)) => v1 == v2,
      (Self::Closure(v1), Self::Closure(v2)) => Arc::ptr_eq(v1, v2),
      (Self::Native(v1), Self::Native(v2)) => Arc::ptr_eq(v1, v2),
      (Self::Array(v1), Self::Array(v2)) => v1 == v2,
      (Self::Json(a), Self::Json(b)) => a == b,
      (Self::Void, Self::Void) => true,
      _ => false
    }
  }
}

impl<C: CustomType> fmt::Debug for Value<C> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Float(v) => write!(f, "{}", v),
      Self::Int(v) => write!(f, "{}", v),
      Self::Bool(v) => write!(f, "{}", v),
      Self::String(v) => write!(f, "\"{}\"", v),
      Self::Closure(v) => write!(f, "{:?}", v),
      Self::Native(v) => write!(f, "{:?}", v),
      Self::Array(_) => write!(f, "(array)"),
      Self::Json(v) => write!(f, "{:?}", v),
      Self::Void => write!(f, "(void)")
    }
  }
}

impl<C: CustomType> From<i32> for Value<C> {
  fn from(v: i32) -> Value<C> { Value::Int(v) }
}

impl<C: CustomType> From<f64> for Value<C> {
  fn from(v: f64) -> Value<C> { Value::Float(v) }
}

impl<C: CustomType> From<bool> for Value<C> {
  fn from(v: bool) -> Value<C> { Value::Bool(v) }
}

impl<C: CustomType> From<String> for Value<C> {
  fn from(v: String) -> Value<C> { Value::String(v.into()) }
}

impl<C: CustomType> From<&str> for Value<C> {
  fn from(v: &str) -> Value<C> { v.to_string().into() }
}

impl<C: CustomType> From<Closure<C>> for Value<C> {
  fn from(v: Closure<C>) -> Value<C> { Value::Closure(Arc::new(v)) }
}

impl<C: CustomType> From<Arc<Closure<C>>> for Value<C> {
  fn from(v: Arc<Closure<C>>) -> Value<C> { Value::Closure(v) }
}

impl<C: CustomType> From<Json> for Value<C> {
  fn from(v: Json) -> Value<C> { Value::Json(v) }
}

impl<C: CustomType> Value<C> {
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

  pub fn as_json_mut(&mut self) -> &mut Json {
    match self {
      Self::Json(v) => v,
      _ => panic!("Not an array.")
    }
  }

  pub fn as_array_mut(&mut self) -> &mut Vec<Value<C>> {
    match self {
      Self::Array(v) => v,
      _ => panic!("Not an array.")
    }
  }

  pub fn as_closure(&self) -> Arc<Closure<C>> {
    match self {
      Self::Closure(v) => v.clone(),
      _ => panic!("Not a closure: {:?}", self)
    }
  }

  // `shift` acts like `clone` for most value types, but for mutable or otherwise un-cloneable values, it will
  // instead take the value, leaving a void in its place.
  pub fn shift(&mut self) -> Value<C> {
    match self {
      Self::Float(v) => Self::Float(*v),
      Self::Int(v) => Self::Int(*v),
      Self::Bool(v) => Self::Bool(*v),
      Self::String(v) => Self::String(v.clone()),
      Self::Closure(v) => Self::Closure(v.clone()),
      Self::Native(v) => Self::Native(v.clone()),
      Self::Array(v) => Self::Array(v.iter_mut().map(|v| v.shift()).collect()),
      Self::Json(v) => Self::Json(v.clone()),
      Self::Void => panic!("Cannot access an evaculated value.")
    }
  }

  pub fn op_negate(&self) -> Value<C> {
    match self {
      Self::Float(v) => Self::Float(-*v),
      Self::Int(v) => Self::Int(-*v),
      Self::Json(Json::Number(n)) if n.is_u64() => Self::Json(Json::Number((-(n.as_u64().unwrap() as i64)).into())),
      Self::Json(Json::Number(n)) if n.is_i64() => Self::Json(Json::Number((-n.as_i64().unwrap()).into())),
      Self::Json(Json::Number(n)) if n.is_f64() => {
        Self::Json(Json::Number(Number::from_f64(-n.as_f64().unwrap()).unwrap()))
      }
      _ => panic!("No negation for {:?}", self)
    }
  }

  pub fn op_not(&self) -> Value<C> {
    match self {
      Self::Bool(v) => Self::Bool(!*v),
      Self::Json(Json::Bool(n)) => Self::Json(Json::Bool(!n)),
      _ => panic!("No negation for {:?}", self)
    }
  }

  pub fn op_add(&self, other: &Value<C>) -> Value<C> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Float(v1 + v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Int(v1 + v2),
      (Self::String(v1), Value::String(v2)) => Self::String(concat(v1, v2).into()),
      (Self::Int(a), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(*a as f64 + b.as_f64().unwrap()).unwrap()))
      }
      (Self::Float(a), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(*a as f64 + b.as_f64().unwrap()).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Int(b)) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() + *b as f64).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Float(b)) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() + *b as f64).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() + b.as_f64().unwrap()).unwrap()))
      }
      (Self::String(a), Self::Json(Json::String(b))) => Self::Json(Json::String(concat(a, b))),
      (Self::Json(Json::String(a)), Self::String(b)) => Self::Json(Json::String(concat(a, b))),
      (Self::Json(Json::String(a)), Self::Json(Json::String(b))) => Self::Json(Json::String(concat(a, b))),
      (o1, o2) => panic!("Can't add mismatch types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_subtract(&self, other: &Value<C>) -> Value<C> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Float(v1 - v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Int(v1 - v2),
      (Self::Int(a), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(*a as f64 - b.as_f64().unwrap()).unwrap()))
      }
      (Self::Float(a), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(*a as f64 - b.as_f64().unwrap()).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Int(b)) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() - *b as f64).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Float(b)) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() - *b as f64).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() - b.as_f64().unwrap()).unwrap()))
      }
      (o1, o2) => panic!("Can't subtract mismatch types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_multiply(&self, other: &Value<C>) -> Value<C> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Float(v1 * v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Int(v1 * v2),
      (Self::Int(a), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(*a as f64 * b.as_f64().unwrap()).unwrap()))
      }
      (Self::Float(a), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(*a as f64 * b.as_f64().unwrap()).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Int(b)) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() * *b as f64).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Float(b)) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() * *b as f64).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() * b.as_f64().unwrap()).unwrap()))
      }
      (o1, o2) => panic!("Can't multiply mismatch types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_divide(&self, other: &Value<C>) -> Value<C> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Float(v1 / v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Int(v1 / v2),
      (Self::Int(a), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(*a as f64 / b.as_f64().unwrap()).unwrap()))
      }
      (Self::Float(a), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(*a as f64 / b.as_f64().unwrap()).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Int(b)) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() / *b as f64).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Float(b)) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() / *b as f64).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() / b.as_f64().unwrap()).unwrap()))
      }
      (o1, o2) => panic!("Can't divide mismatch types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_mod(&self, other: &Value<C>) -> Value<C> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Float(v1 % v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Int(v1 % v2),
      (Self::Int(a), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(*a as f64 % b.as_f64().unwrap()).unwrap()))
      }
      (Self::Float(a), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(*a as f64 % b.as_f64().unwrap()).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Int(b)) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() % *b as f64).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Float(b)) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() % *b as f64).unwrap()))
      }
      (Self::Json(Json::Number(a)), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Number(Number::from_f64(a.as_f64().unwrap() % b.as_f64().unwrap()).unwrap()))
      }
      (o1, o2) => panic!("Can't divide mismatch types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_gt(&self, other: &Value<C>) -> Value<C> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Bool(v1 > v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Bool(v1 > v2),
      (Self::Int(a), Self::Json(Json::Number(b))) => Self::Json(Json::Bool(*a as f64 > b.as_f64().unwrap())),
      (Self::Float(a), Self::Json(Json::Number(b))) => Self::Json(Json::Bool(*a as f64 > b.as_f64().unwrap())),
      (Self::Json(Json::Number(a)), Self::Int(b)) => Self::Json(Json::Bool(a.as_f64().unwrap() > *b as f64)),
      (Self::Json(Json::Number(a)), Self::Float(b)) => Self::Json(Json::Bool(a.as_f64().unwrap() > *b as f64)),
      (Self::Json(Json::Number(a)), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Bool(a.as_f64().unwrap() > b.as_f64().unwrap()))
      }
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_gte(&self, other: &Value<C>) -> Value<C> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Bool(v1 >= v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Bool(v1 >= v2),
      (Self::Int(a), Self::Json(Json::Number(b))) => Self::Json(Json::Bool(*a as f64 >= b.as_f64().unwrap())),
      (Self::Float(a), Self::Json(Json::Number(b))) => Self::Json(Json::Bool(*a as f64 >= b.as_f64().unwrap())),
      (Self::Json(Json::Number(a)), Self::Int(b)) => Self::Json(Json::Bool(a.as_f64().unwrap() >= *b as f64)),
      (Self::Json(Json::Number(a)), Self::Float(b)) => Self::Json(Json::Bool(a.as_f64().unwrap() >= *b as f64)),
      (Self::Json(Json::Number(a)), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Bool(a.as_f64().unwrap() >= b.as_f64().unwrap()))
      }
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_lt(&self, other: &Value<C>) -> Value<C> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Bool(v1 < v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Bool(v1 < v2),
      (Self::Int(a), Self::Json(Json::Number(b))) => Self::Json(Json::Bool((*a as f64) < b.as_f64().unwrap())),
      (Self::Float(a), Self::Json(Json::Number(b))) => Self::Json(Json::Bool((*a as f64) < b.as_f64().unwrap())),
      (Self::Json(Json::Number(a)), Self::Int(b)) => Self::Json(Json::Bool(a.as_f64().unwrap() < *b as f64)),
      (Self::Json(Json::Number(a)), Self::Float(b)) => Self::Json(Json::Bool(a.as_f64().unwrap() < *b as f64)),
      (Self::Json(Json::Number(a)), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Bool(a.as_f64().unwrap() < b.as_f64().unwrap()))
      }
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_lte(&self, other: &Value<C>) -> Value<C> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Bool(v1 <= v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Bool(v1 <= v2),
      (Self::Int(a), Self::Json(Json::Number(b))) => Self::Json(Json::Bool(*a as f64 <= b.as_f64().unwrap())),
      (Self::Float(a), Self::Json(Json::Number(b))) => Self::Json(Json::Bool(*a as f64 <= b.as_f64().unwrap())),
      (Self::Json(Json::Number(a)), Self::Int(b)) => Self::Json(Json::Bool(a.as_f64().unwrap() <= *b as f64)),
      (Self::Json(Json::Number(a)), Self::Float(b)) => Self::Json(Json::Bool(a.as_f64().unwrap() <= *b as f64)),
      (Self::Json(Json::Number(a)), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Bool(a.as_f64().unwrap() <= b.as_f64().unwrap()))
      }
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_eq(&self, other: &Value<C>) -> Value<C> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Bool(is_eq(*v1, *v2)),
      (Self::Int(v1), Value::Int(v2)) => Self::Bool(v1 == v2),
      (Self::Int(a), Self::Json(Json::Number(b))) => Self::Json(Json::Bool(is_eq(*a as f64, b.as_f64().unwrap()))),
      (Self::Float(a), Self::Json(Json::Number(b))) => Self::Json(Json::Bool(is_eq(*a as f64, b.as_f64().unwrap()))),
      (Self::Json(Json::Number(a)), Self::Int(b)) => Self::Json(Json::Bool(is_eq(a.as_f64().unwrap(), *b as f64))),
      (Self::Json(Json::Number(a)), Self::Float(b)) => Self::Json(Json::Bool(is_eq(a.as_f64().unwrap(), *b as f64))),
      (Self::Json(Json::Number(a)), Self::Json(Json::Number(b))) => {
        Self::Json(Json::Bool(is_eq(a.as_f64().unwrap(), b.as_f64().unwrap())))
      }
      (Self::String(v1), Value::String(v2)) => Self::Bool(v1 == v2),
      (Self::Json(Json::String(a)), Self::String(b)) => Self::Bool(a.as_str() == b.as_ref()),
      (Self::String(a), Self::Json(Json::String(b))) => Self::Bool(a.as_ref() == b.as_str()),
      (Self::Bool(v1), Value::Bool(v2)) => Self::Bool(v1 == v2),
      (Self::Bool(a), Self::Json(Json::Bool(b))) => Self::Bool(a == b),
      (Self::Json(Json::Bool(a)), Self::Bool(b)) => Self::Bool(a == b),
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_neq(&self, other: &Value<C>) -> Value<C> { Value::Bool(!self.op_eq(other).as_bool()) }

  pub fn op_and(&self, other: &Value<C>) -> Value<C> {
    match (self, other) {
      (Self::Bool(v1), Value::Bool(v2)) => Self::Bool(*v1 && *v2),
      (Self::Bool(v1), Value::Json(Json::Bool(v2))) => Self::Json(Json::Bool(*v1 && *v2)),
      (Value::Json(Json::Bool(v1)), Self::Bool(v2)) => Self::Json(Json::Bool(*v1 && *v2)),
      (Value::Json(Json::Bool(v1)), Self::Json(Json::Bool(v2))) => Self::Json(Json::Bool(*v1 && *v2)),
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn op_or(&self, other: &Value<C>) -> Value<C> {
    match (self, other) {
      (Self::Bool(v1), Value::Bool(v2)) => Self::Bool(*v1 || *v2),
      (Self::Bool(v1), Value::Json(Json::Bool(v2))) => Self::Json(Json::Bool(*v1 || *v2)),
      (Value::Json(Json::Bool(v1)), Self::Bool(v2)) => Self::Json(Json::Bool(*v1 || *v2)),
      (Value::Json(Json::Bool(v1)), Self::Json(Json::Bool(v2))) => Self::Json(Json::Bool(*v1 || *v2)),
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

fn concat(v1: &impl ToString, v2: &impl ToString) -> String { format!("{}{}", v1.to_string(), v2.to_string()) }

fn is_eq(v1: f64, v2: f64) -> bool { (v1 - v2).abs() < f64::EPSILON }

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
  use crate::types::NoCustom;

  #[test]
  fn value_clone() {
    assert_eq!(Value::<NoCustom>::Float(1.0).shift(), Value::Float(1.0));
  }
}
