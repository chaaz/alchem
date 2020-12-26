//! The values representable in our language.

use crate::common::{Closure, Function, Native};
use std::cmp::PartialEq;
use std::fmt;
use std::sync::Arc;

pub enum Value {
  Float(f64),
  Int(i32),
  Bool(bool),
  String(Arc<str>),
  Closure(Arc<Closure>),
  Native(Native),
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
      (Self::Native(v1), Self::Native(v2)) => v1 as *const Native == v2 as *const Native,
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
      Self::Native(_) => write!(f, "(native)"),
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
      Self::Native(v) => Self::Native(*v),
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

pub enum Declared {
  Float(f64),
  Int(i32),
  Bool(bool),
  String(Arc<str>),
  Function(Arc<Function>),
  Native(Native)
}

impl PartialEq for Declared {
  fn eq(&self, other: &Declared) -> bool {
    match (self, other) {
      (Self::Float(v1), Self::Float(v2)) => v1 == v2,
      (Self::Int(v1), Self::Int(v2)) => v1 == v2,
      (Self::Bool(v1), Self::Bool(v2)) => v1 == v2,
      (Self::String(v1), Self::String(v2)) => v1 == v2,
      (Self::Function(v1), Self::Function(v2)) => Arc::ptr_eq(v1, v2),
      (Self::Native(v1), Self::Native(v2)) => v1 as *const Native == v2 as *const Native,
      _ => false
    }
  }
}

impl Clone for Declared {
  fn clone(&self) -> Declared {
    match self {
      Self::Float(v) => Self::Float(*v),
      Self::Int(v) => Self::Int(*v),
      Self::Bool(v) => Self::Bool(*v),
      Self::String(v) => Self::String(v.clone()),
      Self::Function(v) => Self::Function(v.clone()),
      Self::Native(v) => Self::Native(*v)
    }
  }
}

impl fmt::Debug for Declared {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Float(v) => write!(f, "{}", v),
      Self::Int(v) => write!(f, "{}", v),
      Self::Bool(v) => write!(f, "{}", v),
      Self::String(v) => write!(f, "\"{}\"", v),
      Self::Function(v) => write!(f, "{:?}", v),
      Self::Native(_) => write!(f, "(native)")
    }
  }
}

impl From<i32> for Declared {
  fn from(v: i32) -> Declared { Declared::Int(v) }
}

impl From<f64> for Declared {
  fn from(v: f64) -> Declared { Declared::Float(v) }
}

impl From<bool> for Declared {
  fn from(v: bool) -> Declared { Declared::Bool(v) }
}

impl From<String> for Declared {
  fn from(v: String) -> Declared { Declared::String(v.into()) }
}

impl From<&str> for Declared {
  fn from(v: &str) -> Declared { v.to_string().into() }
}

impl From<Function> for Declared {
  fn from(v: Function) -> Declared { Declared::Function(Arc::new(v)) }
}

impl From<Arc<Function>> for Declared {
  fn from(v: Arc<Function>) -> Declared { Declared::Function(v) }
}

impl Declared {
  pub fn as_str(&self) -> Option<&str> {
    match self {
      Self::String(s) => Some(s),
      _ => None
    }
  }

  pub fn as_function(&self) -> Arc<Function> {
    match self {
      Self::Function(v) => v.clone(),
      _ => panic!("Not a function: {:?}", self)
    }
  }

  pub fn to_value(&self) -> Value {
    match self {
      Self::Float(v) => Value::Float(*v),
      Self::Int(v) => Value::Int(*v),
      Self::Bool(v) => Value::Bool(*v),
      Self::String(v) => Value::String(v.clone()),
      Self::Function(_) => panic!("Can't create a function value."),
      Self::Native(v) => Value::Native(*v)
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn value_clone() {
    assert_eq!(Value::Float(1.0).shift(), Value::Float(1.0));
  }
}
