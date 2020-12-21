//! The values representable in our language.

use crate::common::{Closure, Function, Native};
use crate::errors::Result;
use std::cmp::PartialEq;
use std::fmt;
use std::sync::Arc;

pub enum Value {
  Float(f64),
  Int(i32),
  Bool(bool),
  String(Arc<str>),
  Function(Arc<Function>),
  Closure(Arc<Closure>),
  Native(Native)
}

impl PartialEq for Value {
  fn eq(&self, other: &Value) -> bool {
    match (self, other) {
      (Self::Float(v1), Self::Float(v2)) => v1 == v2,
      (Self::Int(v1), Self::Int(v2)) => v1 == v2,
      (Self::Bool(v1), Self::Bool(v2)) => v1 == v2,
      (Self::String(v1), Self::String(v2)) => v1 == v2,
      (Self::Function(v1), Self::Function(v2)) => Arc::ptr_eq(v1, v2),
      (Self::Closure(v1), Self::Closure(v2)) => Arc::ptr_eq(v1, v2),
      (Self::Native(v1), Self::Native(v2)) => v1 as *const Native == v2 as *const Native,
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
      Self::Function(v) => write!(f, "{:?}", v),
      Self::Closure(v) => write!(f, "{:?}", v),
      Self::Native(_) => write!(f, "(native)")
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

impl From<Function> for Value {
  fn from(v: Function) -> Value { Value::Function(Arc::new(v)) }
}

impl From<Arc<Function>> for Value {
  fn from(v: Arc<Function>) -> Value { Value::Function(v) }
}

impl From<Closure> for Value {
  fn from(v: Closure) -> Value { Value::Closure(Arc::new(v)) }
}

impl From<Arc<Closure>> for Value {
  fn from(v: Arc<Closure>) -> Value { Value::Closure(v) }
}

impl Value {
  pub fn cloneable(&self) -> bool { true }

  pub fn as_str(&self) -> Option<&str> {
    match self {
      Self::String(s) => Some(s),
      _ => None
    }
  }

  pub fn try_bool(&self) -> Result<bool> {
    match self {
      Self::Bool(v) => Ok(*v),
      _ => err!(Compile, "Not a boolean: {:?}", self)
    }
  }

  pub fn try_function(&self) -> Result<Arc<Function>> {
    match self {
      Self::Function(v) => Ok(v.clone()),
      _ => err!(Compile, "Not a function: {:?}", self)
    }
  }

  pub fn try_clone(&self) -> Value {
    match self {
      Self::Float(v) => Self::Float(*v),
      Self::Int(v) => Self::Int(*v),
      Self::Bool(v) => Self::Bool(*v),
      Self::String(v) => Self::String(v.clone()),
      Self::Function(v) => Self::Function(v.clone()),
      Self::Closure(v) => Self::Closure(v.clone()),
      Self::Native(v) => Self::Native(*v)
    }
  }

  pub fn try_negate(self) -> Result<Value> {
    match self {
      Self::Float(v) => Ok(Self::Float(-v)),
      Self::Int(v) => Ok(Self::Int(-v)),
      _ => bail!(Runtime, "No negation for {:?}", self)
    }
  }

  pub fn try_not(self) -> Result<Value> {
    match self {
      Self::Bool(v) => Ok(Self::Bool(!v)),
      _ => bail!(Runtime, "No negation for {:?}", self)
    }
  }

  pub fn try_add(&self, other: &Value) -> Value {
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

  pub fn try_subtract(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Float(v1 - v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Int(v1 - v2),
      (o1, o2) => panic!("Can't subtract mismatch types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn try_multiply(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Ok(Self::Float(v1 * v2)),
      (Self::Int(v1), Value::Int(v2)) => Ok(Self::Int(v1 * v2)),
      (o1, o2) => bail!(Runtime, "Can't multiply mismatch types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn try_divide(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Ok(Self::Float(v1 / v2)),
      (Self::Int(v1), Value::Int(v2)) => Ok(Self::Int(v1 / v2)),
      (o1, o2) => bail!(Runtime, "Can't divide mismatch types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn try_mod(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Ok(Self::Float(v1 % v2)),
      (Self::Int(v1), Value::Int(v2)) => Ok(Self::Int(v1 % v2)),
      (o1, o2) => bail!(Runtime, "Can't divide mismatch types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn try_gt(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Ok(Self::Bool(v1 > v2)),
      (Self::Int(v1), Value::Int(v2)) => Ok(Self::Bool(v1 > v2)),
      (o1, o2) => bail!(Runtime, "Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn try_gte(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Ok(Self::Bool(v1 >= v2)),
      (Self::Int(v1), Value::Int(v2)) => Ok(Self::Bool(v1 >= v2)),
      (o1, o2) => bail!(Runtime, "Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn try_lt(&self, other: &Value) -> Value {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Self::Bool(v1 < v2),
      (Self::Int(v1), Value::Int(v2)) => Self::Bool(v1 < v2),
      (o1, o2) => panic!("Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn try_lte(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Ok(Self::Bool(v1 <= v2)),
      (Self::Int(v1), Value::Int(v2)) => Ok(Self::Bool(v1 <= v2)),
      (o1, o2) => bail!(Runtime, "Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn try_eq(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Ok(Self::Bool((v1 - v2).abs() < f64::EPSILON)),
      (Self::Int(v1), Value::Int(v2)) => Ok(Self::Bool(v1 == v2)),
      (Self::Bool(v1), Value::Bool(v2)) => Ok(Self::Bool(v1 == v2)),
      (Self::String(v1), Value::String(v2)) => Ok(Self::Bool(v1 == v2)),
      (o1, o2) => bail!(Runtime, "Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn try_neq(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Ok(Self::Bool((v1 - v2).abs() >= f64::EPSILON)),
      (Self::Int(v1), Value::Int(v2)) => Ok(Self::Bool(v1 != v2)),
      (Self::Bool(v1), Value::Bool(v2)) => Ok(Self::Bool(v1 != v2)),
      (Self::String(v1), Value::String(v2)) => Ok(Self::Bool(v1 != v2)),
      (o1, o2) => bail!(Runtime, "Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn try_and(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Bool(v1), Value::Bool(v2)) => Ok(Self::Bool(v1 && v2)),
      (o1, o2) => bail!(Runtime, "Can't compare types: {:?}, {:?}", o1, o2)
    }
  }

  pub fn try_or(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Bool(v1), Value::Bool(v2)) => Ok(Self::Bool(v1 || v2)),
      (o1, o2) => bail!(Runtime, "Can't compare types: {:?}, {:?}", o1, o2)
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn value_clone() {
    assert_eq!(Value::Float(1.0).try_clone().unwrap(), Value::Float(1.0));
  }
}
