//! The values representable in our language.

use crate::errors::Result;
use std::sync::Arc;

#[derive(Debug, PartialEq)]
pub enum Value {
  Float(f64),
  Int(i32),
  Bool(bool),
  String(Arc<str>)
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

  pub fn try_clone(&self) -> Result<Value> {
    if !self.cloneable() {
      bail!(Runtime, "Can't clone {:?}", self);
    }

    match self {
      Self::Float(v) => Ok(Self::Float(*v)),
      Self::Int(v) => Ok(Self::Int(*v)),
      Self::Bool(v) => Ok(Self::Bool(*v)),
      Self::String(v) => Ok(Self::String(v.clone()))
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

  pub fn try_add(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Ok(Self::Float(v1 + v2)),
      (Self::Int(v1), Value::Int(v2)) => Ok(Self::Int(v1 + v2)),
      (Self::String(v1), Value::String(v2)) => {
        let mut s = v1.to_string();
        s.push_str(&v2);
        Ok(Self::String(s.into()))
      }
      (o1, o2) => bail!(Runtime, "Can't add mismatch types: {:?}, {:?}", o1, o2),
    }
  }

  pub fn try_subtract(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Ok(Self::Float(v1 - v2)),
      (Self::Int(v1), Value::Int(v2)) => Ok(Self::Int(v1 - v2)),
      (o1, o2) => bail!(Runtime, "Can't subtract mismatch types: {:?}, {:?}", o1, o2),
    }
  }

  pub fn try_multiply(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Ok(Self::Float(v1 * v2)),
      (Self::Int(v1), Value::Int(v2)) => Ok(Self::Int(v1 * v2)),
      (o1, o2) => bail!(Runtime, "Can't multiply mismatch types: {:?}, {:?}", o1, o2),
    }
  }

  pub fn try_divide(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Ok(Self::Float(v1 / v2)),
      (Self::Int(v1), Value::Int(v2)) => Ok(Self::Int(v1 / v2)),
      (o1, o2) => bail!(Runtime, "Can't divide mismatch types: {:?}, {:?}", o1, o2),
    }
  }

  pub fn try_mod(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Ok(Self::Float(v1 % v2)),
      (Self::Int(v1), Value::Int(v2)) => Ok(Self::Int(v1 % v2)),
      (o1, o2) => bail!(Runtime, "Can't divide mismatch types: {:?}, {:?}", o1, o2),
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

  pub fn try_lt(self, other: Value) -> Result<Value> {
    match (self, other) {
      (Self::Float(v1), Value::Float(v2)) => Ok(Self::Bool(v1 < v2)),
      (Self::Int(v1), Value::Int(v2)) => Ok(Self::Bool(v1 < v2)),
      (o1, o2) => bail!(Runtime, "Can't compare types: {:?}, {:?}", o1, o2)
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

pub struct ValueArray {
  values: Vec<Value>
}

impl Default for ValueArray {
  fn default() -> ValueArray { ValueArray::new() }
}

impl ValueArray {
  pub fn new() -> ValueArray { ValueArray { values: Vec::new() } }
  pub fn len(&self) -> usize { self.values.len() }
  pub fn is_empty(&self) -> bool { self.values.is_empty() }
  pub fn get(&self, ind: usize) -> Option<&Value> { self.values.get(ind) }
  pub fn iter(&self) -> impl Iterator<Item = &Value> { self.values.iter() }

  pub fn add(&mut self, v: Value) -> usize {
    self.values.push(v);
    self.values.len() - 1
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn simple() {
    let mut va = ValueArray::new();
    assert_eq!(va.add(Value::Float(3.0)), 0);
    assert_eq!(va.values.len(), 1);
  }

  #[test]
  fn value_clone() {
    assert_eq!(Value::Float(1.0).try_clone().unwrap(), Value::Float(1.0));
  }
}
