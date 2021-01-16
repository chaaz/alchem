//! Compile-time type information

use crate::common::{Function, MorphIndex};
use crate::either::IterEither3::{A, B, C};
use std::iter::{empty, once};
use std::sync::{Arc, Weak};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq)]
pub struct Array {
  types: Vec<Type>,
}

impl Default for Array {
  fn default() -> Self { Array::new() }
}

impl Array {
  pub fn new() -> Array { Array { types: Vec::new() } }
  pub fn types(&self) -> &[Type] { &self.types }
  pub fn get(&self, ind: usize) -> &Type { self.types.get(ind).unwrap() }
  pub fn len(&self) -> usize { self.types.len() }
  pub fn add(&mut self, t: Type) { self.types.push(t); }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Object {
  types: HashMap<String, Type>,
  indexes: Vec<String>
}

impl Default for Object {
  fn default() -> Self { Object::new() }
}

impl Object {
  pub fn new() -> Object { Object { types: HashMap::new(), indexes: Vec::new() } }
  pub fn types(&self) -> &HashMap<String, Type> { &self.types }
  pub fn indexes(&self) -> &[String] { &self.indexes }
  pub fn index_of(&self, key: &str) -> Option<usize> { self.indexes.iter().position(|k| k == key) }
  pub fn get(&self, key: &str) -> &Type { self.types.get(key).unwrap() }
  pub fn len(&self) -> usize { self.indexes.len() }

  pub fn add(&mut self, key: String, t: Type) {
    self.types.insert(key, t);
    self.indexes = self.types.keys().cloned().collect();
    self.indexes.sort();
  }
}

#[derive(Clone, Debug)]
pub enum Type {
  Number,
  Bool,
  String,
  Object(Arc<Object>),
  Array(Arc<Array>),
  FnSync(Weak<Function>), // Weak, so that we can collapse functions later.
  Unset,
  DependsOn(DependsOn)
}

impl PartialEq for Type {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Number, Self::Number) => true,
      (Self::Bool, Self::Bool) => true,
      (Self::String, Self::String) => true,
      (Self::FnSync(a), Self::FnSync(b)) => Weak::ptr_eq(a, b),
      (Self::Unset, Self::Unset) => true,
      (Self::Object(a), Self::Object(b)) => a == b,
      (Self::Array(a), Self::Array(b)) => a == b,
      _ => false
    }
  }
}

impl Eq for Type {}

impl Type {
  pub fn is_depends(&self) -> bool { matches!(self, Self::DependsOn(_)) }
  pub fn and_depends(self, other: Self) -> Type { Type::DependsOn(self.into_depends().and(other.into_depends())) }
  pub fn or_depends(self, other: Self) -> Type { Type::DependsOn(self.into_depends().or(other.into_depends())) }

  pub fn depends(func: &Arc<Function>, inst_ind: usize) -> Type {
    Type::DependsOn(DependsOn::unit(MorphIndex::weak(func, inst_ind)))
  }

  pub fn into_depends(self) -> DependsOn {
    match self {
      Self::DependsOn(d) => d,
      _ => panic!("Not a dependent type.")
    }
  }

  pub fn depends_iter(&self) -> impl Iterator<Item = &MorphIndex> {
    match self {
      Self::DependsOn(d) => d.index_iter(),
      _ => panic!("Not a dependent type.")
    }
  }

  pub fn is_known(&self) -> bool { !matches!(self, Self::DependsOn(_) | Self::Unset) }

  pub fn as_function(&self) -> Weak<Function> {
    match self {
      Self::FnSync(f) => f.clone(),
      _ => panic!("Type is not a function.")
    }
  }

  pub fn as_array(&self) -> &Array {
    match self {
      Self::Array(a) => a,
      other => panic!("Type {:?} is not an array.", other)
    }
  }

  pub fn as_object(&self) -> &Object {
    match self {
      Self::Object(o) => o,
      other => panic!("Type {:?} is not an object.", other)
    }
  }
}

#[derive(Clone, Debug)]
pub enum DependsOn {
  None,
  Unit(MorphIndex),
  And(Box<DependsOn>, Box<DependsOn>),
  Or(Box<DependsOn>, Box<DependsOn>)
}

impl DependsOn {
  pub fn func(func: &Arc<Function>, inst_ind: usize) -> DependsOn { DependsOn::unit(MorphIndex::weak(func, inst_ind)) }
  pub fn unit(i: MorphIndex) -> DependsOn { DependsOn::Unit(i) }
  pub fn and(self, i: DependsOn) -> DependsOn { DependsOn::And(Box::new(self), Box::new(i)) }
  pub fn or(self, i: DependsOn) -> DependsOn { DependsOn::Or(Box::new(self), Box::new(i)) }

  pub fn is_known(&self) -> bool {
    match self {
      Self::None => true,
      Self::Unit(i) => i.is_known(),
      Self::And(a, b) => a.is_known() && b.is_known(),
      Self::Or(a, b) => a.is_known() || b.is_known()
    }
  }

  pub fn index_iter(&self) -> impl Iterator<Item = &MorphIndex> {
    match self {
      Self::None => A(empty()),
      Self::Unit(i) => B(once(i)),
      Self::And(a, b) | Self::Or(a, b) => {
        C(Box::new(a.index_iter().chain(b.index_iter())) as Box<dyn Iterator<Item = &MorphIndex>>)
      }
    }
  }
}
