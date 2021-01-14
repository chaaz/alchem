//! Compile-time type information

use crate::common::{Function, MorphIndex};
use std::sync::Weak;
use std::iter::{empty, once};
use crate::either::IterEither3::{A, B, C};

#[derive(Clone, Debug)]
pub enum Type {
  Int,
  Bool,
  String,
  FnSync(Weak<Function>), // Weak, so that we can collapse functions later.
  Unset,
  DependsOn(DependsOn)
}

impl PartialEq for Type {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Int, Self::Int) => true,
      (Self::Bool, Self::Bool) => true,
      (Self::String, Self::String) => true,
      (Self::FnSync(a), Self::FnSync(b)) => Weak::ptr_eq(a, b),
      (Self::Unset, Self::Unset) => true,
      _ => false
    }
  }
}

impl Eq for Type {}

impl Type {
  pub fn is_depends(&self) -> bool {
    matches!(self, Self::DependsOn(_))
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

  pub fn is_known(&self) -> bool {
    !matches!(self, Self::DependsOn(_) | Self::Unset)
  }

  pub fn as_function(&self) -> Weak<Function> {
    match self {
      Self::FnSync(f) => f.clone(),
      _ => panic!("Type is not a function.")
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
