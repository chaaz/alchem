//! Compile-time type information

use crate::common::{Function, MorphIndex};
use crate::either::IterEither3::{A, B, C};
use std::collections::HashMap;
use std::fmt;
use std::iter::{empty, once};
use std::sync::{Arc, Weak};

#[derive(Debug, PartialEq, Eq)]
pub struct Array<C: CustomType> {
  types: Vec<Type<C>>
}

impl<C: CustomType + 'static> Default for Array<C> {
  fn default() -> Self { Array::new() }
}

impl<C: CustomType + 'static> Array<C> {
  pub fn new() -> Array<C> { Array { types: Vec::new() } }
  pub fn types(&self) -> &[Type<C>] { &self.types }
  pub fn get(&self, ind: usize) -> &Type<C> { self.types.get(ind).unwrap() }
  pub fn len(&self) -> usize { self.types.len() }
  pub fn add(&mut self, t: Type<C>) { self.types.push(t); }
  pub fn is_single_use(&self) -> bool { self.types.iter().any(|v| v.is_single_use()) }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Object<C: CustomType> {
  types: HashMap<String, Type<C>>,
  index: Vec<String>
}

impl<C: CustomType + 'static> Default for Object<C> {
  fn default() -> Self { Object::new() }
}

impl<C: CustomType + 'static> Object<C> {
  pub fn new() -> Object<C> { Object { types: HashMap::new(), index: Vec::new() } }
  pub fn types(&self) -> &HashMap<String, Type<C>> { &self.types }
  pub fn index(&self) -> &[String] { &self.index }
  pub fn index_of(&self, key: &str) -> Option<usize> { self.index.iter().position(|k| k == key) }
  pub fn get(&self, key: &str) -> &Type<C> { self.types.get(key).unwrap() }
  pub fn len(&self) -> usize { self.index.len() }
  pub fn is_single_use(&self) -> bool { self.types.values().any(|v| v.is_single_use()) }

  pub fn add(&mut self, key: String, t: Type<C>) {
    self.types.insert(key, t);
    self.index = self.types.keys().cloned().collect();
    self.index.sort();
  }
}

#[derive(Clone, Debug)]
pub enum Type<C>
where
  C: CustomType
{
  Number,
  Bool,
  String,
  Object(Arc<Object<C>>),
  Array(Arc<Array<C>>),
  FnSync(Weak<Function<C>>), // Weak, so that we can collapse functions later.
  Json,
  Custom(C),
  Unset,
  DependsOn(DependsOn<C>)
}

impl<C: CustomType> PartialEq for Type<C> {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Number, Self::Number) => true,
      (Self::Bool, Self::Bool) => true,
      (Self::String, Self::String) => true,
      (Self::FnSync(a), Self::FnSync(b)) => Weak::ptr_eq(a, b),
      (Self::Unset, Self::Unset) => true,
      (Self::Json, Self::Json) => true,
      (Self::Object(a), Self::Object(b)) => a == b,
      (Self::Array(a), Self::Array(b)) => a == b,
      (Self::Custom(a), Self::Custom(b)) => a == b,
      _ => false
    }
  }
}

impl<C: CustomType> Eq for Type<C> {}

impl<C> Type<C>
where
  C: CustomType + 'static
{
  pub fn is_depends(&self) -> bool { matches!(self, Self::DependsOn(_)) }
  pub fn and_depends(self, other: Self) -> Type<C> { Type::DependsOn(self.into_depends().and(other.into_depends())) }
  pub fn or_depends(self, other: Self) -> Type<C> { Type::DependsOn(self.into_depends().or(other.into_depends())) }

  pub fn depends(func: &Arc<Function<C>>, inst_ind: usize) -> Type<C> {
    Type::DependsOn(DependsOn::unit(MorphIndex::weak(func, inst_ind)))
  }

  pub fn is_single_use(&self) -> bool {
    match self {
      Self::Object(o) => o.is_single_use(),
      Self::Array(a) => a.is_single_use(),
      Self::FnSync(f) => f.upgrade().unwrap().is_single_use(),
      Self::String => true,
      Self::Custom(c) => c.is_single_use(),
      _ => false
    }
  }

  pub fn is_json(&self) -> bool { matches!(self, Self::Json) }

  pub fn as_custom(&self) -> &C {
    match self {
      Self::Custom(c) => c,
      other => panic!("Not a custom type: {:?}", other)
    }
  }

  pub fn as_custom_mut(&mut self) -> &mut C {
    match self {
      Self::Custom(c) => c,
      other => panic!("Not a custom type: {:?}", other)
    }
  }

  pub fn into_custom(self) -> C {
    match self {
      Self::Custom(c) => c,
      other => panic!("Not a custom type: {:?}", other)
    }
  }

  pub fn into_depends(self) -> DependsOn<C> {
    match self {
      Self::DependsOn(d) => d,
      _ => panic!("Not a dependent type.")
    }
  }

  pub fn depends_iter(&self) -> impl Iterator<Item = &MorphIndex<C>> {
    match self {
      Self::DependsOn(d) => d.index_iter(),
      _ => panic!("Not a dependent type.")
    }
  }

  pub fn is_known(&self) -> bool { !matches!(self, Self::DependsOn(_) | Self::Unset) }

  pub fn as_function(&self) -> Weak<Function<C>> {
    match self {
      Self::FnSync(f) => f.clone(),
      _ => panic!("Type is not a function.")
    }
  }

  pub fn as_array(&self) -> &Array<C> {
    match self {
      Self::Array(a) => a,
      other => panic!("Type {:?} is not an array.", other)
    }
  }

  pub fn as_object(&self) -> &Object<C> {
    match self {
      Self::Object(o) => o,
      other => panic!("Type {:?} is not an object.", other)
    }
  }

  pub fn into_object(self) -> Arc<Object<C>> {
    match self {
      Self::Object(o) => o,
      other => panic!("Type {:?} is not an object.", other)
    }
  }
}

pub trait CustomValue: PartialEq + Eq + fmt::Debug {
  fn shift(&mut self) -> Self;
}

pub trait CustomType: PartialEq + Eq + IsSingle + fmt::Debug + Clone {
  type Collapsed: Clone + fmt::Debug;
  type Value: CustomValue;
  fn collapse(&self) -> Self::Collapsed;
}

pub trait IsSingle {
  fn is_single_use(&self) -> bool;
}

#[derive(Debug)]
pub enum NoValue {}

impl CustomValue for NoValue {
  fn shift(&mut self) -> Self { panic!("Can't shift non-existent NoValue") }
}

impl PartialEq for NoValue {
  fn eq(&self, _other: &Self) -> bool { true }
}

impl Eq for NoValue {}

#[derive(Clone, Debug)]
pub enum NoCustom {}

impl CustomType for NoCustom {
  type Collapsed = ();
  type Value = NoValue;
  fn collapse(&self) {}
}

impl PartialEq for NoCustom {
  fn eq(&self, _other: &Self) -> bool { true }
}

impl Eq for NoCustom {}

impl IsSingle for NoCustom {
  fn is_single_use(&self) -> bool { false }
}

#[derive(Clone, Debug)]
pub enum DependsOn<C: CustomType> {
  None,
  Unit(MorphIndex<C>),
  And(Box<DependsOn<C>>, Box<DependsOn<C>>),
  Or(Box<DependsOn<C>>, Box<DependsOn<C>>)
}

impl<C: CustomType + 'static> DependsOn<C> {
  pub fn func(func: &Arc<Function<C>>, inst_ind: usize) -> DependsOn<C> {
    DependsOn::unit(MorphIndex::weak(func, inst_ind))
  }

  pub fn unit(i: MorphIndex<C>) -> DependsOn<C> { DependsOn::Unit(i) }
  pub fn and(self, i: DependsOn<C>) -> DependsOn<C> { DependsOn::And(Box::new(self), Box::new(i)) }
  pub fn or(self, i: DependsOn<C>) -> DependsOn<C> { DependsOn::Or(Box::new(self), Box::new(i)) }

  pub fn is_known(&self) -> bool {
    match self {
      Self::None => true,
      Self::Unit(i) => i.is_known(),
      Self::And(a, b) => a.is_known() && b.is_known(),
      Self::Or(a, b) => a.is_known() || b.is_known()
    }
  }

  pub fn index_iter(&self) -> impl Iterator<Item = &MorphIndex<C>> {
    match self {
      Self::None => A(empty()),
      Self::Unit(i) => B(once(i)),
      Self::And(a, b) | Self::Or(a, b) => {
        C(Box::new(a.index_iter().chain(b.index_iter())) as Box<dyn Iterator<Item = &MorphIndex<C>>>)
      }
    }
  }
}
