//! Compile-time type information

use crate::common::{Function, MorphIndex};
use crate::either::IterEither3::{A, B, C};
use crate::pick;
use std::collections::HashMap;
use std::fmt;
use std::iter::{empty, once};
use std::sync::{Arc, Weak};

#[derive(Debug, PartialEq, Eq)]
pub struct Array<C: CustomType + Send + 'static> {
  types: Vec<Type<C>>
}

impl<C: CustomType + Send + 'static> Default for Array<C> {
  fn default() -> Self { Array::new() }
}

impl<C: CustomType + Send + 'static> Array<C> {
  pub fn new() -> Array<C> { Array { types: Vec::new() } }
  pub fn types(&self) -> &[Type<C>] { &self.types }
  pub fn into_types(self) -> Vec<Type<C>> { self.types }
  pub fn get(&self, ind: usize) -> &Type<C> { self.types.get(ind).unwrap() }
  pub fn len(&self) -> usize { self.types.len() }
  pub fn is_empty(&self) -> bool { self.types.is_empty() }
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
  pub fn is_empty(&self) -> bool { self.index.is_empty() }
  pub fn is_single_use(&self) -> bool { self.types.values().any(|v| v.is_single_use()) }

  pub fn ordered(&self) -> impl Iterator<Item = &Type<C>> + '_ {
    self.index.iter().map(move |k| self.types.get(k).unwrap())
  }

  pub fn into_ordered(self) -> impl Iterator<Item = Type<C>> {
    let Object { mut types, index } = self;
    index.into_iter().map(move |o| types.remove(&o).unwrap())
  }

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
  String(Option<String>),
  Object(Arc<Object<C>>),
  Array(Arc<Array<C>>),
  Iter(Box<Type<C>>),
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
      (Self::String(_), Self::String(_)) => true,
      (Self::FnSync(a), Self::FnSync(b)) => Weak::ptr_eq(a, b),
      (Self::Unset, Self::Unset) => true,
      (Self::Json, Self::Json) => true,
      (Self::Iter(a), Self::Iter(b)) => a == b,
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
  pub fn and_depends(self, other: Self) -> Type<C> { Type::DependsOn(self.into_depends().and(other.into_depends())) }
  pub fn or_depends(self, other: Self) -> Type<C> { Type::DependsOn(self.into_depends().or(other.into_depends())) }

  pub fn depends(func: &Arc<Function<C>>, inst_ind: usize) -> Type<C> {
    Type::DependsOn(DependsOn::unit(MorphIndex::weak(func, inst_ind)))
  }

  pub fn is_depends(&self) -> bool { matches!(self, Self::DependsOn(_)) }
  pub fn is_object(&self) -> bool { matches!(self, Type::Object(..)) }
  pub fn is_array(&self) -> bool { matches!(self, Type::Array(..)) }
  pub fn is_iter(&self) -> bool { matches!(self, Type::Iter(..)) }
  pub fn is_function(&self) -> bool { matches!(self, Type::FnSync(..)) }
  pub fn is_json(&self) -> bool { matches!(self, Self::Json) }
  pub fn is_known(&self) -> bool { !matches!(self, Self::DependsOn(_) | Self::Unset) }
  pub fn is_string(&self) -> bool { matches!(self, Self::String(_)) }
  pub fn is_string_literal(&self) -> bool { matches!(self, Self::String(Some(_))) }
  pub fn is_number(&self) -> bool { matches!(self, Self::Number) }
  pub fn is_bool(&self) -> bool { matches!(self, Self::Bool) }
  pub fn is_unset(&self) -> bool { matches!(self, Self::Unset) }

  pub fn is_single_use(&self) -> bool {
    match self {
      Self::Object(o) => o.is_single_use(),
      Self::Array(a) => a.is_single_use(),
      Self::FnSync(f) => f.upgrade().unwrap().is_single_use(),
      Self::Custom(c) => c.is_single_use(),
      _ => false
    }
  }

  pub fn as_custom(&self) -> &C { pick!(self, Self::Custom(c) => c, "Not a custom type: {:?}") }
  pub fn as_custom_mut(&mut self) -> &mut C { pick!(self, Self::Custom(c) => c, "Not a custom type: {:?}") }
  pub fn as_array(&self) -> &Array<C> { pick!(self, Self::Array(a) => a, "Not an array: {:?}") }
  pub fn as_depends(&self) -> &DependsOn<C> { pick!(self, Self::DependsOn(d) => d, "Not a dependent type: {:?}") }
  pub fn depends_iter(&self) -> impl Iterator<Item = &MorphIndex<C>> { self.as_depends().index_iter() }
  pub fn as_object(&self) -> &Object<C> { pick!(self, Self::Object(o) => o, "Not an object: {:?}") }
  pub fn as_string_literal(&self) -> &str { pick!(self, Self::String(Some(s)) => s, "Not a string literal: {:?}") }
  pub fn as_string(&self) -> &Option<String> { pick!(self, Self::String(s) => s, "Not a string type: {:?}") }
  pub fn as_iter(&self) -> &Type<C> { pick!(self, Self::Iter(s) => s, "Not an iter: {:?}") }

  pub fn as_function(&self) -> Weak<Function<C>> {
    pick!(self, Self::FnSync(f) => f.clone(), "Type {:?} is not a function.")
  }

  pub fn into_custom(self) -> C { pick!(self, Self::Custom(c) => c, "Not a custom type: {:?}") }
  pub fn into_depends(self) -> DependsOn<C> { pick!(self, Self::DependsOn(d) => d, "Not a dependent type: {:?}") }
  pub fn into_object(self) -> Arc<Object<C>> { pick!(self, Self::Object(o) => o, "Not an object: {:?}") }
}

pub trait Runtime: Clone + Send + 'static {}

pub trait CustomMeta: Clone + Send + Sync + 'static {
  fn update(&self) -> Self;
  fn init() -> Self;
}

pub trait CustomValue: Sized + Send + fmt::Debug {
  fn shift(&mut self) -> Option<Self>;
}

pub trait CustomType: PartialEq + Eq + IsSingle + fmt::Debug + Clone + Send + Sync + 'static {
  type Collapsed: Clone + Send + Sync + fmt::Debug;
  type Value: CustomValue;
  type Runtime: Runtime;
  type Meta: CustomMeta;
  fn collapse(&self) -> Self::Collapsed;
}

pub trait IsSingle {
  fn is_single_use(&self) -> bool;
}

#[derive(Debug)]
pub enum NoValue {}

impl CustomValue for NoValue {
  fn shift(&mut self) -> Option<Self> { panic!("Can't shift non-existent NoValue") }
}

impl Runtime for () {}

impl CustomMeta for () {
  fn update(&self) {}
  fn init() {}
}

#[derive(Clone, Debug)]
pub enum NoCustom {}

impl CustomType for NoCustom {
  type Collapsed = ();
  type Value = NoValue;
  type Runtime = ();
  type Meta = ();

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

  pub fn all(mut i: impl Iterator<Item = (Arc<Function<C>>, usize)>) -> DependsOn<C> {
    if let Some((f, ind)) = i.next() {
      let mut d = DependsOn::func(&f, ind);
      for (f, ind) in i {
        d = d.and(DependsOn::func(&f, ind));
      }
      d
    } else {
      DependsOn::None
    }
  }

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
