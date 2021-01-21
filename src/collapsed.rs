//! "Collapsed" values are stripped of compile-time-only features; this makes them more accessable to the VM.
//!
//! It may be helpful to think of the types defined here are the "real" types, and their equivalents in `common`
//! and `value` as builders.

use crate::common::{Instr, MorphStatus, Native, NativeInfo};
use crate::types::{Type, CustomType, Object, Array};
use crate::value::Value;
use std::fmt;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct CollapsedArray<C: CustomType> {
  types: Vec<CollapsedType<C>>,
}

impl<C: CustomType + 'static> CollapsedArray<C> {
  pub fn from_common(o: &Array<C>) -> CollapsedArray<C> {
    let types = o.types().iter().map(|t| CollapsedType::from_common(t)).collect();
    CollapsedArray { types }
  }

  pub fn into_types(self) -> impl Iterator<Item = CollapsedType<C>> { self.types.into_iter() }
}

#[derive(Clone, Debug)]
pub struct CollapsedObject<C: CustomType> {
  types: Vec<CollapsedType<C>>,
  index: Vec<String>
}

impl<C: CustomType + 'static> CollapsedObject<C> {
  pub fn from_common(o: &Object<C>) -> CollapsedObject<C> {
    let index = o.index().to_vec();
    let types = index.iter().map(|i| CollapsedType::from_common(&o.types()[i])).collect();
    CollapsedObject { types, index }
  }

  pub fn into_key_types(self) -> impl Iterator<Item = (String, CollapsedType<C>)> {
    self.index.into_iter().zip(self.types.into_iter())
  }
}

#[derive(Clone, Debug)]
pub enum CollapsedType<C>
where
  C: CustomType
{
  Reflective,
  Object(CollapsedObject<C>),
  Array(CollapsedArray<C>),
  Custom(C::Collapsed)
}

impl<C: CustomType + 'static> CollapsedType<C> {
  pub fn from_common(t: &Type<C>) -> CollapsedType<C> {
    match t {
      Type::Unset | Type::DependsOn(_) => panic!("Can't collapse unknown type."),
      Type::Object(o) => CollapsedType::Object(CollapsedObject::from_common(o)),
      Type::Array(a) => CollapsedType::Array(CollapsedArray::from_common(a)),
      Type::Custom(c) => CollapsedType::Custom(c.collapse()),
      _ => CollapsedType::Reflective
    }
  }
}

pub enum Declared<C: CustomType> {
  Float(f64),
  Int(i32),
  Bool(bool),
  String(Arc<str>),
  Function(Arc<Function<C>>),
  Native(Arc<FuncNative<C>>)
}

impl<C: CustomType> fmt::Debug for Declared<C> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Float(v) => write!(f, "{}", v),
      Self::Int(v) => write!(f, "{}", v),
      Self::Bool(v) => write!(f, "{}", v),
      Self::String(v) => write!(f, "\"{}\"", v),
      Self::Function(v) => write!(f, "{:?}", v),
      Self::Native(v) => write!(f, "{:?}", v)
    }
  }
}

impl<C: CustomType + 'static> Declared<C> {
  pub fn from_common(d: crate::value::Declared<C>) -> Declared<C> {
    use crate::value::Declared as D;

    match d {
      D::Float(v) => Declared::Float(v),
      D::Int(v) => Declared::Int(v),
      D::Bool(v) => Declared::Bool(v),
      D::String(v) => Declared::String(v),
      D::Function(v) => collapse_function(v)
    }
  }

  pub fn as_str(&self) -> Option<&str> {
    match self {
      Self::String(s) => Some(s),
      _ => None
    }
  }

  pub fn as_function(&self) -> Arc<Function<C>> {
    match self {
      Self::Function(v) => v.clone(),
      _ => panic!("Not a function: {:?}", self)
    }
  }

  pub fn to_value(&self) -> Value<C> {
    match self {
      Self::Float(v) => Value::Float(*v),
      Self::Int(v) => Value::Int(*v),
      Self::Bool(v) => Value::Bool(*v),
      Self::String(v) => Value::String(v.clone()),
      Self::Function(_) => {
        // You have to use `as_function`, and then generate a closure out of it: see handling of Opcode::closure
        // in `src/vm.rs`
        panic!("Can't declare a function value.")
      }
      Self::Native(v) => Value::Native(v.clone())
    }
  }
}

pub fn collapse_function<C: CustomType + 'static>(f: Arc<crate::common::Function<C>>) -> Declared<C> {
  use crate::common::FnType;

  match f.fn_type() {
    FnType::Native(ntv, _) => Declared::Native(Arc::new(FuncNative::from_common(*ntv, f))),
    FnType::Alchem(..) => Declared::Function(Arc::new(Function::from_common(f)))
  }
}

pub struct FuncNative<C: CustomType> {
  arity: u8,
  native: Native<C>,
  instances: Vec<NativeInfo<C>>
}

impl<C: CustomType> fmt::Debug for FuncNative<C> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let arg_n = if self.arity == 1 { "arg" } else { "args" };
    write!(f, "native({} {})", self.arity, arg_n)
  }
}

impl<C: CustomType + 'static> FuncNative<C> {
  pub fn from_common(native: Native<C>, f: Arc<crate::common::Function<C>>) -> FuncNative<C> {
    let f = Arc::try_unwrap(f).expect("Multiple references to function during collapse.");
    let (arity, morphs) = f.into_collapse();
    let instances = morphs
      .into_iter()
      .map(|i| match i.into_status() {
        MorphStatus::NativeCompleted(ninfo, _) => ninfo,
        _ => panic!("Native instance not completed natively.")
      })
      .collect();

    FuncNative { arity, native, instances }
  }

  pub fn arity(&self) -> u8 { self.arity }
  pub fn native(&self) -> &Native<C> { &self.native }
  pub fn instances(&self) -> &[NativeInfo<C>] { &self.instances }
}

pub struct Function<C: CustomType> {
  arity: u8,
  instances: Vec<Chunk<C>>
}

impl<C: CustomType> fmt::Debug for Function<C> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let arg_n = if self.arity == 1 { "arg" } else { "args" };
    write!(f, "fn({} {})", self.arity, arg_n)
  }
}

impl<C: CustomType + 'static> Function<C> {
  pub fn from_common(f: Arc<crate::common::Function<C>>) -> Function<C> {
    let f = Arc::try_unwrap(f).expect("Multiple references to function during collapse.");
    let (arity, morphs) = f.into_collapse();
    let instances = morphs
      .into_iter()
      .map(|i| match i.into_status() {
        MorphStatus::Completed(chunk, _) => Chunk::from_common(chunk),
        _ => panic!("Function instance not completed.")
      })
      .collect();

    Function { arity, instances }
  }

  pub fn arity(&self) -> u8 { self.arity }
  pub fn instances(&self) -> &[Chunk<C>] { &self.instances }
  pub fn chunk(&self, inst_ind: usize) -> &Chunk<C> { &self.instances[inst_ind] }
}

pub struct Chunk<C: CustomType> {
  constants: Constants<C>,
  code: Vec<Instr>
}

impl<C: CustomType + 'static> Chunk<C> {
  pub fn from_common(c: crate::common::Chunk<C>) -> Chunk<C> {
    let (constants, code) = c.into_collapse();
    Chunk { constants: Constants::from_common(constants), code }
  }

  pub fn code_len(&self) -> usize { self.code.len() }
  pub fn code_is_empty(&self) -> bool { self.code.is_empty() }
  pub fn at(&self, ind: usize) -> Option<&Instr> { self.code.get(ind) }

  // FAST -- might be used for stack trace
  // #[inline]
  // pub fn at_fast(&self, ind: usize) -> &Instr { &self.code[ind] }

  pub fn constants_len(&self) -> usize { self.constants.len() }
  pub fn get_constant(&self, ind: usize) -> Option<&Declared<C>> { self.constants.get(ind) }

  pub fn debug(&self) {
    println!("constants:");
    for (i, c) in self.constants.iter().enumerate() {
      println!("  {:>04}: {:?}", i, c);
    }
    println!("byecode:");
    for (i, op) in self.code.iter().enumerate() {
      println!("  {:>04}: {:?}", i, op);
    }
  }
}

struct Constants<C: CustomType> {
  values: Vec<Declared<C>>
}

impl<C: CustomType + 'static> Constants<C> {
  pub fn from_common(c: Vec<crate::value::Declared<C>>) -> Constants<C> {
    Constants { values: c.into_iter().map(Declared::from_common).collect() }
  }

  pub fn len(&self) -> usize { self.values.len() }
  pub fn get(&self, ind: usize) -> Option<&Declared<C>> { self.values.get(ind) }
  pub fn iter(&self) -> impl Iterator<Item = &Declared<C>> { self.values.iter() }
}
