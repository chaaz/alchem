//! Common info for the parser.

use crate::collapsed::{Captured, CollapsedType, RunMeta};
use crate::compiler::{Compiler, Destructure};
use crate::scanner::Token;
use crate::scope::ZeroMode;
use crate::types::{CustomType, DependsOn, Type};
use crate::value::{Declared, Value};
use crate::vm::Runner;
use std::collections::HashMap;
use std::fmt;
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex, Weak};

const MAX_CONSTANTS: usize = 255;
const MAX_MONOMORPHS: usize = 20;

pub type Native<C> = for<'r> fn(
  Vec<Value<C>>,
  Captured<C>,
  RunMeta<C>,
  &'r mut Runner<C>
) -> Pin<Box<dyn Future<Output = Value<C>> + Send + 'r>>;

pub type TypeNative<C> = for<'r> fn(
  Vec<Type<C>>,
  TypeCaptured<C>,
  &'r Globals<C>
) -> Pin<Box<dyn Future<Output = MorphStatus<C>> + Send + 'r>>;

pub type TypeCaptured<C> = Vec<Type<C>>;
pub type TypeMatch<C> = fn(&[Type<C>], &[Type<C>]) -> bool;
pub type ObjUpvalues<C> = Mutex<Vec<ObjUpvalue<C>>>;
pub type Globals<C> = HashMap<String, Arc<Function<C>>>;
pub type KnownUpvals<C> = HashMap<String, (usize, Type<C>)>;

pub struct Function<C: CustomType> {
  arity: u8,
  fn_type: FnType<C>,
  instances: Mutex<Vec<Monomorph<C>>>,
  known_upvals: KnownUpvals<C>
}

impl<C: CustomType> fmt::Debug for Function<C> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let arg_n = if self.arity == 1 { "arg" } else { "args" };
    let upv_n = if self.known_upvals.len() == 1 { "capture" } else { "captures" };
    write!(f, "fn({} {}),({} {})", self.arity, arg_n, self.known_upvals.len(), upv_n)
  }
}

impl<C: CustomType + 'static> Function<C> {
  pub fn script(chunk: Chunk<C>, stype: Type<C>) -> Function<C> {
    Function {
      arity: 0,
      fn_type: FnType::Alchem(Vec::new(), Vec::new()),
      known_upvals: HashMap::new(),
      instances: Mutex::new(vec![Monomorph {
        args: Vec::new(),
        status: MorphStatus::Completed(chunk, stype),
        build_dependencies: Vec::new(),
        type_dependencies: DependsOn::None,
        build_dependents: Vec::new(),
        type_dependents: Vec::new()
      }])
    }
  }

  pub fn new_alchem(
    arity: u8, param_names: Vec<Destructure>, code: Vec<Token>, known_upvals: KnownUpvals<C>
  ) -> Function<C> {
    Function { arity, fn_type: FnType::Alchem(param_names, code), instances: Mutex::new(Vec::new()), known_upvals }
  }

  pub fn full_native(
    arity: u8, native: Native<C>, type_native: TypeNative<C>, type_match: TypeMatch<C>, cap_types: Vec<Type<C>>,
    captures: Vec<Value<C>>
  ) -> Function<C> {
    Function {
      arity,
      fn_type: FnType::Native(native, type_native, type_match, cap_types, Mutex::new(captures)),
      instances: Mutex::new(Vec::new()),
      known_upvals: HashMap::new()
    }
  }

  pub fn new_native(arity: u8, native: Native<C>, type_native: TypeNative<C>) -> Function<C> {
    Function::full_native(arity, native, type_native, default_typematch, Vec::new(), Vec::new())
  }

  pub fn capture_native(
    arity: u8, native: Native<C>, type_native: TypeNative<C>, cap_types: Vec<Type<C>>, captures: Vec<Value<C>>
  ) -> Function<C> {
    Function {
      arity,
      fn_type: FnType::Native(native, type_native, default_typematch, cap_types, Mutex::new(captures)),
      instances: Mutex::new(Vec::new()),
      known_upvals: HashMap::new()
    }
  }

  pub fn is_single_use(&self) -> bool {
    match self.fn_type() {
      FnType::Alchem(..) => self.known_upvals.values().any(|(_, t)| t.is_single_use()),
      FnType::Native(.., ct, _) => ct.iter().any(|t| t.is_single_use())
    }
  }

  pub fn into_collapse(self) -> (FnType<C>, u8, Vec<Monomorph<C>>) {
    let arity = match self.fn_type() {
      FnType::Native(..) => self.arity,
      FnType::Alchem(pnames, _) => pnames.iter().map(|d| d.idents_len()).sum::<usize>() as u8
    };
    (self.fn_type, arity, self.instances.into_inner().unwrap())
  }

  pub fn fn_type(&self) -> &FnType<C> { &self.fn_type }
  pub fn arity(&self) -> u8 { self.arity }
  pub fn smart_name(&self) -> String { "...".into() }

  pub fn param_names(&self) -> Option<&[Destructure]> {
    match self.fn_type() {
      FnType::Native(..) => None,
      FnType::Alchem(pnames, _) => Some(pnames)
    }
  }

  pub fn incr_arity(&mut self) -> u8 {
    self.arity += 1;
    self.arity
  }

  pub async fn find_or_build(
    self: Arc<Function<C>>, args: Vec<Type<C>>, globals: &Globals<C>
  ) -> (FunctionIndex, Option<Type<C>>) {
    match self.fn_type() {
      FnType::Native(..) => {
        let (i, ftype) = self.find_or_build_ext(args, globals).await;
        (FunctionIndex::native(i), ftype)
      }
      FnType::Alchem(pnames, _) => {
        assert_eq!(args.len(), pnames.len());
        let extrs = pnames
          .iter()
          .zip(args.iter())
          .map(|(d, a)| {
            let idents_len = d.idents_len();
            d.extract(a.clone(), idents_len, ExtractionPart::empty()).1
          })
          .collect();
        let new_args = pnames.iter().zip(args.iter()).flat_map(|(d, a)| d.types(a.clone())).collect();
        let (i, ftype) = self.find_or_build_ext(new_args, globals).await;
        (FunctionIndex::alchem(i, extrs), ftype)
      }
    }
  }

  pub async fn find_or_build_ext(
    self: Arc<Function<C>>, args: Vec<Type<C>>, globals: &Globals<C>
  ) -> (usize, Option<Type<C>>) {
    match self.find_known_type(&args) {
      Some(found) => found,
      None => {
        let inst_ind = self.reserve_inst(args);
        (inst_ind, self.replay(inst_ind, globals).await)
      }
    }
  }

  pub fn known_type(&self, inst_ind: usize) -> Option<Type<C>> {
    let instances = self.instances.try_lock().unwrap();
    instances[inst_ind].known_type()
  }

  pub fn find_known_type(&self, args: &[Type<C>]) -> Option<(usize, Option<Type<C>>)> {
    let instances = self.instances.try_lock().unwrap();
    if let FnType::Native(_, _, tm, ..) = self.fn_type() {
      instances.iter().enumerate().find(|(_, m)| tm(&m.args, args)).map(|(i, m)| (i, m.known_type()))
    } else {
      instances.iter().enumerate().find(|(_, m)| m.args == args).map(|(i, m)| (i, m.known_type()))
    }
  }

  pub fn reserve_inst(&self, args: Vec<Type<C>>) -> usize {
    let mut instances = self.instances.try_lock().unwrap();
    instances.push(Monomorph::new(args));
    if !self.fn_type().is_native() && instances.len() > MAX_MONOMORPHS {
      panic!("Too many non-native function instances: {}", instances.len());
    }
    instances.len() - 1
  }

  pub fn replay_if_ready<'s>(
    self: Arc<Function<C>>, inst_ind: usize, globals: &'s Globals<C>
  ) -> Pin<Box<dyn Future<Output = Option<Type<C>>> + Send + 's>> {
    let (needs_rebuild, build_deps, needs_type, type_deps) = {
      let instances = self.instances.try_lock().unwrap();
      let morph = &instances[inst_ind];

      let needs_rebuild = morph.status.needs_rebuild();
      let build_deps = if needs_rebuild { morph.build_dependencies.clone() } else { Vec::new() };

      let needs_type = !morph.status.is_known();
      let type_deps = if needs_type { morph.type_dependencies.clone() } else { DependsOn::None };

      (needs_rebuild, build_deps, needs_type, type_deps)
    };

    Box::pin(async move {
      if (needs_rebuild && build_deps.iter().all(|d| d.is_known())) || (needs_type && type_deps.is_known()) {
        self.replay(inst_ind, globals).await
      } else {
        self.known_type(inst_ind)
      }
    })
  }

  pub fn replay<'s>(
    self: Arc<Function<C>>, inst_ind: usize, globals: &'s Globals<C>
  ) -> Pin<Box<dyn Future<Output = Option<Type<C>>> + Send + 's>> {
    Box::pin(self.do_replay(inst_ind, globals))
  }

  async fn do_replay(self: Arc<Function<C>>, inst_ind: usize, globals: &Globals<C>) -> Option<Type<C>> {
    // TODO(later): protect against indeterminate recursive types, by ensuring that the type deps for a function
    // does not depend strictly on itself or at all on any of its other instances.
    //
    // ex. `f = fn(x) { = f({ x: x }) }; = f(1)`
    //
    // This maybe relates to the same with return types?
    //
    // ex. `f = fn(x) { = { x: f(x) } }; = f(1)`
    //
    // Currently (somewhat) protected via MAX_MONOMORPHS

    let (args, old_type) = {
      let mut instances = self.instances.try_lock().unwrap();
      let Monomorph { args, status, .. } = &mut instances[inst_ind];
      (args.clone(), status.known_type())
    };
    self.unlink_all(inst_ind);

    let status = match self.fn_type() {
      FnType::Native(_, type_fn, _, tc, _) => (type_fn)(args, tc.clone(), globals).await,
      FnType::Alchem(pnames, code) => {
        let self_index = MorphIndex::weak(&self, inst_ind);
        let code = code.clone().into_iter();
        let compiler = Compiler::<C>::replay(pnames.clone(), args, code, self.known_upvals.clone(), globals);
        let (scope_zero, rtype) = compiler.compile().await;

        match scope_zero.into_mode() {
          ZeroMode::Emitting(chunk) => MorphStatus::Completed(chunk, rtype),
          ZeroMode::Dependent(inds) => {
            for ind in &inds {
              ind.add_build_dependent(self_index.clone());
            }
            if rtype.is_depends() {
              for ind in rtype.depends_iter() {
                ind.add_type_dependent(self_index.clone());
              }
            }

            let rtype = {
              let mut instances = self.instances.try_lock().unwrap();
              let morph = &mut instances[inst_ind];
              morph.build_dependencies = inds;
              if rtype.is_depends() {
                morph.type_dependencies = rtype.into_depends();
                Type::Unset
              } else {
                rtype
              }
            };

            MorphStatus::Known(rtype)
          }
        }
      }
    };

    let status = match status {
      MorphStatus::Known(rtype) if rtype.is_depends() => {
        let mut instances = self.instances.try_lock().unwrap();
        let morph = &mut instances[inst_ind];
        morph.type_dependencies = rtype.into_depends();
        MorphStatus::Known(Type::Unset)
      }
      other => other
    };
    let new_type = status.known_type();

    // Verify that we haven't regressed.
    debug_assert!(old_type.is_none() || new_type.is_some());

    let ready_deps = {
      let mut instances = self.instances.try_lock().unwrap();
      let morph = &mut instances[inst_ind];
      morph.set_status(status);

      if old_type.is_none() && new_type.is_some() {
        let type_deps = std::mem::take(morph.type_dependents_mut());
        let build_deps = std::mem::take(morph.build_dependents_mut());
        Some((type_deps, build_deps))
      } else {
        None
      }
    };

    if let Some((type_deps, build_deps)) = ready_deps {
      for index in type_deps {
        index.replay_if_ready(globals).await;
      }
      for index in build_deps {
        index.replay_if_ready(globals).await;
      }
    }

    // Re-extract the new type, since it may have been replayed.
    let new_type = {
      let instances = self.instances.try_lock().unwrap();
      instances[inst_ind].known_type()
    };
    new_type
  }

  fn unlink_all(self: &Arc<Function<C>>, inst_ind: usize) {
    let (build_dpcs, type_dpcs) = {
      let mut instances = self.instances.try_lock().unwrap();
      let morph = &mut instances[inst_ind];
      (morph.build_dependencies.clone(), morph.type_dependencies.clone())
    };

    let my_index = MorphIndex::weak(self, inst_ind);
    for depc in build_dpcs {
      depc.remove_build_dependent(my_index.clone());
    }
    for depc in type_dpcs.index_iter() {
      depc.remove_type_dependent(my_index.clone());
    }

    {
      let mut instances = self.instances.try_lock().unwrap();
      let morph = &mut instances[inst_ind];
      morph.build_dependencies.clear();
      morph.type_dependencies = DependsOn::None;
    }
  }
}

#[derive(Clone, Debug)]
pub struct FunctionIndex {
  index: usize,
  extracts: Option<Vec<Extraction>>
}

impl FunctionIndex {
  pub fn native(index: usize) -> FunctionIndex { FunctionIndex { index, extracts: None } }
  pub fn alchem(index: usize, e: Vec<Extraction>) -> FunctionIndex { FunctionIndex { index, extracts: Some(e) } }
  pub fn empty(index: usize) -> FunctionIndex { FunctionIndex { index, extracts: Some(Vec::new()) } }

  pub fn index(&self) -> usize { self.index }
  pub fn extracts(&self) -> &Option<Vec<Extraction>> { &self.extracts }
}

pub enum FnType<C: CustomType> {
  Native(Native<C>, TypeNative<C>, TypeMatch<C>, TypeCaptured<C>, Mutex<Vec<Value<C>>>),
  Alchem(Vec<Destructure>, Vec<Token>)
}

impl<C: CustomType> FnType<C> {
  pub fn empty() -> FnType<C> { FnType::Alchem(Vec::new(), Vec::new()) }
  fn is_native(&self) -> bool { matches!(self, Self::Native(..)) }
}

pub fn default_typematch<C: CustomType>(t1: &[Type<C>], t2: &[Type<C>]) -> bool { t1 == t2 }

pub struct Monomorph<C: CustomType> {
  args: Vec<Type<C>>,
  status: MorphStatus<C>,
  build_dependencies: Vec<MorphIndex<C>>,
  type_dependencies: DependsOn<C>,
  build_dependents: Vec<MorphIndex<C>>,
  type_dependents: Vec<MorphIndex<C>>
}

impl<C: CustomType + 'static> Monomorph<C> {
  pub fn new(args: Vec<Type<C>>) -> Monomorph<C> {
    Monomorph {
      args,
      status: MorphStatus::Reserved,
      build_dependencies: Vec::new(),
      build_dependents: Vec::new(),
      type_dependencies: DependsOn::None,
      type_dependents: Vec::new()
    }
  }

  pub fn scripted(chunk: Chunk<C>, stype: Type<C>) -> Monomorph<C> {
    Monomorph {
      args: Vec::new(),
      status: MorphStatus::Completed(chunk, stype),
      build_dependencies: Vec::new(),
      type_dependencies: DependsOn::None,
      build_dependents: Vec::new(),
      type_dependents: Vec::new()
    }
  }

  pub fn into_status(self) -> MorphStatus<C> { self.status }
  pub fn is_known(&self) -> bool { self.status.is_known() }
  pub fn known_type(&self) -> Option<Type<C>> { self.status.known_type() }
  pub fn set_status(&mut self, status: MorphStatus<C>) { self.status = status; }

  pub fn type_dependents_mut(&mut self) -> &mut Vec<MorphIndex<C>> { &mut self.type_dependents }
  pub fn build_dependents_mut(&mut self) -> &mut Vec<MorphIndex<C>> { &mut self.build_dependents }
}

pub enum MorphStatus<C: CustomType> {
  Reserved,
  Known(Type<C>),
  Completed(Chunk<C>, Type<C>),
  NativeCompleted(NativeInfo<C>, Type<C>)
}

impl<C: CustomType + 'static> MorphStatus<C> {
  pub fn known_type(&self) -> Option<Type<C>> {
    match self {
      Self::Known(t) | Self::Completed(_, t) | Self::NativeCompleted(_, t) if t.is_known() => Some(t.clone()),
      _ => None
    }
  }

  pub fn needs_rebuild(&self) -> bool { !matches!(self, Self::Completed(..)) }

  pub fn is_known(&self) -> bool { self.known_type().is_some() }
}

#[derive(Clone)]
pub struct NativeInfo<C: CustomType> {
  call_indexes: Vec<FunctionIndex>,
  collapsed: Vec<CollapsedType<C>>,
  functions: Vec<Arc<Function<C>>>
}

impl<C: CustomType> Default for NativeInfo<C> {
  fn default() -> NativeInfo<C> { NativeInfo::new() }
}

impl<C: CustomType> NativeInfo<C> {
  pub fn new() -> NativeInfo<C> {
    NativeInfo { call_indexes: Vec::new(), collapsed: Vec::new(), functions: Vec::new() }
  }

  pub fn add_call_index(&mut self, ci: FunctionIndex) { self.call_indexes.push(ci); }
  pub fn add_type(&mut self, t: CollapsedType<C>) { self.collapsed.push(t); }
  pub fn add_function(&mut self, f: Arc<Function<C>>) { self.functions.push(f); }

  #[allow(clippy::type_complexity)]
  pub fn into_parts(self) -> (Vec<FunctionIndex>, Vec<CollapsedType<C>>, Vec<Arc<Function<C>>>) {
    let NativeInfo { call_indexes, collapsed, functions } = self;
    (call_indexes, collapsed, functions)
  }
}

#[derive(Clone, Debug)]
pub struct MorphIndex<C: CustomType> {
  function: Weak<Function<C>>,
  inst_index: usize
}

impl<C: CustomType> PartialEq for MorphIndex<C> {
  fn eq(&self, other: &MorphIndex<C>) -> bool {
    self.function.ptr_eq(&other.function) && self.inst_index == other.inst_index
  }
}

impl<C: CustomType> Eq for MorphIndex<C> {}

impl<C: CustomType + 'static> MorphIndex<C> {
  pub fn weak(function: &Arc<Function<C>>, inst_index: usize) -> MorphIndex<C> {
    MorphIndex { function: Arc::downgrade(function), inst_index }
  }

  pub fn exists(&self) -> bool { self.function.strong_count() > 0 }

  pub fn is_known(&self) -> bool { self.operate_morph(|morph| morph.is_known()).unwrap_or(true) }

  async fn replay_if_ready(&self, globals: &Globals<C>) -> Option<Option<Type<C>>> {
    match self.function.upgrade() {
      Some(func) => Some(func.replay_if_ready(self.inst_index, globals).await),
      None => None
    }
  }

  pub fn add_build_dependency(&self, i: MorphIndex<C>) {
    self.operate_morph(|morph| {
      if !morph.build_dependencies.contains(&i) {
        morph.build_dependencies.push(i);
      }
    });
  }

  pub fn add_build_dependent(&self, i: MorphIndex<C>) {
    self.operate_morph(|morph| {
      if !morph.build_dependents.contains(&i) {
        morph.build_dependents.push(i);
      }
    });
  }

  pub fn remove_build_dependent(&self, i: MorphIndex<C>) {
    self.operate_morph(|morph| morph.build_dependents.retain(|d| &i != d));
  }

  pub fn add_type_dependent(&self, i: MorphIndex<C>) {
    self.operate_morph(|morph| {
      if !morph.type_dependents.contains(&i) {
        morph.type_dependents.push(i);
      }
    });
  }

  pub fn remove_type_dependent(&self, i: MorphIndex<C>) {
    self.operate_morph(|morph| morph.type_dependents.retain(|d| &i != d));
  }

  fn operate_morph<T: 'static, F: FnOnce(&mut Monomorph<C>) -> T>(&self, f: F) -> Option<T> {
    self.function.upgrade().map(|func| f(&mut func.instances.try_lock().unwrap()[self.inst_index]))
  }
}

pub struct Upval {
  index: usize,
  is_local: bool
}

impl fmt::Debug for Upval {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}: {}", self.index, if self.is_local { "local" } else { "upval" })
  }
}

impl Upval {
  pub fn new(index: usize, is_local: bool) -> Upval { Upval { index, is_local } }

  pub fn index(&self) -> usize { self.index }
  pub fn is_local(&self) -> bool { self.is_local }
}

pub struct Closure<C: CustomType> {
  function: Arc<crate::collapsed::Function<C>>,
  upvalues: Mutex<Vec<ObjUpvalue<C>>>
}

impl<C: CustomType> fmt::Debug for Closure<C> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    // TODO(later): improve locked output
    let upvalues = {
      match self.upvalues.try_lock() {
        Ok(u) => format!("{:?}", u),
        Err(_) => "[in use]".to_string()
      }
    };
    write!(f, "{:?}={}", self.function, upvalues)
  }
}

impl<C: CustomType + 'static> Closure<C> {
  pub fn new(function: Arc<crate::collapsed::Function<C>>, upvalues: Vec<ObjUpvalue<C>>) -> Closure<C> {
    Closure { function, upvalues: Mutex::new(upvalues) }
  }

  pub fn arity(&self) -> u8 { self.function.arity() }
  pub fn function(&self) -> &crate::collapsed::Function<C> { &self.function }
  pub fn upvalues(&self) -> &ObjUpvalues<C> { &self.upvalues }
  pub fn chunk(&self, inst_ind: usize) -> &crate::collapsed::Chunk<C> { self.function.chunk(inst_ind) }

  pub fn flip_upval(&self, index: usize, value: Value<C>) {
    self.upvalues.try_lock().unwrap().get_mut(index).unwrap().flip(value);
  }
}

// Using "ObjUpvalue" (the runtime object) as the name here, to distinguish from "Upval" that is primarily a
// compiler concern.
pub enum ObjUpvalue<C: CustomType> {
  Open(usize),
  Closed(Value<C>) // Arc<Value> ?
}

impl<C: CustomType> fmt::Debug for ObjUpvalue<C> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Open(loc) => write!(f, "open({})", loc),
      Self::Closed(v) => write!(f, "closed({:?})", v)
    }
  }
}

impl<C: CustomType> ObjUpvalue<C> {
  pub fn new(location: usize) -> ObjUpvalue<C> { ObjUpvalue::Open(location) }

  pub fn location(&self) -> Option<usize> {
    match self {
      Self::Open(loc) => Some(*loc),
      _ => None
    }
  }

  pub fn obtain(&mut self, stack: &mut [Value<C>]) -> Value<C> {
    match self {
      Self::Open(loc) => stack[*loc].shift(),
      Self::Closed(v) => v.shift()
    }
  }

  pub fn flip(&mut self, val: Value<C>) {
    match self {
      Self::Open(_) => {
        *self = Self::Closed(val);
      }
      _ => panic!("ObjUpvalue already flipped.")
    }
  }

  pub fn shift(&mut self) -> ObjUpvalue<C> {
    match self {
      Self::Open(loc) => Self::Open(*loc),
      Self::Closed(v) => Self::Closed(v.shift()) // _ => panic!("Can't clone closed upvalue")
    }
  }
}

pub struct Chunk<C: CustomType> {
  constants: Constants<C>,
  code: Vec<Instr>
}

impl<C: CustomType> Default for Chunk<C> {
  fn default() -> Chunk<C> { Chunk::new() }
}

impl<C: CustomType> Chunk<C> {
  pub fn new() -> Chunk<C> { Chunk { constants: Constants::new(), code: Vec::new() } }
  pub fn into_collapse(self) -> (Vec<Declared<C>>, Vec<Instr>) { (self.constants.values, self.code) }

  pub fn add_instr(&mut self, instr: Instr) -> usize {
    self.code.push(instr);
    self.code.len() - 1
  }

  pub fn add_code_anon(&mut self, op: Opcode) -> usize { self.add_instr(Instr::anon(op)) }
  pub fn code_len(&self) -> usize { self.code.len() }
  pub fn code_is_empty(&self) -> bool { self.code.is_empty() }
  pub fn at(&self, ind: usize) -> Option<&Instr> { self.code.get(ind) }

  // FAST
  #[inline]
  pub fn at_fast(&self, ind: usize) -> &Instr { &self.code[ind] }

  pub fn add_value_anon(&mut self, v: Declared<C>) {
    let cc = self.add_constant(v);
    self.add_code_anon(Opcode::Constant(cc));
  }

  pub fn patch_jump(&mut self, ind: usize, val: u16) {
    match self.code.get_mut(ind).map(|instr| instr.op_mut()) {
      Some(Opcode::Jump(v)) | Some(Opcode::JumpIfFalse(v)) => {
        *v = val;
      }
      other => panic!("Illegal jump address: {:?}", other)
    }
  }

  pub fn add_constant(&mut self, cnst: Declared<C>) -> usize { self.constants.add(cnst) }
  pub fn constants_len(&self) -> usize { self.constants.len() }
  pub fn constants_is_empty(&self) -> bool { self.constants.is_empty() }
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

impl<C: CustomType> Default for Constants<C> {
  fn default() -> Constants<C> { Constants::new() }
}

impl<C: CustomType> Constants<C> {
  pub fn new() -> Constants<C> { Constants { values: Vec::new() } }
  pub fn len(&self) -> usize { self.values.len() }
  pub fn is_empty(&self) -> bool { self.values.is_empty() }
  pub fn get(&self, ind: usize) -> Option<&Declared<C>> { self.values.get(ind) }
  pub fn iter(&self) -> impl Iterator<Item = &Declared<C>> { self.values.iter() }

  pub fn add(&mut self, v: Declared<C>) -> usize {
    self.values.push(v);
    if self.len() > MAX_CONSTANTS {
      panic!("Too many constants in one chunk: {}", self.len());
    }
    self.len() - 1
  }
}

#[derive(Debug)]
pub struct Instr {
  pos: usize,
  op: Opcode
}

impl Instr {
  pub fn new(op: Opcode, pos: usize) -> Instr { Instr { pos, op } }
  pub fn anon(op: Opcode) -> Instr { Instr { pos: 0, op } }
  pub fn pos(&self) -> usize { self.pos }
  pub fn op(&self) -> &Opcode { &self.op }
  pub fn op_mut(&mut self) -> &mut Opcode { &mut self.op }
}

#[derive(Debug)]
pub enum Opcode {
  Add,
  Subtract,
  Multiply,
  Divide,
  Mod,
  And,
  Or,
  Gt,
  Gte,
  Lt,
  Lte,
  Equals,
  NotEquals,
  Constant(usize),
  Closure(usize, Vec<Upval>),
  Negate,
  Not,
  Return,
  GetLocal(usize),
  GetUpval(usize),
  GetGlobal(usize),
  Pop,
  RotateUp(usize),
  CloseUpvalue,
  JumpIfFalse(u16),
  Jump(u16),
  Call(usize, u8),
  Object(Vec<usize>),
  Array(usize),
  GetIndex(usize),
  GetJsonIndex(usize),
  GetJsonKey(String),
  Extract(Extraction)
}

impl Opcode {
  pub fn initial_jump_if_false() -> Opcode { Self::JumpIfFalse(0) }
  pub fn initial_jump() -> Opcode { Self::Jump(0) }
}

#[derive(Debug, Clone)]
pub struct Extraction {
  parts: Vec<ExtractionPart>
}

impl Extraction {
  pub fn single(part: ExtractionPart) -> Extraction { Extraction { parts: vec![part] } }
  pub fn from_parts(parts: Vec<ExtractionPart>) -> Extraction { Extraction { parts } }
  pub fn into_parts(self) -> Vec<ExtractionPart> { self.parts }
  pub fn idents_len(&self) -> usize { self.parts.len() }
  pub fn parts(&self) -> &[ExtractionPart] { &self.parts }

  pub fn extracted<C: CustomType>(&self, mut val: Value<C>) -> impl Iterator<Item = Value<C>> + '_ {
    self.parts().iter().map(move |part| {
      let mut target = &mut val;
      for ind in part.inds() {
        target = target.as_array_mut().get_mut(*ind).unwrap();
      }
      target.shift()
    })
  }
}

#[derive(Debug, Clone)]
pub struct ExtractionPart {
  inds: Vec<usize>
}

impl ExtractionPart {
  pub fn empty() -> ExtractionPart { ExtractionPart { inds: Vec::new() } }

  pub fn push(&self, i: usize) -> ExtractionPart {
    let mut v = self.inds.clone();
    v.push(i);
    ExtractionPart { inds: v }
  }

  pub fn inds(&self) -> &[usize] { &self.inds }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::types::NoCustom;

  #[test]
  fn simple() {
    let mut chunk = Chunk::<NoCustom>::new();
    assert_eq!(chunk.add_code_anon(Opcode::Return), 0);
    assert_eq!(chunk.code.len(), 1);
  }

  #[test]
  fn const_int() {
    let mut chunk = Chunk::<NoCustom>::new();
    assert_eq!(chunk.add_constant(Declared::Float(1.2)), 0);
    assert_eq!(chunk.add_code_anon(Opcode::Constant(0)), 0);
    assert_eq!(chunk.code_len(), 1);
    assert_eq!(chunk.constants_len(), 1);
  }

  #[test]
  fn constants() {
    let mut va = Constants::<NoCustom>::new();
    assert_eq!(va.add(Declared::Float(3.0)), 0);
    assert_eq!(va.values.len(), 1);
  }
}
