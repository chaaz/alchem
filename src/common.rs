//! Common info for the parser.

use crate::compiler::Compiler;
use crate::scanner::Token;
use crate::scope::ZeroMode;
use crate::types::{DependsOn, Type};
use crate::value::{Declared, Value};
use crate::vm::Runner;
use std::collections::HashMap;
use std::fmt;
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex, Weak};

const MAX_CONSTANTS: usize = 255;
const MAX_MONOMORPHS: usize = 20;

pub type Native =
  for<'r> fn(Vec<Value>, NativeInfo, &'r mut Runner) -> Pin<Box<dyn Future<Output = Value> + Send + 'r>>;
pub type TypeNative = fn(Vec<Type>, &Globals) -> MorphStatus;
pub type ObjUpvalues = Mutex<Vec<ObjUpvalue>>;
pub type Globals = HashMap<String, Arc<Function>>;

pub struct Function {
  arity: u8,
  fn_type: FnType,
  instances: Mutex<Vec<Monomorph>>,
  known_upvals: HashMap<String, (usize, Type)>
}

pub enum FnType {
  Native(Native, TypeNative),
  Alchem(Vec<String>, Vec<Token>)
}

impl fmt::Debug for Function {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let arg_n = if self.arity == 1 { "arg" } else { "args" };
    let upv_n = if self.known_upvals.len() == 1 { "capture" } else { "captures" };
    write!(f, "fn({} {}),({} {})", self.arity, arg_n, self.known_upvals.len(), upv_n)
  }
}

impl Function {
  pub fn script(chunk: Chunk, stype: Type) -> Function {
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
    arity: u8, param_names: Vec<String>, code: Vec<Token>, known_upvals: HashMap<String, (usize, Type)>
  ) -> Function {
    Function { arity, fn_type: FnType::Alchem(param_names, code), instances: Mutex::new(Vec::new()), known_upvals }
  }

  pub fn new_native(arity: u8, native: Native, type_native: TypeNative) -> Function {
    Function {
      arity,
      fn_type: FnType::Native(native, type_native),
      instances: Mutex::new(Vec::new()),
      known_upvals: HashMap::new()
    }
  }

  pub fn into_collapse(self) -> (u8, Vec<Monomorph>) { (self.arity, self.instances.into_inner().unwrap()) }
  pub fn fn_type(&self) -> &FnType { &self.fn_type }
  pub fn arity(&self) -> u8 { self.arity }
  pub fn smart_name(&self) -> String { "...".into() }

  pub fn incr_arity(&mut self) -> u8 {
    self.arity += 1;
    self.arity
  }

  pub fn find_or_build(self: &Arc<Function>, args: Vec<Type>, globals: &Globals) -> (usize, Option<Type>) {
    self.find_known_type(&args).unwrap_or_else(|| {
      let inst_ind = self.reserve_inst(args);
      (inst_ind, self.replay(inst_ind, globals))
    })
  }

  pub fn known_type(&self, inst_ind: usize) -> Option<Type> {
    let instances = self.instances.try_lock().unwrap();
    instances[inst_ind].known_type()
  }

  pub fn find_known_type(&self, args: &[Type]) -> Option<(usize, Option<Type>)> {
    let instances = self.instances.try_lock().unwrap();
    instances.iter().enumerate().find(|(_, m)| m.args == args).map(|(i, m)| (i, m.known_type()))
  }

  pub fn reserve_inst(&self, args: Vec<Type>) -> usize {
    let mut instances = self.instances.try_lock().unwrap();
    instances.push(Monomorph::new(args));
    if instances.len() > MAX_MONOMORPHS {
      panic!("Too many function instances: {}", instances.len());
    }
    instances.len() - 1
  }

  pub fn replay_if_ready(self: &Arc<Function>, inst_ind: usize, globals: &Globals) -> Option<Type> {
    let (needs_rebuild, build_deps, needs_type, type_deps) = {
      let instances = self.instances.try_lock().unwrap();
      let morph = &instances[inst_ind];

      let needs_rebuild = morph.status.needs_rebuild();
      let build_deps = if needs_rebuild { morph.build_dependencies.clone() } else { Vec::new() };

      let needs_type = !morph.status.is_known();
      let type_deps = if needs_type { morph.type_dependencies.clone() } else { DependsOn::None };

      (needs_rebuild, build_deps, needs_type, type_deps)
    };

    if (needs_rebuild && build_deps.iter().all(|d| d.is_known())) || (needs_type && type_deps.is_known()) {
      self.replay(inst_ind, globals)
    } else {
      self.known_type(inst_ind)
    }
  }

  pub fn replay(self: &Arc<Function>, inst_ind: usize, globals: &Globals) -> Option<Type> {
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
      FnType::Native(_, type_fn) => (type_fn)(args, globals),
      FnType::Alchem(pnames, code) => {
        let self_index = MorphIndex::weak(self, inst_ind);
        let code = code.clone().into_iter();
        let compiler = Compiler::replay(pnames.clone(), args, code, self.known_upvals.clone(), globals);
        let (scope_zero, rtype) = compiler.compile();

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
        let type_deps = std::mem::replace(morph.type_dependents_mut(), Vec::new());
        let build_deps = std::mem::replace(morph.build_dependents_mut(), Vec::new());
        Some((type_deps, build_deps))
      } else {
        None
      }
    };

    if let Some((type_deps, build_deps)) = ready_deps {
      for index in type_deps {
        index.replay_if_ready(globals);
      }
      for index in build_deps {
        index.replay_if_ready(globals);
      }
    }

    // Re-extract the new type, since it may have been replayed.
    let new_type = {
      let instances = self.instances.try_lock().unwrap();
      instances[inst_ind].known_type()
    };
    new_type
  }

  fn unlink_all(self: &Arc<Function>, inst_ind: usize) {
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

pub struct Monomorph {
  args: Vec<Type>,
  status: MorphStatus,
  build_dependencies: Vec<MorphIndex>,
  type_dependencies: DependsOn,
  build_dependents: Vec<MorphIndex>,
  type_dependents: Vec<MorphIndex>
}

impl Monomorph {
  pub fn new(args: Vec<Type>) -> Monomorph {
    Monomorph {
      args,
      status: MorphStatus::Reserved,
      build_dependencies: Vec::new(),
      build_dependents: Vec::new(),
      type_dependencies: DependsOn::None,
      type_dependents: Vec::new()
    }
  }

  pub fn into_status(self) -> MorphStatus { self.status }
  pub fn is_known(&self) -> bool { self.status.is_known() }
  pub fn known_type(&self) -> Option<Type> { self.status.known_type() }
  pub fn set_status(&mut self, status: MorphStatus) { self.status = status; }

  pub fn type_dependents_mut(&mut self) -> &mut Vec<MorphIndex> { &mut self.type_dependents }
  pub fn build_dependents_mut(&mut self) -> &mut Vec<MorphIndex> { &mut self.build_dependents }
}

pub enum MorphStatus {
  Reserved,
  Known(Type),
  Completed(Chunk, Type),
  NativeCompleted(NativeInfo, Type)
}

impl MorphStatus {
  pub fn known_type(&self) -> Option<Type> {
    match self {
      Self::Known(t) | Self::Completed(_, t) | Self::NativeCompleted(_, t) if t.is_known() => Some(t.clone()),
      _ => None
    }
  }

  pub fn needs_rebuild(&self) -> bool { !matches!(self, Self::Completed(..)) }

  pub fn is_known(&self) -> bool { self.known_type().is_some() }
}

#[derive(Clone)]
pub struct NativeInfo {
  call_indexes: Vec<usize>
}

impl Default for NativeInfo {
  fn default() -> NativeInfo { NativeInfo::new() }
}

impl NativeInfo {
  pub fn new() -> NativeInfo { NativeInfo { call_indexes: Vec::new() } }
  pub fn add_call_index(&mut self, ci: usize) { self.call_indexes.push(ci); }
  pub fn call_indexes(&self) -> &[usize] { &self.call_indexes }
}

#[derive(Clone, Debug)]
pub struct MorphIndex {
  function: Weak<Function>,
  inst_index: usize
}

impl PartialEq for MorphIndex {
  fn eq(&self, other: &MorphIndex) -> bool {
    self.function.ptr_eq(&other.function) && self.inst_index == other.inst_index
  }
}

impl Eq for MorphIndex {}

impl MorphIndex {
  pub fn weak(function: &Arc<Function>, inst_index: usize) -> MorphIndex {
    MorphIndex { function: Arc::downgrade(function), inst_index }
  }

  pub fn exists(&self) -> bool { self.function.strong_count() > 0 }

  pub fn is_known(&self) -> bool { self.operate_morph(|morph| morph.is_known()).unwrap_or(true) }

  fn replay_if_ready(&self, globals: &Globals) -> Option<Option<Type>> {
    self.operate_func(|func, inst_ind| func.replay_if_ready(inst_ind, globals))
  }

  pub fn add_build_dependency(&self, i: MorphIndex) {
    self.operate_morph(|morph| {
      if !morph.build_dependencies.contains(&i) {
        morph.build_dependencies.push(i);
      }
    });
  }

  pub fn add_build_dependent(&self, i: MorphIndex) {
    self.operate_morph(|morph| {
      if !morph.build_dependents.contains(&i) {
        morph.build_dependents.push(i);
      }
    });
  }

  pub fn remove_build_dependent(&self, i: MorphIndex) {
    self.operate_morph(|morph| morph.build_dependents.retain(|d| &i != d));
  }

  pub fn add_type_dependent(&self, i: MorphIndex) {
    self.operate_morph(|morph| {
      if !morph.type_dependents.contains(&i) {
        morph.type_dependents.push(i);
      }
    });
  }

  pub fn remove_type_dependent(&self, i: MorphIndex) {
    self.operate_morph(|morph| morph.type_dependents.retain(|d| &i != d));
  }

  fn operate_func<T: 'static, F: FnOnce(Arc<Function>, usize) -> T>(&self, f: F) -> Option<T> {
    self.function.upgrade().map(|func| f(func, self.inst_index))
  }

  fn operate_morph<T: 'static, F: FnOnce(&mut Monomorph) -> T>(&self, f: F) -> Option<T> {
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

pub struct Closure {
  function: Arc<crate::collapsed::Function>,
  upvalues: Mutex<Vec<ObjUpvalue>>
}

impl fmt::Debug for Closure {
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

impl Closure {
  pub fn new(function: Arc<crate::collapsed::Function>, upvalues: Vec<ObjUpvalue>) -> Closure {
    Closure { function, upvalues: Mutex::new(upvalues) }
  }

  pub fn arity(&self) -> u8 { self.function.arity() }
  pub fn function(&self) -> &crate::collapsed::Function { &self.function }
  pub fn upvalues(&self) -> &ObjUpvalues { &self.upvalues }
  pub fn chunk(&self, inst_ind: usize) -> &crate::collapsed::Chunk { self.function.chunk(inst_ind) }

  pub fn flip_upval(&self, index: usize, value: Value) {
    self.upvalues.try_lock().unwrap().get_mut(index).unwrap().flip(value);
  }
}

// Using "ObjUpvalue" (the runtime object) as the name here, to distinguish from "Upval" that is primarily a
// compiler concern.
pub enum ObjUpvalue {
  Open(usize),
  Closed(Value) // Arc<Value> ?
}

impl fmt::Debug for ObjUpvalue {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Open(loc) => write!(f, "open({})", loc),
      Self::Closed(v) => write!(f, "closed({:?})", v)
    }
  }
}

impl ObjUpvalue {
  pub fn new(location: usize) -> ObjUpvalue { ObjUpvalue::Open(location) }

  pub fn location(&self) -> usize {
    match self {
      Self::Open(loc) => *loc,
      _ => panic!("No location in closed upvalue")
    }
  }

  pub fn obtain(&mut self, stack: &mut [Value]) -> Value {
    match self {
      Self::Open(loc) => stack[*loc].shift(),
      Self::Closed(v) => v.shift()
    }
  }

  pub fn flip(&mut self, val: Value) {
    match self {
      Self::Open(_) => {
        *self = Self::Closed(val);
      }
      _ => panic!("ObjUpvalue already flipped.")
    }
  }
}

impl Clone for ObjUpvalue {
  fn clone(&self) -> ObjUpvalue {
    match self {
      Self::Open(loc) => Self::Open(*loc),
      _ => panic!("Can't clone closed upvalue")
    }
  }
}

pub struct Chunk {
  constants: Constants,
  code: Vec<Instr>
}

impl Default for Chunk {
  fn default() -> Chunk { Chunk::new() }
}

impl Chunk {
  pub fn new() -> Chunk { Chunk { constants: Constants::new(), code: Vec::new() } }
  pub fn into_collapse(self) -> (Vec<Declared>, Vec<Instr>) { (self.constants.values, self.code) }

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
  pub fn too_far(&self, ind: usize) -> bool { ind >= self.code.len() }

  // FAST
  #[inline]
  pub fn at_fast(&self, ind: usize) -> &Instr { &self.code[ind] }

  pub fn add_value_anon(&mut self, v: Declared) {
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

  pub fn add_constant(&mut self, cnst: Declared) -> usize { self.constants.add(cnst) }
  pub fn constants_len(&self) -> usize { self.constants.len() }
  pub fn constants_is_empty(&self) -> bool { self.constants.is_empty() }
  pub fn get_constant(&self, ind: usize) -> Option<&Declared> { self.constants.get(ind) }

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

struct Constants {
  values: Vec<Declared>
}

impl Default for Constants {
  fn default() -> Constants { Constants::new() }
}

impl Constants {
  pub fn new() -> Constants { Constants { values: Vec::new() } }
  pub fn len(&self) -> usize { self.values.len() }
  pub fn is_empty(&self) -> bool { self.values.is_empty() }
  pub fn get(&self, ind: usize) -> Option<&Declared> { self.values.get(ind) }
  pub fn iter(&self) -> impl Iterator<Item = &Declared> { self.values.iter() }

  pub fn add(&mut self, v: Declared) -> usize {
    self.values.push(v);
    if self.len() > MAX_CONSTANTS {
      panic!("Too many constants in one chunk: {}", self.len());
    }
    self.len() - 1
  }
}

#[derive(Debug)]
pub struct Instr {
  loc: usize,
  op: Opcode
}

impl Instr {
  pub fn new(op: Opcode, loc: usize) -> Instr { Instr { loc, op } }
  pub fn anon(op: Opcode) -> Instr { Instr { loc: 0, op } }
  pub fn loc(&self) -> usize { self.loc }
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
  GetIndex(usize)
}

impl Opcode {
  pub fn initial_jump_if_false() -> Opcode { Self::JumpIfFalse(0) }
  pub fn initial_jump() -> Opcode { Self::Jump(0) }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn simple() {
    let mut chunk = Chunk::new();
    assert_eq!(chunk.add_code_anon(Opcode::Return), 0);
    assert_eq!(chunk.code.len(), 1);
  }

  #[test]
  fn const_int() {
    let mut chunk = Chunk::new();
    assert_eq!(chunk.add_constant(Declared::Float(1.2)), 0);
    assert_eq!(chunk.add_code_anon(Opcode::Constant(0)), 0);
    assert_eq!(chunk.code_len(), 1);
    assert_eq!(chunk.constants_len(), 1);
  }

  #[test]
  fn constants() {
    let mut va = Constants::new();
    assert_eq!(va.add(Declared::Float(3.0)), 0);
    assert_eq!(va.values.len(), 1);
  }
}
