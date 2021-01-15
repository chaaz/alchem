//! Scope and emit rules for the compiler.

use crate::common::{Chunk, Function, Instr, MorphIndex, Opcode, Upval};
use crate::scanner::Token;
use crate::types::Type;
use crate::value::Declared;
use std::collections::HashMap;
use std::sync::Arc;

const MAX_LOCALS: usize = 255;
pub type Jump = usize;

pub struct Collector {
  collecting: bool,
  skipped: bool,
  tokens: Vec<Token>
}

impl Collector {
  pub fn new() -> Collector { Collector { collecting: false, skipped: false, tokens: Vec::new() } }

  pub fn start(&mut self) {
    self.tokens.clear();
    self.collecting = true;
    self.skipped = false;
  }

  pub fn collect(&mut self, token: Token) {
    // Skip the first token, which will be the open curly.
    if self.collecting {
      if self.skipped {
        self.tokens.push(token);
      } else {
        self.skipped = true;
      }
    }
  }

  pub fn report(&mut self) -> Vec<Token> {
    self.collecting = false;
    self.skipped = false;
    std::mem::replace(&mut self.tokens, Vec::new())
  }
}

pub struct ScopeStack {
  zero: ScopeZero,
  one: Option<ScopeOne>,
  later: Vec<ScopeLater>,
  last_line: usize,
  collector: Collector
}

impl ScopeStack {
  pub fn new() -> ScopeStack {
    ScopeStack {
      zero: ScopeZero::known(HashMap::new()),
      one: None,
      later: Vec::new(),
      last_line: 0,
      collector: Collector::new()
    }
  }

  pub fn known(known_upvals: HashMap<String, (usize, Type)>) -> ScopeStack {
    ScopeStack {
      zero: ScopeZero::known(known_upvals),
      one: None,
      later: Vec::new(),
      last_line: 0,
      collector: Collector::new()
    }
  }

  pub fn link_build_depends(&mut self, index: &MorphIndex) { self.zero.link_build_depends(index); }
  pub fn collect(&mut self, token: Token) { self.collector.collect(token); }

  pub fn len(&self) -> usize {
    if self.one.is_some() {
      self.later.len() + 2
    } else {
      1
    }
  }

  pub fn last_line(&self) -> usize { self.last_line }
  pub fn set_last_line(&mut self, last_line: usize) { self.last_line = last_line; }

  pub fn add_constant(&mut self, v: Declared) -> usize {
    if let Some(chunk) = self.current_chunk() {
      chunk.add_constant(v)
    } else {
      0
    }
  }

  pub fn emit_instr(&mut self, code: Opcode) {
    let line = self.last_line;
    if let Some(chunk) = self.current_chunk() {
      chunk.add_instr(Instr::new(code, line));
    }
  }

  pub fn emit_value(&mut self, v: Declared, vtype: Type) -> Type {
    let line = self.last_line;
    if let Some(chunk) = self.current_chunk() {
      let cc = chunk.add_constant(v);
      chunk.add_instr(Instr::new(Opcode::Constant(cc), line));
      vtype
    } else {
      vtype
    }
  }

  pub fn emit_closure(&mut self, upvals: Vec<Upval>, function: Function) -> Arc<Function> {
    let line = self.last_line;
    if let Some(chunk) = self.current_chunk() {
      let function = Arc::new(function);
      let cc = chunk.add_constant(function.clone().into());
      chunk.add_instr(Instr::new(Opcode::Closure(cc, upvals), line));
      function
    } else {
      panic!("Can't emit closure in nested scope.");
    }
  }

  pub fn emit_jump(&mut self, code: Opcode) -> Jump {
    let line = self.last_line;
    if let Some(chunk) = self.current_chunk() {
      chunk.add_instr(Instr::new(code, line));
      chunk.code_len() - 1
    } else {
      // Just use a fake value
      0
    }
  }

  pub fn patch_jump(&mut self, offset: Jump) {
    if let Some(chunk) = self.current_chunk() {
      let jump = (chunk.code_len() - offset - 1) as u16;
      chunk.patch_jump(offset, jump);
    }
  }

  fn current_chunk(&mut self) -> Option<&mut Chunk> {
    if self.len() == 1 {
      if let ZeroMode::Emitting(chunk) = &mut self.zero.mode {
        Some(chunk)
      } else {
        None
      }
    } else {
      None
    }
  }

  pub fn push_scope(&mut self) {
    if self.one.is_none() {
      self.one = Some(ScopeOne::new());
    } else {
      self.later.push(ScopeLater::new());
    }
  }

  pub fn start_collecting(&mut self) {
    if self.len() == 2 {
      self.collector.start();
    }
  }

  pub fn pop_scope_later(&mut self) -> ScopeLater { self.later.pop().unwrap() }

  #[allow(clippy::let_and_return)]
  pub fn pop_scope_one(&mut self) -> (ScopeOne, Vec<Token>) {
    if !self.later.is_empty() {
      panic!("Can't pop from scope > 2");
    } else if self.one.is_some() {
      let tokens = self.collector.report();
      let scope = self.one.take().unwrap();
      (scope, tokens)
    } else {
      panic!("Can't pop from scope == 1");
    }

    // TODO(later): print code during monomorph
    //
    // let scope = self.scope.pop().unwrap();
    // #[cfg(feature = "verbose")]
    // if !self.had_error && self.scope.is_empty() {
    //   println!("\nCompiled code ({}):", scope.function().smart_name());
    //   scope.function().chunk().debug();
    // }
  }

  pub fn pop_scope_zero(self) -> ScopeZero { self.zero }

  pub fn add_local(&mut self, name: String) { self.locals_mut().add_local(Local::new(name)); }

  pub fn resolve_local_at(&self, scope_ind: usize, name: &str) -> Option<(usize, Type)> {
    self.locals_at(scope_ind).resolve_local(name)
  }

  pub fn resolve_local(&self, name: &str) -> Option<(usize, Type)> { self.locals().resolve_local(name) }

  pub fn mark_initialized(&mut self, vtype: Type) { self.locals_mut().mark_last_initialized(vtype); }

  fn set_captured_at(&mut self, scope_ind: usize, locals_ind: usize, captured: bool) {
    self.locals_at_mut(scope_ind).set_captured(locals_ind, captured);
  }

  pub fn resolve_upval(&mut self, name: &str) -> Option<(usize, Type)> {
    let scope_ind = self.len() - 1;
    self.resolve_upval_recurse(name, scope_ind)
  }

  fn resolve_upval_recurse(&mut self, name: &str, scope_ind: usize) -> Option<(usize, Type)> {
    if scope_ind == 0 {
      return self.zero.pre_found_upval(name).cloned();
    }

    let prev_local = self.resolve_local_at(scope_ind - 1, name);
    if let Some((i, vtype)) = prev_local {
      self.set_captured_at(scope_ind - 1, i, true);
      let upval_ind =
        if scope_ind == 1 { self.one.as_mut().unwrap().add_upval(name, i, true, vtype.clone()) } else { 0 };
      return Some((upval_ind, vtype));
    }

    let prev_upval = self.resolve_upval_recurse(name, scope_ind - 1);
    if let Some((i, vtype)) = prev_upval {
      let upval_ind =
        if scope_ind == 1 { self.one.as_mut().unwrap().add_upval(name, i, false, vtype.clone()) } else { 0 };
      return Some((upval_ind, vtype));
    }

    None
  }

  pub fn begin_scope(&mut self) { self.locals_mut().incr_depth(); }

  pub fn end_scope(&mut self) {
    self.locals_mut().decr_depth();

    if self.len() > 1 {
      self.locals_mut().truncate();
    } else {
      let drained = self.zero.locals_mut().drain();

      self.emit_instr(Opcode::RotateUp(drained.len() + 1));
      for val in drained.into_iter().rev() {
        if val.is_captured() {
          self.emit_instr(Opcode::CloseUpvalue);
        } else {
          self.emit_instr(Opcode::Pop);
        }
      }
    }
  }

  fn locals_at_mut(&mut self, scope_ind: usize) -> &mut Locals {
    if scope_ind == 0 {
      self.zero.locals_mut()
    } else if scope_ind == 1 {
      self.one.as_mut().unwrap().locals_mut()
    } else {
      self.later[scope_ind - 2].locals_mut()
    }
  }

  fn locals_at(&self, scope_ind: usize) -> &Locals {
    if scope_ind == 0 {
      self.zero.locals()
    } else if scope_ind == 1 {
      self.one.as_ref().unwrap().locals()
    } else {
      self.later[scope_ind - 2].locals()
    }
  }

  fn locals_mut(&mut self) -> &mut Locals { self.locals_at_mut(self.len() - 1) }
  fn locals(&self) -> &Locals { self.locals_at(self.len() - 1) }
}

pub struct ScopeZero {
  mode: ZeroMode,

  // read only
  known_upvals: HashMap<String, (usize, Type)>,

  // common
  locals: Locals
}

impl ScopeZero {
  pub fn known(known_upvals: HashMap<String, (usize, Type)>) -> ScopeZero {
    ScopeZero { mode: ZeroMode::Emitting(Chunk::new()), known_upvals, locals: Locals::new() }
  }

  fn locals(&self) -> &Locals { &self.locals }
  fn locals_mut(&mut self) -> &mut Locals { &mut self.locals }

  pub fn pre_found_upval(&self, name: &str) -> Option<&(usize, Type)> { self.known_upvals.get(name) }

  fn flip_depends(&mut self) -> &mut Vec<MorphIndex> {
    if matches!(self.mode, ZeroMode::Emitting(_)) {
      self.mode = ZeroMode::Dependent(Vec::new());
    }

    match &mut self.mode {
      ZeroMode::Dependent(v) => v,
      _ => panic!("Somehow didn't flip.")
    }
  }

  pub fn link_build_depends(&mut self, index: &MorphIndex) {
    let depends = self.flip_depends();
    if !depends.contains(index) {
      depends.push(index.clone())
    }
  }

  pub fn into_mode(self) -> ZeroMode { self.mode }

  pub fn into_chunk(self) -> Chunk {
    match self.mode {
      ZeroMode::Emitting(chunk) => chunk,
      _ => panic!("Scope has dependencies.")
    }
  }
}

pub enum ZeroMode {
  Emitting(Chunk),
  Dependent(Vec<MorphIndex>)
}

pub struct ScopeOne {
  // by end of level 0 fn_sync, have captured `arity`, `code`, `name`, `upvals`
  // Function:
  //   arity: u8,
  //   code: Vec<Instr>,
  //   instances: Vec<(Vec<Type>, Chunk)>,
  //   name: Option<String>,
  //   upvals: HashMap<String, (usize, Type)>

  // by end of level 0 fn_sync, have captured `upvals`
  upvals: Vec<Upval>,

  // write only
  known_upvals: HashMap<String, (usize, Type)>,

  // common
  locals: Locals
}

impl ScopeOne {
  pub fn new() -> ScopeOne { ScopeOne { upvals: Vec::new(), known_upvals: HashMap::new(), locals: Locals::new() } }
  fn locals(&self) -> &Locals { &self.locals }
  fn locals_mut(&mut self) -> &mut Locals { &mut self.locals }

  pub fn add_upval(&mut self, name: impl ToString, index: usize, is_local: bool, utype: Type) -> usize {
    match self.upvals.iter().position(|v| v.index() == index && v.is_local() == is_local) {
      Some(p) => p,
      None => self.push_upval(name.to_string(), Upval::new(index, is_local), utype)
    }
  }

  fn push_upval(&mut self, name: String, upval: Upval, utype: Type) -> usize {
    self.upvals.push(upval);
    let ind = self.upvals.len() - 1;
    self.known_upvals.insert(name, (ind, utype));
    ind
  }

  pub fn into_upvals(self) -> (Vec<Upval>, HashMap<String, (usize, Type)>) { (self.upvals, self.known_upvals) }
}

pub struct ScopeLater {
  locals: Locals
}

impl ScopeLater {
  pub fn new() -> ScopeLater { ScopeLater { locals: Locals::new() } }
  fn locals(&self) -> &Locals { &self.locals }
  fn locals_mut(&mut self) -> &mut Locals { &mut self.locals }
}

struct Locals {
  locals: Vec<Local>,
  scope_depth: u16
}

impl Locals {
  pub fn new() -> Locals { Locals { locals: vec![Local::new(String::new())], scope_depth: 0 } }

  pub fn incr_depth(&mut self) { self.scope_depth += 1; }
  pub fn decr_depth(&mut self) { self.scope_depth -= 1; }

  pub fn truncate(&mut self) {
    if let Some(p) = self.locals.iter().position(|l| l.depth() > self.scope_depth) {
      self.locals.truncate(p);
    }
  }

  pub fn drain(&mut self) -> Vec<Local> {
    if let Some(p) = self.locals.iter().position(|l| l.depth() > self.scope_depth) {
      self.locals.split_off(p)
    } else {
      Vec::new()
    }
  }

  pub fn add_local(&mut self, local: Local) {
    if self.is_defined(local.name()) {
      panic!("Already defined local variable \"{}\".", local.name());
    }

    if self.locals.len() >= MAX_LOCALS {
      panic!("Too many locals: {}", self.locals.len());
    }

    self.locals.push(local);
  }

  fn is_defined(&self, name: &str) -> bool {
    self.locals.iter().rev().take_while(|l| l.depth() >= self.scope_depth).any(|l| l.name() == name)
  }

  pub fn resolve_local(&self, name: &str) -> Option<(usize, Type)> {
    let i = self.locals.iter().enumerate().rev().find(|(_, l)| l.name() == name);
    match i {
      None => None,
      Some((i, local)) => {
        if self.initialized(i) {
          Some((i, local.local_type()))
        } else {
          panic!("Can't read local \"{}\" in its own initializer.", name)
        }
      }
    }
  }

  pub fn mark_last_initialized(&mut self, utype: Type) {
    let local = self.locals.last_mut().unwrap();
    local.depth = self.scope_depth;
    local.local_type = utype;
  }

  pub fn initialized(&self, index: usize) -> bool {
    let local = &self.locals[index];
    local.depth > 0
  }

  pub fn set_captured(&mut self, locals_ind: usize, captured: bool) { self.locals[locals_ind].set_captured(captured); }
}

#[derive(Debug)]
struct Local {
  name: String,
  depth: u16,
  is_captured: bool,
  local_type: Type
}

impl Local {
  pub fn new(name: String) -> Local { Local { name, depth: 0, is_captured: false, local_type: Type::Unset } }
  pub fn name(&self) -> &str { &self.name }
  pub fn depth(&self) -> u16 { self.depth }
  pub fn is_captured(&self) -> bool { self.is_captured }
  pub fn set_captured(&mut self, cap: bool) { self.is_captured = cap }
  pub fn local_type(&self) -> Type { self.local_type.clone() }
}
