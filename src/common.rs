//! Common info for the parser.

use super::value::{Value, ValueArray};
use super::errors::Result;
use std::fmt;
use std::sync::Arc;

pub type Native = fn(&[Value]) -> Result<Value>;

pub struct Function {
  arity: u8,
  chunk: Chunk,
  name: Option<String>
}

impl fmt::Debug for Function {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "fn {}({} args) {{ ... }}", self.smart_name(), self.arity)
  }
}

impl Default for Function {
  fn default() -> Function { Function::new() }
}

impl Function {
  pub fn new() -> Function { Function { arity: 0, chunk: Chunk::new(), name: None } }
  pub fn arity(&self) -> u8 { self.arity }
  pub fn chunk(&self) -> &Chunk { &self.chunk }
  pub fn chunk_mut(&mut self) -> &mut Chunk { &mut self.chunk }
  pub fn into_chunk(self) -> Chunk { self.chunk }
  pub fn name(&self) -> &Option<String> { &self.name }

  pub fn smart_name(&self) -> String {
    match &self.name {
      Some(n) => n.clone(),
      None => "...".into()
    }
  }

  pub fn incr_arity(&mut self) -> u8 {
    self.arity += 1;
    self.arity
  }
}

pub struct Closure {
  function: Arc<Function>,
  captures: Vec<Value>
}

impl fmt::Debug for Closure {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}+{:?}", self.function, self.captures)
  }
}

impl Closure {
  pub fn new(function: Arc<Function>) -> Closure { Closure { function, captures: Vec::new() } }
  pub fn arity(&self) -> u8 { self.function.arity() }
  pub fn chunk(&self) -> &Chunk { self.function.chunk() }
  pub fn name(&self) -> &Option<String> { self.function.name() }
  pub fn smart_name(&self) -> String { self.function.smart_name() }
  pub fn function(&self) -> &Function { &self.function }
}

pub struct Chunk {
  constants: ValueArray,
  code: Vec<Instr>
}

impl Default for Chunk {
  fn default() -> Chunk { Chunk::new() }
}

impl Chunk {
  pub fn new() -> Chunk { Chunk { constants: ValueArray::new(), code: Vec::new() } }

  pub fn add_instr(&mut self, instr: Instr) -> usize {
    self.code.push(instr);
    self.code.len() - 1
  }

  pub fn add_code_anon(&mut self, op: Opcode) -> usize { self.add_instr(Instr::anon(op)) }
  pub fn code_len(&self) -> usize { self.code.len() }
  pub fn code_is_empty(&self) -> bool { self.code.is_empty() }
  pub fn at(&self, ind: usize) -> Option<&Instr> { self.code.get(ind) }

  pub fn add_value_anon(&mut self, v: Value) -> Result<()> {
    let cc = self.add_constant(v)?;
    self.add_code_anon(Opcode::Constant(cc));
    Ok(())
  }

  pub fn patch_jump(&mut self, ind: usize, val: u16) -> Result<()> {
    match self.code.get_mut(ind).map(|instr| instr.op_mut()) {
      Some(Opcode::Jump(v)) | Some(Opcode::JumpIfFalse(v)) => {
        *v = val;
        Ok(())
      }
      other => err!(Compile, "Illegal jump address: {:?}", other)
    }
  }

  pub fn constants(&self) -> &ValueArray { &self.constants }
  pub fn add_constant(&mut self, cnst: Value) -> Result<usize> { self.constants.add(cnst) }
  pub fn constants_len(&self) -> usize { self.constants.len() }
  pub fn constants_is_empty(&self) -> bool { self.constants.is_empty() }
  pub fn get_constant(&self, ind: usize) -> Option<&Value> { self.constants.get(ind) }

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

// TODO: force into u8
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
  Closure(usize),
  Negate,
  Not,
  Return,
  GetLocal(usize),
  GetGlobal(usize),
  Pop,
  Popout(usize),
  JumpIfFalse(u16),
  Jump(u16),
  Call(u8)
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
    assert_eq!(chunk.add_code(Opcode::Return), 0);
    assert_eq!(chunk.code.len(), 1);
  }

  #[test]
  fn const_int() {
    let mut chunk = Chunk::new();
    assert_eq!(chunk.add_constant(Value::Double(1.2)), 0);
    assert_eq!(chunk.add_code(Opcode::Constant(0)), 0);
    assert_eq!(chunk.code_len(), 1);
    assert_eq!(chunk.constants_len(), 1);
  }
}
