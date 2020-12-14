//! Common info for the parser.

use super::value::{Value, ValueArray};

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

  pub fn add_code(&mut self, op: Opcode) -> usize { self.add_instr(Instr::anon(op)) }
  pub fn code_len(&self) -> usize { self.code.len() }
  pub fn code_is_empty(&self) -> bool { self.code.is_empty() }
  pub fn at(&self, ind: usize) -> Option<&Instr> { self.code.get(ind) }

  pub fn constants(&self) -> &ValueArray { &self.constants }
  pub fn add_constant(&mut self, cnst: Value) -> usize { self.constants.add(cnst) }
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
    println!()
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
  Negate,
  Not,
  Return,
  DefineGlobal(usize),
  GetGlobal(usize),
  GetLocal(usize),
  Popout(usize)
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
