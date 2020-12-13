//! The actual VM for parsing the bytecode.

use crate::compiler::compile;
use crate::common::{Chunk, Opcode, Instr};
use crate::value::Value;
use crate::errors::{Result, ErrorKind};

type Stack = Vec<Value>;

pub fn interpret(source: &str) -> Result<Value> {
  execute(compile(source)?)
}

pub fn execute(chunk: Chunk) -> Result<Value> {
  let mut vm = Vm::init(chunk);
  vm.run()
}

pub struct Vm {
  debug: bool,
  chunk: Chunk,
  ip: usize,
  stack: Stack
}

impl Vm {
  pub fn init(chunk: Chunk) -> Vm { Vm { debug: true, ip: 0, chunk, stack: Vec::new() } }

  pub fn run(&mut self) -> Result<Value> {
    while let Some(instr) = self.chunk.at(self.ip) {
      if self.debug {
        println!("Stack: ");
        for (i, v) in self.stack.iter().enumerate().rev() {
          println!("  {:>0width$} : {:?}", i, v, width=4);
        }
        println!("Executing {:>04}: {:?}", self.ip, instr);
      }

      self.ip += 1;
      if let Some(r) = handle_op(instr, &self.chunk, &mut self.stack, &mut self.ip)? {
        if self.debug {
          println!()
        }
        return Ok(r);
      }
    }

    err!(Compile, "No return.")
  }
}

fn pop(stack: &mut Stack) -> Result<Value> {
  stack.pop().ok_or_else(|| bad!(Compile, "No stack."))
}

fn handle_op(instr: &Instr, chunk: &Chunk, stack: &mut Stack, ip: &mut usize) -> Result<Option<Value>> {
  let r = try_handle_op(instr, chunk, stack, ip);
  if matches!(&r, Err(e) if matches!(e.kind(), ErrorKind::Runtime(..))) {
    println!("Error at {}: {}", instr.loc(), r.as_ref().unwrap_err());
  }
  r
}

fn try_handle_op(instr: &Instr, chunk: &Chunk, stack: &mut Stack, _ip: &mut usize) -> Result<Option<Value>> {
  match instr.op() {
    Opcode::Return => return Ok(Some(pop(stack)?)),

    Opcode::Constant(c) => {
      let lit = chunk.get_constant(*c).unwrap();
      stack.push(lit.try_clone()?);
    }

    Opcode::Not => unary(stack, |v| v.try_not())?,
    Opcode::Negate => unary(stack, |v| v.try_negate())?,
    Opcode::Add => binary(stack, |v, w| v.try_add(w))?,
    Opcode::Subtract => binary(stack, |v, w| v.try_subtract(w))?,
    Opcode::Multiply => binary(stack, |v, w| v.try_multiply(w))?,
    Opcode::Divide => binary(stack, |v, w| v.try_divide(w))?,
    Opcode::Mod => binary(stack, |v, w| v.try_mod(w))?,
    Opcode::And => binary(stack, |v, w| v.try_and(w))?,
    Opcode::Or => binary(stack, |v, w| v.try_or(w))?,
    Opcode::Gt => binary(stack, |v, w| v.try_gt(w))?,
    Opcode::Gte => binary(stack, |v, w| v.try_gte(w))?,
    Opcode::Lt => binary(stack, |v, w| v.try_lt(w))?,
    Opcode::Lte => binary(stack, |v, w| v.try_lte(w))?,
    Opcode::Equals => binary(stack, |v, w| v.try_eq(w))?,
    Opcode::NotEquals => binary(stack, |v, w| v.try_neq(w))?,
  }

  Ok(None)
}

fn unary<F: FnOnce(Value) -> Result<Value>>(stack: &mut Stack, f: F) -> Result<()> {
  let v1 = pop(stack)?;
  stack.push(f(v1)?);
  Ok(())
}

fn binary<F: FnOnce(Value, Value) -> Result<Value>>(stack: &mut Stack, f: F) -> Result<()> {
  let v1 = pop(stack)?;
  let v2 = pop(stack)?;
  stack.push(f(v2, v1)?);
  Ok(())
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::value::Value;

  #[test]
  fn exc_simple() {
    let mut chunk = Chunk::new();
    let c = chunk.add_constant(Value::Float(1.2));
    chunk.add_code(Opcode::Constant(c));
    chunk.add_code(Opcode::Return);
    assert_eq!(execute(chunk).unwrap(), Value::Float(1.2));
  }

  #[test]
  fn exc_negate() {
    let mut chunk = Chunk::new();
    let c = chunk.add_constant(Value::Float(1.2));
    chunk.add_code(Opcode::Constant(c));
    chunk.add_code(Opcode::Negate);
    chunk.add_code(Opcode::Return);
    assert_eq!(execute(chunk).unwrap(), Value::Float(-1.2));
  }

  #[test]
  fn exc_binary_ops() {
    let mut chunk = Chunk::new();
    let c1 = chunk.add_constant(Value::Float(1.2));
    let c2 = chunk.add_constant(Value::Float(2.1));
    let c3 = chunk.add_constant(Value::Float(3.0));
    chunk.add_code(Opcode::Constant(c1));
    chunk.add_code(Opcode::Constant(c2));
    chunk.add_code(Opcode::Add);
    chunk.add_code(Opcode::Constant(c3));
    chunk.add_code(Opcode::Divide);
    chunk.add_code(Opcode::Return);
    assert_eq!(execute(chunk).unwrap(), Value::Float(3.3 / 3.0));
  }

  #[test]
  fn itp_simple() {
    let code = "return 0";
    assert_eq!(interpret(code).unwrap(), Value::Float(0.0));
  }
}
