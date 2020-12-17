//! The actual VM for parsing the bytecode.

use crate::compiler::compile;
use crate::common::{Chunk, Opcode, Instr, Closure, Native, ObjUpvalue};
use crate::value::Value;
use crate::errors::Result;
use std::sync::Arc;
use std::cmp::{min, max};
use std::collections::HashMap;

const FRAMES_MAX: usize = 255;

type Stack = Vec<Value>;
type CallStack = Vec<CallFrame>;
type Globals = HashMap<String, Value>;
type ObjUpvalues = [ObjUpvalue];

pub struct Vm {
  debug: bool,
  call_stack: CallStack,
  stack: Stack,
  globals: Globals
}

impl Default for Vm {
  fn default() -> Vm { Vm::new() }
}

impl Vm {
  pub fn new() -> Vm { Vm { debug: true, call_stack: Vec::new(), stack: Vec::new(), globals: HashMap::new() } }

  pub fn add_native(&mut self, name: impl ToString, native: Native) {
    self.globals.insert(name.to_string(), Value::Native(native));
  }

  pub fn interpret(&mut self, source: &str) -> Result<Value> {
    debug_assert_eq!(self.stack.len(), 0);
    debug_assert_eq!(self.call_stack.len(), 0);

    // last minute code changes
    let function = compile(source)?;

    // lock it in
    let function = Arc::new(function);
    let closure = Arc::new(Closure::new(function, Vec::new()));

    self.stack.push(closure.clone().into());
    call(closure, 0, self.stack.len())?.perform(&mut self.call_stack);

    self.run()
  }

  pub fn run(&mut self) -> Result<Value> {
    let r = self.try_run();
    if r.is_err() {
      self.debug_stack_trace();
    }
    r
  }

  pub fn try_run(&mut self) -> Result<Value> {
    let Vm { debug, call_stack, stack, globals } = self;

    debug_start(*debug);

    'outer: loop {
      let frame = call_stack.last_mut().unwrap();
      let (ip, upvals, chunk, slots) = frame.parts();

      loop {
        match chunk.at(*ip) {
          None => break 'outer,
          Some(instr) => {
            debug_instr(*debug, stack, *ip, instr);
            *ip += 1;
            if let Some(stack_op) = handle_op(instr, globals, upvals, chunk, slots, stack, ip)? {
              stack_op.perform(call_stack);
              if call_stack.len() > FRAMES_MAX {
                bail!(Runtime, "Stack overflow: {}.", call_stack.len());
              }
              if call_stack.is_empty() {
                debug_end(*debug, stack);
                return Ok(pop(stack)?);
              }
              break;
            }
          }
        }
      }
    }
    debug_end(*debug, stack);
    bail!(Runtime, "Missing return.");
  }

  fn debug_stack_trace(&self) {
    println!("\nRuntime error:");
    for frame in self.call_stack.iter().rev() {
      let closure = frame.closure();
      let ip = max(1, min(closure.chunk().code_len(), frame.ip()));
      let instr = closure.chunk().at(ip - 1).unwrap();
      println!("  at {} in {}: {:?}", instr.loc(), closure.smart_name(), instr);
    }
  }
}

fn debug_start(debug: bool) {
  if debug {
    println!();
  }
}

fn debug_instr(debug: bool, stack: &[Value], ip: usize, instr: &Instr) {
  if debug {
    println!("Stack: ");
    for (i, v) in stack.iter().enumerate().rev() {
      println!("  {:>0width$} : {:?}", i, v, width=4);
    }
    println!("Executing {:>04}: {:?}", ip, instr);
  }
}

fn debug_end(debug: bool, stack: &[Value]) {
  if debug {
    println!("Final Stack: ");
    for (i, v) in stack.iter().enumerate().rev() {
      println!("  {:>0width$} : {:?}", i, v, width=4);
    }
  }
}

fn pop(stack: &mut Stack) -> Result<Value> {
  stack.pop().ok_or_else(|| bad!(Compile, "No stack."))
}

fn peek(stack: &mut Stack) -> Result<&Value> {
  stack.last().ok_or_else(|| bad!(Compile, "No stack."))
}

fn rpeek(stack: &mut Stack, back: usize) -> Result<&Value> {
  if stack.len() > back {
    Ok(stack.get(stack.len() - 1 - back).unwrap())
  } else {
    err!(Compile, "Not enough stack.")
  }
}

fn handle_op(
  instr: &Instr, globals: &Globals, upvalues: &ObjUpvalues, chunk: &Chunk, slots: &usize, stack: &mut Stack,
  ip: &mut usize
) -> Result<Option<StackOp>> {
  match instr.op() {
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
    Opcode::GetLocal(l) => stack.push(stack[*l + *slots].try_clone()?),
    Opcode::GetGlobal(l) => {
      let name = chunk.get_constant(*l).unwrap().as_str().unwrap();
      stack.push(globals.get(name).ok_or_else(|| bad!(Runtime, "No such variable \"{}\".", name))?.try_clone()?);
    }
    Opcode::GetUpval(u) => stack.push(stack[upvalues[*u].location()].try_clone()?),
    Opcode::Jump(offset) => *ip += *offset as usize,
    Opcode::JumpIfFalse(offset) => {
      if !peek(stack)?.try_bool()? {
        *ip += *offset as usize
      }
    }
    Opcode::Pop => drop(stack.pop()),
    Opcode::Popout(c) => {
      if *c > 0 {
        stack.swap_remove(stack.len() - c - 1);
        stack.truncate(stack.len() - c + 1);
      }
    }
    Opcode::Call(argc) => return call_value(stack, *argc),
    Opcode::Return => {
      stack.swap_remove(*slots);
      stack.truncate(slots + 1);
      return Ok(Some(StackOp::Pop));
    }
    Opcode::Closure(c, upvals) => {
      let val = chunk.get_constant(*c).unwrap();

      let new_upvalues = upvals.iter().map(|v| {
        if v.is_local() {
          capture_upvalue(*slots + v.index())
        } else {
          upvalues[v.index()].clone()
        }
      }).collect();

      let closure = Closure::new(val.try_function().unwrap(), new_upvalues);
      stack.push(closure.into());
    }
  }

  Ok(None)
}

fn capture_upvalue(i: usize) -> ObjUpvalue { ObjUpvalue::new(i) }

enum StackOp {
  Push(CallFrame),
  Pop,
}

impl StackOp {
  pub fn perform(self, call_stack: &mut CallStack) {
    match self {
      Self::Push(f) => call_stack.push(f),
      Self::Pop => drop(call_stack.pop())
    }
  }
}

fn call_value(stack: &mut Stack, argc: u8) -> Result<Option<StackOp>> {
  let value = rpeek(stack, argc as usize)?;
  match value {
    Value::Closure(f) => Ok(Some(call(f.clone(), argc, stack.len())?)),
    Value::Native(f) => call_native(*f, argc, stack),
    other => err!(Runtime, "Not a function: {:?}", other)
  }
}

fn call(closure: Arc<Closure>, argc: u8, stack_len: usize) -> Result<StackOp> {
  if argc != closure.arity() {
    bail!(Runtime, "Calling arity {} with {} args.", closure.arity(), argc);
  }
  Ok(StackOp::Push(CallFrame::new(closure, stack_len - (argc as usize) - 1)))
}

fn call_native(native: Native, argc: u8, stack: &mut Stack) -> Result<Option<StackOp>> {
  let argc = argc as usize;
  let result = (native)(&stack[stack.len() - argc ..])?;
  let rem = stack.len() - argc;
  stack.truncate(rem);
  *stack.get_mut(rem - 1).unwrap() = result;
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

pub struct CallFrame {
  closure: Arc<Closure>,
  ip: usize,
  slots: usize
}

impl CallFrame {
  pub fn new(closure: Arc<Closure>, slots: usize) -> CallFrame {
    CallFrame { closure, ip: 0, slots }
  }

  pub fn closure(&self) -> &Closure { &self.closure }
  pub fn ip(&self) -> usize { self.ip }
  pub fn ip_mut(&mut self) -> &mut usize { &mut self.ip }
  pub fn slots(&self) -> usize { self.slots }
  pub fn slots_mut(&mut self) -> &mut usize { &mut self.slots }
  pub fn parts(&mut self) -> (&mut usize, &ObjUpvalues, &Chunk, &usize) {
    (&mut self.ip, self.closure.upvalues(), self.closure.chunk(), &self.slots)
  }
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
