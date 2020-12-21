//! The actual VM for parsing the bytecode.

use crate::common::{Chunk, Closure, Instr, Native, ObjUpvalue, ObjUpvalues, Opcode};
use crate::compiler::compile;
use crate::errors::Result;
use crate::value::Value;
use std::cmp::{max, min};
use std::collections::HashMap;
use std::sync::Arc;
use crate::inline::Inline;

const FRAMES_MAX: usize = 255;

type CallStack = Vec<CallFrame>;
type Globals = HashMap<String, Value>;
type OpenUpvalues = HashMap<usize, Vec<(Arc<Closure>, usize)>>;
type Stack = Inline<Value>;

pub struct Vm {
  call_stack: CallStack,
  stack: Stack,
  globals: Globals,
  open_upvals: OpenUpvalues
}

impl Default for Vm {
  fn default() -> Vm { Vm::new() }
}

impl Vm {
  pub fn new() -> Vm {
    Vm { call_stack: Vec::new(), stack: Stack::new(), globals: HashMap::new(), open_upvals: HashMap::new() }
  }

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
    call(closure, 0, self.stack.len())?.perform(&mut self.call_stack)?;

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
    let Vm { call_stack, stack, globals, open_upvals } = self;

    #[cfg(feature = "verbose")]
    debug_start();

    'outer: loop {
      let frame = call_stack.last_mut().unwrap();
      let (ip, upvals, chunk, slots) = frame.parts();
      let upvals = upvals.try_lock()?;

      loop {
        if let Some(instr) = chunk.at(*ip) {
          #[cfg(feature = "verbose")]
          debug_instr(stack, *ip, instr);
          *ip += 1;
          if let Some(stack_op) = handle_op(instr, globals, &upvals, open_upvals, chunk, slots, stack, ip)? {
            drop(upvals);
            stack_op.perform(call_stack)?;
            if call_stack.len() > FRAMES_MAX {
              bail!(Runtime, "Stack overflow: {}.", call_stack.len());
            }
            if call_stack.is_empty() {
              #[cfg(feature = "verbose")]
              debug_end(stack);
              return Ok(stack.pop());
            }
            break;
          }
        } else {
          break 'outer
        }
      }
    }
    #[cfg(feature = "verbose")]
    debug_end(stack);
    bail!(Runtime, "Missing return.");
  }

  fn debug_stack_trace(&self) {
    println!("\nRuntime error:");
    for frame in self.call_stack.iter().rev() {
      let closure = frame.closure();
      let ip = max(1, min(closure.chunk().code_len(), frame.ip()));
      let instr = closure.chunk().at_fast(ip - 1);
      println!("  at {} in {}: {:?}", instr.loc(), closure.smart_name(), instr);
    }
  }
}

#[cfg(feature = "verbose")]
fn debug_start() {
  println!();
}

#[cfg(feature = "verbose")]
fn debug_instr(stack: &[Value], ip: usize, instr: &Instr) {
  println!("Stack: ");
  for (i, v) in stack.iter().enumerate().rev() {
    println!("  {:>0width$} : {:?}", i, v, width = 4);
  }
  println!("Executing {:>04}: {:?}", ip, instr);
}

#[cfg(feature = "verbose")]
fn debug_end(stack: &[Value]) {
  println!("Final Stack: ");
  for (i, v) in stack.iter().enumerate().rev() {
    println!("  {:>0width$} : {:?}", i, v, width = 4);
  }
}

fn rpeek(stack: &Stack, back: usize) -> &Value { stack.get(stack.len() - 1 - back) }

#[allow(clippy::too_many_arguments)]
fn handle_op(
  instr: &Instr, globals: &Globals, upvalues: &[ObjUpvalue], open_upvals: &mut OpenUpvalues, chunk: &Chunk,
  slots: &usize, stack: &mut Stack, ip: &mut usize
) -> Result<Option<StackOp>> {
  match instr.op() {
    Opcode::Lt => binary_fast(stack, |v, w| v.try_lt(w)),
    Opcode::GetLocal(l) => stack.push(stack.get(*l + *slots).try_clone()?),
    Opcode::Constant(c) => {
      let lit = chunk.get_constant(*c).unwrap();
      stack.push(lit.try_clone()?);
    }
    Opcode::Not => unary(stack, |v| v.try_not())?,
    Opcode::Negate => unary(stack, |v| v.try_negate())?,
    Opcode::Add => binary_fast(stack, |v, w| v.try_add(w)),
    Opcode::Subtract => binary_fast(stack, |v, w| v.try_subtract(w)),
    Opcode::Multiply => binary(stack, |v, w| v.try_multiply(w))?,
    Opcode::Divide => binary(stack, |v, w| v.try_divide(w))?,
    Opcode::Mod => binary(stack, |v, w| v.try_mod(w))?,
    Opcode::And => binary(stack, |v, w| v.try_and(w))?,
    Opcode::Or => binary(stack, |v, w| v.try_or(w))?,
    Opcode::Gt => binary(stack, |v, w| v.try_gt(w))?,
    Opcode::Gte => binary(stack, |v, w| v.try_gte(w))?,
    Opcode::Lte => binary(stack, |v, w| v.try_lte(w))?,
    Opcode::Equals => binary(stack, |v, w| v.try_eq(w))?,
    Opcode::NotEquals => binary(stack, |v, w| v.try_neq(w))?,
    Opcode::GetGlobal(l) => {
      let name = chunk.get_constant(*l).unwrap().as_str().unwrap();
      stack.push(globals.get(name).ok_or_else(|| bad!(Runtime, "No such variable \"{}\".", name))?.try_clone()?);
    }
    Opcode::Jump(offset) => *ip += *offset as usize,
    Opcode::JumpIfFalse(offset) => {
      if !stack.last().try_bool()? {
        *ip += *offset as usize
      }
    }
    Opcode::Pop => stack.drop(),
    Opcode::RotateUp(len) => {
      if *len > 1 {
        let stack_len = stack.len();
        stack[stack_len - len ..].rotate_right(1);
      }
    }
    Opcode::Call(argc) => return call_value(stack, *argc),
    Opcode::Return => {
      let mut index = stack.len() - 1;
      while index > *slots {
        // FAST
        // if let Some(list) = open_upvals.get(&index) {
        //   close_upvalues(list, &stack[index])?;
        //   open_upvals.remove(&index);
        // }
        index -= 1;
      }

      stack.swap_remove(*slots);
      stack.truncate(slots + 1);
      return Ok(Some(StackOp::Pop));
    }
    Opcode::GetUpval(u) => stack.push(upvalues[*u].obtain(stack)?),
    Opcode::Closure(c, upvals) => {
      let val = chunk.get_constant(*c).unwrap();
      let new_upvalues: Vec<_> = upvals
        .iter()
        .map(
          |v| if v.is_local() { capture_upvalue(*slots + v.index()) } else { upvalues[v.index()].try_clone().unwrap() }
        )
        .collect();

      let uv_inds: Vec<_> = new_upvalues.iter().map(|v| v.location().unwrap()).collect();
      let closure = Arc::new(Closure::new(val.try_function().unwrap(), new_upvalues));
      for (ci, uvi) in uv_inds.into_iter().enumerate() {
        open_upvals.entry(uvi).or_insert(Vec::new()).push((closure.clone(), ci));
      }
      stack.push(closure.into());
    }
    Opcode::CloseUpvalue => {
      let val = stack.pop();
      let index = stack.len();
      if let Some(list) = open_upvals.get(&index) {
        close_upvalues(list, &val)?;
        open_upvals.remove(&index);
      }
    }
  }

  Ok(None)
}

fn capture_upvalue(i: usize) -> ObjUpvalue { ObjUpvalue::new(i) }

// Upvalues are done a little bit differently than they are in the book: since the language is immutable, we can
// safely push things on the stack
fn close_upvalues(list: &[(Arc<Closure>, usize)], val: &Value) -> Result<()> {
  for (closure, ci) in list {
    closure.flip_upval(*ci, val.try_clone()?)?;
  }
  Ok(())
}

enum StackOp {
  Push(CallFrame),
  Pop
}

impl StackOp {
  pub fn perform(self, call_stack: &mut CallStack) -> Result<()> {
    match self {
      Self::Push(f) => call_stack.push(f),
      Self::Pop => drop(call_stack.pop().ok_or_else(|| bad!(Internal, "Can't pop empty call stack."))?)
    }
    Ok(())
  }
}

fn call_value(stack: &mut Stack, argc: u8) -> Result<Option<StackOp>> {
  let value = rpeek(stack, argc as usize);
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
  *stack.get_mut(rem - 1) = result;
  Ok(None)
}

fn unary<F: FnOnce(Value) -> Result<Value>>(stack: &mut Stack, f: F) -> Result<()> {
  let v1 = stack.pop();
  stack.push(f(v1)?);
  Ok(())
}

fn binary<F: FnOnce(Value, Value) -> Result<Value>>(stack: &mut Stack, f: F) -> Result<()> {
  let v1 = stack.pop();
  let v2 = stack.pop();
  stack.push(f(v2, v1)?);
  Ok(())
}

fn binary_fast<F: FnOnce(&Value, &Value) -> Value>(stack: &mut Stack, f: F) {
  let last = stack.len() - 1;
  *stack.get_mut(last - 1) = f(stack.get(last - 1), stack.get(last));
  stack.drop();
}

pub struct CallFrame {
  closure: Arc<Closure>,
  ip: usize,
  slots: usize
}

impl CallFrame {
  pub fn new(closure: Arc<Closure>, slots: usize) -> CallFrame { CallFrame { closure, ip: 0, slots } }

  pub fn closure(&self) -> &Closure { &self.closure }
  pub fn ip(&self) -> usize { self.ip }
  pub fn ip_mut(&mut self) -> &mut usize { &mut self.ip }
  pub fn slots(&self) -> usize { self.slots }
  pub fn slots_mut(&mut self) -> &mut usize { &mut self.slots }
  pub fn parts(&mut self) -> (&mut usize, &ObjUpvalues, &Chunk, &usize) {
    (&mut self.ip, self.closure.upvalues(), self.closure.chunk(), &self.slots)
  }
}
