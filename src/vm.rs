//! The actual VM for parsing the bytecode.

use crate::common::{Chunk, Closure, Instr, Native, ObjUpvalue, ObjUpvalues, Opcode};
use crate::compiler::compile;
use crate::errors::Result;
use crate::value::{Value, Declared};
use std::cmp::{max, min};
use std::collections::HashMap;
use std::sync::Arc;
use crate::inline::Inline;
use std::future::Future;
use std::fmt;

const FRAMES_MAX: usize = 255;

type CallStack = Vec<CallFrame>;
type Globals = HashMap<String, Declared>;
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
    self.globals.insert(name.to_string(), Declared::Native(native));
  }

  pub fn interpret(self, source: &str) -> Result<Runner> {
    debug_assert_eq!(self.stack.len(), 0);
    debug_assert_eq!(self.call_stack.len(), 0);

    // last minute code changes
    let function = compile(source)?;

    // lock it in
    let function = Arc::new(function);
    let closure = Arc::new(Closure::new(function, Vec::new()));

    let mut ftr = Runner {
      call_stack: self.call_stack,
      stack: self.stack,
      globals: self.globals,
      open_upvals: self.open_upvals
    };

    ftr.start_closure(closure)?;
    Ok(ftr)
  }
}

pub struct Runner {
  call_stack: CallStack,
  stack: Stack,
  globals: Globals,
  open_upvals: OpenUpvalues
}

impl Runner {
  pub fn run(&mut self) -> Result<Stopped> {
    let r = self.try_run();
    if r.is_err() {
      self.debug_stack_trace();
    }
    r
  }

  pub fn run_closure(&mut self, closure: Arc<Closure>) -> Result<Stopped> {
    self.start_closure(closure)?;
    self.run()
  }

  pub fn run_with_value(&mut self, v: Value) -> Result<Stopped> {
    self.stack.push(v);
    self.run()
  }

  fn start_closure(&mut self, closure: Arc<Closure>) -> Result<()> {
    self.stack.push(closure.clone().into());
    call_closure(closure, 0, self.stack.len(), true)?.perform(&mut self.call_stack)?;
    Ok(())
  }

  fn try_run(&mut self) -> Result<Stopped> {
    let Runner { call_stack, stack, globals, open_upvals } = self;

    #[cfg(feature = "verbose")]
    debug_start();

    'outer: loop {
      let frame = call_stack.last_mut().unwrap();
      let (ip, upvals, chunk, slots) = frame.parts();
      let mut upvals = upvals.try_lock()?;

      loop {
        if let Some(instr) = chunk.at(*ip) {
          #[cfg(feature = "verbose")]
          debug_instr(stack, *ip, instr);
          *ip += 1;
          match handle_op(instr, globals, &mut upvals, open_upvals, chunk, slots, stack, ip)? {
            Handled::StackOp(stack_op) => {
              drop(upvals);
              let exit = stack_op.perform(call_stack)?;
              if call_stack.len() > FRAMES_MAX {
                return err!(Runtime, "Stack overflow: {}.", call_stack.len());
              }
              if exit || call_stack.is_empty() {
                #[cfg(feature = "verbose")]
                debug_end(stack, exit && !call_stack.is_empty());
                return Ok(Stopped::Done(stack.pop()));
              }
              break;
            }
            Handled::Native(native_run) => {
              return Ok(Stopped::Native(native_run));
            }
            Handled::Future(ftr) => {
              return Ok(Stopped::Future(ftr));
            }
            Handled::None => ()
          }
        } else {
          break 'outer
        }
      }
    }
    #[cfg(feature = "verbose")]
    debug_end(stack, false);
    err!(Runtime, "Missing return.")
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

pub struct NativeRun {
  native: Native,
  args: Vec<Value>
}

impl NativeRun {
  pub fn run(&mut self, runner: &mut Runner) -> Result<Value> {
    (self.native)(&self.args, runner)
  }
}

#[allow(clippy::too_many_arguments)]
fn handle_op(
  instr: &Instr, globals: &Globals, upvalues: &mut [ObjUpvalue], open_upvals: &mut OpenUpvalues, chunk: &Chunk,
  slots: &usize, stack: &mut Stack, ip: &mut usize
) -> Result<Handled> {
  match instr.op() {
    Opcode::Lt => binary_fast(stack, |v, w| v.try_lt(w)),
    Opcode::GetLocal(l) => {
      let v = stack.get_mut(*l + *slots).shift();
      stack.push(v);
    }
    Opcode::Constant(c) => {
      let lit = chunk.get_constant(*c).unwrap().to_value();
      stack.push(lit);
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
      stack.push(globals.get(name).ok_or_else(|| bad!(Runtime, "No such variable \"{}\".", name))?.to_value());
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
      return Ok(Handled::StackOp(StackOp::Pop));
    }
    Opcode::GetUpval(u) => {
      let v = upvalues[*u].obtain(stack);
      stack.push(v)
    }
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
      let mut val = stack.pop();
      let index = stack.len();
      if let Some(list) = open_upvals.get(&index) {
        close_upvalues(list, &mut val)?;
        open_upvals.remove(&index);
      }
    }
    Opcode::Await => {
      return Ok(Handled::Future(stack.pop().into_future()));
    }
  }

  Ok(Handled::None)
}

fn capture_upvalue(i: usize) -> ObjUpvalue { ObjUpvalue::new(i) }

// Upvalues are done a little bit differently than they are in the book: since the language is immutable, we can
// safely push values from the stack.
fn close_upvalues(list: &[(Arc<Closure>, usize)], val: &mut Value) -> Result<()> {
  for (closure, ci) in list {
    closure.flip_upval(*ci, val.shift())?;
  }
  Ok(())
}

fn rpeek(stack: &Stack, back: usize) -> &Value { stack.get(stack.len() - 1 - back) }

pub enum Stopped {
  Future(Box<dyn Future<Output = Result<Value>> + 'static + Send + Unpin>),
  Native(NativeRun),
  Done(Value)
}

impl fmt::Debug for Stopped {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Future(_) => write!(f, "(future)"),
      Self::Native(_) => write!(f, "(native)"),
      Self::Done(v) => write!(f, "{:?}", v),
    }
  }
}

enum Handled {
  StackOp(StackOp),
  Future(Box<dyn Future<Output = Result<Value>> + 'static + Send + Unpin>),
  Native(NativeRun),
  None
}

enum StackOp {
  Push(CallFrame),
  Pop
}

impl StackOp {
  pub fn perform(self, call_stack: &mut CallStack) -> Result<bool> {
    match self {
      Self::Push(f) => {
        call_stack.push(f);
        Ok(false)
      }
      Self::Pop => {
        let frame = call_stack.pop().ok_or_else(|| bad!(Internal, "Can't pop empty call stack."))?;
        Ok(frame.exit())
      }
    }
  }
}

fn call_value(stack: &mut Stack, argc: u8) -> Result<Handled> {
  let value = rpeek(stack, argc as usize);
  match value {
    Value::Closure(f) => Ok(Handled::StackOp(call_closure(f.clone(), argc, stack.len(), false)?)),
    Value::Native(f) => Ok(call_native(*f, argc, stack)),
    other => err!(Runtime, "Not a function: {:?}", other)
  }
}

fn call_closure(closure: Arc<Closure>, argc: u8, stack_len: usize, exit: bool) -> Result<StackOp> {
  if argc != closure.arity() {
    bail!(Runtime, "Calling arity {} with {} args.", closure.arity(), argc);
  }
  Ok(StackOp::Push(CallFrame::new(closure, stack_len - (argc as usize) - 1, exit)))
}

fn call_native(native: Native, argc: u8, stack: &mut Stack) -> Handled {
  let mut args = Vec::new();
  for _ in 0 .. argc {
    args.push(stack.pop());
  }
  args.reverse();
  stack.pop(); // once more for the native

  Handled::Native(NativeRun { native, args })
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
  slots: usize,
  exit: bool
}

impl CallFrame {
  pub fn new(closure: Arc<Closure>, slots: usize, exit: bool) -> CallFrame { CallFrame { closure, ip: 0, slots, exit } }

  pub fn closure(&self) -> &Closure { &self.closure }
  pub fn ip(&self) -> usize { self.ip }
  pub fn ip_mut(&mut self) -> &mut usize { &mut self.ip }
  pub fn slots(&self) -> usize { self.slots }
  pub fn slots_mut(&mut self) -> &mut usize { &mut self.slots }
  pub fn exit(&self) -> bool { self.exit }

  pub fn parts(&mut self) -> (&mut usize, &ObjUpvalues, &Chunk, &usize) {
    (&mut self.ip, self.closure.upvalues(), self.closure.chunk(), &self.slots)
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
fn debug_end(stack: &[Value], early_exit: bool) {
  println!("Final Stack: (early exit {})", early_exit);
  for (i, v) in stack.iter().enumerate().rev() {
    println!("  {:>0width$} : {:?}", i, v, width = 4);
  }
}
