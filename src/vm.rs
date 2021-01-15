//! The actual VM for parsing the bytecode.

use crate::collapsed::{Chunk, Declared, FuncNative};
use crate::common::{Closure, Instr, Native, NativeInfo, ObjUpvalue, ObjUpvalues, Opcode};
use crate::compiler::{collapse_script, compile};
use crate::inline::Inline;
use crate::value::Value;
use std::collections::HashMap;
use std::fmt;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

const FRAMES_MAX: usize = 255;

type CallStack = Vec<CallFrame>;
type Globals = HashMap<String, Declared>;
type Stack = Inline<Value>;

pub struct Vm {
  call_stack: CallStack,
  stack: Stack,
  open_upvals: OpenUpvalues
}

impl Default for Vm {
  fn default() -> Vm { Vm::new() }
}

impl Vm {
  pub fn new() -> Vm { Vm { call_stack: Vec::new(), stack: Stack::new(), open_upvals: OpenUpvalues::new() } }

  pub async fn interpret(self, source: &str, globals: crate::common::Globals) -> Value {
    debug_assert_eq!(self.stack.len(), 0);
    debug_assert_eq!(self.call_stack.len(), 0);

    // last minute code changes
    let (script, stype) = compile(source, &globals);
    let (function, inst_ind, globals) = collapse_script(script, stype, globals);

    // lock it in
    let function = Arc::new(function);
    let closure = Arc::new(Closure::new(function, Vec::new()));

    let mut ftr = Runner { call_stack: self.call_stack, stack: self.stack, globals, open_upvals: self.open_upvals };

    ftr.run_closure(closure, inst_ind, Vec::new()).await
  }
}

pub struct Runner {
  call_stack: CallStack,
  stack: Stack,
  globals: Globals,
  open_upvals: OpenUpvalues
}

impl Runner {
  pub async fn run_closure(&mut self, closure: Arc<Closure>, inst_ind: usize, args: Vec<Value>) -> Value {
    // TODO(later): also handle native, via either a separate "run_native", or a "run_value" with switch.

    self.stack.push(closure.clone().into());
    debug_assert!(args.len() < 255);
    let args_len = args.len() as u8;
    self.stack.append_vec(args);

    call_closure(closure, inst_ind, args_len, &mut self.stack, true).perform(&mut self.call_stack);
    self.run_loop().await
  }

  async fn run_loop(&mut self) -> Value {
    loop {
      match self.run() {
        Stopped::Native(ntv) => {
          let v = ntv.run(self).await;
          self.stack.push(v);
        }
        Stopped::Done(v) => return v
      }
    }
  }

  fn run(&mut self) -> Stopped {
    self.try_run()

    // TODO(later): inject error handling everywhere
    //
    // if r.is_err() {
    //   self.debug_stack_trace();
    // }
  }

  fn try_run(&mut self) -> Stopped {
    let Runner { call_stack, stack, globals, open_upvals } = self;

    #[cfg(feature = "verbose")]
    debug_start();

    'outer: loop {
      let frame = call_stack.last_mut().unwrap();
      let (ip, upvals, chunk, slots) = frame.parts();
      let mut upvals = upvals.try_lock().unwrap();

      loop {
        if let Some(instr) = chunk.at(*ip) {
          #[cfg(feature = "verbose")]
          debug_instr(stack, *ip, instr);
          *ip += 1;
          match handle_op(instr, globals, &mut upvals, open_upvals, chunk, slots, stack, ip) {
            Handled::StackOp(stack_op) => {
              drop(upvals);
              let exit = stack_op.perform(call_stack);
              if call_stack.len() > FRAMES_MAX {
                panic!("Stack overflow: {}.", call_stack.len());
              }
              if exit || call_stack.is_empty() {
                #[cfg(feature = "verbose")]
                debug_end(stack, exit && !call_stack.is_empty());
                return Stopped::Done(stack.pop());
              }
              break;
            }
            Handled::Native(native_run) => {
              return Stopped::Native(native_run);
            }
            Handled::None => ()
          }
        } else {
          break 'outer;
        }
      }
    }

    #[cfg(feature = "verbose")]
    debug_end(stack, false);
    panic!("Missing return.")
  }

  // fn debug_stack_trace(&self) {
  //   println!("\nRuntime error:");
  //   for frame in self.call_stack.iter().rev() {
  //     let closure = frame.closure();
  //     let ip = max(1, min(closure.chunk().code_len(), frame.ip()));
  //     let instr = closure.chunk().at_fast(ip - 1);
  //     println!("  at {} in {}: {:?}", instr.loc(), closure.smart_name(), instr);
  //   }
  // }
}

pub struct NativeRun {
  native: Native,
  args: Vec<Value>,
  native_info: NativeInfo
}

impl NativeRun {
  pub fn run<'r>(self, runner: &'r mut Runner) -> Pin<Box<dyn Future<Output = Value> + Send + 'r>> {
    (self.native)(self.args, self.native_info, runner)
  }
}

#[allow(clippy::too_many_arguments)]
fn handle_op(
  instr: &Instr, globals: &Globals, upvalues: &mut [ObjUpvalue], open_upvals: &mut OpenUpvalues, chunk: &Chunk,
  slots: &usize, stack: &mut Stack, ip: &mut usize
) -> Handled {
  match instr.op() {
    Opcode::Lt => binary(stack, |v, w| v.op_lt(w)),
    Opcode::GetLocal(l) => {
      let v = stack.get_mut(*l + *slots).shift();
      stack.push(v);
    }
    Opcode::Constant(c) => {
      let lit = chunk.get_constant(*c).unwrap().to_value();
      stack.push(lit);
    }
    Opcode::Not => unary(stack, |v| v.op_not()),
    Opcode::Negate => unary(stack, |v| v.op_negate()),
    Opcode::Add => binary(stack, |v, w| v.op_add(w)),
    Opcode::Subtract => binary(stack, |v, w| v.op_subtract(w)),
    Opcode::Multiply => binary(stack, |v, w| v.op_multiply(w)),
    Opcode::Divide => binary(stack, |v, w| v.op_divide(w)),
    Opcode::Mod => binary(stack, |v, w| v.op_mod(w)),
    Opcode::And => binary(stack, |v, w| v.op_and(w)),
    Opcode::Or => binary(stack, |v, w| v.op_or(w)),
    Opcode::Gt => binary(stack, |v, w| v.op_gt(w)),
    Opcode::Gte => binary(stack, |v, w| v.op_gte(w)),
    Opcode::Lte => binary(stack, |v, w| v.op_lte(w)),
    Opcode::Equals => binary(stack, |v, w| v.op_eq(w)),
    Opcode::NotEquals => binary(stack, |v, w| v.op_neq(w)),
    Opcode::GetGlobal(l) => {
      let name = chunk.get_constant(*l).unwrap().as_str().unwrap();
      stack.push(globals.get(name).unwrap().to_value());
    }
    Opcode::Jump(offset) => *ip += *offset as usize,
    Opcode::JumpIfFalse(offset) => {
      if !stack.last().as_bool() {
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
    Opcode::Call(inst_ind, argc) => return call_value(stack, *inst_ind, *argc),
    Opcode::Return => {
      open_upvals.close_gte(*slots, stack);
      stack.swap_remove(*slots);
      stack.truncate(slots + 1);
      return Handled::StackOp(StackOp::Pop);
    }
    Opcode::GetUpval(u) => {
      let v = upvalues[*u].obtain(stack);
      stack.push(v)
    }
    Opcode::Closure(c, upvals) => {
      let val = chunk.get_constant(*c).unwrap();
      let new_upvalues: Vec<_> = upvals
        .iter()
        .map(|v| if v.is_local() { capture_upvalue(*slots + v.index()) } else { upvalues[v.index()].clone() })
        .collect();

      let uv_inds: Vec<_> = new_upvalues.iter().map(|v| v.location()).collect();
      let closure = Arc::new(Closure::new(val.as_function(), new_upvalues));
      open_upvals.insert_opens(uv_inds, &closure);
      stack.push(closure.into());
    }
    Opcode::CloseUpvalue => {
      let index = stack.len() - 1;
      open_upvals.close_gte(index, stack);
      stack.drop();
    }
  }

  Handled::None
}

struct OpenUpvalues {
  #[allow(clippy::type_complexity)]
  per_slot: Vec<(usize, Vec<(Arc<Closure>, usize)>)>
}

impl OpenUpvalues {
  pub fn new() -> OpenUpvalues { OpenUpvalues { per_slot: Vec::new() } }

  pub fn insert_opens(&mut self, uv_inds: Vec<usize>, closure: &Arc<Closure>) {
    for (ci, uvi) in uv_inds.into_iter().enumerate() {
      let ind = self.per_slot.iter().rposition(|(s, _)| s < &uvi).map(|p| p + 1).unwrap_or(0);

      if ind == self.per_slot.len() || self.per_slot[ind].0 > uvi {
        self.per_slot.insert(ind, (uvi, vec![(closure.clone(), ci)]));
      } else if self.per_slot[ind].0 == uvi {
        self.per_slot[ind].1.push((closure.clone(), ci));
      }
    }
  }

  pub fn close_gte(&mut self, target: usize, stack: &mut Stack) {
    let per_slot = &mut self.per_slot;
    while !per_slot.is_empty() && per_slot.last().unwrap().0 >= target {
      let (slot, locs) = per_slot.pop().unwrap();
      close_upvalues(&locs, &mut stack[slot]);
    }
  }
}

fn capture_upvalue(i: usize) -> ObjUpvalue { ObjUpvalue::new(i) }

// Upvalues are done a little bit differently than they are in the book: since the language is immutable, we can
// safely push values from the stack.
fn close_upvalues(list: &[(Arc<Closure>, usize)], val: &mut Value) {
  for (closure, ci) in list {
    closure.flip_upval(*ci, val.shift());
  }
}

fn rpeek(stack: &Stack, back: usize) -> &Value { stack.get(stack.len() - 1 - back) }

pub enum Stopped {
  Native(NativeRun),
  Done(Value)
}

impl fmt::Debug for Stopped {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Native(_) => write!(f, "(native)"),
      Self::Done(v) => write!(f, "{:?}", v)
    }
  }
}

enum Handled {
  StackOp(StackOp),
  Native(NativeRun),
  None
}

enum StackOp {
  Push(CallFrame),
  Pop
}

impl StackOp {
  pub fn perform(self, call_stack: &mut CallStack) -> bool {
    match self {
      Self::Push(f) => {
        call_stack.push(f);
        false
      }
      Self::Pop => {
        let frame = call_stack.pop().expect("Can't pop empty call stack.");
        frame.exit()
      }
    }
  }
}

fn call_value(stack: &mut Stack, inst_ind: usize, argc: u8) -> Handled {
  let value = rpeek(stack, argc as usize);

  match value {
    Value::Closure(f) => Handled::StackOp(call_closure(f.clone(), inst_ind, argc, stack, false)),
    Value::Native(f) => Handled::Native(call_native(f.clone(), inst_ind, argc, stack)),
    other => panic!("Not a function: {:?}", other)
  }
}

fn call_closure(f: Arc<Closure>, inst_ind: usize, argc: u8, stack: &mut Stack, exit: bool) -> StackOp {
  if argc != f.arity() {
    panic!("Calling arity {} with {} args.", f.arity(), argc);
  }
  let stack_len = stack.len();
  StackOp::Push(CallFrame::new(f, inst_ind, stack_len - (argc as usize) - 1, exit))
}

fn call_native(f: Arc<FuncNative>, inst_ind: usize, argc: u8, stack: &mut Stack) -> NativeRun {
  if argc != f.arity() {
    panic!("Calling arity {} with {} args.", f.arity(), argc);
  }
  let native_info = f.instances()[inst_ind].clone();
  let native = *f.native();

  let mut args = Vec::new();
  for _ in 0 .. argc {
    args.push(stack.pop());
  }
  args.reverse();
  stack.drop(); // once more for the native

  NativeRun { native, args, native_info }
}

fn unary<F: FnOnce(&Value) -> Value>(stack: &mut Stack, f: F) {
  let last = stack.len() - 1;
  *stack.get_mut(last) = f(stack.get(last));
}

fn binary<F: FnOnce(&Value, &Value) -> Value>(stack: &mut Stack, f: F) {
  let last = stack.len() - 1;
  *stack.get_mut(last - 1) = f(stack.get(last - 1), stack.get(last));
  stack.drop();
}

pub struct CallFrame {
  closure: Arc<Closure>,
  inst_ind: usize,
  ip: usize,
  slots: usize,
  exit: bool
}

impl CallFrame {
  pub fn new(closure: Arc<Closure>, inst_ind: usize, slots: usize, exit: bool) -> CallFrame {
    CallFrame { closure, inst_ind, ip: 0, slots, exit }
  }

  pub fn closure(&self) -> &Closure { &self.closure }
  pub fn ip(&self) -> usize { self.ip }
  pub fn ip_mut(&mut self) -> &mut usize { &mut self.ip }
  pub fn slots(&self) -> usize { self.slots }
  pub fn slots_mut(&mut self) -> &mut usize { &mut self.slots }
  pub fn exit(&self) -> bool { self.exit }
  pub fn inst_ind(&self) -> usize { self.inst_ind }

  pub fn parts(&mut self) -> (&mut usize, &ObjUpvalues, &Chunk, &usize) {
    let inst_ind = self.inst_ind();
    (&mut self.ip, self.closure.upvalues(), self.closure.chunk(inst_ind), &self.slots)
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
