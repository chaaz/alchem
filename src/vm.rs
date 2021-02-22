//! The actual VM for parsing the bytecode.

use crate::collapsed::{Chunk, CollapsedInfo, Declared, FuncNative, RunMeta};
use crate::common::{Closure, FunctionIndex, Instr, Native, ObjUpvalue, ObjUpvalues, Opcode};
pub use crate::compiler::{collapse_script, compile, script_to_closure};
use crate::inline::Inline;
use crate::types::CustomType;
use crate::value::Value;
use std::collections::HashMap;
use std::fmt;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

const FRAMES_MAX: usize = 255;

type CallStack<C> = Vec<CallFrame<C>>;
type Globals<C> = HashMap<String, Declared<C>>;
type Stack<C> = Inline<Value<C>>;

pub struct Vm<C: CustomType> {
  call_stack: CallStack<C>,
  stack: Stack<C>,
  open_upvals: OpenUpvalues<C>,
  runtime: C::Runtime
}

impl<C: CustomType + 'static> Vm<C> {
  pub fn new(runtime: C::Runtime) -> Vm<C> {
    Vm { call_stack: Vec::new(), stack: Stack::new(), open_upvals: OpenUpvalues::new(), runtime }
  }

  pub async fn interpret(self, source: &str, globals: crate::common::Globals<C>) -> Value<C> {
    debug_assert_eq!(self.stack.len(), 0);
    debug_assert_eq!(self.call_stack.len(), 0);

    // last minute code changes
    let (scope, stype) = compile(source, &globals).await;
    let (function, inst_ind, globals) = collapse_script(scope, stype, globals);

    // lock it in
    let function = Arc::new(function);
    let closure = Arc::new(Closure::new(function, Vec::new()));

    let mut ftr = Runner {
      call_stack: self.call_stack,
      stack: self.stack,
      globals,
      open_upvals: self.open_upvals,
      runtime: self.runtime
    };

    ftr.run_closure(closure, FunctionIndex::empty(inst_ind), RunMeta::new(0), Vec::new()).await
  }
}

pub struct Runner<C: CustomType> {
  call_stack: CallStack<C>,
  stack: Stack<C>,
  globals: Globals<C>,
  open_upvals: OpenUpvalues<C>,
  runtime: C::Runtime
}

impl<C: CustomType + 'static> Runner<C> {
  pub fn capture(&mut self) -> Runner<C> {
    self.open_upvals.close_gte(0, &mut self.stack);
    Runner {
      call_stack: Vec::new(),
      stack: Stack::new(),
      globals: self.globals.clone(),
      open_upvals: OpenUpvalues::new(),
      runtime: self.runtime.clone()
    }
  }

  pub fn fresh(globals: Globals<C>, runtime: C::Runtime) -> Runner<C> {
    Runner { call_stack: Vec::new(), stack: Stack::new(), globals, open_upvals: OpenUpvalues::new(), runtime }
  }

  pub async fn into_run_value(
    mut self, value: Value<C>, inst_ind: FunctionIndex, meta: RunMeta<C>, args: Vec<Value<C>>
  ) -> Value<C> {
    match value {
      Value::Closure(closure) => self.run_closure(closure, inst_ind, meta, args).await,
      Value::Native(func_native) => self.run_native(func_native, inst_ind, meta, args).await,
      other => panic!("Value is not runnable: {:?}", other)
    }
  }

  pub async fn run_value(
    &mut self, value: Value<C>, inst_ind: FunctionIndex, meta: RunMeta<C>, args: Vec<Value<C>>
  ) -> Value<C> {
    match value {
      Value::Closure(closure) => self.run_closure(closure, inst_ind, meta, args).await,
      Value::Native(func_native) => self.run_native(func_native, inst_ind, meta, args).await,
      other => panic!("Value is not runnable: {:?}", other)
    }
  }

  pub async fn run_closure(
    &mut self, closure: Arc<Closure<C>>, inst_ind: FunctionIndex, meta: RunMeta<C>, args: Vec<Value<C>>
  ) -> Value<C> {
    self.stack.push(closure.clone().into());
    let index = inst_ind.index();
    let args: Vec<_> = if let Some(extracts) = inst_ind.extracts().as_ref() {
      assert_eq!(extracts.len(), args.len());
      args.into_iter().zip(extracts.iter()).flat_map(|(a, e)| e.extracted(a)).collect()
    } else {
      panic!("no extracts for alchemy run_value");
    };
    debug_assert!(args.len() < 255);
    let args_len = args.len() as u8;
    self.stack.append_vec(args);

    call_closure(closure, index, args_len, &mut self.stack, true).perform(&mut self.call_stack);
    self.run_loop(meta).await
  }

  pub async fn run_native(
    &mut self, func_native: Arc<FuncNative<C>>, inst_ind: FunctionIndex, meta: RunMeta<C>, args: Vec<Value<C>>
  ) -> Value<C> {
    let native_info = func_native.instances()[inst_ind.index()].clone();
    let native = func_native.native();
    (native)(args, native_info, meta, self).await
  }

  async fn run_loop(&mut self, meta: RunMeta<C>) -> Value<C> {
    loop {
      match self.run(&meta) {
        Stopped::Native(ntv) => {
          let v = ntv.run(self).await;
          self.stack.push(v);
        }
        Stopped::Done(v) => return v
      }
    }
  }

  fn run(&mut self, meta: &RunMeta<C>) -> Stopped<C> {
    self.try_run(meta)

    // TODO(later): inject error handling everywhere
    //
    // if r.is_err() {
    //   self.debug_stack_trace();
    // }
  }

  fn try_run(&mut self, meta: &RunMeta<C>) -> Stopped<C> {
    let Runner { call_stack, stack, globals, open_upvals, .. } = self;

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
          match handle_op(instr, globals, &mut upvals, open_upvals, chunk, slots, stack, ip, meta) {
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

  pub fn runtime_mut(&mut self) -> &mut C::Runtime { &mut self.runtime }
  pub fn runtime(&mut self) -> &C::Runtime { &self.runtime }
}

pub struct NativeRun<C: CustomType> {
  native: Native<C>,
  args: Vec<Value<C>>,
  native_info: CollapsedInfo<C>,
  meta: RunMeta<C>
}

impl<C: CustomType + 'static> NativeRun<C> {
  pub fn run<'r>(self, runner: &'r mut Runner<C>) -> Pin<Box<dyn Future<Output = Value<C>> + Send + 'r>> {
    (self.native)(self.args, self.native_info, self.meta, runner)
  }
}

#[allow(clippy::too_many_arguments)]
fn handle_op<C: CustomType + 'static>(
  instr: &Instr, globals: &Globals<C>, upvalues: &mut [ObjUpvalue<C>], open_upvals: &mut OpenUpvalues<C>,
  chunk: &Chunk<C>, slots: &usize, stack: &mut Stack<C>, ip: &mut usize, meta: &RunMeta<C>
) -> Handled<C> {
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
    Opcode::Call(inst_ind, argc) => {
      return call_value(stack, *inst_ind, *argc, meta, instr.pos());
    }
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
    Opcode::Array(len) => {
      let stack_len = stack.len();
      let mut parts = Vec::new();
      for i in 0 .. *len {
        parts.push(stack[stack_len - (len - i)].shift());
      }
      if *len > 0 {
        stack[stack_len - len] = Value::Array(parts);
        stack.truncate(stack_len - (len - 1));
      } else {
        stack.push(Value::Array(parts));
      }
    }
    Opcode::Object(inds) => {
      let stack_len = stack.len();
      let inds_len = inds.len();
      let mut parts = Vec::new();
      for _ in 0 .. inds_len {
        parts.push(Value::Int(0));
      }
      for (i, ind) in inds.iter().enumerate() {
        parts[*ind] = stack[stack_len - (inds_len - i)].shift();
      }
      if inds_len > 0 {
        stack[stack_len - inds_len] = Value::Array(parts);
        stack.truncate(stack_len - (inds_len - 1));
      } else {
        stack.push(Value::Array(parts));
      }
    }
    Opcode::GetIndex(ind) => {
      let stack_len = stack.len();
      stack[stack_len - 1] = stack[stack_len - 1].as_array_mut()[*ind].shift();
    }
    Opcode::GetJsonIndex(ind) => {
      let stack_len = stack.len();
      stack[stack_len - 1] = Value::Json(stack[stack_len - 1].as_json_mut().as_array_mut().unwrap().swap_remove(*ind))
    }
    Opcode::GetJsonKey(name) => {
      let stack_len = stack.len();
      stack[stack_len - 1] =
        Value::Json(stack[stack_len - 1].as_json_mut().as_object_mut().unwrap().remove(name).unwrap())
    }
    Opcode::Extract(extn) => {
      let arr = stack.pop();
      for target in extn.extracted(arr) {
        stack.push(target);
      }
      // for part in extn.parts() {
      //   let mut target = &mut arr;
      //   for ind in part.inds() {
      //     target = target.as_array_mut().get_mut(*ind).unwrap();
      //   }
      //   stack.push(target.shift());
      // }
    }
  }

  Handled::None
}

struct OpenUpvalues<C: CustomType> {
  #[allow(clippy::type_complexity)]
  per_slot: Vec<(usize, Vec<(Arc<Closure<C>>, usize)>)>
}

impl<C: CustomType + 'static> OpenUpvalues<C> {
  pub fn new() -> OpenUpvalues<C> { OpenUpvalues { per_slot: Vec::new() } }

  pub fn insert_opens(&mut self, uv_inds: Vec<usize>, closure: &Arc<Closure<C>>) {
    for (ci, uvi) in uv_inds.into_iter().enumerate() {
      let ind = self.per_slot.iter().rposition(|(s, _)| s < &uvi).map(|p| p + 1).unwrap_or(0);

      if ind == self.per_slot.len() || self.per_slot[ind].0 > uvi {
        self.per_slot.insert(ind, (uvi, vec![(closure.clone(), ci)]));
      } else if self.per_slot[ind].0 == uvi {
        self.per_slot[ind].1.push((closure.clone(), ci));
      }
    }
  }

  pub fn close_gte(&mut self, target: usize, stack: &mut Stack<C>) {
    let per_slot = &mut self.per_slot;
    while !per_slot.is_empty() && per_slot.last().unwrap().0 >= target {
      let (slot, locs) = per_slot.pop().unwrap();
      close_upvalues(&locs, &mut stack[slot]);
    }
  }
}

fn capture_upvalue<C: CustomType>(i: usize) -> ObjUpvalue<C> { ObjUpvalue::new(i) }

// Upvalues are done a little bit differently than they are in the book: since the language is immutable, we can
// safely push values from the stack.
fn close_upvalues<C: CustomType + 'static>(list: &[(Arc<Closure<C>>, usize)], val: &mut Value<C>) {
  for (closure, ci) in list {
    closure.flip_upval(*ci, val.shift());
  }
}

fn rpeek<C: CustomType>(stack: &Stack<C>, back: usize) -> &Value<C> { stack.get(stack.len() - 1 - back) }

pub enum Stopped<C: CustomType> {
  Native(NativeRun<C>),
  Done(Value<C>)
}

impl<C: CustomType> fmt::Debug for Stopped<C> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Native(_) => write!(f, "(native)"),
      Self::Done(v) => write!(f, "{:?}", v)
    }
  }
}

enum Handled<C: CustomType> {
  StackOp(StackOp<C>),
  Native(NativeRun<C>),
  None
}

enum StackOp<C: CustomType> {
  Push(CallFrame<C>),
  Pop
}

impl<C: CustomType + 'static> StackOp<C> {
  pub fn perform(self, call_stack: &mut CallStack<C>) -> bool {
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

fn call_value<C: CustomType + 'static>(
  stack: &mut Stack<C>, inst_ind: usize, argc: u8, meta: &RunMeta<C>, pos: usize
) -> Handled<C> {
  let value = rpeek(stack, argc as usize);

  match value {
    Value::Closure(f) => Handled::StackOp(call_closure(f.clone(), inst_ind, argc, stack, false)),
    Value::Native(f) => Handled::Native(call_native(f.clone(), inst_ind, argc, stack, meta.update(pos))),
    other => panic!("Not a function: {:?}", other)
  }
}

fn call_closure<C: CustomType + 'static>(
  f: Arc<Closure<C>>, inst_ind: usize, argc: u8, stack: &mut Stack<C>, exit: bool
) -> StackOp<C> {
  assert_eq!(argc, f.arity(), "Calling arity {} with {} args.", f.arity(), argc);
  let stack_len = stack.len();
  StackOp::Push(CallFrame::new(f, inst_ind, stack_len - (argc as usize) - 1, exit))
}

fn call_native<C: CustomType + 'static>(
  f: Arc<FuncNative<C>>, inst_ind: usize, argc: u8, stack: &mut Stack<C>, meta: RunMeta<C>
) -> NativeRun<C> {
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

  NativeRun { native, args, native_info, meta }
}

fn unary<C: CustomType, F: FnOnce(&Value<C>) -> Value<C>>(stack: &mut Stack<C>, f: F) {
  let last = stack.len() - 1;
  *stack.get_mut(last) = f(stack.get(last));
}

fn binary<C: CustomType, F: FnOnce(&Value<C>, &Value<C>) -> Value<C>>(stack: &mut Stack<C>, f: F) {
  let last = stack.len() - 1;
  *stack.get_mut(last - 1) = f(stack.get(last - 1), stack.get(last));
  stack.drop();
}

pub struct CallFrame<C: CustomType> {
  closure: Arc<Closure<C>>,
  inst_ind: usize,
  ip: usize,
  slots: usize,
  exit: bool
}

impl<C: CustomType + 'static> CallFrame<C> {
  pub fn new(closure: Arc<Closure<C>>, inst_ind: usize, slots: usize, exit: bool) -> CallFrame<C> {
    CallFrame { closure, inst_ind, ip: 0, slots, exit }
  }

  pub fn closure(&self) -> &Closure<C> { &self.closure }
  pub fn ip(&self) -> usize { self.ip }
  pub fn ip_mut(&mut self) -> &mut usize { &mut self.ip }
  pub fn slots(&self) -> usize { self.slots }
  pub fn slots_mut(&mut self) -> &mut usize { &mut self.slots }
  pub fn exit(&self) -> bool { self.exit }
  pub fn inst_ind(&self) -> usize { self.inst_ind }

  pub fn parts(&mut self) -> (&mut usize, &ObjUpvalues<C>, &Chunk<C>, &usize) {
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
