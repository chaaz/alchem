//! An unsafe stack representation.

use crate::value::Value;
use std::ptr;
use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut};
use std::slice;

pub struct Stack {
  inline: MaybeUninit<[Value; 1024]>,
  len: usize
}

impl Stack {
  pub fn new() -> Stack {
    Stack { inline: MaybeUninit::uninit(), len: 0 }
  }

  #[inline]
  pub fn len(&self) -> usize { self.len }

  #[inline]
  pub fn drop(&mut self) { self.len -= 1; }

  #[inline]
  pub fn truncate(&mut self, len: usize) { self.len = len; }

  #[inline]
  pub fn push(&mut self, v: Value) {
    unsafe {
      let myptr = self.inline.as_mut_ptr() as *mut Value;
      ptr::write(myptr.add(self.len), v);
      self.len += 1;
    }
  }

  #[inline]
  pub fn pop(&mut self) -> Value {
    unsafe {
      let myptr = self.inline.as_mut_ptr() as *const Value;
      self.len -= 1;
      ptr::read(myptr.add(self.len))
    }
  }

  #[inline]
  pub fn get(&self, i: usize) -> &Value {
    unsafe {
      let myptr = self.inline.as_ptr().add(i) as *const Value;
      &*myptr
    }
  }

  #[inline]
  pub fn last(&self) -> &Value { self.get(self.len - 1) }

  #[inline]
  pub fn get_mut(&mut self, i: usize) -> &mut Value {
    unsafe {
      let myptr = self.inline.as_mut_ptr().add(i) as *mut Value;
      &mut *myptr
    }
  }

  #[inline]
  pub fn swap_remove(&mut self, index: usize) -> Value {
    let len = self.len;
    self.swap(len - 1, index);
    self.pop()
  }
}

impl Deref for Stack {
  type Target = [Value];

  #[inline]
  fn deref(&self) -> &[Value] {
    unsafe {
      let myptr = self.inline.as_ptr() as *const Value;
      slice::from_raw_parts(myptr, self.len)
    }
  }
}

impl DerefMut for Stack {
  #[inline]
  fn deref_mut(&mut self) -> &mut [Value] {
    unsafe {
      let myptr = self.inline.as_mut_ptr() as *mut Value;
      slice::from_raw_parts_mut(myptr, self.len)
    }
  }
}
