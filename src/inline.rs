//! An unsafe stack representation.
//!
//! Using this struct is **extremely unsafe**, since we are manipulating raw pointers with very little
//! understanding of what we're doing. Also, if we do anything stupid, like overflow or underflow, we will at
//! *best* panic, and more likely full-on seg fault.
//!
//! Still, using this implementation instead of a vector speeds up our language by as much a 50%, since it's so
//! heavily used in the inner "handle_op" method of the VM, and bypasses all the usual time-consuming Rust
//! guardrails like type safety and bounds checking.

use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut};
use std::ptr;
use std::slice;

const MAX: usize = 1024;

pub struct Inline<T> {
  inline: MaybeUninit<[T; MAX]>,
  len: usize
}

impl<T> Inline<T> {
  pub fn new() -> Inline<T> { Inline { inline: MaybeUninit::uninit(), len: 0 } }

  #[inline]
  pub fn len(&self) -> usize { self.len }

  #[inline]
  pub fn drop(&mut self) { drop(self.pop()); }

  #[inline]
  pub fn truncate(&mut self, len: usize) {
    while self.len > len {
      self.drop();
    }
  }

  #[inline]
  pub fn push(&mut self, v: T) {
    unsafe {
      let myptr = self.inline.as_mut_ptr() as *mut T;
      ptr::write(myptr.add(self.len), v);
      self.len += 1;
    }
  }

  #[inline]
  pub fn append_vec(&mut self, mut vs: Vec<T>) {
    unsafe {
      let other: *const [T] = vs.as_slice();
      let count = (*other).len();
      let len = self.len();
      ptr::copy_nonoverlapping(other as *const T, (self.inline.as_mut_ptr() as *mut T).add(len), count);
      self.len += count;
      vs.set_len(0);
    }
  }

  #[inline]
  pub fn pop(&mut self) -> T {
    unsafe {
      let myptr = self.inline.as_mut_ptr() as *const T;
      self.len -= 1;
      ptr::read(myptr.add(self.len))
    }
  }

  #[inline]
  pub fn get(&self, i: usize) -> &T {
    unsafe {
      let myptr = (self.inline.as_ptr() as *const T).add(i);
      &*myptr
    }
  }

  #[inline]
  pub fn last(&self) -> &T { self.get(self.len - 1) }

  #[inline]
  pub fn get_mut(&mut self, i: usize) -> &mut T {
    unsafe {
      let myptr = (self.inline.as_mut_ptr() as *mut T).add(i);
      &mut *myptr
    }
  }

  #[inline]
  pub fn swap_remove(&mut self, index: usize) -> T {
    let len = self.len;
    self.swap(len - 1, index);
    self.pop()
  }
}

impl<T> Deref for Inline<T> {
  type Target = [T];

  #[inline]
  fn deref(&self) -> &[T] {
    unsafe {
      let myptr = self.inline.as_ptr() as *const T;
      slice::from_raw_parts(myptr, self.len)
    }
  }
}

impl<T> DerefMut for Inline<T> {
  #[inline]
  fn deref_mut(&mut self) -> &mut [T] {
    unsafe {
      let myptr = self.inline.as_mut_ptr() as *mut T;
      slice::from_raw_parts_mut(myptr, self.len)
    }
  }
}

#[cfg(test)]
mod test {
  // TODO(later) : definitely needs tests
}
