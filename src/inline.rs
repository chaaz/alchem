//! An unsafe stack representation.
//!
//! Using this struct is **extremely unsafe**, since we are manipulating raw pointers with very little
//! understanding of what we're doing. Also, if we do anything stupid, like overflow or underflow, we will at
//! *best* panic, and more likely seg fault.
//!
//! Still, using this implementation instead of a vector speeds up our language by as much a 50%, since it's so
//! heavily used in the inner "handle_op" method of the VM, and bypasses all the usual time-consuming Rust
//! guardrails like type safety and bounds checking.

// Just use a Vec for now, to ensure we don't blow our stack.

use std::ops::{Deref, DerefMut};

pub struct Inline<T> {
  vals: Vec<T>
}

impl<T> Inline<T> {
  pub fn new() -> Inline<T> { Inline { vals: Vec::new() } }

  #[inline]
  pub fn len(&self) -> usize { self.vals.len() }

  #[inline]
  pub fn drop(&mut self) { drop(self.pop()); }

  #[inline]
  pub fn truncate(&mut self, len: usize) { self.vals.truncate(len); }

  #[inline]
  pub fn push(&mut self, v: T) { self.vals.push(v); }

  #[inline]
  pub fn pop(&mut self) -> T { self.vals.pop().unwrap() }

  #[inline]
  pub fn append_vec(&mut self, mut vs: Vec<T>) { self.vals.append(&mut vs); }

  #[inline]
  pub fn get(&self, i: usize) -> &T { self.vals.get(i).unwrap() }

  #[inline]
  pub fn get_mut(&mut self, i: usize) -> &mut T { self.vals.get_mut(i).unwrap() }

  #[inline]
  pub fn last(&self) -> &T { self.get(self.len() - 1) }

  #[inline]
  pub fn swap_remove(&mut self, index: usize) -> T { self.vals.swap_remove(index) }
}

impl<T> Deref for Inline<T> {
  type Target = [T];

  #[inline]
  fn deref(&self) -> &[T] { self.vals.deref() }
}

impl<T> DerefMut for Inline<T> {
  #[inline]
  fn deref_mut(&mut self) -> &mut [T] { self.vals.deref_mut() }
}

// use std::mem::MaybeUninit;
// use std::ops::{Deref, DerefMut};
// use std::ptr;
// use std::slice;
//
// const MAX: usize = 512;
//
// pub struct Inline<T> {
//   inline: MaybeUninit<[T; MAX]>,
//   len: usize
// }
//
// impl<T> Inline<T> {
//   pub fn new() -> Inline<T> { Inline { inline: MaybeUninit::uninit(), len: 0 } }
//
//   #[inline]
//   pub fn len(&self) -> usize { self.len }
//
//   #[inline]
//   pub fn drop(&mut self) { drop(self.pop()); }
//
//   #[inline]
//   pub fn truncate(&mut self, len: usize) {
//     while self.len > len {
//       self.drop();
//     }
//   }
//
//   #[inline]
//   pub fn push(&mut self, v: T) {
//     unsafe {
//       let myptr = self.inline.as_mut_ptr() as *mut T;
//       ptr::write(myptr.add(self.len), v);
//       self.len += 1;
//     }
//   }
//
//   #[inline]
//   pub fn append_vec(&mut self, mut vs: Vec<T>) {
//     unsafe {
//       let other: *const [T] = vs.as_slice();
//       let count = (*other).len();
//       let len = self.len();
//       ptr::copy_nonoverlapping(other as *const T, (self.inline.as_mut_ptr() as *mut T).add(len), count);
//       self.len += count;
//       vs.set_len(0);
//     }
//   }
//
//   #[inline]
//   pub fn pop(&mut self) -> T {
//     unsafe {
//       let myptr = self.inline.as_mut_ptr() as *const T;
//       self.len -= 1;
//       ptr::read(myptr.add(self.len))
//     }
//   }
//
//   #[inline]
//   pub fn get(&self, i: usize) -> &T {
//     unsafe {
//       let myptr = (self.inline.as_ptr() as *const T).add(i);
//       &*myptr
//     }
//   }
//
//   #[inline]
//   pub fn last(&self) -> &T { self.get(self.len - 1) }
//
//   #[inline]
//   pub fn get_mut(&mut self, i: usize) -> &mut T {
//     unsafe {
//       let myptr = (self.inline.as_mut_ptr() as *mut T).add(i);
//       &mut *myptr
//     }
//   }
//
//   #[inline]
//   pub fn swap_remove(&mut self, index: usize) -> T {
//     let len = self.len;
//     self.swap(len - 1, index);
//     self.pop()
//   }
// }
//
// impl<T> Deref for Inline<T> {
//   type Target = [T];
//
//   #[inline]
//   fn deref(&self) -> &[T] {
//     unsafe {
//       let myptr = self.inline.as_ptr() as *const T;
//       slice::from_raw_parts(myptr, self.len)
//     }
//   }
// }
//
// impl<T> DerefMut for Inline<T> {
//   #[inline]
//   fn deref_mut(&mut self) -> &mut [T] {
//     unsafe {
//       let myptr = self.inline.as_mut_ptr() as *mut T;
//       slice::from_raw_parts_mut(myptr, self.len)
//     }
//   }
// }

#[cfg(test)]
mod test {
  // TODO(later) : definitely needs tests
}
