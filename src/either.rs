//! A sum type for different kinds of iterators.

pub enum IterEither2<A, B> {
  A(A),
  B(B)
}

impl<A, B> Iterator for IterEither2<A, B>
where
  A: Iterator,
  B: Iterator<Item = A::Item>
{
  type Item = A::Item;

  fn next(&mut self) -> Option<A::Item> {
    match self {
      IterEither2::A(a) => a.next(),
      IterEither2::B(b) => b.next()
    }
  }
}

pub enum IterEither3<A, B, C> {
  A(A),
  B(B),
  C(C)
}

impl<A, B, C> Iterator for IterEither3<A, B, C>
where
  A: Iterator,
  B: Iterator<Item = A::Item>,
  C: Iterator<Item = A::Item>
{
  type Item = A::Item;

  fn next(&mut self) -> Option<A::Item> {
    match self {
      IterEither3::A(a) => a.next(),
      IterEither3::B(b) => b.next(),
      IterEither3::C(c) => c.next()
    }
  }
}
