//! Error handling is all based on `error-chain`.

use error_chain::error_chain;

error_chain! {
  foreign_links {
    Io(std::io::Error);
    ParseInt(std::num::ParseIntError);
    ParseFloat(std::num::ParseFloatError);
    Infallible(std::convert::Infallible);
  }

  errors {
    Compile(v: String) { description("compiler error"), display("compile error: '{}'", v) }
    Runtime(v: String) { description("runtime error"), display("runtime error: '{}'", v) }
    Internal(v: String) { description("internal error"), display("internal error: '{}'", v) }
  }

  skip_msg_variant
}

impl<'a, T: ?Sized> From<std::sync::TryLockError<std::sync::MutexGuard<'a, T>>> for Error {
  fn from(err: std::sync::TryLockError<std::sync::MutexGuard<'a, T>>) -> Error {
    Error::from_kind(ErrorKind::Internal(format!("Mutex try lock error {:?}", err)))
  }
}

#[macro_export]
macro_rules! err {
  ($k:tt, $($arg:tt)*) => (
    std::result::Result::Err($crate::errors::Error::from_kind($crate::errors::ErrorKind::$k(format!($($arg)*))))
  )
}

#[macro_export]
macro_rules! bad {
  ($k:tt, $($arg:tt)*) => ($crate::errors::Error::from_kind($crate::errors::ErrorKind::$k(format!($($arg)*))))
}

#[macro_export]
macro_rules! bail {
  ($($arg:tt)*) => (return ($crate::err!($($arg)*)))
}

// #[macro_export]
// macro_rules! pick {
//   ($s:expr, $( $p:pat => $e:expr ),*) => ( match $s { $( $p => $e ),* } );
// }

#[macro_export]
macro_rules! pick {
  ($s:expr, $p:pat => $e:expr, $er:expr) => {
    match $s {
      $p => $e,
      o => panic!($er, o)
    }
  };
}

#[macro_export]
macro_rules! pick_opt {
  ($s:expr, $p:pat => $e:expr) => {
    match $s {
      $p => std::option::Option::Some($e),
      _ => std::option::Option::None
    }
  };
}
