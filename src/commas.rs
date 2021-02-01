//! Handling comma-separated lists in the compiler gets its own module, since it's so useful. The code below
//! allows for an optional trailing comma after the final item.
//!
//! We use an enum to capture all the different uses of commas, since genericising it into an async function
//! creates a big lifetime headache that I don't feel like dealing with.

use crate::compiler::{Compiler, Destructure};
use crate::scanner::TokenTypeDiscr;
use crate::types::{Array, CustomType, Object, Type};
use std::collections::HashMap;

pub async fn handle_commas<C: CustomType + 'static>(
  compiler: &mut Compiler<'_, C>, closer: TokenTypeDiscr, max_elm: usize, descr_elm: &str, handler: HandleCommas
) -> CommasHandled<C> {
  let mut separated = true;
  let mut items = 0;
  let mut handled = handler.prep();

  while compiler.current_ttd() != closer {
    items += 1;
    if items >= max_elm {
      panic!("More than {} {}.", max_elm, descr_elm);
    }
    if !separated {
      panic!("Missing comma after {}.", descr_elm);
    }

    handler.handle(&mut handled, compiler).await;

    separated = false;
    if compiler.current_ttd() == TokenTypeDiscr::Comma {
      compiler.consume(TokenTypeDiscr::Comma);
      separated = true;
    }
  }
  compiler.consume(closer);
  handled
}

pub enum HandleCommas {
  Array,
  DestrSquare,
  DestrCurl,
  Object,
  Params,
  Args
}

impl HandleCommas {
  pub fn prep<C: CustomType>(&self) -> CommasHandled<C> {
    match self {
      Self::Array => CommasHandled::Array(Array::new()),
      Self::DestrSquare => CommasHandled::DestrSquare(Vec::new()),
      Self::DestrCurl => CommasHandled::DestrCurl(HashMap::new(), Vec::new()),
      Self::Object => CommasHandled::Object(Object::new(), Vec::new()),
      Self::Params => CommasHandled::Params(0, Vec::new()),
      Self::Args => CommasHandled::Args(Vec::new())
    }
  }

  pub async fn handle<C: CustomType>(&self, handled: &mut CommasHandled<C>, compiler: &mut Compiler<'_, C>) {
    match self {
      Self::Array => handled.array_mut().add(compiler.expression().await),
      Self::DestrSquare => handled.destr_square_mut().push(compiler.destructure().await),

      Self::DestrCurl => {
        let (d, ord) = handled.destr_curl_mut();

        compiler.consume(TokenTypeDiscr::Identifier);
        let name = compiler.previous_tt().as_identifier().to_string();
        let sub = match compiler.current_ttd() {
          TokenTypeDiscr::Colon => {
            compiler.advance();
            compiler.destructure().await
          }
          TokenTypeDiscr::Comma | TokenTypeDiscr::CloseCurl => {
            compiler.declare_variable(name.clone());
            Destructure::Ident(name.clone())
          }
          other => panic!("Bad follow token for map item: {:?}", other)
        };
        d.insert(name.clone(), sub);
        ord.push(name);
      }

      Self::Object => {
        let (object, stack_order) = handled.object_mut();

        compiler.consume(TokenTypeDiscr::Identifier);
        let name = compiler.previous_tt().as_identifier().to_string();
        let t = match compiler.current_ttd() {
          TokenTypeDiscr::Colon => {
            compiler.advance();
            compiler.expression().await
          }
          TokenTypeDiscr::Comma | TokenTypeDiscr::CloseCurl => compiler.variable().await,
          other => panic!("Bad follow token for map item: {:?}", other)
        };
        object.add(name.clone(), t);
        stack_order.push(name);
      }

      Self::Params => {
        let (arity, param_names) = handled.params_mut();
        *arity += 1;
        param_names.push(compiler.parse_variable());
        compiler.mark_last_initialized(Type::Unset);
      }

      Self::Args => {
        let list = handled.args_mut();
        list.push(compiler.expression().await);
      }
    }
  }
}

pub enum CommasHandled<C: CustomType> {
  Array(Array<C>),
  DestrSquare(Vec<Destructure>),
  DestrCurl(HashMap<String, Destructure>, Vec<String>),
  Object(Object<C>, Vec<String>),
  Params(u8, Vec<String>),
  Args(Vec<Type<C>>)
}

impl<C: CustomType + 'static> CommasHandled<C> {
  pub fn array_mut(&mut self) -> &mut Array<C> {
    match self {
      Self::Array(a) => a,
      _ => panic!("Not an array.")
    }
  }

  pub fn into_array(self) -> Array<C> {
    match self {
      Self::Array(a) => a,
      _ => panic!("Not an array.")
    }
  }

  pub fn destr_square_mut(&mut self) -> &mut Vec<Destructure> {
    match self {
      Self::DestrSquare(a) => a,
      _ => panic!("Not an destr_square.")
    }
  }

  pub fn into_destr_square(self) -> Vec<Destructure> {
    match self {
      Self::DestrSquare(a) => a,
      _ => panic!("Not an destr_square.")
    }
  }

  pub fn destr_curl_mut(&mut self) -> (&mut HashMap<String, Destructure>, &mut Vec<String>) {
    match self {
      Self::DestrCurl(d, ord) => (d, ord),
      _ => panic!("Not an destr_curl.")
    }
  }

  pub fn into_destr_curl(self) -> (HashMap<String, Destructure>, Vec<String>) {
    match self {
      Self::DestrCurl(d, ord) => (d, ord),
      _ => panic!("Not an destr_curl.")
    }
  }

  pub fn object_mut(&mut self) -> (&mut Object<C>, &mut Vec<String>) {
    match self {
      Self::Object(d, ord) => (d, ord),
      _ => panic!("Not an object.")
    }
  }

  pub fn into_object(self) -> (Object<C>, Vec<String>) {
    match self {
      Self::Object(d, ord) => (d, ord),
      _ => panic!("Not an object.")
    }
  }

  pub fn params_mut(&mut self) -> (&mut u8, &mut Vec<String>) {
    match self {
      Self::Params(a, ord) => (a, ord),
      _ => panic!("Not an object.")
    }
  }

  pub fn into_params(self) -> (u8, Vec<String>) {
    match self {
      Self::Params(a, ord) => (a, ord),
      _ => panic!("Not an object.")
    }
  }

  pub fn args_mut(&mut self) -> &mut Vec<Type<C>> {
    match self {
      Self::Args(a) => a,
      _ => panic!("Not an destr_square.")
    }
  }

  pub fn into_args(self) -> Vec<Type<C>> {
    match self {
      Self::Args(a) => a,
      _ => panic!("Not an destr_square.")
    }
  }
}
