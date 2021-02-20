//! Handling comma-separated lists in the compiler gets its own module, since it's so useful. The code below
//! allows for an optional trailing comma after the final item.
//!
//! We use an enum to capture all the different uses of commas, since genericising it into an async function
//! creates a big lifetime headache that I don't feel like dealing with.

use crate::common::{ExtractionPart, Opcode};
use crate::compiler::{Compiler, Destructure};
use crate::pick;
use crate::scanner::TokenTypeDiscr;
use crate::types::{Array, CustomType, Object, Type};
use std::collections::HashMap;
use std::vec::IntoIter;

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
  Args(Option<Vec<Destructure>>)
}

impl HandleCommas {
  pub fn prep<C: CustomType>(&self) -> CommasHandled<C> {
    match self {
      Self::Array => CommasHandled::Array(Array::new()),
      Self::DestrSquare => CommasHandled::DestrSquare(Vec::new()),
      Self::DestrCurl => CommasHandled::DestrCurl(HashMap::new(), Vec::new()),
      Self::Object => CommasHandled::Object(Object::new(), Vec::new()),
      Self::Params => CommasHandled::Params(0, Vec::new()),
      Self::Args(v) => CommasHandled::Args(0, v.clone().map(|v| v.into_iter()), Vec::new())
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
        let destruct = compiler.destructure().await;
        let idents_len = destruct.idents_len();
        compiler.extract(&destruct, Type::Unset, idents_len, ExtractionPart::empty(), true);
        param_names.push(destruct);
      }

      Self::Args(..) => {
        let (arity, dests, list) = handled.args_mut();
        *arity += 1;
        let etype = compiler.expression().await;
        if let Some(destruct) = dests.as_mut().map(|d| d.next().unwrap()) {
          let idents_len = destruct.idents_len();
          let (c, extract) = compiler.extract(&destruct, etype.clone(), idents_len, ExtractionPart::empty(), false);
          let all_types = destruct.types(etype);
          assert_eq!(c, idents_len);
          assert_eq!(extract.idents_len(), idents_len);
          compiler.emit_instr(Opcode::Extract(extract));
          list.extend(all_types);
        } else {
          list.push(etype);
        }
      }
    }
  }
}

#[derive(Debug)]
pub enum CommasHandled<C: CustomType> {
  Array(Array<C>),
  DestrSquare(Vec<Destructure>),
  DestrCurl(HashMap<String, Destructure>, Vec<String>),
  Object(Object<C>, Vec<String>),
  Params(u8, Vec<Destructure>),
  Args(u8, Option<IntoIter<Destructure>>, Vec<Type<C>>)
}

impl<C: CustomType + 'static> CommasHandled<C> {
  pub fn array_mut(&mut self) -> &mut Array<C> { pick!(self, Self::Array(a) => a, "Not an array: {:?}") }
  pub fn into_array(self) -> Array<C> { pick!(self, Self::Array(a) => a, "Not an array: {:?}") }

  pub fn destr_square_mut(&mut self) -> &mut Vec<Destructure> {
    pick!(self, Self::DestrSquare(a) => a, "Not a destr_square: {:?}")
  }

  pub fn into_destr_square(self) -> Vec<Destructure> {
    pick!(self, Self::DestrSquare(a) => a, "Not a destr_square: {:?}")
  }

  pub fn destr_curl_mut(&mut self) -> (&mut HashMap<String, Destructure>, &mut Vec<String>) {
    pick!(self, Self::DestrCurl(d, ord) => (d, ord), "Not an destr_curl: {:?}")
  }

  pub fn into_destr_curl(self) -> (HashMap<String, Destructure>, Vec<String>) {
    pick!(self, Self::DestrCurl(d, ord) => (d, ord), "Not an destr_curl: {:?}")
  }

  pub fn object_mut(&mut self) -> (&mut Object<C>, &mut Vec<String>) {
    pick!(self, Self::Object(d, ord) => (d, ord), "Not an object: {:?}")
  }

  pub fn into_object(self) -> (Object<C>, Vec<String>) {
    pick!(self, Self::Object(d, ord) => (d, ord), "Not an object: {:?}")
  }

  pub fn params_mut(&mut self) -> (&mut u8, &mut Vec<Destructure>) {
    pick!(self, Self::Params(a, ord) => (a, ord), "Not params: {:?}")
  }

  pub fn into_params(self) -> (u8, Vec<Destructure>) {
    pick!(self, Self::Params(a, ord) => (a, ord), "Not params: {:?}")
  }

  pub fn args_mut(&mut self) -> (&mut u8, &mut Option<IntoIter<Destructure>>, &mut Vec<Type<C>>) {
    pick!(self, Self::Args(a, d, t) => (a, d, t), "Not an destr_square: {:?}")
  }

  pub fn into_args(self) -> (u8, Option<IntoIter<Destructure>>, Vec<Type<C>>) {
    pick!(self, Self::Args(a, d, t) => (a, d, t), "Not an destr_square: {:?}")
  }
}
