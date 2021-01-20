//! The alchem compiler.

use crate::collapsed::collapse_function;
use crate::common::{Function, Globals, MorphIndex, Opcode, Upval, Extraction, ExtractionPart, KnownUpvals};
use crate::errors::Error;
use crate::scanner::{Scanner, Token, TokenType, TokenTypeDiscr};
use crate::scope::{Jump, ScopeLater, ScopeOne, ScopeStack, ScopeZero};
use crate::types::{DependsOn, Type, Object, Array, NoCustom, CustomType};
use crate::value::Declared;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::str::FromStr;
use std::sync::Arc;
use std::vec::IntoIter;
use std::marker::PhantomData;

lazy_static! {
  static ref RULES: HashMap<TokenTypeDiscr, Rule<NoCustom>> = construct_rules();
}

type Prefix<C> = Option<for<'r> fn(&'r mut Compiler<C>) -> Type<C>>;
type Infix<C> = Option<for<'r> fn(&'r mut Compiler<C>, &Type<C>) -> Type<C>>;

pub fn compile<C: CustomType + 'static>(
  source: &str, globals: &Globals<C>
) -> (ScopeZero<C>, Type<C>) {
  let mut scanner = Scanner::new(source);
  let compiler = Compiler::new(scanner.drain_into_iter(), globals);
  let (zero, stype) = compiler.compile();
  (zero, stype)
}

pub fn collapse_script<C: CustomType + 'static>(
  scope: ScopeZero<C>, stype: Type<C>, globals: Globals<C>
) -> (crate::collapsed::Function, usize, HashMap<String, crate::collapsed::Declared>) {
  let chunk = scope.into_chunk();
  let function = Function::script(chunk, stype);
  let globals = globals.into_iter().map(|(k, v)| (k, collapse_function(v))).collect();
  (crate::collapsed::Function::from_common(Arc::new(function)), 0, globals)
}

pub struct Compiler<'g, C>
where
  C: CustomType
{
  scanner: IntoIter<Token>,
  current: Token,
  previous: Token,
  had_error: bool,
  panic_mode: bool,
  scope: ScopeStack<C>,
  globals: &'g Globals<C>,
  rules: HashMap<TokenTypeDiscr, Rule<C>>,
  _target: PhantomData<fn() -> C>
}

impl<'g, C> Compiler<'g, C>
where
  C: CustomType + 'static
{
  pub fn new(scanner: IntoIter<Token>, globals: &'g Globals<C>) -> Compiler<'g, C> {
    let mut compiler = Compiler {
      current: Token::new(TokenType::Bof, 0),
      previous: Token::new(TokenType::Bof, 0),
      scanner,
      had_error: false,
      panic_mode: false,
      scope: ScopeStack::new(),
      globals,
      rules: construct_rules(),
      _target: PhantomData
    };
    compiler.advance();
    compiler.begin_scope();
    compiler
  }

  pub fn replay(
    pnames: Vec<String>, args: Vec<Type<C>>, code: IntoIter<Token>, known_upvals: KnownUpvals<C>,
    globals: &'g Globals<C>
  ) -> Compiler<'g, C> {
    let mut compiler = Compiler {
      current: Token::new(TokenType::Bof, 0),
      previous: Token::new(TokenType::Bof, 0),
      scanner: code,
      had_error: false,
      panic_mode: false,
      scope: ScopeStack::known(known_upvals),
      globals,
      rules: construct_rules(),
      _target: PhantomData
    };

    compiler.advance();
    compiler.begin_scope();

    assert_eq!(pnames.len(), args.len());
    for (p, a) in pnames.into_iter().zip(args.into_iter()) {
      compiler.declare_variable(p);
      compiler.mark_last_initialized(a);
    }

    compiler
  }

  pub fn compile(mut self) -> (ScopeZero<C>, Type<C>) {
    let btype = self.body();
    self.emit_instr(Opcode::Return);

    if self.had_error {
      panic!("previous errors.");
    }
    let zero = self.end_compiler();
    (zero, btype)
  }

  fn end_compiler(self) -> ScopeZero<C> {
    debug_assert_eq!(self.scope.len(), 1);
    self.pop_scope_zero()
  }

  fn advance(&mut self) {
    let next = self.next_token();
    #[cfg(feature = "verbose")]
    {
      println!("Got next token: {:?}", next);
    }
    let old = std::mem::replace(&mut self.previous, std::mem::replace(&mut self.current, next));
    self.scope.collect(old);
    self.scope.set_last_line(self.previous.line());
  }

  fn next_token(&mut self) -> Token {
    for token in &mut self.scanner {
      if token.is_error() {
        error_token(&mut self.had_error, &mut self.panic_mode, &token, "parse error");
      } else {
        return token;
      }
    }
    Token::new(TokenType::Eof, self.scope.last_line())
  }

  fn error_current(&mut self, msg: &str) { error_token(&mut self.had_error, &mut self.panic_mode, &self.current, msg); }

  fn consume(&mut self, expect: TokenTypeDiscr) {
    if !self.token_match(expect) {
      self.error_current(&format!("Expected {:?}", expect));
    }
  }

  fn token_match(&mut self, ttd: TokenTypeDiscr) -> bool {
    if self.token_check(ttd) {
      self.advance();
      true
    } else {
      false
    }
  }

  fn token_check(&self, ttd: TokenTypeDiscr) -> bool { self.current_ttd() == ttd }

  fn synchronize(&mut self) {
    self.panic_mode = false;

    while self.current_ttd() != TokenTypeDiscr::Eof {
      if self.previous_ttd() == TokenTypeDiscr::Semi {
        return;
      }

      match self.current_ttd() {
        TokenTypeDiscr::Identifier | TokenTypeDiscr::OpenSquare | TokenTypeDiscr::OpenCurl => return,
        _ => ()
      }

      self.advance();
    }
  }

  fn body(&mut self) -> Type<C> {
    loop {
      match self.current_ttd() {
        TokenTypeDiscr::Equals => {
          self.advance();
          break self.expression();
        }
        TokenTypeDiscr::Eof => panic!("unexpected EOF"),
        _ => {
          self.assignment();
          if self.panic_mode {
            self.synchronize();
          }
        }
      }
    }
  }

  fn block_content(&mut self) -> Type<C> {
    self.begin_scope();
    let btype = self.body();
    self.end_scope();
    btype
  }

  fn assignment(&mut self) {
    match self.current_ttd() {
      TokenTypeDiscr::Identifier => {
        let _name = self.parse_variable();
        self.consume(TokenTypeDiscr::Equals);
        let vtype = self.expression();
        self.consume(TokenTypeDiscr::Semi);
        self.mark_last_initialized(vtype);
      }
      TokenTypeDiscr::OpenSquare | TokenTypeDiscr::OpenCurl => {
        let destruct = self.destructure();
        self.consume(TokenTypeDiscr::Equals);
        let vtype = self.expression();
        self.consume(TokenTypeDiscr::Semi);
        let idents_len = destruct.idents_len();
        let (c, extraction) = self.extract(&destruct, vtype, idents_len, ExtractionPart::empty());
        assert_eq!(c, idents_len);
        assert_eq!(extraction.idents_len(), idents_len);
        self.emit_instr(Opcode::Extract(extraction))
      }
      other => panic!("Unexpected assignment token {:?}", other)
    }
  }

  fn destructure(&mut self) -> Destructure {
    match self.current_ttd() {
      TokenTypeDiscr::Identifier => {
        self.advance();
        if let TokenType::Identifier(s) = self.previous.token_type() {
          let s = s.to_string();
          self.declare_variable(s.clone());
          Destructure::Ident(s)
        } else {
          panic!("Should have been ident during destructure.");
        }
      }
      TokenTypeDiscr::OpenSquare => {
        self.advance();
        let mut d = Vec::new();
        self.handle_commas(TokenTypeDiscr::CloseSquare, 255, "array destructure", |this| {
          d.push(this.destructure());
        });
        Destructure::Array(d)
      }
      TokenTypeDiscr::OpenCurl => {
        self.advance();
        let mut d = HashMap::new();
        let mut ord = Vec::new();

        self.handle_commas(TokenTypeDiscr::CloseCurl, 255, "object destructure", |this| {
          this.consume(TokenTypeDiscr::Identifier);
          let name = this.previous.token_type().as_identifier().to_string();
          let sub = match this.current_ttd() {
            TokenTypeDiscr::Colon => {
              this.advance();
              this.destructure()
            }
            TokenTypeDiscr::Comma | TokenTypeDiscr::CloseCurl => {
              this.declare_variable(name.clone());
              Destructure::Ident(name.clone())
            }
            other => panic!("Bad follow token for map item: {:?}", other)
          };
          d.insert(name.clone(), sub);
          ord.push(name);
        });

        Destructure::Object(d, ord)
      }
      other => panic!("Unexpected token to extract: {:?}", other)
    }
  }

  fn extract(
    &mut self, destruct: &Destructure, dtype: Type<C>, at: usize, extr: ExtractionPart
  ) -> (usize, Extraction) {
    match destruct {
      Destructure::Ident(_) => {
        self.mark_initialized(at, dtype);
        (1, Extraction::single(extr))
      }
      Destructure::Array(v) => {
        let mut total = 0;
        let parts = v.iter().enumerate().flat_map(|(i, d)| {
          let sub_type = dtype.as_array().get(i).clone();
          let (counted, ex) = self.extract(d, sub_type, at - total, extr.push(i));
          total += counted;
          ex.into_parts().into_iter()
        }).collect();
        (total, Extraction::from_parts(parts))
      }
      Destructure::Object(m, ord) => {
        let mut total = 0;
        let parts = ord.iter().flat_map(|key| {
          let d = m.get(key).unwrap();
          let i = dtype.as_object().index_of(key).unwrap();
          let sub_type = dtype.as_object().get(key).clone();
          let (counted, ex) = self.extract(d, sub_type, at - total, extr.push(i));
          total += counted;
          ex.into_parts().into_iter()
        }).collect();
        (total, Extraction::from_parts(parts))
      }
    }
  }

  fn parse_variable(&mut self) -> String {
    self.consume(TokenTypeDiscr::Identifier);
    if let TokenType::Identifier(s) = self.previous.token_type() {
      let name = s.to_string();
      self.declare_variable(name.clone());
      name
    } else {
      panic!("Unexpected token for parse variable: {:?}", self.previous)
    }
  }

  fn block(&mut self) -> Type<C> {
    self.consume(TokenTypeDiscr::OpenCurl);
    let btype = self.block_content();
    self.consume(TokenTypeDiscr::CloseCurl);
    btype
  }

  fn expression(&mut self) -> Type<C> { self.parse_precendence(Precedence::Or) }

  fn array(&mut self) -> Type<C> {
    let mut array = Array::new();

    self.handle_commas(TokenTypeDiscr::CloseSquare, 255, "array member", |this| {
      array.add(this.expression());
    });

    self.emit_instr(Opcode::Array(array.len()));
    Type::Array(Arc::new(array))
  }

  fn object(&mut self) -> Type<C> {
    let mut object = Object::new();
    let mut stack_order = Vec::new();

    self.handle_commas(TokenTypeDiscr::CloseCurl, 255, "object member", |this| {
      this.consume(TokenTypeDiscr::Identifier);
      let name = this.previous.token_type().as_identifier().to_string();
      let t = match this.current_ttd() {
        TokenTypeDiscr::Colon => {
          this.advance();
          this.expression()
        }
        TokenTypeDiscr::Comma | TokenTypeDiscr::CloseCurl => this.variable(),
        other => panic!("Bad follow token for map item: {:?}", other)
      };
      object.add(name.clone(), t);
      stack_order.push(name);
    });

    let order = stack_order.iter().map(|n| object.index_of(n).unwrap()).collect();
    self.emit_instr(Opcode::Object(order));
    Type::Object(Arc::new(object))
  }

  fn literal(&mut self) -> Type<C> {
    // `self.emit_value(to_value::<$t>($v)?)` doesn't work because of
    // https://github.com/rust-lang/rust/issues/56254
    macro_rules! emit_value {
      ($v:tt, $t:ty, $ty:expr) => {{
        let to_valued = to_value::<$t, _>($v);
        self.emit_value(to_valued, $ty)
      }};
    }

    match self.previous.token_type() {
      TokenType::IntLit(v) => emit_value!(v, i32, Type::Number),
      TokenType::FloatLit(v) => emit_value!(v, f64, Type::Number),
      TokenType::StringLit(v) => emit_value!(v, String, Type::String),
      TokenType::TrueLit => self.emit_value(Declared::Bool(true), Type::Bool),
      TokenType::FalseLit => self.emit_value(Declared::Bool(false), Type::Bool),
      other => panic!("Unexpected literal {:?}", other)
    }
  }

  fn variable(&mut self) -> Type<C> {
    if let TokenType::Identifier(s) = self.previous.token_type() {
      let name = s.to_string();
      if let Some((c, vtype)) = self.resolve_local(&name) {
        self.emit_instr(Opcode::GetLocal(c));
        vtype
      } else if let Some((u, utype)) = self.resolve_upval(&name) {
        self.emit_instr(Opcode::GetUpval(u));
        utype
      } else {
        let g = self.add_constant(name.clone().into());
        self.emit_instr(Opcode::GetGlobal(g));

        // assume all globals are natives.
        let f = self.globals.get(&name).unwrap_or_else(|| panic!("No global type \"{}\".", name));
        Type::FnSync(Arc::downgrade(f))
      }
    } else {
      panic!("Unexpected token for named variable {:?}", self.previous)
    }
  }

  fn grouping(&mut self) -> Type<C> {
    let outtype = self.expression();
    self.consume(TokenTypeDiscr::CloseParen);
    outtype
  }

  fn if_block(&mut self) -> Type<C> {
    let test_type = self.expression();
    assert!(self.scope.len() > 1 || test_type != Type::Unset);
    assert!(!test_type.is_known() || test_type == Type::Bool);
    let false_jump = self.emit_jump(Opcode::initial_jump_if_false());
    self.emit_instr(Opcode::Pop);
    let mut b1_type = self.block();
    assert!(self.scope.len() > 1 || b1_type != Type::Unset);
    let done_jump = self.emit_jump(Opcode::initial_jump());
    self.patch_jump(false_jump);
    self.emit_instr(Opcode::Pop);

    let mut d2_jumps = Vec::new();
    while let TokenTypeDiscr::ElseifWord = self.current_ttd() {
      self.consume(TokenTypeDiscr::ElseifWord);
      let in_test_type = self.expression();
      assert!(self.scope.len() > 1 || in_test_type != Type::Unset);
      assert!(!in_test_type.is_known() || in_test_type == Type::Bool);
      let f2_jump = self.emit_jump(Opcode::initial_jump_if_false());
      self.emit_instr(Opcode::Pop);
      let bx_type = self.block();
      assert!(self.scope.len() > 1 || bx_type != Type::Unset);
      assert!(!b1_type.is_known() || !bx_type.is_known() || b1_type == bx_type);
      if self.scope.len() == 1 {
        if b1_type.is_depends() && bx_type.is_depends() {
          b1_type = b1_type.or_depends(bx_type);
        } else if !bx_type.is_depends() {
          b1_type = bx_type;
        }
      }
      d2_jumps.push(self.emit_jump(Opcode::initial_jump()));
      self.patch_jump(f2_jump);
      self.emit_instr(Opcode::Pop);
    }

    self.consume(TokenTypeDiscr::ElseWord);
    let b2_type = self.block();
    assert!(self.scope.len() > 1 || b2_type != Type::Unset);
    assert!(!b1_type.is_known() || !b2_type.is_known() || b1_type == b2_type);
    if self.scope.len() == 1 {
      if b1_type.is_depends() && b2_type.is_depends() {
        b1_type = b1_type.or_depends(b2_type);
      } else if !b2_type.is_depends() {
        b1_type = b2_type;
      }
    }
    self.patch_jump(done_jump);
    for jump in d2_jumps {
      self.patch_jump(jump);
    }

    b1_type
  }

  fn fn_sync(&mut self) -> Type<C> {
    self.push_scope();
    self.begin_scope();

    self.consume(TokenTypeDiscr::OpenParen);
    let mut arity: u8 = 0;
    let mut param_names = Vec::new();

    self.handle_commas(TokenTypeDiscr::CloseParen, 255, "parameter", |this| {
      arity += 1;
      param_names.push(this.parse_variable());
      this.mark_last_initialized(Type::Unset);
    });

    self.consume(TokenTypeDiscr::OpenCurl);
    self.scope.start_collecting();
    self.body();
    self.consume(TokenTypeDiscr::CloseCurl);
    self.emit_instr(Opcode::Return);

    if self.scope.len() == 2 {
      let (scope_one, code) = self.pop_scope_one();
      let (upvals, known_upvals) = scope_one.into_upvals();
      let function = Function::new_alchem(arity, param_names, code, known_upvals);
      let function = self.emit_closure(upvals, function);
      Type::FnSync(Arc::downgrade(&function))
    } else {
      self.pop_scope_later();
      Type::Unset
    }
  }

  fn call(&mut self, intype: &Type<C>) -> Type<C> {
    let args = self.argument_list();

    if self.scope.len() == 1 {
      let function = intype.as_function();
      let args_len = args.len();
      assert!(args_len < 256);
      let args_len: u8 = args_len as u8;

      // thesis: because of scope boundries, it is impossible to compile a function call in which the function
      // does not exist, so we can safely upgrade the function pointer.
      let function = function.upgrade().unwrap();
      let (inst_ind, ftype) = function.find_or_build(args, self.globals);

      match ftype {
        Some(ftype) => {
          self.emit_instr(Opcode::Call(inst_ind, args_len));
          ftype
        }
        _ => {
          let index = MorphIndex::weak(&function, inst_ind);
          self.scope.link_build_depends(&index);
          Type::DependsOn(DependsOn::unit(index))
        }
      }
    } else {
      Type::Unset
    }
  }

  fn argument_list(&mut self) -> Vec<Type<C>> {
    let mut list = Vec::new();
    self.handle_commas(TokenTypeDiscr::CloseParen, 255, "argument", |this| {
      list.push(this.expression());
    });
    list
  }

  fn unary(&mut self) -> Type<C> {
    let ttd = self.previous_ttd();
    let outtype = self.parse_precendence(Precedence::Unary);

    match ttd {
      TokenTypeDiscr::Minus => {
        assert!(!outtype.is_known() || outtype == Type::Number);
        self.emit_instr(Opcode::Negate);
      }
      TokenTypeDiscr::Bang => {
        assert!(!outtype.is_known() || outtype == Type::Bool);
        self.emit_instr(Opcode::Not);
      }
      other => panic!("Unexpected unary op: {:?}", other)
    }
    outtype
  }

  fn dot(&mut self, ltype: &Type<C>) -> Type<C> {
    self.advance();
    match self.previous.token_type() {
      TokenType::Identifier(s) => {
        let name = s.to_string();
        let ind = ltype.as_object().index_of(&name).unwrap_or_else(|| panic!("No such index for \"{}\".", name));
        self.emit_instr(Opcode::GetIndex(ind));
        ltype.as_object().get(&name).clone()
      }
      TokenType::IntLit(s) => {
        let ind = s.parse().unwrap();
        self.emit_instr(Opcode::GetIndex(ind));
        ltype.as_array().get(ind).clone()
      }
      other => panic!("Unknown dot argument {:?}.", other)
    }
  }

  fn indot(&mut self, intype: &Type<C>) -> Type<C> {
    self.advance();
    match self.previous.token_type() {
      TokenType::IntLit(s) => {
        let ind = s.parse().unwrap();
        self.emit_instr(Opcode::GetIndex(ind));
        self.consume(TokenTypeDiscr::CloseSquare);
        intype.as_array().get(ind).clone()
      }
      other => panic!("Index must be a literal integer, not {:?}.", other)
    }
  }

  fn binary(&mut self, ltype: &Type<C>) -> Type<C> {
    let ttd = self.previous_ttd();
    let precedence = self.get_rule(ttd).precedence().up();
    let mut rtype = self.parse_precendence(precedence);

    if self.scope.len() > 1 {
      return Type::Unset;
    }

    // At level 1, we can accept unknown types, but not unset or mismatched types.
    assert!(ltype != &Type::Unset);
    assert!(rtype != Type::Unset);
    assert!(!ltype.is_known() || !rtype.is_known() || ltype.is_json() || rtype.is_json() || ltype == &rtype);

    match ttd {
      TokenTypeDiscr::Minus
      | TokenTypeDiscr::Star
      | TokenTypeDiscr::Slash
      | TokenTypeDiscr::Percent
      | TokenTypeDiscr::Gt
      | TokenTypeDiscr::Lt
      | TokenTypeDiscr::Gte
      | TokenTypeDiscr::Lte => {
        assert!(!ltype.is_known() || ltype.is_json() || ltype == &Type::Number)
      }
      TokenTypeDiscr::Plus => {
        assert!(!ltype.is_known() || ltype.is_json() || ltype == &Type::Number || ltype == &Type::String)
      }
      TokenTypeDiscr::DoubleEq | TokenTypeDiscr::NotEq => {
        assert!(!ltype.is_known() || ltype.is_json() || ltype == &Type::Number || ltype == &Type::String || ltype == &Type::Bool)
      }
      _ => ()
    }

    match ttd {
      TokenTypeDiscr::DoubleEq
      | TokenTypeDiscr::NotEq
      | TokenTypeDiscr::Gt
      | TokenTypeDiscr::Lt
      | TokenTypeDiscr::Gte
      | TokenTypeDiscr::Lte => {
        if ltype.is_depends() && rtype.is_depends() {
          rtype = rtype.and_depends(ltype.clone());
        } else if ltype.is_depends() {
          rtype = ltype.clone();
        } else if !rtype.is_depends() && ltype.is_known() && rtype.is_known() && !ltype.is_json() && !rtype.is_json() {
          rtype = Type::Bool;
        } else if !rtype.is_depends() && ltype.is_json() {
          rtype = Type::Json;
        }
      }
      _ => ()
    }

    match ttd {
      TokenTypeDiscr::Plus => self.emit_instr(Opcode::Add),
      TokenTypeDiscr::Minus => self.emit_instr(Opcode::Subtract),
      TokenTypeDiscr::Star => self.emit_instr(Opcode::Multiply),
      TokenTypeDiscr::Slash => self.emit_instr(Opcode::Divide),
      TokenTypeDiscr::Percent => self.emit_instr(Opcode::Mod),
      TokenTypeDiscr::Gt => self.emit_instr(Opcode::Gt),
      TokenTypeDiscr::Lt => self.emit_instr(Opcode::Lt),
      TokenTypeDiscr::Gte => self.emit_instr(Opcode::Gte),
      TokenTypeDiscr::Lte => self.emit_instr(Opcode::Lte),
      TokenTypeDiscr::DoubleEq => self.emit_instr(Opcode::Equals),
      TokenTypeDiscr::NotEq => self.emit_instr(Opcode::NotEquals),
      other => panic!("Unexpected binary op: {:?}", other)
    }

    rtype
  }

  fn and(&mut self, intype: &Type<C>) -> Type<C> {
    let end_jump = self.emit_jump(Opcode::initial_jump_if_false());
    self.emit_instr(Opcode::Pop);
    let outtype = self.parse_precendence(Precedence::And);
    assert_eq!(intype, &Type::Bool);
    assert_eq!(outtype, Type::Bool);
    self.patch_jump(end_jump);
    Type::Bool
  }

  fn or(&mut self, intype: &Type<C>) -> Type<C> {
    let else_jump = self.emit_jump(Opcode::initial_jump_if_false());
    let end_jump = self.emit_jump(Opcode::initial_jump());
    self.patch_jump(else_jump);
    self.emit_instr(Opcode::Pop);
    let outtype = self.parse_precendence(Precedence::Or);
    assert_eq!(intype, &Type::Bool);
    assert_eq!(outtype, Type::Bool);
    self.patch_jump(end_jump);
    Type::Bool
  }

  fn get_rule(&self, tt: TokenTypeDiscr) -> &Rule<C> { &self.rules[&tt] }
  fn previous_rule(&self) -> &Rule<C> { self.get_rule(self.previous_ttd()) }
  fn current_rule(&self) -> &Rule<C> { self.get_rule(self.current_ttd()) }
  fn previous_ttd(&self) -> TokenTypeDiscr { self.previous.token_type().discr() }
  fn current_ttd(&self) -> TokenTypeDiscr { self.current.token_type().discr() }

  fn parse_precendence(&mut self, prec: Precedence) -> Type<C> {
    self.advance();

    let prefix = self.previous_rule().prefix().expect("Expected prefix expression.");
    let mut outtype = prefix(self);

    while prec <= self.current_rule().precedence() {
      self.advance();
      outtype = (self.previous_rule().infix().unwrap())(self, &outtype);
    }

    outtype
  }

  // Defer to scope

  fn add_constant(&mut self, v: Declared<C>) -> usize { self.scope.add_constant(v) }
  fn emit_instr(&mut self, code: Opcode) { self.scope.emit_instr(code); }
  fn emit_value(&mut self, v: Declared<C>, vtype: Type<C>) -> Type<C> { self.scope.emit_value(v, vtype) }
  fn emit_closure(&mut self, u: Vec<Upval>, f: Function<C>) -> Arc<Function<C>> { self.scope.emit_closure(u, f) }
  fn emit_jump(&mut self, code: Opcode) -> Jump { self.scope.emit_jump(code) }
  fn patch_jump(&mut self, offset: Jump) { self.scope.patch_jump(offset); }
  fn declare_variable(&mut self, name: String) { self.scope.add_local(name); }
  fn mark_initialized(&mut self, ind: usize, vtype: Type<C>) { self.scope.mark_initialized(ind, vtype); }
  fn mark_last_initialized(&mut self, vtype: Type<C>) { self.scope.mark_last_initialized(vtype); }
  fn resolve_local(&mut self, name: &str) -> Option<(usize, Type<C>)> { self.scope.resolve_local(name) }
  fn resolve_upval(&mut self, name: &str) -> Option<(usize, Type<C>)> { self.scope.resolve_upval(name) }
  fn begin_scope(&mut self) { self.scope.begin_scope() }
  fn end_scope(&mut self) { self.scope.end_scope() }
  fn push_scope(&mut self) { self.scope.push_scope(); }
  fn pop_scope_zero(self) -> ScopeZero<C> { self.scope.pop_scope_zero() }
  fn pop_scope_one(&mut self) -> (ScopeOne<C>, Vec<Token>) { self.scope.pop_scope_one() }
  fn pop_scope_later(&mut self) -> ScopeLater<C> { self.scope.pop_scope_later() }

  fn handle_commas<F: FnMut(&mut Self)>(&mut self, closer: TokenTypeDiscr, max_elm: usize, descr_elm: &str, mut f: F) {
    let mut separated = true;
    let mut items = 0;
    while self.current_ttd() != closer {
      items += 1;
      if items >= max_elm {
        panic!("More than {} {}.", max_elm, descr_elm);
      }
      if !separated {
        panic!("Missing comma after {}.", descr_elm);
      }

      f(self);

      separated = false;
      if self.current_ttd() == TokenTypeDiscr::Comma {
        self.consume(TokenTypeDiscr::Comma);
        separated = true;
      }
    }
    self.consume(closer);
  }
}

#[derive(Debug)]
enum Destructure {
  Ident(String),
  Array(Vec<Destructure>),
  Object(HashMap<String, Destructure>, Vec<String>)
}

impl Destructure {
  fn idents_len(&self) -> usize {
    match self {
      Self::Ident(_) => 1,
      Self::Array(v) => v.iter().map(|d| d.idents_len()).sum(),
      Self::Object(m, _) => m.values().map(|d| d.idents_len()).sum()
    }
  }
}

fn variable<C: CustomType + 'static>(compiler: &mut Compiler<C>) -> Type<C> { compiler.variable() }
fn unary<C: CustomType + 'static>(compiler: &mut Compiler<C>) -> Type<C> { compiler.unary() }
fn literal<C: CustomType + 'static>(compiler: &mut Compiler<C>) -> Type<C> { compiler.literal() }
fn array<C: CustomType + 'static>(compiler: &mut Compiler<C>) -> Type<C> { compiler.array() }
fn object<C: CustomType + 'static>(compiler: &mut Compiler<C>) -> Type<C> { compiler.object() }
fn grouping<C: CustomType + 'static>(compiler: &mut Compiler<C>) -> Type<C> { compiler.grouping() }
fn if_block<C: CustomType + 'static>(compiler: &mut Compiler<C>) -> Type<C> { compiler.if_block() }
fn fn_sync<C: CustomType + 'static>(compiler: &mut Compiler<C>) -> Type<C> { compiler.fn_sync() }
fn binary<C: CustomType + 'static>(compiler: &mut Compiler<C>, intype: &Type<C>) -> Type<C> { compiler.binary(intype) }
fn indot<C: CustomType + 'static>(compiler: &mut Compiler<C>, intype: &Type<C>) -> Type<C> { compiler.indot(intype) }
fn dot<C: CustomType + 'static>(compiler: &mut Compiler<C>, intype: &Type<C>) -> Type<C> { compiler.dot(intype) }
fn and<C: CustomType + 'static>(compiler: &mut Compiler<C>, intype: &Type<C>) -> Type<C> { compiler.and(intype) }
fn or<C: CustomType + 'static>(compiler: &mut Compiler<C>, intype: &Type<C>) -> Type<C> { compiler.or(intype) }
fn call<C: CustomType + 'static>(compiler: &mut Compiler<C>, intype: &Type<C>) -> Type<C> { compiler.call(intype) }

fn to_value<V, C>(v: &str) -> Declared<C>
where
  V: Into<Declared<C>> + FromStr,
  Error: From<<V as FromStr>::Err>,
  C: CustomType
{
  v.parse::<V>().map_err(Error::from).unwrap().into()
}

struct Rule<C>
where
  C: CustomType
{
  prefix: Prefix<C>,
  infix: Infix<C>,
  precedence: Precedence
}

impl<C> Rule<C>
where
  C: CustomType
{
  pub fn new(prefix: Prefix<C>, infix: Infix<C>, precedence: Precedence) -> Rule<C> {
    Rule { prefix, infix, precedence }
  }

  pub fn prefix(&self) -> Prefix<C> { self.prefix }
  pub fn infix(&self) -> Infix<C> { self.infix }
  pub fn precedence(&self) -> Precedence { self.precedence }
}

fn error_token(had_error: &mut bool, panic_mode: &mut bool, token: &Token, msg: &str) {
  *had_error = true;
  if *panic_mode {
    return;
  }
  *panic_mode = true;
  println!("Parse error: {:?}: {}", token, msg);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
  None,
  Or,
  And,
  Equality,
  Comparison,
  Term,
  Factor,
  Unary,
  Call,
  Primary
}

impl Precedence {
  pub fn up(&self) -> Precedence {
    match self {
      Self::None => Self::Or,
      Self::Or => Self::And,
      Self::And => Self::Equality,
      Self::Equality => Self::Comparison,
      Self::Comparison => Self::Term,
      Self::Term => Self::Factor,
      Self::Factor => Self::Unary,
      Self::Unary => Self::Call,
      Self::Call => Self::Primary,
      Self::Primary => panic!("No precedence higher than primary.")
    }
  }
}

fn construct_rules<C: CustomType + 'static>() -> HashMap<TokenTypeDiscr, Rule<C>> {
  let mut rules = HashMap::new();

  rules.insert(TokenTypeDiscr::Bof, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::Eof, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::Comma, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::Equals, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::Semi, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::Colon, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::OpenCurl, Rule::new(Some(object), None, Precedence::None));
  rules.insert(TokenTypeDiscr::CloseCurl, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::OpenSquare, Rule::new(Some(array), Some(indot), Precedence::Call));
  rules.insert(TokenTypeDiscr::CloseSquare, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::DoubleAnd, Rule::new(None, Some(and), Precedence::And));
  rules.insert(TokenTypeDiscr::DoubleOr, Rule::new(None, Some(or), Precedence::Or));
  rules.insert(TokenTypeDiscr::Gt, Rule::new(None, Some(binary), Precedence::Comparison));
  rules.insert(TokenTypeDiscr::Lt, Rule::new(None, Some(binary), Precedence::Comparison));
  rules.insert(TokenTypeDiscr::Gte, Rule::new(None, Some(binary), Precedence::Comparison));
  rules.insert(TokenTypeDiscr::Lte, Rule::new(None, Some(binary), Precedence::Comparison));
  rules.insert(TokenTypeDiscr::DoubleEq, Rule::new(None, Some(binary), Precedence::Equality));
  rules.insert(TokenTypeDiscr::NotEq, Rule::new(None, Some(binary), Precedence::Equality));
  rules.insert(TokenTypeDiscr::Plus, Rule::new(None, Some(binary), Precedence::Term));
  rules.insert(TokenTypeDiscr::Minus, Rule::new(Some(unary), Some(binary), Precedence::Term));
  rules.insert(TokenTypeDiscr::Star, Rule::new(None, Some(binary), Precedence::Factor));
  rules.insert(TokenTypeDiscr::Slash, Rule::new(None, Some(binary), Precedence::Factor));
  rules.insert(TokenTypeDiscr::Percent, Rule::new(None, Some(binary), Precedence::Factor));
  rules.insert(TokenTypeDiscr::Bang, Rule::new(Some(unary), None, Precedence::Unary));
  rules.insert(TokenTypeDiscr::OpenParen, Rule::new(Some(grouping), Some(call), Precedence::Call));
  rules.insert(TokenTypeDiscr::CloseParen, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::Dot, Rule::new(None, Some(dot), Precedence::Call));
  rules.insert(TokenTypeDiscr::FnWord, Rule::new(Some(fn_sync), None, Precedence::None));
  rules.insert(TokenTypeDiscr::IfWord, Rule::new(Some(if_block), None, Precedence::None));
  rules.insert(TokenTypeDiscr::ElseifWord, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::ElseWord, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::TrueLit, Rule::new(Some(literal), None, Precedence::None));
  rules.insert(TokenTypeDiscr::FalseLit, Rule::new(Some(literal), None, Precedence::None));
  rules.insert(TokenTypeDiscr::IntLit, Rule::new(Some(literal), None, Precedence::None));
  rules.insert(TokenTypeDiscr::FloatLit, Rule::new(Some(literal), None, Precedence::None));
  rules.insert(TokenTypeDiscr::StringLit, Rule::new(Some(literal), None, Precedence::None));
  rules.insert(TokenTypeDiscr::Identifier, Rule::new(Some(variable), None, Precedence::None));
  rules.insert(TokenTypeDiscr::Error, Rule::new(None, None, Precedence::None));

  rules
}
