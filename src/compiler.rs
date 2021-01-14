//! The alchem compiler.

use crate::common::{Function, MorphIndex, Opcode, Upval};
use crate::errors::Error;
use crate::scanner::{Scanner, Token, TokenType, TokenTypeDiscr};
use crate::scope::{ScopeStack, ScopeZero, ScopeOne, ScopeLater, Jump};
use crate::types::{Type, DependsOn};
use crate::value::Declared;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::str::FromStr;
use std::sync::Arc;
use std::vec::IntoIter;

lazy_static! {
  static ref RULES: HashMap<TokenTypeDiscr, Rule> = construct_rules();
}

pub fn compile(source: &str) -> (ScopeZero, Type) {
  let mut scanner = Scanner::new(source);
  let compiler = Compiler::new(scanner.drain_into_iter());
  compiler.compile()
}

pub fn build_function_with_0args_from_scope0(scope: ScopeZero, stype: Type) -> (crate::collapsed::Function, usize) {
  let chunk = scope.into_chunk();
  let function = Function::script(chunk, stype);
  (crate::collapsed::Function::from_common(Arc::new(function)), 0)
}

pub struct Compiler {
  scanner: IntoIter<Token>,
  current: Token,
  previous: Token,
  had_error: bool,
  panic_mode: bool,
  scope: ScopeStack,
  globals: HashMap<String, Type>,
}

impl Compiler {
  pub fn new(scanner: IntoIter<Token>) -> Compiler {
    let mut compiler = Compiler {
      current: Token::new(TokenType::Bof, 0),
      previous: Token::new(TokenType::Bof, 0),
      scanner,
      had_error: false,
      panic_mode: false,
      scope: ScopeStack::new(),
      globals: HashMap::new()
    };
    compiler.advance();
    compiler.begin_scope();
    compiler
  }

  pub fn replay(
    pnames: Vec<String>, args: Vec<Type>, code: IntoIter<Token>, known_upvals: HashMap<String, (usize, Type)>
  ) -> Compiler {
    let mut compiler = Compiler {
      current: Token::new(TokenType::Bof, 0),
      previous: Token::new(TokenType::Bof, 0),
      scanner: code,
      had_error: false,
      panic_mode: false,
      scope: ScopeStack::known(known_upvals),
      globals: HashMap::new()
    };

    compiler.advance();
    compiler.begin_scope();

    assert_eq!(pnames.len(), args.len());
    for (p, a) in pnames.into_iter().zip(args.into_iter()) {
      compiler.declare_variable(p);
      compiler.mark_initialized(a);
    }

    compiler
  }

  pub fn compile(mut self) -> (ScopeZero, Type) {
    let btype = self.body();
    self.emit_instr(Opcode::Return);

    if self.had_error {
      panic!("previous errors.");
    }
    let zero = self.end_compiler();
    (zero, btype)
  }

  fn end_compiler(self) -> ScopeZero {
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

  fn body(&mut self) -> Type {
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

  fn block_content(&mut self) -> Type {
    self.begin_scope();
    let btype = self.body();
    self.end_scope();
    btype
  }

  fn assignment(&mut self) {
    if self.token_check(TokenTypeDiscr::Identifier) {
      let name = self.parse_variable();
      self.consume(TokenTypeDiscr::Equals);
      let vtype = self.expression();
      self.consume(TokenTypeDiscr::Semi);
      self.mark_initialized(vtype);
      println!("mark_initialized \"{}\".", name);
    } else {
      panic!("Unexpected assignment token {:?}", self.current)
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

  fn block(&mut self) -> Type {
    self.consume(TokenTypeDiscr::OpenCurl);
    let btype = self.block_content();
    self.consume(TokenTypeDiscr::CloseCurl);
    btype
  }

  fn expression(&mut self) -> Type { self.parse_precendence(Precedence::Or) }

  fn literal(&mut self) -> Type {
    // `self.emit_value(to_value::<$t>($v)?)` doesn't work because of
    // https://github.com/rust-lang/rust/issues/56254
    macro_rules! emit_value {
      ($v:tt, $t:ty, $ty:expr) => {{
        let to_valued = to_value::<$t>($v);
        self.emit_value(to_valued, $ty)
      }};
    }

    match self.previous.token_type() {
      TokenType::IntLit(v) => emit_value!(v, i32, Type::Int),
      TokenType::FloatLit(v) => emit_value!(v, f64, Type::Int),
      TokenType::StringLit(v) => emit_value!(v, String, Type::String),
      TokenType::TrueLit => self.emit_value(Declared::Bool(true), Type::Bool),
      TokenType::FalseLit => self.emit_value(Declared::Bool(false), Type::Bool),
      other => panic!("Unexpected literal {:?}", other)
    }
  }

  fn variable(&mut self) -> Type {
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
        self.globals.get(&name).unwrap_or_else(|| panic!("No global type \"{}\".", name)).clone()
      }
    } else {
      panic!("Unexpected token for named variable {:?}", self.previous)
    }
  }

  fn grouping(&mut self) -> Type {
    let outtype = self.expression();
    self.consume(TokenTypeDiscr::CloseParen);
    outtype
  }

  fn if_block(&mut self) -> Type {
    let test_type = self.expression();
    assert_eq!(test_type, Type::Bool);
    let false_jump = self.emit_jump(Opcode::initial_jump_if_false());
    self.emit_instr(Opcode::Pop);
    let b1_type = self.block();
    let done_jump = self.emit_jump(Opcode::initial_jump());
    self.patch_jump(false_jump);
    self.emit_instr(Opcode::Pop);

    let mut d2_jumps = Vec::new();
    while let TokenTypeDiscr::ElseifWord = self.current_ttd() {
      self.consume(TokenTypeDiscr::ElseifWord);
      let test_type = self.expression();
      assert_eq!(test_type, Type::Bool);
      let f2_jump = self.emit_jump(Opcode::initial_jump_if_false());
      self.emit_instr(Opcode::Pop);
      let bx_type = self.block();
      assert_eq!(b1_type, bx_type);
      d2_jumps.push(self.emit_jump(Opcode::initial_jump()));
      self.patch_jump(f2_jump);
      self.emit_instr(Opcode::Pop);
    }

    self.consume(TokenTypeDiscr::ElseWord);
    let b2_type = self.block();
    assert_eq!(b1_type, b2_type);
    self.patch_jump(done_jump);
    for jump in d2_jumps {
      self.patch_jump(jump);
    }

    b1_type
  }

  fn fn_sync(&mut self) -> Type {
    self.push_scope();
    self.begin_scope();

    self.consume(TokenTypeDiscr::OpenParen);
    let mut separated = true;
    let mut arity: u8 = 0;
    let mut param_names = Vec::new();
    while self.current_ttd() != TokenTypeDiscr::CloseParen {
      if !separated {
        panic!("Missing comma after parameter {}.", arity - 1);
      }
      if arity == 255 {
        panic!("More than {} parameters.", arity)
      }
      arity += 1;
      param_names.push(self.parse_variable());
      self.mark_initialized(Type::Unset);

      separated = false;
      if self.current_ttd() == TokenTypeDiscr::Comma {
        self.advance();
        separated = true;
      }
    }
    self.consume(TokenTypeDiscr::CloseParen);
    self.consume(TokenTypeDiscr::OpenCurl);
    self.scope.start_collecting();
    self.body();
    self.consume(TokenTypeDiscr::CloseCurl);
    self.emit_instr(Opcode::Return);

    println!("ending function at {}", self.scope.len());

    if self.scope.len() == 2 {

      println!("ending 2");

      let (scope_one, code) = self.pop_scope_one();
      let (upvals, known_upvals) = scope_one.into_upvals();
      println!("captured known_upvals: {:?}", known_upvals);
      let function = Function::new_alchem(arity, param_names, code, known_upvals);
      let function = self.emit_closure(upvals, function);
      Type::FnSync(Arc::downgrade(&function))
    } else {
      self.pop_scope_later();
      Type::Unset
    }
  }

  fn call(&mut self, intype: &Type) -> Type {
    let args = self.argument_list();

    if self.scope.len() == 1 {
      let function = intype.as_function();
      let args_len = args.len();
      assert!(args_len < 256);
      let args_len: u8 = args_len as u8;

      // thesis: because of scope boundries, it is impossible to compile a function call in which the function
      // does not exist, so we can safely upgrade the function pointer.
      let function = function.upgrade().unwrap();

      let (inst_ind, ftype) = function.find_known_type(&args).unwrap_or_else(|| {
        let inst_ind = function.reserve_inst(args);
        (inst_ind, function.replay(inst_ind))
      });

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

  fn argument_list(&mut self) -> Vec<Type> {
    let mut list = Vec::new();
    let mut separated = true;
    while self.current_ttd() != TokenTypeDiscr::CloseParen {
      if list.len() == 255 {
        panic!("More than {} function arguments.", list.len());
      }
      if !separated {
        panic!("Missing comma after argument.");
      }
      list.push(self.expression());

      separated = false;
      if self.current_ttd() == TokenTypeDiscr::Comma {
        self.consume(TokenTypeDiscr::Comma);
        separated = true;
      }
    }
    self.consume(TokenTypeDiscr::CloseParen);
    list
  }

  fn unary(&mut self) -> Type {
    let ttd = self.previous_ttd();
    let outtype = self.parse_precendence(Precedence::Unary);

    match ttd {
      TokenTypeDiscr::Minus => {
        assert_eq!(outtype, Type::Int);
        self.emit_instr(Opcode::Negate);
      }
      TokenTypeDiscr::Bang => {
        assert_eq!(outtype, Type::Bool);
        self.emit_instr(Opcode::Not);
      }
      other => panic!("Unexpected unary op: {:?}", other)
    }
    outtype
  }

  fn binary(&mut self, ltype: &Type) -> Type {
    let ttd = self.previous_ttd();
    let precedence = self.get_rule(ttd).precedence().up();
    let mut rtype = self.parse_precendence(precedence);

    assert_eq!(ltype, &rtype);

    match ttd {
      TokenTypeDiscr::Minus
      | TokenTypeDiscr::Star
      | TokenTypeDiscr::Slash
      | TokenTypeDiscr::Percent
      | TokenTypeDiscr::Gt
      | TokenTypeDiscr::Lt
      | TokenTypeDiscr::Gte
      | TokenTypeDiscr::Lte => {
        assert_eq!(ltype, &Type::Int)
      }
      TokenTypeDiscr::Plus => {
        assert!(ltype == &Type::Int || ltype == &Type::String)
      }
      TokenTypeDiscr::DoubleEq
      | TokenTypeDiscr::NotEq => {
        assert!(ltype == &Type::Int || ltype == &Type::String || ltype == &Type::Bool)
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
        rtype = Type::Bool;
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

  fn and(&mut self, intype: &Type) -> Type {
    let end_jump = self.emit_jump(Opcode::initial_jump_if_false());
    self.emit_instr(Opcode::Pop);
    let outtype = self.parse_precendence(Precedence::And);
    assert_eq!(intype, &Type::Bool);
    assert_eq!(outtype, Type::Bool);
    self.patch_jump(end_jump);
    Type::Bool
  }

  fn or(&mut self, intype: &Type) -> Type {
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

  fn get_rule(&self, tt: TokenTypeDiscr) -> &Rule { &RULES[&tt] }
  fn previous_rule(&self) -> &Rule { self.get_rule(self.previous_ttd()) }
  fn current_rule(&self) -> &Rule { self.get_rule(self.current_ttd()) }
  fn previous_ttd(&self) -> TokenTypeDiscr { self.previous.token_type().discr() }
  fn current_ttd(&self) -> TokenTypeDiscr { self.current.token_type().discr() }

  fn parse_precendence(&mut self, prec: Precedence) -> Type {
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

  fn add_constant(&mut self, v: Declared) -> usize { self.scope.add_constant(v) }
  fn emit_instr(&mut self, code: Opcode) { self.scope.emit_instr(code); }
  fn emit_value(&mut self, v: Declared, vtype: Type) -> Type { self.scope.emit_value(v, vtype) }
  fn emit_closure(&mut self, u: Vec<Upval>, f: Function) -> Arc<Function> { self.scope.emit_closure(u, f) }
  fn emit_jump(&mut self, code: Opcode) -> Jump { self.scope.emit_jump(code) }
  fn patch_jump(&mut self, offset: Jump) { self.scope.patch_jump(offset); }
  fn declare_variable(&mut self, name: String) { self.scope.add_local(name); }
  fn mark_initialized(&mut self, vtype: Type) { self.scope.mark_initialized(vtype); }
  fn resolve_local(&self, name: &str) -> Option<(usize, Type)> { self.scope.resolve_local(name) }
  fn resolve_upval(&mut self, name: &str) -> Option<(usize, Type)> { self.scope.resolve_upval(name) }
  fn begin_scope(&mut self) { self.scope.begin_scope() }
  fn end_scope(&mut self) { self.scope.end_scope() }
  fn push_scope(&mut self) { self.scope.push_scope(); }
  fn pop_scope_zero(self) -> ScopeZero { self.scope.pop_scope_zero() }
  fn pop_scope_one(&mut self) -> (ScopeOne, Vec<Token>) { self.scope.pop_scope_one() }
  fn pop_scope_later(&mut self) -> ScopeLater { self.scope.pop_scope_later() }
}

fn variable(compiler: &mut Compiler) -> Type { compiler.variable() }
fn unary(compiler: &mut Compiler) -> Type { compiler.unary() }
fn literal(compiler: &mut Compiler) -> Type { compiler.literal() }
fn grouping(compiler: &mut Compiler) -> Type { compiler.grouping() }
fn if_block(compiler: &mut Compiler) -> Type { compiler.if_block() }
fn fn_sync(compiler: &mut Compiler) -> Type { compiler.fn_sync() }
fn binary(compiler: &mut Compiler, intype: &Type) -> Type { compiler.binary(intype) }
fn and(compiler: &mut Compiler, intype: &Type) -> Type { compiler.and(intype) }
fn or(compiler: &mut Compiler, intype: &Type) -> Type { compiler.or(intype) }
fn call(compiler: &mut Compiler, intype: &Type) -> Type { compiler.call(intype) }

fn to_value<V>(v: &str) -> Declared
where
  V: Into<Declared> + FromStr,
  Error: From<<V as FromStr>::Err>
{
  v.parse::<V>().map_err(Error::from).unwrap().into()
}

struct Rule {
  prefix: Option<for<'r> fn(&'r mut Compiler) -> Type>,
  infix: Option<for<'r> fn(&'r mut Compiler, &Type) -> Type>,
  precedence: Precedence
}

impl Rule {
  pub fn new(
    prefix: Option<for<'r> fn(&'r mut Compiler) -> Type>,
    infix: Option<for<'r> fn(&'r mut Compiler, &Type) -> Type>,
    precedence: Precedence
  ) -> Rule {
    Rule { prefix, infix, precedence }
  }

  pub fn prefix(&self) -> Option<fn(&mut Compiler) -> Type> { self.prefix }
  pub fn infix(&self) -> Option<fn(&mut Compiler, &Type) -> Type> { self.infix }
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

fn construct_rules() -> HashMap<TokenTypeDiscr, Rule> {
  let mut rules = HashMap::new();

  rules.insert(TokenTypeDiscr::Bof, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::Eof, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::Comma, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::Equals, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::Semi, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::Colon, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::OpenCurl, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::CloseCurl, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::OpenSquare, Rule::new(None, None, Precedence::None));
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
  rules.insert(TokenTypeDiscr::Dot, Rule::new(None, Some(binary), Precedence::Call));
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
