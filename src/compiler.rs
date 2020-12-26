//! The alchem compiler.

use crate::common::{Chunk, Function, Instr, Opcode, Upval};
use crate::errors::Error;
use crate::scanner::{Scanner, Token, TokenType, TokenTypeDiscr};
use crate::value::Declared;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::str::FromStr;

const MAX_LOCALS: usize = 255;
type Jump = usize;

lazy_static! {
  static ref RULES: HashMap<TokenTypeDiscr, Rule> = construct_rules();
}

pub fn compile(source: &str) -> Function {
  let scanner = Scanner::new(source);
  let compiler = Compiler::new(scanner);
  compiler.compile()
}

struct Compiler<'s> {
  current: Token<'s>,
  previous: Token<'s>,
  last_line: usize,
  scanner: Scanner<'s>,
  had_error: bool,
  panic_mode: bool,
  scope: Vec<Scope>
}

impl<'s> Compiler<'s> {
  pub fn new(scanner: Scanner<'s>) -> Compiler<'s> {
    let mut compiler = Compiler {
      current: Token::new(TokenType::Bof, 0),
      previous: Token::new(TokenType::Bof, 0),
      last_line: 0,
      scanner,
      had_error: false,
      panic_mode: false,
      scope: vec![Scope::new()]
    };
    compiler.advance();
    compiler
  }

  pub fn compile(mut self) -> Function {
    self.begin_scope();
    self.body();
    self.emit_instr(Opcode::Return);

    if self.had_error {
      panic!("previous errors.");
    }
    self.end_compiler()
  }

  fn advance(&mut self) {
    let next = self.next_token();
    #[cfg(feature = "verbose")]
    {
      println!("Got next token: {:?}", next);
    }
    self.previous = std::mem::replace(&mut self.current, next);
    self.last_line = self.previous.line();
  }

  fn next_token(&mut self) -> Token<'s> {
    for token in &mut self.scanner {
      if token.is_error() {
        error_token(&mut self.had_error, &mut self.panic_mode, &token, "parse error");
      } else {
        return token;
      }
    }
    Token::new(TokenType::Eof, self.last_line)
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

  fn emit_instr(&mut self, code: Opcode) {
    let line = self.last_line;
    self.current_chunk().add_instr(Instr::new(code, line));
  }

  fn emit_jump(&mut self, code: Opcode) -> Jump {
    self.emit_instr(code);
    self.current_chunk().code_len() - 1
  }

  fn patch_jump(&mut self, offset: Jump) {
    let jump = (self.current_chunk().code_len() - offset - 1) as u16;
    self.current_chunk().patch_jump(offset, jump);
  }

  fn current_chunk(&mut self) -> &mut Chunk { self.top_scope_mut().function.chunk_mut() }

  fn end_compiler(mut self) -> Function {
    debug_assert_eq!(self.scope.len(), 1);
    let scope = self.pop_scope();
    debug_assert_eq!(scope.upvals.len(), 0);
    debug_assert_eq!(scope.function.upvals(), 0);
    scope.function
  }

  fn push_scope(&mut self) { self.scope.push(Scope::new()); }

  #[allow(clippy::let_and_return)]
  fn pop_scope(&mut self) -> Scope {
    let scope = self.scope.pop().unwrap();
    #[cfg(feature = "verbose")]
    if !self.had_error {
      println!("\nCompiled code ({}):", scope.function().smart_name());
      scope.function.chunk().debug();
    }
    scope
  }

  fn body(&mut self) {
    loop {
      match self.current_ttd() {
        TokenTypeDiscr::Equals => {
          self.advance();
          self.expression();
          break;
        }
        TokenTypeDiscr::Eof => break,
        _ => {
          self.assignment();
          if self.panic_mode {
            self.synchronize();
          }
        }
      }
    }
  }

  fn block_content(&mut self) {
    self.begin_scope();
    self.body();
    self.end_scope();
  }

  fn begin_scope(&mut self) { self.top_scope_mut().incr_depth(); }

  fn end_scope(&mut self) {
    self.top_scope_mut().decr_depth();

    let drained = self.top_scope_mut().drain();
    self.emit_instr(Opcode::RotateUp(drained.len() + 1));
    for val in drained {
      if val.is_captured() {
        self.emit_instr(Opcode::CloseUpvalue);
      } else {
        self.emit_instr(Opcode::Pop);
      }
    }
  }

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

  fn assignment(&mut self) {
    if self.token_check(TokenTypeDiscr::Identifier) {
      self.parse_variable();
      self.consume(TokenTypeDiscr::Equals);
      self.expression();
      self.consume(TokenTypeDiscr::Semi);
      self.mark_initialized();
    } else {
      panic!("Unexpected assignment token {:?}", self.current)
    }
  }

  fn mark_initialized(&mut self) { self.top_scope_mut().mark_last_initialized(); }

  fn parse_variable(&mut self) {
    self.consume(TokenTypeDiscr::Identifier);
    if let TokenType::Identifier(s) = self.previous.token_type() {
      let name = s.to_string();
      self.declare_variable(name)
    } else {
      panic!("Unexpected token for parse variable: {:?}", self.previous)
    }
  }

  fn declare_variable(&mut self, name: String) {
    if self.top_scope().defined(&name) {
      panic!("Already defined variable {}", name);
    }
    self.add_local(name)
  }

  fn add_local(&mut self, name: String) {
    let local = Local::new(name);
    if self.top_scope().len() > MAX_LOCALS {
      panic!("Too many locals: {}", self.top_scope().len());
    }
    self.top_scope_mut().add_local(local);
  }

  fn expression(&mut self) { self.parse_precendence(Precedence::Or) }

  fn block(&mut self) {
    self.consume(TokenTypeDiscr::OpenCurl);
    self.block_content();
    self.consume(TokenTypeDiscr::CloseCurl);
  }

  fn literal(&mut self) {
    // `self.emit_value(to_value::<$t>($v)?)` doesn't work because of
    // https://github.com/rust-lang/rust/issues/56254
    macro_rules! emit_value {
      ($v:tt, $t:ty) => {{
        let to_valued = to_value::<$t>($v);
        self.emit_value(to_valued)
      }};
    }

    match self.previous.token_type() {
      TokenType::IntLit(v) => emit_value!(v, i32),
      TokenType::FloatLit(v) => emit_value!(v, f64),
      TokenType::StringLit(v) => emit_value!(v, String),
      TokenType::TrueLit => self.emit_value(Declared::Bool(true)),
      TokenType::FalseLit => self.emit_value(Declared::Bool(false)),
      other => panic!("Unexpected literal {:?}", other)
    }
  }

  fn variable(&mut self) { self.named_variable() }

  fn named_variable(&mut self) {
    if let TokenType::Identifier(s) = self.previous.token_type() {
      let name = s.to_string();
      if let Some(c) = self.resolve_local(&name) {
        self.emit_instr(Opcode::GetLocal(c));
      } else if let Some(u) = self.resolve_upval(&name) {
        self.emit_instr(Opcode::GetUpval(u));
      } else {
        let g = self.add_constant(name.into());
        self.emit_instr(Opcode::GetGlobal(g));
      }
    } else {
      panic!("Unexpected token for named variable {:?}", self.previous)
    }
  }

  fn resolve_local(&self, name: &str) -> Option<usize> { self.top_scope().resolve_local(name) }

  fn resolve_upval(&mut self, name: &str) -> Option<usize> {
    let scope_ind = self.scope.len() - 1;
    self.resolve_upval_recurse(name, scope_ind)
  }

  fn resolve_upval_recurse(&mut self, name: &str, scope_ind: usize) -> Option<usize> {
    if scope_ind == 0 {
      return None;
    }

    let prev_local = self.scope_at(scope_ind - 1).unwrap().resolve_local(name);
    if let Some(i) = prev_local {
      self.scope_at_mut(scope_ind - 1).unwrap().locals_mut()[i].set_captured(true);
      return Some(self.scope_at_mut(scope_ind).unwrap().add_upval(i, true));
    }

    let prev_upval = self.resolve_upval_recurse(name, scope_ind - 1);
    if let Some(i) = prev_upval {
      return Some(self.scope_at_mut(scope_ind).unwrap().add_upval(i, false));
    }

    None
  }

  fn grouping(&mut self) {
    self.expression();
    self.consume(TokenTypeDiscr::CloseParen);
  }

  fn if_block(&mut self) {
    self.expression();
    let false_jump = self.emit_jump(Opcode::initial_jump_if_false());
    self.emit_instr(Opcode::Pop);
    self.block();
    let done_jump = self.emit_jump(Opcode::initial_jump());
    self.patch_jump(false_jump);
    self.emit_instr(Opcode::Pop);

    let mut d2_jumps = Vec::new();
    while let TokenTypeDiscr::ElseifWord = self.current_ttd() {
      self.consume(TokenTypeDiscr::ElseifWord);
      self.expression();
      let f2_jump = self.emit_jump(Opcode::initial_jump_if_false());
      self.emit_instr(Opcode::Pop);
      self.block();
      d2_jumps.push(self.emit_jump(Opcode::initial_jump()));
      self.patch_jump(f2_jump);
      self.emit_instr(Opcode::Pop);
    }

    self.consume(TokenTypeDiscr::ElseWord);
    self.block();
    self.patch_jump(done_jump);
    for jump in d2_jumps {
      self.patch_jump(jump);
    }
  }

  fn fn_sync(&mut self) {
    self.push_scope();
    self.begin_scope();

    self.consume(TokenTypeDiscr::OpenParen);
    let mut separated = true;
    while self.current_ttd() != TokenTypeDiscr::CloseParen {
      let arity = self.scope_function().arity();
      if !separated {
        panic!("Missing comma after parameter {}.", arity - 1);
      }
      if arity == 255 {
        panic!("More than {} parameters.", self.scope_function().arity())
      }
      self.scope_function_mut().incr_arity();
      self.parse_variable();
      self.mark_initialized();

      separated = false;
      if self.current_ttd() == TokenTypeDiscr::Comma {
        self.consume(TokenTypeDiscr::Comma);
        separated = true;
      }
    }
    self.consume(TokenTypeDiscr::CloseParen);
    self.consume(TokenTypeDiscr::OpenCurl);
    self.body();
    self.consume(TokenTypeDiscr::CloseCurl);
    self.emit_instr(Opcode::Return);

    let scope = self.pop_scope();
    self.emit_closure(scope);
  }

  fn call(&mut self) {
    let arg_count = self.argument_list();
    self.emit_instr(Opcode::Call(arg_count));
  }

  fn argument_list(&mut self) -> u8 {
    let mut count = 0;
    let mut separated = true;
    while self.current_ttd() != TokenTypeDiscr::CloseParen {
      if count == 255 {
        panic!("More than {} function arguments.", count);
      }
      if !separated {
        panic!("Missing comma after argument.");
      }
      self.expression();
      count += 1;

      separated = false;
      if self.current_ttd() == TokenTypeDiscr::Comma {
        self.consume(TokenTypeDiscr::Comma);
        separated = true;
      }
    }
    self.consume(TokenTypeDiscr::CloseParen);
    count
  }

  fn unary(&mut self) {
    let ttd = self.previous_ttd();
    self.parse_precendence(Precedence::Unary);

    match ttd {
      TokenTypeDiscr::Minus => self.emit_instr(Opcode::Negate),
      TokenTypeDiscr::Bang => self.emit_instr(Opcode::Not),
      other => panic!("Unexpected unary op: {:?}", other)
    }
  }

  fn binary(&mut self) {
    let ttd = self.previous_ttd();
    let precedence = self.get_rule(ttd).precedence().up();

    self.parse_precendence(precedence);

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
  }

  fn and(&mut self) {
    let end_jump = self.emit_jump(Opcode::initial_jump_if_false());
    self.emit_instr(Opcode::Pop);
    self.parse_precendence(Precedence::And);
    self.patch_jump(end_jump);
  }

  fn or(&mut self) {
    let else_jump = self.emit_jump(Opcode::initial_jump_if_false());
    let end_jump = self.emit_jump(Opcode::initial_jump());
    self.patch_jump(else_jump);
    self.emit_instr(Opcode::Pop);
    self.parse_precendence(Precedence::Or);
    self.patch_jump(end_jump)
  }

  fn get_rule(&self, tt: TokenTypeDiscr) -> &Rule { &RULES[&tt] }

  fn previous_rule(&self) -> &Rule { self.get_rule(self.previous_ttd()) }
  fn current_rule(&self) -> &Rule { self.get_rule(self.current_ttd()) }
  fn previous_ttd(&self) -> TokenTypeDiscr { self.previous.token_type().discr() }
  fn current_ttd(&self) -> TokenTypeDiscr { self.current.token_type().discr() }

  fn emit_value(&mut self, v: Declared) {
    let cc = self.add_constant(v);
    self.emit_instr(Opcode::Constant(cc));
  }

  fn emit_closure(&mut self, s: Scope) {
    let Scope { upvals, function, .. } = s;
    let cc = self.add_constant(function.into());
    self.emit_instr(Opcode::Closure(cc, upvals));
  }

  fn add_constant(&mut self, v: Declared) -> usize { self.current_chunk().add_constant(v) }

  fn parse_precendence(&mut self, prec: Precedence) {
    self.advance();

    if let Some(prefix) = self.previous_rule().prefix() {
      prefix(self);
    } else {
      panic!("Expected expression.");
    }

    while prec <= self.current_rule().precedence() {
      self.advance();
      (self.previous_rule().infix().unwrap())(self);
    }
  }

  fn top_scope_mut(&mut self) -> &mut Scope { self.scope.last_mut().unwrap() }
  fn top_scope(&self) -> &Scope { self.scope.last().unwrap() }
  fn scope_at(&self, ind: usize) -> Option<&Scope> { self.scope.get(ind) }
  fn scope_at_mut(&mut self, ind: usize) -> Option<&mut Scope> { self.scope.get_mut(ind) }
  fn scope_function(&self) -> &Function { self.scope.last().unwrap().function() }
  fn scope_function_mut(&mut self) -> &mut Function { self.scope.last_mut().unwrap().function_mut() }
}

struct Scope {
  function: Function,
  locals: Vec<Local>,
  scope_depth: u16,
  upvals: Vec<Upval>
}

impl Scope {
  pub fn new() -> Scope {
    Scope { function: Function::new(), locals: vec![Local::new(String::new())], scope_depth: 0, upvals: Vec::new() }
  }

  pub fn locals(&self) -> &[Local] { &self.locals }
  pub fn locals_mut(&mut self) -> &mut Vec<Local> { &mut self.locals }
  pub fn incr_depth(&mut self) { self.scope_depth += 1; }
  pub fn decr_depth(&mut self) { self.scope_depth -= 1; }
  pub fn add_local(&mut self, local: Local) { self.locals.push(local); }
  pub fn len(&self) -> usize { self.locals.len() }
  pub fn initialized(&self, i: usize) -> bool { self.locals[i].depth() > 0 }
  pub fn function(&self) -> &Function { &self.function }
  pub fn function_mut(&mut self) -> &mut Function { &mut self.function }

  pub fn upvals(&self) -> &[Upval] { &self.upvals }
  pub fn upvals_len(&self) -> usize { self.upvals.len() }

  pub fn push_upval(&mut self, upval: Upval) {
    self.upvals.push(upval);
    self.function.incr_upvals();
  }

  pub fn mark_last_initialized(&mut self) { self.locals.last_mut().unwrap().depth = self.scope_depth; }

  pub fn add_upval(&mut self, index: usize, is_local: bool) -> usize {
    match self.upvals().iter().position(|v| v.index() == index && v.is_local() == is_local) {
      Some(p) => p,
      None => {
        self.push_upval(Upval::new(index, is_local));
        self.upvals_len() - 1
      }
    }
  }

  pub fn drain(&mut self) -> Vec<Local> {
    if let Some(p) = self.locals.iter().position(|l| l.depth() > self.scope_depth) {
      self.locals.split_off(p)
    } else {
      Vec::new()
    }
  }

  pub fn defined(&self, name: &str) -> bool {
    self.locals.iter().rev().take_while(|l| l.depth() >= self.scope_depth).any(|l| l.name() == name)
  }

  pub fn resolve_local(&self, name: &str) -> Option<usize> {
    let i = self.locals().iter().enumerate().rev().find(|(_, l)| l.name() == name).map(|(i, _)| i);
    match i {
      None => None,
      Some(i) => {
        if self.initialized(i) {
          Some(i)
        } else {
          panic!("Can't read local in its own initializer")
        }
      }
    }
  }
}

#[derive(Debug)]
struct Local {
  name: String,
  depth: u16,
  is_captured: bool
}

impl Local {
  pub fn new(name: String) -> Local { Local { name, depth: 0, is_captured: false } }
  pub fn name(&self) -> &str { &self.name }
  pub fn depth(&self) -> u16 { self.depth }
  pub fn is_captured(&self) -> bool { self.is_captured }
  pub fn set_captured(&mut self, cap: bool) { self.is_captured = cap }
}

fn variable(compiler: &mut Compiler) { compiler.variable() }
fn binary(compiler: &mut Compiler) { compiler.binary() }
fn and(compiler: &mut Compiler) { compiler.and() }
fn or(compiler: &mut Compiler) { compiler.or() }
fn unary(compiler: &mut Compiler) { compiler.unary() }
fn literal(compiler: &mut Compiler) { compiler.literal() }
fn grouping(compiler: &mut Compiler) { compiler.grouping() }
fn if_block(compiler: &mut Compiler) { compiler.if_block() }
fn fn_sync(compiler: &mut Compiler) { compiler.fn_sync() }
fn call(compiler: &mut Compiler) { compiler.call() }

fn to_value<V>(v: &str) -> Declared
where
  V: Into<Declared> + FromStr,
  Error: From<<V as FromStr>::Err>
{
  v.parse::<V>().map_err(Error::from).unwrap().into()
}

struct Rule {
  prefix: Option<for<'r, 's> fn(&'r mut Compiler<'s>)>,
  infix: Option<for<'r, 's> fn(&'r mut Compiler<'s>)>,
  precedence: Precedence
}

impl Rule {
  pub fn new(
    prefix: Option<for<'r, 's> fn(&'r mut Compiler<'s>)>, infix: Option<for<'r, 's> fn(&'r mut Compiler<'s>)>,
    precedence: Precedence
  ) -> Rule {
    Rule { prefix, infix, precedence }
  }

  pub fn prefix(&self) -> Option<fn(&mut Compiler)> { self.prefix }
  pub fn infix(&self) -> Option<fn(&mut Compiler)> { self.infix }
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
