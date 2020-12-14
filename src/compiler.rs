//! The alchem compiler.

use crate::errors::{Result, Error};
use crate::common::{Chunk, Instr, Opcode};
use crate::scanner::{Scanner, Token, TokenType, TokenTypeDiscr};
use crate::value::Value;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::str::FromStr;

type Jump = usize;

const MAX_CONSTANTS: usize = 255;
const MAX_LOCALS: usize = 255;

lazy_static! {
  static ref RULES: HashMap<TokenTypeDiscr, Rule> = construct_rules();
}

pub fn compile(source: &str) -> Result<Chunk> {
  let scanner = Scanner::new(source);

  let mut compiler = Compiler::new(scanner);
  compiler.compile()?;

  Ok(compiler.into_chunk())
}

pub struct Compiler<'s> {
  current: Token<'s>,
  previous: Token<'s>,
  last_line: usize,
  scanner: Scanner<'s>,
  had_error: bool,
  panic_mode: bool,
  target: Chunk,
  debug: bool,
  scope: LocalStack
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
      target: Chunk::new(),
      debug: true,
      scope: LocalStack::new()
    };
    compiler.advance();
    compiler
  }

  pub fn compile(&mut self) -> Result<()> {
    self.block_content()?;
    self.end_compiler();
    Ok(())
  }

  pub fn into_chunk(self) -> Chunk { self.target }

  pub fn advance(&mut self) {
    let next = self.next_token();
    if self.debug {
      println!("Got next token: {:?}", next);
    }
    self.previous = std::mem::replace(&mut self.current, next);
    self.last_line = self.previous.line();
  }

  pub fn next_token(&mut self) -> Token<'s> {
    for token in &mut self.scanner {
      if token.is_error() {
        error_token(&mut self.had_error, &mut self.panic_mode, &token, "parse error");
      } else {
        return token;
      }
    }
    Token::new(TokenType::Eof, self.last_line)
  }

  pub fn error_current(&mut self, msg: &str) {
    error_token(&mut self.had_error, &mut self.panic_mode, &self.current, msg);
  }

  pub fn consume(&mut self, expect: TokenTypeDiscr) {
    if !self.token_match(expect) {
      self.error_current(&format!("Expected {:?}", expect));
    }
  }

  pub fn token_match(&mut self, ttd: TokenTypeDiscr) -> bool {
    if self.token_check(ttd) {
      self.advance();
      true
    } else {
      false
    }
  }

  fn token_check(&self, ttd: TokenTypeDiscr) -> bool { self.current_ttd() == ttd }

  pub fn emit_instr(&mut self, code: Opcode) {
    let line = self.last_line;
    self.current_chunk().add_instr(Instr::new(code, line));
  }

  pub fn emit_jump(&mut self, code: Opcode) -> Jump {
    self.emit_instr(code);
    self.current_chunk().code_len() - 1
  }

  pub fn patch_jump(&mut self, offset: Jump) -> Result<()> {
    let jump = (self.current_chunk().code_len() - offset - 1) as u16;
    self.current_chunk().patch_jump(offset, jump)
  }

  pub fn current_chunk(&mut self) -> &mut Chunk { &mut self.target }

  pub fn end_compiler(&mut self) {
    self.emit_instr(Opcode::Return);
    if self.debug && !self.had_error {
      println!("Compiled code:");
      self.current_chunk().debug();
    }
  }

  pub fn body(&mut self) -> Result<()> {
    loop {
      match self.current_ttd() {
        TokenTypeDiscr::Equals | TokenTypeDiscr::PointLeft => {
          self.advance();
          self.expression()?;
          break;
        }
        TokenTypeDiscr::Eof => break,
        _ => {
          self.assignment()?;
          if self.panic_mode {
            self.synchronize();
          }
        }
      }
    }
    Ok(())
  }

  pub fn block_content(&mut self) -> Result<()> {
    self.begin_scope();
    self.body()?;
    self.end_scope();
    Ok(())
  }

  pub fn begin_scope(&mut self) {
    self.scope.incr_depth();
  }

  pub fn end_scope(&mut self) {
    self.scope.decr_depth();
    let drain_depth = self.scope.drain_depth();
    self.emit_instr(Opcode::Popout(drain_depth));
  }

  pub fn synchronize(&mut self) {
    self.panic_mode = false;

    while self.current_ttd() != TokenTypeDiscr::Eof {
      if self.previous_ttd() == TokenTypeDiscr::Semi {
        return;
      }

      match self.current_ttd() {
        TokenTypeDiscr::Identifier
          | TokenTypeDiscr::OpenSquare
          | TokenTypeDiscr::OpenCurl => return,
        _ => ()
      }

      self.advance();
    }
  }

  pub fn assignment(&mut self) -> Result<()> {
    match self.current_ttd() {
      // Temporary to allow checking for nested blocks. Blocks should actually be allowed as expressions, except
      // that opening an expression with '{' will compete with "object literal" expressions.
      TokenTypeDiscr::OpenCurl => {
        self.block()?;
        self.emit_instr(Opcode::Pop);
        Ok(())
      }
      _ => {
        if self.token_check(TokenTypeDiscr::Identifier) {
          self.parse_variable()?;
          self.consume(TokenTypeDiscr::Equals);
          self.expression()?;
          self.consume(TokenTypeDiscr::Semi);
          self.mark_initialized();
          Ok(())
        } else {
          err!(Compile, "Unexpected assignment token {:?}", self.current)
        }
      }
    }
  }

  pub fn mark_initialized(&mut self) {
    self.scope.mark_last_initialized();
  }

  pub fn parse_variable(&mut self) -> Result<()> {
    self.consume(TokenTypeDiscr::Identifier);
    if let TokenType::Identifier(s) = self.previous.token_type() {
      let name = s.to_string();
      self.declare_variable(name)
    } else {
      err!(Compile, "Unexpected token for parse variable: {:?}", self.previous)
    }
  }

  pub fn declare_variable(&mut self, name: String) -> Result<()> {
    if self.scope.defined(&name) {
      bail!(Runtime, "Already defined variable {}", name);
    }
    self.add_local(name)
  }

  pub fn add_local(&mut self, name: String) -> Result<()> {
    let local = Local::new(name);
    if self.scope.len() > MAX_LOCALS {
      bail!(Runtime, "Too many locals: {}", self.scope.len());
    }
    self.scope.add_local(local);
    Ok(())
  }

  pub fn expression(&mut self) -> Result<()> { self.parse_precendence(Precedence::Or) }

  pub fn block(&mut self) -> Result<()> {
    self.consume(TokenTypeDiscr::OpenCurl);
    self.block_content()?;
    self.consume(TokenTypeDiscr::CloseCurl);
    Ok(())
  }

  pub fn literal(&mut self) -> Result<()> {
    // `self.emit_value(to_value::<$t>($v)?)` doesn't work because of
    // https://github.com/rust-lang/rust/issues/56254
    macro_rules! emit_value {
      ($v:tt, $t:ty) => (
        { let to_valued = to_value::<$t>($v)?; self.emit_value(to_valued) }
      )
    }

    match self.previous.token_type() {
      TokenType::IntLit(v) => emit_value!(v, i32),
      TokenType::FloatLit(v) => emit_value!(v, f64),
      TokenType::StringLit(v) => emit_value!(v, String),
      TokenType::TrueLit => self.emit_value(Value::Bool(true)),
      TokenType::FalseLit => self.emit_value(Value::Bool(false)),
      other => bail!(Compile, "Unexpected literal {:?}", other)
    }
  }

  pub fn variable(&mut self) -> Result<()> { self.named_variable() }

  pub fn named_variable(&mut self) -> Result<()> {
    if let TokenType::Identifier(s) = self.previous.token_type() {
      let name = s.to_string();
      if let Some(c) = self.resolve_local(&name)? {
        self.emit_instr(Opcode::GetLocal(c));
        Ok(())
      } else {
        err!(Runtime, "No variable {} could be found.", name)
      }
    } else {
      err!(Compile, "Unexpected token for named variable {:?}", self.previous)
    }
  }

  pub fn resolve_local(&self, name: &str) -> Result<Option<usize>> {
    let i = self.scope.locals().iter().enumerate().rev().find(|(_, l)| l.name() == name).map(|(i, _)| i);
    match i {
      None => Ok(None),
      Some(i) => {
        if self.scope.initialized(i) {
          Ok(Some(i))
        } else {
          err!(Compile, "Can't read local in its own initializer")
        }
      }
    }
  }

  pub fn grouping(&mut self) -> Result<()> {
    self.expression()?;
    self.consume(TokenTypeDiscr::CloseParen);
    Ok(())
  }

  pub fn if_block(&mut self) -> Result<()> {
    self.expression()?;
    let false_jump = self.emit_jump(Opcode::initial_jump_if_false());
    self.emit_instr(Opcode::Pop);
    self.block()?;
    let done_jump = self.emit_jump(Opcode::initial_jump());
    self.patch_jump(false_jump)?;
    self.emit_instr(Opcode::Pop);

    let mut d2_jumps = Vec::new();
    while let TokenTypeDiscr::ElseifWord = self.current_ttd() {
      self.consume(TokenTypeDiscr::ElseifWord);
      self.expression()?;
      let f2_jump = self.emit_jump(Opcode::initial_jump_if_false());
      self.emit_instr(Opcode::Pop);
      self.block()?;
      d2_jumps.push(self.emit_jump(Opcode::initial_jump()));
      self.patch_jump(f2_jump)?;
      self.emit_instr(Opcode::Pop);
    }

    self.consume(TokenTypeDiscr::ElseWord);
    self.block()?;
    self.patch_jump(done_jump)?;
    for jump in d2_jumps {
      self.patch_jump(jump)?;
    }
    Ok(())
  }

  pub fn unary(&mut self) -> Result<()> {
    let ttd = self.previous_ttd();
    self.parse_precendence(Precedence::Unary)?;

    match ttd {
      TokenTypeDiscr::Minus => self.emit_instr(Opcode::Negate),
      TokenTypeDiscr::Bang => self.emit_instr(Opcode::Not),
      other => bail!(Compile, "Unexpected unary op: {:?}", other)
    }
    Ok(())
  }

  pub fn binary(&mut self) -> Result<()> {
    let ttd = self.previous_ttd();
    let precedence = self.get_rule(ttd)?.precedence().up()?;

    self.parse_precendence(precedence)?;

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
      other => bail!(Compile, "Unexpected binary op: {:?}", other)
    }
    Ok(())
  }

  pub fn and(&mut self) -> Result<()> {
    let end_jump = self.emit_jump(Opcode::initial_jump_if_false());
    self.emit_instr(Opcode::Pop);
    self.parse_precendence(Precedence::And)?;
    self.patch_jump(end_jump)
  }

  pub fn or(&mut self) -> Result<()> {
    let else_jump = self.emit_jump(Opcode::initial_jump_if_false());
    let end_jump = self.emit_jump(Opcode::initial_jump());
    self.patch_jump(else_jump)?;
    self.emit_instr(Opcode::Pop);
    self.parse_precendence(Precedence::Or)?;
    self.patch_jump(end_jump)
  }

  fn get_rule(&self, tt: TokenTypeDiscr) -> Result<&Rule> {
    RULES.get(&tt).ok_or_else(|| bad!(Internal, "No rule for token {:?}", tt))
  }

  fn previous_rule(&self) -> Result<&Rule> { self.get_rule(self.previous_ttd()) }
  fn current_rule(&self) -> Result<&Rule> { self.get_rule(self.current_ttd()) }
  fn previous_ttd(&self) -> TokenTypeDiscr { self.previous.token_type().discr() }
  fn current_ttd(&self) -> TokenTypeDiscr { self.current.token_type().discr() }

  pub fn emit_value(&mut self, v: Value) -> Result<()> {
    let cc = self.current_chunk().add_constant(v);
    if cc > MAX_CONSTANTS {
      bail!(Compile, "Too many constants in one chunk: {}", cc);
    }
    self.emit_instr(Opcode::Constant(cc));
    Ok(())
  }

  fn parse_precendence(&mut self, prec: Precedence) -> Result<()> {
    self.advance();

    if let Some(prefix) = self.previous_rule()?.prefix() {
      prefix(self)?;
    } else {
      bail!(Compile, "Expected expression.");
    }

    while prec <= self.current_rule()?.precedence() {
      self.advance();
      (self.previous_rule()?.infix().unwrap())(self)?;
    }

    Ok(())
  }
}

pub struct LocalStack {
  locals: Vec<Local>,
  scope_depth: u16
}

impl Default for LocalStack {
  fn default() -> LocalStack { LocalStack::new() }
}

impl LocalStack {
  pub fn new() -> LocalStack { LocalStack { locals: Vec::new(), scope_depth: 0 } }
  pub fn locals(&self) -> &[Local] { &self.locals }
  pub fn scope_depth(&self) -> u16 { self.scope_depth }
  pub fn incr_depth(&mut self) { self.scope_depth += 1; }
  pub fn decr_depth(&mut self) { self.scope_depth -= 1; }
  pub fn add_local(&mut self, local: Local) { self.locals.push(local); }
  pub fn len(&self) -> usize { self.locals.len() }
  pub fn is_empty(&self) -> bool { self.locals.is_empty() }
  pub fn initialized(&self, i: usize) -> bool { self.locals[i].depth() > 0 }

  pub fn mark_last_initialized(&mut self) {
    let last = self.locals.len() - 1;
    self.locals[last].depth = self.scope_depth;
  }

  pub fn drain_depth(&mut self) -> usize {
    let orig_len = self.locals.len();
    if let Some(p) = self.locals.iter().position(|l| l.depth() > self.scope_depth) {
      self.locals.truncate(p);
      orig_len - p
    } else {
      0
    }
  }

  pub fn defined(&self, name: &str) -> bool {
    self.locals.iter().rev().take_while(|l| l.depth() >= self.scope_depth).any(|l| l.name() == name)
  }
}

#[derive(Debug)]
pub struct Local {
  name: String,
  depth: u16
}

impl Local {
  pub fn new(name: String) -> Local { Local { name, depth: 0 } }
  pub fn name(&self) -> &str { &self.name }
  pub fn depth(&self) -> u16 { self.depth }
}

pub fn variable(compiler: &mut Compiler) -> Result<()> { compiler.variable() }
pub fn binary(compiler: &mut Compiler) -> Result<()> { compiler.binary() }
pub fn and(compiler: &mut Compiler) -> Result<()> { compiler.and() }
pub fn or(compiler: &mut Compiler) -> Result<()> { compiler.or() }
pub fn unary(compiler: &mut Compiler) -> Result<()> { compiler.unary() }
pub fn literal(compiler: &mut Compiler) -> Result<()> { compiler.literal() }
pub fn grouping(compiler: &mut Compiler) -> Result<()> { compiler.grouping() }
pub fn if_block(compiler: &mut Compiler) -> Result<()> { compiler.if_block() }

pub fn to_value<V>(v: &str) -> Result<Value>
where
  V: Into<Value> + FromStr,
  Error: From<<V as FromStr>::Err>
{
  Ok(v.parse::<V>()?.into())
}

struct Rule {
  prefix: Option<for<'r, 's> fn(&'r mut Compiler<'s>) -> Result<()>>,
  infix: Option<for<'r, 's> fn(&'r mut Compiler<'s>) -> Result<()>>,
  precedence: Precedence
}

impl Rule {
  pub fn new(
    prefix: Option<for<'r, 's> fn(&'r mut Compiler<'s>) -> Result<()>>,
    infix: Option<for<'r, 's> fn(&'r mut Compiler<'s>) -> Result<()>>,
    precedence: Precedence
  ) -> Rule {
    Rule { prefix, infix, precedence }
  }

  pub fn prefix(&self) -> Option<fn(&mut Compiler) -> Result<()>> { self.prefix }
  pub fn infix(&self) -> Option<fn(&mut Compiler) -> Result<()>> { self.infix }
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

pub fn fake_compile(source: &str) -> Result<Chunk> {
  let scanner = Scanner::new(source);

  let mut line = 0;
  for token in scanner {
    if token.is_error() {
      println!("Got error: {:?}", token);
      break;
    }

    if token.line() != line {
      line = token.line();
      print!("{:4} ", line);
    } else {
      print!("   | ");
    }
    println!("Token {:?}", token.token_type());

    if token.is_eof() {
      break;
    }
  }

  Ok(Chunk::new())
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
  pub fn up(&self) -> Result<Precedence> {
    let up = match self {
      Self::None => Self::Or,
      Self::Or => Self::And,
      Self::And => Self::Equality,
      Self::Equality => Self::Comparison,
      Self::Comparison => Self::Term,
      Self::Term => Self::Factor,
      Self::Factor => Self::Unary,
      Self::Unary => Self::Call,
      Self::Call => Self::Primary,
      Self::Primary => bail!(Internal, "No precedence higher than primary.")
    };
    Ok(up)
  }
}

fn construct_rules() -> HashMap<TokenTypeDiscr, Rule> {
  let mut rules = HashMap::new();

  rules.insert(TokenTypeDiscr::Bof, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::Eof, Rule::new(None, None, Precedence::None));
	rules.insert(TokenTypeDiscr::Comma, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::Equals, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::PointLeft, Rule::new(None, None, Precedence::None));
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
  rules.insert(TokenTypeDiscr::OpenParen, Rule::new(Some(grouping), None, Precedence::None));
  rules.insert(TokenTypeDiscr::CloseParen, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::Dot, Rule::new(None, Some(binary), Precedence::Call));
  rules.insert(TokenTypeDiscr::FnWord, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::FnAsyncWord, Rule::new(None, None, Precedence::None));
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
