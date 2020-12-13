//! The alchem compiler.

use crate::errors::{Result, Error};
use crate::common::{Chunk, Instr, Opcode};
use crate::scanner::{Scanner, Token, TokenType, TokenTypeDiscr};
use crate::value::Value;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::str::FromStr;

const MAX_CONSTANTS: usize = 255;

lazy_static! {
  static ref RULES: HashMap<TokenTypeDiscr, Rule> = construct_rules();
}

pub fn compile(source: &str) -> Result<Chunk> {
  let scanner = Scanner::new(source);

  let mut compiler = Compiler::new(scanner);

  compiler.expression()?;
  compiler.consume(TokenType::Eof);
  compiler.end_compiler();

  Ok(compiler.into_chunk())
}

pub struct Compiler<'s> {
  current: Token<'s>,
  previous: Token<'s>,
  last_line: usize,
  scanner: Scanner<'s>,
  had_error: bool,
  is_cascading: bool,
  target: Chunk,
  debug: bool
}

impl<'s> Compiler<'s> {
  pub fn new(scanner: Scanner<'s>) -> Compiler<'s> {
    let mut compiler = Compiler {
      current: Token::new(TokenType::Bof, 0),
      previous: Token::new(TokenType::Bof, 0),
      last_line: 0,
      scanner,
      had_error: false,
      is_cascading: false,
      target: Chunk::new(),
      debug: true
    };
    compiler.advance();
    compiler
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
        error_token(&mut self.had_error, &mut self.is_cascading, &token, "parse error");
      } else {
        return token;
      }
    }
    Token::new(TokenType::Eof, self.last_line)
  }

  pub fn error_current(&mut self, msg: &str) {
    error_token(&mut self.had_error, &mut self.is_cascading, &self.current, msg);
  }

  pub fn consume(&mut self, expect: TokenType) {
    if self.current.token_type() == &expect {
      self.advance();
      return;
    }

    self.error_current(&format!("Expected {:?}", expect));
  }

  pub fn emit_instr(&mut self, code: Opcode) {
    let line = self.last_line;
    self.current_chunk().add_instr(Instr::new(code, line));
  }

  pub fn current_chunk(&mut self) -> &mut Chunk { &mut self.target }

  pub fn end_compiler(&mut self) {
    self.emit_instr(Opcode::Return);
    if self.debug && !self.had_error {
      println!("compiled code:");
      self.current_chunk().debug();
    }
  }

  pub fn expression(&mut self) -> Result<()> { self.parse_precendence(Precedence::Or) }

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
      other => bail!(Internal, "Unexpected literal {:?}", other)
    }
  }

  pub fn grouping(&mut self) -> Result<()> {
    self.expression()?;
    self.consume(TokenType::CloseParen);
    Ok(())
  }

  pub fn unary(&mut self) -> Result<()> {
    let ttd = self.previous_ttd();
    self.parse_precendence(Precedence::Unary)?;

    match ttd {
      TokenTypeDiscr::Minus => self.emit_instr(Opcode::Negate),
      TokenTypeDiscr::Bang => self.emit_instr(Opcode::Not),
      other => bail!(Internal, "Unexpected unary op: {:?}", other)
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
      TokenTypeDiscr::DoubleAnd => self.emit_instr(Opcode::And),
      TokenTypeDiscr::DoubleOr => self.emit_instr(Opcode::Or),
      other => bail!(Internal, "Unexpected binary op: {:?}", other)
    }
    Ok(())
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

pub fn to_value<V>(v: &str) -> Result<Value>
where
  V: Into<Value> + FromStr,
  Error: From<<V as FromStr>::Err>
{
  Ok(v.parse::<V>()?.into())
}

pub fn binary(compiler: &mut Compiler) -> Result<()> { compiler.binary() }
pub fn unary(compiler: &mut Compiler) -> Result<()> { compiler.unary() }
pub fn literal(compiler: &mut Compiler) -> Result<()> { compiler.literal() }
pub fn grouping(compiler: &mut Compiler) -> Result<()> { compiler.grouping() }

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

fn error_token(had_error: &mut bool, is_cascading: &mut bool, token: &Token, msg: &str) {
  *had_error = true;
  if *is_cascading {
    return;
  }
  *is_cascading = true;
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
  rules.insert(TokenTypeDiscr::DoubleAnd, Rule::new(None, Some(binary), Precedence::And));
  rules.insert(TokenTypeDiscr::DoubleOr, Rule::new(None, Some(binary), Precedence::Or));
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
  rules.insert(TokenTypeDiscr::IfWord, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::ElseifWord, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::ElseWord, Rule::new(None, None, Precedence::None));
  rules.insert(TokenTypeDiscr::TrueLit, Rule::new(Some(literal), None, Precedence::None));
  rules.insert(TokenTypeDiscr::FalseLit, Rule::new(Some(literal), None, Precedence::None));
  rules.insert(TokenTypeDiscr::IntLit, Rule::new(Some(literal), None, Precedence::None));
  rules.insert(TokenTypeDiscr::FloatLit, Rule::new(Some(literal), None, Precedence::None));
  rules.insert(TokenTypeDiscr::StringLit, Rule::new(Some(literal), None, Precedence::None));
	rules.insert(TokenTypeDiscr::Identifier, Rule::new(None, None, Precedence::None));
	rules.insert(TokenTypeDiscr::Error, Rule::new(None, None, Precedence::None));

  rules
}
