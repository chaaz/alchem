//! The scanner for the alchem language.

use crate::pick;
use std::str::CharIndices;
use std::vec::IntoIter;

pub struct Scanner<'s> {
  was_dot: bool,
  input: &'s str,
  iter: CharIndices<'s>,
  line: usize
}

impl<'s> Scanner<'s> {
  pub fn new(input: &'s str) -> Scanner { Scanner { input, iter: input.char_indices(), line: 1, was_dot: false } }
}

impl<'s> Iterator for Scanner<'s> {
  type Item = Token;

  fn next(&mut self) -> Option<Token> {
    self.skip_whitespace();
    self.iter.next().map(|(st, c)| {
      let token_type = match c {
        '%' => TokenType::Percent,
        '(' => TokenType::OpenParen,
        ')' => TokenType::CloseParen,
        '*' => TokenType::Star,
        '+' => TokenType::Plus,
        '-' => TokenType::Minus,
        '.' => TokenType::Dot,
        '/' => TokenType::Slash,
        ':' => TokenType::Colon,
        ';' => TokenType::Semi,
        '[' => TokenType::OpenSquare,
        ']' => TokenType::CloseSquare,
        '{' => TokenType::OpenCurl,
        '}' => TokenType::CloseCurl,
        ',' => TokenType::Comma,
        '!' => self.if_next('=', TokenType::NotEq, TokenType::Bang),
        '<' => self.if_next('=', TokenType::Lte, TokenType::Lt),
        '=' => self.if_next('=', TokenType::DoubleEq, TokenType::Equals),
        '>' => self.if_next('=', TokenType::Gte, TokenType::Gt),
        '"' => self.next_string(st),
        '&' => self.if_next('&', TokenType::DoubleAnd, TokenType::Error(format!("Incomplete && at {}.", st))),
        '|' => self.if_next('|', TokenType::DoubleOr, TokenType::Error(format!("Incomplete || at {}.", st))),
        c if c.is_digit(10) => self.next_number(st),
        c if c.is_ascii_alphabetic() || c == '_' => self.next_identifier(st),
        _ => TokenType::Error(format!("Unexpected character at {}.", st))
      };
      self.was_dot = token_type == TokenType::Dot;

      Token::new(token_type, st)
    })
  }
}

impl<'s> Scanner<'s> {
  pub fn drain_into_iter(&mut self) -> IntoIter<Token> { self.collect::<Vec<_>>().into_iter() }

  fn next_identifier(&mut self, open: usize) -> TokenType {
    let mut close = open + 1;
    while let Some((i, c)) = self.peek() {
      close = i;
      if c.is_ascii_alphabetic() || c.is_digit(10) || c == '_' {
        close += 1;
        self.iter.next();
      } else {
        break;
      }
    }

    match &self.input[open .. close] {
      "fn" => TokenType::FnWord,
      "if" => TokenType::IfWord,
      "elseif" => TokenType::ElseifWord,
      "else" => TokenType::ElseWord,
      "false" => TokenType::FalseLit,
      "true" => TokenType::TrueLit,
      id => TokenType::Identifier(id.to_string())
    }
  }

  fn next_number(&mut self, open: usize) -> TokenType {
    let mut is_int = true;
    let mut has_trail = false;
    let mut close = open + 1;
    while let Some((i, c)) = self.peek() {
      close = i;
      if !c.is_digit(10) && c != '.' {
        break;
      } else if c == '.' {
        if self.was_dot {
          break;
        }
        if !is_int {
          return TokenType::Error(format!("Illegal number at {}.", open));
        }
        is_int = false;
        has_trail = false;
        close += 1;
        self.iter.next();
      } else {
        has_trail = true;
        close += 1;
        self.iter.next();
      }
    }

    if is_int {
      TokenType::IntLit(self.input[open .. close].to_string())
    } else if has_trail {
      TokenType::FloatLit(self.input[open .. close].to_string())
    } else {
      TokenType::Error(format!("Bad number starting at {}.", open))
    }
  }

  fn next_string(&mut self, open: usize) -> TokenType {
    while let Some((i, c)) = self.iter.next() {
      if c == '"' {
        return TokenType::StringLit(self.input[open + 1 .. i].to_string());
      } else if c == '\n' {
        self.line += 1;
      }
    }
    TokenType::Error(format!("Unterminated string starting at {}.", open))
  }

  fn peek(&self) -> Option<(usize, char)> { self.iter.clone().next() }

  fn skip_whitespace(&mut self) {
    while let Some((_, c)) = self.peek() {
      if c.is_whitespace() {
        if c == '\n' {
          self.line += 1;
        }
        self.iter.next();
      } else if c == '/' {
        if self.iter.as_str().starts_with("//") {
          self.iter.next();
          while !matches!(self.peek(), Some((_, '\n')) | None) {
            self.iter.next();
          }
        } else {
          break;
        }
      } else {
        break;
      }
    }
  }

  fn if_next(&mut self, t: char, if_t: TokenType, if_f: TokenType) -> TokenType {
    match self.peek() {
      Some((_, x)) if x == t => {
        self.iter.next();
        if_t
      }
      _ => if_f
    }
  }
}

#[derive(Debug, Clone)]
pub struct Token {
  pos: usize,
  token_type: TokenType
}

impl Token {
  pub fn new(token_type: TokenType, pos: usize) -> Token { Token { token_type, pos } }

  pub fn is_error(&self) -> bool { self.token_type.is_error() }
  pub fn pos(&self) -> usize { self.pos }
  pub fn token_type(&self) -> &TokenType { &self.token_type }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
  Bof,
  Eof,
  Comma,
  Equals,
  Semi,
  Colon,
  OpenCurl,
  CloseCurl,
  OpenSquare,
  CloseSquare,
  DoubleAnd,
  DoubleOr,
  Gt,
  Lt,
  Gte,
  Lte,
  DoubleEq,
  NotEq,
  Plus,
  Minus,
  Star,
  Slash,
  Percent,
  Bang,
  OpenParen,
  CloseParen,
  Dot,
  FnWord,
  IfWord,
  ElseifWord,
  ElseWord,
  TrueLit,
  FalseLit,
  IntLit(String),
  FloatLit(String),
  StringLit(String),
  Identifier(String),
  Error(String)
}

impl TokenType {
  pub fn as_identifier(&self) -> &str { pick!(self, Self::Identifier(s) => s, "Not an identifier: {:?}") }

  pub fn is_error(&self) -> bool { matches!(self, TokenType::Error(_)) }
  pub fn discr(&self) -> TokenTypeDiscr {
    match self {
      Self::Bof => TokenTypeDiscr::Bof,
      Self::Eof => TokenTypeDiscr::Eof,
      Self::Comma => TokenTypeDiscr::Comma,
      Self::Equals => TokenTypeDiscr::Equals,
      Self::Semi => TokenTypeDiscr::Semi,
      Self::Colon => TokenTypeDiscr::Colon,
      Self::OpenCurl => TokenTypeDiscr::OpenCurl,
      Self::CloseCurl => TokenTypeDiscr::CloseCurl,
      Self::OpenSquare => TokenTypeDiscr::OpenSquare,
      Self::CloseSquare => TokenTypeDiscr::CloseSquare,
      Self::DoubleAnd => TokenTypeDiscr::DoubleAnd,
      Self::DoubleOr => TokenTypeDiscr::DoubleOr,
      Self::Gt => TokenTypeDiscr::Gt,
      Self::Lt => TokenTypeDiscr::Lt,
      Self::Gte => TokenTypeDiscr::Gte,
      Self::Lte => TokenTypeDiscr::Lte,
      Self::DoubleEq => TokenTypeDiscr::DoubleEq,
      Self::NotEq => TokenTypeDiscr::NotEq,
      Self::Plus => TokenTypeDiscr::Plus,
      Self::Minus => TokenTypeDiscr::Minus,
      Self::Star => TokenTypeDiscr::Star,
      Self::Slash => TokenTypeDiscr::Slash,
      Self::Percent => TokenTypeDiscr::Percent,
      Self::Bang => TokenTypeDiscr::Bang,
      Self::OpenParen => TokenTypeDiscr::OpenParen,
      Self::CloseParen => TokenTypeDiscr::CloseParen,
      Self::Dot => TokenTypeDiscr::Dot,
      Self::FnWord => TokenTypeDiscr::FnWord,
      Self::IfWord => TokenTypeDiscr::IfWord,
      Self::ElseifWord => TokenTypeDiscr::ElseifWord,
      Self::ElseWord => TokenTypeDiscr::ElseWord,
      Self::TrueLit => TokenTypeDiscr::TrueLit,
      Self::FalseLit => TokenTypeDiscr::FalseLit,
      Self::IntLit(..) => TokenTypeDiscr::IntLit,
      Self::FloatLit(..) => TokenTypeDiscr::FloatLit,
      Self::StringLit(..) => TokenTypeDiscr::StringLit,
      Self::Identifier(..) => TokenTypeDiscr::Identifier,
      Self::Error(..) => TokenTypeDiscr::Error
    }
  }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TokenTypeDiscr {
  Bof,
  Eof,
  Comma,
  Equals,
  Semi,
  Colon,
  OpenCurl,
  CloseCurl,
  OpenSquare,
  CloseSquare,
  DoubleAnd,
  DoubleOr,
  Gt,
  Lt,
  Gte,
  Lte,
  DoubleEq,
  NotEq,
  Plus,
  Minus,
  Star,
  Slash,
  Percent,
  Bang,
  OpenParen,
  CloseParen,
  Dot,
  FnWord,
  IfWord,
  ElseifWord,
  ElseWord,
  TrueLit,
  FalseLit,
  IntLit,
  FloatLit,
  StringLit,
  Identifier,
  Error
}
