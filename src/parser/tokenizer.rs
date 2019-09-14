use std::convert::TryFrom;

pub mod linecount;
use linecount::Point;
pub mod token;
use token::{Key, Token};

use super::util;
use super::util::GatherMode;
use crate::evaler::r#type::Value;

const STOPPERS: &[u8] = &[b' ', b'(', b')', b'\n', b'[', b']'];

#[derive(Debug)]
pub struct Tokenizer {
    source: Vec<u8>,
    pub index: usize,
    pub linecount: linecount::Point,
}

#[derive(Debug, Clone)]
pub struct Tracked<T: PartialEq + Clone> {
    pub inner: T,
    pub position: linecount::Point,
}
// We only want to compare inner, and ignore if the position is different
impl<T: PartialEq + Clone> PartialEq for Tracked<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl Tracked<Token> {
    // We need recursion for nested tokens
    pub fn untrack_t(self) -> Token {
        match self.inner {
            Token::TrackedGroup(mut tbuf) => {
                Token::Group(tbuf.drain(0..).map(|tt| tt.untrack_t()).collect())
            }
            Token::TrackedRuntimeList(mut tbuf) => {
                Token::RuntimeList(tbuf.drain(0..).map(|tt| tt.untrack_t()).collect())
            }
            Token::TrackedIfStatement(mut branches, mut else_branch) => Token::IfStatement(
                branches
                    .drain(0..)
                    .map(|(mut ctv, mut rtv)| {
                        (
                            ctv.drain(0..).map(|tt| tt.untrack_t()).collect(),
                            rtv.drain(0..).map(|tt| tt.untrack_t()).collect(),
                        )
                    })
                    .collect(),
                else_branch.drain(0..).map(|tt| tt.untrack_t()).collect(),
            ),
            _ => self.inner,
        }
    }
}

impl<T: PartialEq + Clone> Tracked<T> {
    #[inline]
    pub fn sep(self) -> (T, linecount::Point) {
        (self.inner, self.position)
    }
    #[inline]
    pub fn new(inner: T) -> Self {
        Tracked {
            inner,
            position: linecount::Point::new(),
        }
    }
}

impl Tokenizer {
    pub fn new(source: Vec<u8>) -> Self {
        Self {
            source,
            index: 0,
            linecount: Point::new(),
        }
    }

    pub fn build_token(chars: &[u8]) -> token::Result<Token> {
        if let Ok(t) = Value::try_from(chars) {
            let token = Token::V(t);
            return token::Result::Complete(token);
        }

        let first = match chars.get(0) {
            None => return token::Result::Empty,
            Some(t) => *t,
        };
        match Key::matches(first) {
            token::Result::Complete(key) => return token::Result::Complete(Token::Key(key)),
            token::Result::Single(key) => {
                if key.is(chars) {
                    let token = Token::Key(key);
                    return token::Result::Complete(token);
                }
            }
            token::Result::Multiple(key_matches) => {
                for key in key_matches {
                    if key.is(chars) {
                        let token = Token::Key(key.clone());
                        return token::Result::Complete(token);
                    }
                }
            }
            token::Result::Empty => panic!("Empty token result from Key::matches"),
            token::Result::Unmatched => {}
        };
        // Is it a lambda?
        if chars.first() == Some(&b'\\') && chars.last() == Some(&b':') && chars.len() > 2 {
            let token =
                Token::LambdaHeader(String::from_utf8(chars[1..chars.len() - 1].to_vec()).unwrap());
            token::Result::Complete(token)
        } else {
            let token = Token::Word(String::from_utf8(chars.to_vec()).unwrap());
            token::Result::Complete(token)
        }
    }

    pub fn next_token(&mut self, is_header: bool) -> Token {
        if let Some(c) = self.source.get(self.index) {
            // Skip newlines
            if *c == b'\n' {
                // Weird hack to make sure the first newline is counted if first line is empty
                if self.linecount.line == 0 {
                    self.linecount.line += 1;
                }

                self.next();
                return if is_header {
                    // Unless it's an explicitely required newline
                    Token::HeaderFnEndNoArgs
                } else {
                    self.next_token(is_header)
                };
            }
            if *c == b')' {
                self.next();
                return Token::Key(Key::ParenClose);
            }
        }

        let (chars, _) = match self.gather_to(GatherMode::Normal, STOPPERS) {
            None => return Token::EOF,
            Some(gather) => gather,
        };
        let token = Self::build_token(chars);
        match token {
            token::Result::Empty => self.next_token(is_header),
            token::Result::Complete(t) => {
                if let Token::Key(Key::Comment) = t {
                    self.skip_to(b'\n');
                    self.next_token(is_header)
                } else {
                    t
                }
            }
            _ => panic!("Unexpected entity from token parse: {:?}", token),
        }
    }

    pub fn gather_to(&mut self, mode: GatherMode, stop_at: &[u8]) -> Option<(&[u8], u8)> {
        if self.index >= self.source.len() {
            return None;
        }
        let (gathered, was_on) = util::gather_to(mode, &self.source[self.index..], stop_at);

        // We want these stoppers to be included in the next gather
        if was_on == b'(' || was_on == b'\n' || was_on == b'[' || was_on == b')' {
            self.index -= 1;
            if self.source.get(self.index) == Some(&b'\n') {
                self.linecount.line -= 1;
            }
        };

        for _ in 0..=gathered.len() {
            self.index += 1;
            if self.source.get(self.index) == Some(&b'\n') {
                self.linecount.line += 1;
            }
        }
        Some((util::trim(gathered), was_on))
    }

    fn skip_to(&mut self, delim: u8) -> usize {
        let mut i = 0;
        loop {
            if *self.source.get(self.index + i).unwrap_or(&delim) == delim {
                self.progress(i);
                return i;
            }
            i += 1;
        }
    }

    pub fn source(&self) -> &[u8] {
        &self.source
    }

    #[inline]
    pub fn next(&mut self) {
        self.index += 1;
        self.update_lc_f();
    }

    #[inline]
    pub fn get_char(&self) -> u8 {
        self.source[self.index]
    }

    #[inline]
    #[allow(dead_code)]
    fn try_get_char(&self) -> Option<u8> {
        self.source.get(self.index).copied()
    }

    #[inline]
    pub fn peek(&self) -> Option<u8> {
        self.source.get(self.index).copied()
    }

    pub fn last_char(&self) -> u8 {
        self.source[self.index - 1]
    }

    #[inline]
    pub fn regress(&mut self, amount: usize) {
        for _ in 0..amount {
            self.index -= 1;
            self.update_lc_b();
        }
    }

    #[inline]
    pub fn progress(&mut self, amount: usize) {
        for _ in 0..amount {
            self.index += 1;
            self.update_lc_f();
        }
    }
    #[inline]
    pub fn update_lc_f(&mut self) {
        if self.source.get(self.index) == Some(&b'\n') {
            self.linecount.line += 1;
        }
    }
    #[inline]
    pub fn update_lc_b(&mut self) {
        if self.source.get(self.index) == Some(&b'\n') {
            self.linecount.line -= 1;
        }
    }

    pub fn regress_to(&mut self, ident: u8) {
        loop {
            if *self.source.get(self.index).unwrap_or(&ident) == ident {
                return;
            }
            self.regress(1);
        }
    }
}
