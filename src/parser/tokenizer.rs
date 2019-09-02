use std::convert::TryFrom;

pub mod linecount;
use linecount::Point;
pub mod token;
use token::{Key, Token};

use super::util;
use crate::evaler::r#type::Value;
use crate::parser::list;

const STOPPERS: &[u8] = &[b' ', b'(', b')', b'\n'];

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
    /*
    #[inline]
    pub fn untrack(self) -> T {
        self.inner
    }
    */
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
        // Is it a list that contains non-inline values?
        if chars.first() == Some(&b'[') {
            // Here we store each whole entity that's been converted to a token.
            // Some tokens are grouped together such as functions that take parameters.
            let mut tbuf = Vec::new();

            list::build_list(chars, |entity| -> Result<_, ()> {
                // Some entities have multiple tokens, here we've got 3 tokens in one entity [a, b, add c c, d]
                // therefore we're making this a buffer
                let mut nested_internal_tbuf: Vec<Token> = Vec::new();

                let mut index = 0;
                loop {
                    if index >= entity.len() {
                        if nested_internal_tbuf.len() == 1 {
                            tbuf.push(nested_internal_tbuf.remove(0))
                        } else {
                            tbuf.push(Token::Group(nested_internal_tbuf))
                        };
                        return Ok(());
                    }
                    let (raw_t, _was_on) = util::gather_to(&entity[index..], &[b' ', b'\n']);

                    index += raw_t.len();
                    match Tokenizer::build_token(raw_t) {
                        token::Result::Complete(t) => nested_internal_tbuf.push(t),
                        token::Result::Empty => {
                            index += 1;
                            continue;
                        }
                        _ => panic!("ERROR_TODO: Expected complete token form list"),
                    }
                }
            })
            .unwrap();
            return token::Result::Complete(Token::RuntimeList(tbuf));
        }
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

        let chars = match self.gather_to(STOPPERS) {
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

    fn gather_to(&mut self, stop_at: &[u8]) -> Option<&[u8]> {
        if self.index >= self.source.len() {
            return None;
        }
        let (gathered, was_on) = util::gather_to(&self.source[self.index..], stop_at);

        // We want these stoppers to be included in the next gather
        if was_on == b'(' || was_on == b'\n' {
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
        Some(util::trim(gathered))
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

    fn tracked<T: PartialEq + Clone>(&self, v: T) -> Tracked<T> {
        Tracked {
            inner: v,
            position: self.linecount,
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
