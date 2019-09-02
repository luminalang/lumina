use crate::parser::tokenizer::token::Token;
use std::sync::Arc;

pub trait TokenBuffer {
    fn next_token(&mut self) -> Option<&Token>;
    fn reset(&mut self);
}

#[derive(Clone, PartialEq, Debug)]
pub struct SimpleBuffer<'t> {
    inner: &'t [Token],
    r_ip: usize,
}

impl<'t> SimpleBuffer<'t> {
    pub fn new(tokens: &'t [Token]) -> Self {
        Self {
            inner: tokens,
            r_ip: std::usize::MAX,
        }
    }

    pub fn to_inner(&self) -> &[Token] {
        self.inner
    }
}

impl<'t> TokenBuffer for SimpleBuffer<'t> {
    fn next_token(&mut self) -> Option<&Token> {
        if self.r_ip == std::usize::MAX {
            self.r_ip = 0
        } else {
            self.r_ip += 1
        };
        self.inner.get(self.r_ip)
    }

    fn reset(&mut self) {
        self.r_ip = std::usize::MAX;
    }
}

pub struct WhereBuffer {
    inner: Arc<Vec<Vec<Token>>>,
    useid: usize,
    r_ip: usize,
}

impl WhereBuffer {
    pub fn new(ptr: Arc<Vec<Vec<Token>>>, useid: usize) -> Self {
        Self {
            inner: ptr,
            useid,
            r_ip: std::usize::MAX,
        }
    }
}

impl TokenBuffer for WhereBuffer {
    fn next_token(&mut self) -> Option<&Token> {
        if self.r_ip == std::usize::MAX {
            self.r_ip = 0
        } else {
            self.r_ip += 1
        }
        self.inner[self.useid].get(self.r_ip)
    }
    fn reset(&mut self) {
        self.r_ip = std::usize::MAX;
    }
}
