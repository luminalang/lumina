use crate::ir::Value;

pub enum ParamBuffer<'p> {
    Borrowed(&'p [Value]),
    Owned(Vec<Value>),
    // Lazy(Iter<>), ?
}
use ParamBuffer::*;

impl<'p> ParamBuffer<'p> {
    pub fn borrow(&'p self) -> ParamBuffer<'p> {
        match self {
            Borrowed(a) => ParamBuffer::Borrowed(a),
            Owned(a) => ParamBuffer::Borrowed(&a),
        }
    }
    pub fn param_borrow(&'p self, i: usize) -> &'p Value {
        match self {
            Borrowed(a) => &a[i],
            Owned(a) => &a[i],
        }
    }
    pub fn param_consume(&mut self, i: usize) -> Value {
        match self {
            Borrowed(a) => a[i].clone(),
            Owned(a) => {
                let mut v = Value::Nothing;
                std::mem::swap(&mut v, &mut a[i]);
                v
            }
        }
    }
}

impl From<Vec<Value>> for ParamBuffer<'_> {
    fn from(v: Vec<Value>) -> Self {
        Self::Owned(v)
    }
}

/*
pub trait ParamBuffer {
    fn param_borrow(&self, i: usize) -> &Value;
    fn param_consume(&mut self, i: usize) -> Value;
    fn borrow(&self) -> BorrowedParamBuffer<'_>;
}

pub struct BorrowedParamBuffer<'a> {
    content: &'a [Value],
}

impl<'a> ParamBuffer for BorrowedParamBuffer<'a> {
    fn borrow(&self) -> BorrowedParamBuffer<'_> {
        Self {
            content: self.content,
        }
    }
    fn param_borrow(&self, i: usize) -> &Value {
        &self.content[i]
    }
    fn param_consume(&mut self, i: usize) -> Value {
        self.content[i].clone()
    }
}

pub struct OwnedParamBuffer {
    content: Vec<Value>,
}

impl From<Vec<Value>> for OwnedParamBuffer {
    fn from(v: Vec<Value>) -> Self {
        Self { content: v }
    }
}

impl ParamBuffer for OwnedParamBuffer {
    fn borrow(&self) -> BorrowedParamBuffer {
        BorrowedParamBuffer {
            content: &self.content,
        }
    }
    fn param_borrow(&self, i: usize) -> &Value {
        &self.content[i]
    }
    fn param_consume(&mut self, i: usize) -> Value {
        let mut v = Value::Nothing;
        std::mem::swap(&mut v, &mut self.content[i]);
        v
    }
}
*/
