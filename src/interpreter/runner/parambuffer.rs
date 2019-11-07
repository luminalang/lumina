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
    pub fn consume(&mut self) -> ParamBuffer<'p> {
        match self {
            Borrowed(a) => ParamBuffer::Borrowed(&a),
            Owned(a) => {
                let mut v = Vec::new();
                std::mem::swap(&mut v, a);
                ParamBuffer::Owned(v)
            }
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
