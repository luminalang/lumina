use super::IrBuilder;
use crate::parser::{FunctionBuilder, Parser, Token, Type};
use std::borrow::Cow;
use std::hash::Hash;

#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub enum FunctionSource {
    Coordinate(usize, usize),
    Owned(usize, FunctionBuilder),
}

impl From<(usize, usize)> for FunctionSource {
    fn from((fid, funcid): (usize, usize)) -> Self {
        Self::Coordinate(fid, funcid)
    }
}

impl From<(usize, FunctionBuilder)> for FunctionSource {
    fn from((fid, func): (usize, FunctionBuilder)) -> Self {
        Self::Owned(fid, func)
    }
}

impl FunctionSource {
    pub fn returns<'a>(&'a self, parser: &'a Parser) -> &'a Type {
        &self.func(parser).returns
    }
    pub fn body<'a>(&'a self, parser: &'a Parser) -> &'a Token {
        &self.func(parser).body
    }
    pub fn func<'a>(&'a self, parser: &'a Parser) -> &'a FunctionBuilder {
        match self {
            FunctionSource::Coordinate(fid, funcid) => &parser.modules[*fid].functions[*funcid],
            FunctionSource::Owned(_, func) => &func,
        }
    }
    pub fn fid(&self) -> usize {
        match self {
            FunctionSource::Coordinate(fid, _) => *fid,
            FunctionSource::Owned(fid, _) => *fid,
        }
    }
}

impl IrBuilder {
    pub fn gen_id(&self, func: Cow<FunctionSource>) -> usize {
        let mut indexes = self.assigned_indexes.borrow_mut();
        match indexes.get(&func) {
            None => {
                let new_index = indexes.len();
                indexes.insert(func.into_owned(), new_index);
                new_index
            }
            Some(existing) => *existing,
        }
    }
    pub fn try_get_id(&self, func: &FunctionSource) -> Option<usize> {
        self.assigned_indexes.borrow().get(func).copied()
    }
}
