use super::IrBuilder;
use crate::parser::{FunctionBuilder, Type};

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum FunctionSource {
    Coordinate(usize, usize),
    Generical(String, Vec<Type>, Type),
}

impl From<(usize, usize)> for FunctionSource {
    fn from((fid, funcid): (usize, usize)) -> Self {
        Self::Coordinate(fid, funcid)
    }
}

impl From<FunctionBuilder> for FunctionSource {
    fn from(func: FunctionBuilder) -> Self {
        Self::Generical(func.name, func.parameter_types, func.returns)
    }
}

impl IrBuilder {
    pub fn gen_id(&self, fid: usize, funcid: usize) -> usize {
        let mut indexes = self.assigned_indexes.borrow_mut();
        match indexes.get(&FunctionSource::from((fid, funcid))) {
            None => {
                let new_index = indexes.len();
                indexes.insert(FunctionSource::from((fid, funcid)), new_index);
                new_index
            }
            Some(existing) => *existing,
        }
    }
    pub fn try_get_id(&self, fid: usize, funcid: usize) -> Option<usize> {
        self.assigned_indexes
            .borrow()
            .get(&FunctionSource::from((fid, funcid)))
            .copied()
    }
}
