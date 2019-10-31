use super::{Parser, Type};
use crate::env::Environment;
use crate::runtime::ir;
use std::rc::Rc;

mod checker;

#[derive(Debug)]
pub struct IrBuilder {
    parser: Parser,
    completed: Vec<ir::Entity>,
    environment: Rc<Environment>,
}

impl IrBuilder {
    pub fn new(parser: Parser, env: Rc<Environment>) -> Self {
        Self {
            parser,
            environment: env,
            completed: Vec::with_capacity(5),
        }
    }

    pub fn build(self, fid: usize, entry: &str, params: &[Type]) -> Vec<ir::Entity> {
        Vec::new()
    }
}
