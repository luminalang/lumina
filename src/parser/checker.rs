use super::FunctionBuilder;
use crate::parser::{Key, Parser, RawToken, Token, Type};

pub struct TypeChecker<'f> {
    func: (usize, usize),
    parser: &'f Parser,
    infered_types: Vec<Type>,
}

impl<'f> TypeChecker<'f> {
    pub fn new(parser: &'f Parser) -> Self {
        Self {
            func: (0, 0),
            parser,
            infered_types: Vec::new(),
        }
    }

    // pub fn run(&mut self, entrypoint: (usize, usize)) -> Result<Type, ()> {}

    /*
    fn func(&self) -> &FunctionBuilder {
        self.parser.modules[self.func.0].unwrap().functions[self.func.1]
    }
    */
}
