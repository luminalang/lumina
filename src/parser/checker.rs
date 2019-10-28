use super::{FunctionBuilder, Inlined, ParseError, ParseFault, Parser, RawToken, Token, Type, DCE};
use std::cell::RefCell;
use std::rc::Rc;

pub struct TypeChecker {
    pub dce: DCE,
    parser: Parser,
    source: Source,
}

enum Source {
    Func(usize, usize),
    Held(Token),
}

/*
TODO:
    The parameter checking will dupe if the same function is used multiple times, breaking everything!

    Where we're at:
        I need to filter out identifiers to ID's here already
        I think I need to make TypeChecker not have an "active" field.
        I don't know how I'll do this. Not a clue
            I want to consume Parser here.
            And then return from the TypeChecker all the parts the LLIR needs.
*/

impl TypeChecker {
    pub fn new(parser: Parser, fid: usize, funcname: usize) -> Self {
        Self {
            parser,
            source: Source::Func(fid, funcname),
            dce: DCE::default(),
        }
    }

    pub fn run(&mut self) -> Result<Type, ParseError> {
        let t = self.get_token();
        Ok(Type::Nothing)
    }

    fn get_token(&self) -> &Token {
        match &self.source {
            Source::Held(t) => t,
            Source::Func(fid, funcid) => &self.parser.modules[*fid].functions[*funcid].body,
        }
    }
    fn get_token_mut(&mut self) -> &mut Token {
        match &mut self.source {
            Source::Held(f) => f,
            Source::Func(fid, funcid) => &mut self.parser.modules[*fid].functions[*funcid].body,
        }
    }
}
