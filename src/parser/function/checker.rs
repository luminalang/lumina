use super::FunctionBuilder;
use crate::evaler::runner::operator::Operator;
use crate::identifier::r#type::{BaseType, Type};
use crate::parser::index::Index;
use crate::parser::tokenizer::token::{Key, Token};
use crate::parser::tokenizer::Tracked;

pub struct Checker<'a> {
    index: &'a Index,
    /*
    self_header: &'a FunctionHeader,
    tbuf: &'a [Token],
    */
    func: &'a FunctionBuilder<'a>,
    tbuf: Option<&'a [Tracked<Token>]>,
}

impl<'a> Checker<'a> {
    pub fn new(
        tbuf: Option<&'a [Tracked<Token>]>,
        func: &'a FunctionBuilder,
        index: &'a Index,
    ) -> Self {
        Self { index, func, tbuf }
    }

    pub fn validate_function(self) {
        // Anything can be converted to nothing
        if self.func.header.returns == Type::Base(BaseType::Nothing) {
            return self.run(0);
        }
        // Make sure it returns the correct type
        let given = {
            let t = match self.discover_type(&self.func.tokens[0]) {
                Found(t) => t,
                UnsafeValid => return self.run(0),
            };

            match t {
                // This code looks quite ugly for the rather annoying fact that 4 + 4.0 is valid
                // code and becomes float even with 4 being int
                Type::Base(BaseType::Int) => match &self.func.tokens.get(1).map(|v| &v.inner) {
                    Some(Token::Key(Key::Operator(_op))) => {
                        let right = match self.discover_type(&self.func.tokens[2]) {
                            Found(t) => t,
                            UnsafeValid => return self.run(0),
                        };
                        match right {
                            Type::Base(BaseType::Int) => right,
                            Type::Base(BaseType::Float) => right,
                            _ => panic!("ERROR_TODO: Operator type  mismatch"),
                        }
                    }
                    _ => Type::Base(BaseType::Int),
                },
                Type::Base(BaseType::Float) => match &self.func.tokens.get(1).map(|v| &v.inner) {
                    Some(Token::Key(Key::Operator(_op))) => t,
                    None => t,
                    _ => panic!("ERROR_TODO: Malformed syntax at start of function"),
                },
                _ => t,
            }
        };
        if given != self.func.header.returns {
            panic!("ERROR_TODO: Function returns the wrong value")
        }
        // Verify typing of the function body
        self.run(0);
    }

    // TODO: I need to create a wrapper methods for comparing types that take generics into account

    fn run(self, i: usize) {
        if i >= self.tbuf().len() {
            return;
        }
        let token = &self.tbuf()[i];
        match &token.inner {
            Token::Function(fid, funcid) => {
                let header = self
                    .index
                    .get_file_from(*fid)
                    .borrow()
                    .get_func_header(*funcid);
                for count in 0..header.parameters.len() {
                    let given = match self.discover_type(
                        self.tbuf()
                            .get(i + 1 + count)
                            .unwrap_or(&Tracked::new(Token::EOF)),
                    ) {
                        DiscoverResult::Found(given) => given,
                        DiscoverResult::UnsafeValid => header.parameters[count].clone(),
                    };

                    if let Type::Generic(generic_id) = given {
                        println!(
                            "WARNING: Skipping typechecking of generic value {}",
                            generic_id
                        );
                        continue;
                    }
                    if let Type::Generic(generic_id) = header.parameters[count] {
                        println!(
                            "WARNING: Skipping typechecking of generic value {}",
                            generic_id
                        );
                        continue;
                    }

                    // TODO: Proper generics
                    if given != header.parameters[count] {
                        panic!(
                            "ERROR_TODO: Type mismatch: Wanted {:?} got {:?}",
                            header.parameters[count], given
                        );
                    }
                }
            }
            Token::Key(Key::Operator(op)) => {
                let left = match self.discover_type(&self.tbuf()[i - 1]) {
                    Found(t) => t,
                    UnsafeValid => return self.run(i + 1),
                };
                let right = match self.discover_type(&self.tbuf()[i + 1]) {
                    Found(t) => t,
                    UnsafeValid => return self.run(i + 1),
                };
                let (left_ok, right_ok) = op.type_check(left.clone(), right.clone());
                if !left_ok || !right_ok {
                    panic!(
                        "ERROR_TODO: Type mismatch {:?}:{} {:?}:{}",
                        left, left_ok, right, right_ok
                    );
                }
            }
            Token::TrackedGroup(nested) => {
                Checker::new(Some(&nested), self.func, self.index).run(0)
            }
            Token::TrackedIfStatement(branches, else_do) => {
                let mut expect_type = None;

                for branch in branches.iter() {
                    Checker::new(Some(&branch.0), self.func, self.index).run(0);
                    Checker::new(Some(&branch.1), self.func, self.index).run(0);
                    let t = match self.discover_type(&branch.1[0]) {
                        DiscoverResult::Found(t) => t,
                        DiscoverResult::UnsafeValid => continue,
                    };
                    match &expect_type {
                        None => expect_type = Some(t),
                        Some(existing) => {
                            if t != *existing {
                                // TODO: What on earth happens with generics here?
                                panic!("ERROR_TODO: Type mismatch, different returns on branches")
                            }
                        }
                    }
                }
                Checker::new(Some(&else_do), self.func, self.index).run(0);
                let t = match self.discover_type(&else_do[0]) {
                    DiscoverResult::Found(t) => t,
                    DiscoverResult::UnsafeValid => return self.run(i + 1),
                };
                if let Some(existing) = expect_type {
                    if t != existing {
                        // TODO: What on earth happens with generics here?
                        panic!("ERROR_TODO: Type mismatch, different returns on branches")
                    }
                }
            }
            Token::TrackedRuntimeList(nested) => {
                Checker::new(Some(&nested), self.func, self.index).run(0)
            }
            Token::Lambda(_) => panic!("TODO: Lambda type check"),
            Token::Group(_) => panic!(),
            Token::IfStatement(_, _) => panic!(),
            _ => {}
        };
        self.run(i + 1)
    }

    fn tbuf(&self) -> &[Tracked<Token>] {
        self.tbuf.unwrap_or(&self.func.tokens)
    }

    fn discover_type(&self, t: &Tracked<Token>) -> DiscoverResult {
        match &t.inner {
            Token::V(v) => Found(v.into()),
            Token::LambdaValue(_lid) => panic!("Lambda type from token is like not a thing atm"),
            Token::ParamValue(n) => Found(self.func.header.parameters[*n].clone()),
            Token::WhereValue(n) => self.discover_type(&self.func.wheres[*n][0]),
            Token::BridgedFunction(_) => UnsafeValid,
            Token::TrackedGroup(tbuf) => self.discover_type(&tbuf[0]),
            Token::TrackedIfStatement(branches, _else) => self.discover_type(&branches[0].1[0]),
            Token::Recurse => DiscoverResult::Found(self.func.header.returns.clone()),
            Token::Key(Key::PrimitiveExit) => DiscoverResult::UnsafeValid,
            Token::Function(fid, funcid) => {
                let header = self
                    .index
                    .get_file_from(*fid)
                    .borrow()
                    .get_func_header(*funcid);
                Found(header.returns.clone())
            }
            _ => panic!("Cannot discover type of: {:?} || This must mean something invalid slipped through the translator", &t),
        }
    }
}

use DiscoverResult::*;
#[derive(Debug)]
enum DiscoverResult {
    Found(Type),
    //Error(E),
    UnsafeValid,
}
