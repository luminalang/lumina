use super::{FunctionBuilder, Inlined, Parser, RawToken, Token, Type, PRELUDE_FID};
use crate::evaler::bridge;
use std::rc::Rc;

#[derive(Default, Clone)]
struct Position {
    module: usize,
    function: (String, Vec<Type>),
}

pub struct TypeChecker<'f> {
    active: Position,
    parser: &'f Parser<'f>,
    infered_types: Vec<Type>,
}

pub trait Typeable {
    fn get_parameter(&self, ident: &str) -> Option<usize>;
    fn get_parameter_type(&self, pid: usize) -> &Type;
    fn get_return(&self) -> &Type;
    fn entry_point(&self) -> Rc<Token>;
}

impl<'f> TypeChecker<'f> {
    pub fn new(parser: &'f Parser, fmodule: usize, fname: &str, fparams: Vec<Type>) -> Self {
        Self {
            parser,
            active: Position {
                module: fmodule,
                function: (fname.to_owned(), fparams),
            },
            infered_types: Vec::new(),
        }
    }

    fn fork(&mut self, position: Position) -> Self {
        Self {
            active: position,
            parser: self.parser,
            infered_types: Vec::new(),
        }
    }

    pub fn run(&mut self) -> Result<Type, ()> {
        let func = self.func()?;
        let returns = func.get_return().clone();
        let entry = func.entry_point();
        let got = self.type_check(&entry)?;
        if returns != got && returns != Type::Nothing {
            panic!("ET: Return type mismatch. Wanted {} got {}", returns, got);
        }
        Ok(got)
    }

    #[inline]
    fn func(&self) -> Result<Box<&dyn Typeable>, ()> {
        let self_mod = &self.parser.modules[self.active.module];

        if super::is_valid_identifier(&self.active.function.0) {
            // Function
            if let Some((func, _funcid)) = self_mod.functions.get(&self.active.function) {
                return Ok(Box::new(func));
            } else if let Some((func, _funcid)) =
                self.parser.prelude().functions.get(&self.active.function)
            {
                return Ok(Box::new(func));
            }
            panic!("Function {:?} not found", self.active.function);
        } else {
            let key = &(
                // TODO: Allocation can be avoided
                self.active.function.0.clone(),
                [
                    self.active.function.1[0].clone(),
                    self.active.function.1[1].clone(),
                ],
            );
            // Operator
            if let Some((opb, _opid)) = self_mod.operators.get(key) {
                return Ok(Box::new(opb));
            } else if let Some((opb, _opid)) = self.parser.prelude().operators.get(key) {
                return Ok(Box::new(opb));
            }
            panic!("Operator {:?} not found", self.active.function);
        }
    }

    fn type_check(&mut self, t: &Token) -> Result<Type, ()> {
        let r#type = match &t.inner {
            RawToken::Inlined(inlined) => match inlined {
                Inlined::Int(_) => Type::Int,
                Inlined::Float(_) => Type::Float,
                Inlined::Bool(_) => Type::Bool,
                Inlined::Nothing => Type::Nothing,
            },
            RawToken::Parameterized(box entity, params) => {
                let mut param_types = Vec::with_capacity(params.len());
                for param in params.iter() {
                    param_types.push(self.type_check(param)?)
                }
                match &entity.inner {
                    RawToken::Identifier(ident) => {
                        let mut new_pos = self.active.clone();
                        new_pos.function = (String::from(ident), param_types);
                        self.fork(new_pos).run()?
                    }
                    RawToken::ExternalIdentifier(entries) => {
                        let new_fid = self.parser.modules[self.active.module]
                            .imports
                            .get(&entries[0])
                            .copied()
                            .ok_or_else(|| panic!("ET: Import {} not found", entries[0]))?;
                        let new_pos = Position {
                            module: new_fid,
                            function: (entries[1].clone(), param_types),
                        };
                        self.fork(new_pos).run()?
                    }
                    RawToken::RustCall(_bridged_id, r#type) => r#type.clone(),
                    _ => panic!("{:?} cannot take parameters", entity),
                }
            }
            RawToken::RustCall(_bridged_id, r#type) => r#type.clone(),
            RawToken::Operation(box (left, right), op) => {
                let left_t = self.type_check(left)?;
                let right_t = self.type_check(right)?;
                let mut new_pos = self.active.clone();
                new_pos.function = (op.identifier.clone(), vec![left_t, right_t]);
                self.fork(new_pos).run()?
            }
            RawToken::Identifier(constant_ident) => {
                let func = self.func()?;
                if let Some(param_id) = func.get_parameter(constant_ident) {
                    func.get_parameter_type(param_id).clone()
                } else {
                    self.fork(Position {
                        module: self.active.module,
                        function: (constant_ident.clone(), vec![]),
                    })
                    .run()?
                }
            }
            RawToken::FirstStatement(entries) => {
                for entry in entries[0..entries.len() - 1].iter() {
                    self.type_check(entry)?;
                }
                self.type_check(entries.last().unwrap())?
            }
            RawToken::List(entries) => {
                let expect_type = self.type_check(&entries[0])?;
                for entry in entries[1..].iter() {
                    let t = self.type_check(entry)?;
                    if t != expect_type {
                        panic!("ET: Entries in list have different types");
                    }
                }
                expect_type
            }
            RawToken::IfExpression(ifexpr) => {
                let mut expect_type = None;
                for (cond, eval) in ifexpr.branches.iter() {
                    let cv = self.type_check(cond)?;
                    if cv != Type::Bool {
                        panic!(
                            "ET: Condition must result in true or false, but I got {:?}",
                            cv
                        );
                    }
                    let ev = self.type_check(eval)?;
                    if let Some(expected) = &expect_type {
                        if ev != *expected {
                            panic!(
                                "ET: Branches have different types. Wanted {} got {}",
                                expected, ev
                            );
                        }
                    } else {
                        expect_type = Some(ev);
                    }
                }
                let ev = self.type_check(&ifexpr.else_branch)?;
                if let Some(expected) = &expect_type {
                    if ev != *expected {
                        panic!(
                            "ET: Branches have different types. Wanted {} got {}",
                            expected, ev
                        );
                    }
                }
                expect_type.unwrap()
            }
            _ => panic!("Cannot discover type of {:?}", t),
        };
        Ok(r#type)
    }
}
