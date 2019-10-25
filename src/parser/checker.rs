use super::{Inlined, ParseError, ParseFault, Parser, RawToken, Token, Type};
use crate::evaler::bridge;
use std::rc::Rc;

#[derive(Clone)]
struct Position<'f> {
    module: usize,
    function: Box<&'f dyn Typeable>,
    funcid: usize,
}

pub struct TypeChecker<'f> {
    active: Position<'f>,
    parser: &'f Parser,
}

pub trait Typeable {
    fn get_parameter(&self, ident: &str) -> Option<usize>;
    fn get_parameter_type(&self, pid: usize) -> &Type;
    fn check_return(&self, got: &Type) -> Result<(), ParseFault>;
    fn entry_point(&self) -> Rc<Token>;
}

impl<'f> TypeChecker<'f> {
    pub fn new(
        parser: &'f Parser,
        fmodule: usize,
        fname: &str,
        fparams: Vec<Type>,
    ) -> Result<Self, ParseFault> {
        let position = Self::locate_func_from(parser, fmodule, fname, fparams)?;

        Ok(Self {
            parser,
            active: position,
        })
    }

    fn locate_func(
        &self,
        fmodule: usize,
        fname: &str,
        fparams: Vec<Type>,
    ) -> Result<Position<'f>, ParseFault> {
        Self::locate_func_from(self.parser, fmodule, fname, fparams)
    }

    fn locate_func_from(
        parser: &'f Parser,
        fmodule: usize,
        fname: &str,
        fparams: Vec<Type>,
    ) -> Result<Position<'f>, ParseFault> {
        let self_mod = &parser.modules[fmodule];

        if super::is_valid_identifier(fname) {
            // Function

            // Find function name
            match self_mod.functions.get(fname) {
                // No functions with this name exists in that scope
                None => match Self::try_locate_from_prelude(parser, fname, false, &fparams) {
                    // But it does exist in prelude
                    Ok(position) => Ok(position),
                    Err(had_variants) => {
                        if had_variants {
                            Err(ParseFault::FunctionVariantNotFound(
                                fname.to_owned(),
                                fparams.clone(),
                                super::PRELUDE_FID,
                            ))
                        } else {
                            Err(ParseFault::FunctionNotFound(
                                fname.to_owned(),
                                super::PRELUDE_FID,
                            ))
                        }
                    }
                },

                // Find function variants (which function takes these exact types?)
                Some(variants) => match variants.get(&fparams) {
                    // Function name exists but no variant of that function takes these parameters
                    None => match Self::try_locate_from_prelude(parser, fname, false, &fparams) {
                        Ok(position) => Ok(position),
                        Err(_had_variants) => Err(ParseFault::FunctionVariantNotFound(
                            fname.to_owned(),
                            fparams.clone(),
                            fmodule,
                        )),
                    },

                    // Found!
                    Some(func) => Ok(Position {
                        function: Box::new(&func.0),
                        module: fmodule,
                        funcid: func.1,
                    }),
                },
            }
        } else {
            // Operator

            let key = &[fparams[0].clone(), fparams[1].clone()];

            // Find operator name
            match self_mod.operators.get(fname) {
                // No operators with this name exists in that scope
                None => match Self::try_locate_from_prelude(parser, fname, true, &fparams) {
                    Ok(position) => Ok(position),
                    Err(had_variants) => {
                        if had_variants {
                            Err(ParseFault::OperatorVariantNotFound(
                                fname.to_owned(),
                                [fparams[0].clone(), fparams[1].clone()],
                                super::PRELUDE_FID,
                            ))
                        } else {
                            Err(ParseFault::OperatorNotFound(fname.to_owned(), fmodule))
                        }
                    }
                },

                Some(variants) => match variants.get(key) {
                    // Operator name exists but no variant of that operator takes these parameters
                    None => match Self::try_locate_from_prelude(parser, fname, true, &fparams) {
                        Ok(position) => Ok(position),
                        Err(_had_variants) => Err(ParseFault::OperatorVariantNotFound(
                            fname.to_owned(),
                            key.clone(),
                            fmodule,
                        )),
                    },

                    // Found!
                    Some(op) => Ok(Position {
                        function: Box::new(&op.0),
                        module: fmodule,
                        funcid: op.1,
                    }),
                },
            }
        }
    }

    fn try_locate_from_prelude(
        parser: &'f Parser,
        fname: &str,
        op: bool,
        fparams: &[Type],
    ) -> Result<Position<'f>, bool> {
        let prelude = &parser.modules[super::PRELUDE_FID];
        if op {
            let (op, opid) = prelude
                .operators
                .get(fname)
                .ok_or(false)?
                .get(fparams)
                .ok_or(true)?;
            Ok(Position {
                module: super::PRELUDE_FID,
                function: Box::new(op),
                funcid: *opid,
            })
        } else {
            let (func, funcid) = prelude
                .functions
                .get(fname)
                .ok_or(false)?
                .get(fparams)
                .ok_or(true)?;
            Ok(Position {
                module: super::PRELUDE_FID,
                function: Box::new(func),
                funcid: *funcid,
            })
        }
    }

    fn fork(&mut self, position: Position<'f>) -> Self {
        Self {
            active: position,
            parser: self.parser,
        }
    }

    pub fn run(&mut self) -> Result<Type, ParseError> {
        // let func = self.func().map_err(|e| e.to_err(0))?;
        // let returns = func.get_return().clone();
        let entry = self.active.function.entry_point();
        let got = self.type_check(&entry)?;

        self.active
            .function
            .check_return(&got)
            .map_err(|e| e.to_err(0))?;

        Ok(got)
    }

    fn type_check(&mut self, t: &Token) -> Result<Type, ParseError> {
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
                        let new_pos = self
                            .locate_func(self.active.module, ident, param_types)
                            .map_err(|e| e.to_err(entity.source_index))?;

                        self.fork(new_pos).run()?
                    }
                    RawToken::ExternalIdentifier(entries) => {
                        let new_fid = self.parser.modules[self.active.module]
                            .imports
                            .get(&entries[0])
                            .copied()
                            .ok_or_else(|| panic!("ET: Import {} not found", entries[0]))?;
                        let new_pos = self
                            .locate_func(new_fid, &entries[1], param_types)
                            .map_err(|e| e.to_err(t.source_index))?;
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
                let new_pos = self
                    .locate_func(self.active.module, &op.identifier, vec![left_t, right_t])
                    .map_err(|e| e.to_err(t.source_index))?;
                self.fork(new_pos).run()?
            }
            RawToken::Identifier(constant_ident) => {
                let func = &self.active.function;
                if let Some(param_id) = func.get_parameter(constant_ident) {
                    func.get_parameter_type(param_id).clone()
                } else {
                    let new_pos = self
                        .locate_func(self.active.module, constant_ident, vec![])
                        .map_err(|e| e.to_err(t.source_index))?;
                    self.fork(new_pos).run()?
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
