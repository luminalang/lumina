use super::{FunctionBuilder, Inlined, Parser, RawToken, Token, Type};
use crate::evaler::bridge;

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

impl<'f> TypeChecker<'f> {
    pub fn new(
        parser: &'f Parser,
        fmodule: usize,
        fname: &str,
        fparams: Vec<Type>,
        freturn: Type,
    ) -> Self {
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
        let (func, _funcid) = self.parser.modules[self.active.module]
            .functions
            .get(&self.active.function)
            .ok_or_else(|| panic!("ET: Function {:?} not found", self.active.function))?;
        let got = self.type_check(&func.body[0])?;
        if func.returns != got && func.returns != Type::Nothing {
            panic!(
                "ET: Return type mismatch. Wanted {} got {}",
                func.returns, got
            );
        }
        Ok(got)
    }

    #[inline]
    fn func(&self) -> &FunctionBuilder {
        &self
            .parser
            .get_function(
                self.active.module,
                &self.active.function.0,
                self.active.function.1.clone(),
            )
            .unwrap()
            .0
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
            RawToken::Identifier(constant_ident) => {
                if let Some(param_id) = self.func().get_parameter(constant_ident) {
                    self.func().parameter_types[param_id].1.clone()
                } else {
                    self.fork(Position {
                        module: self.active.module,
                        function: (constant_ident.clone(), vec![]),
                    })
                    .run()?
                }
            }
            _ => panic!("Cannot discover type of {:?}", t),
        };
        Ok(r#type)
    }
}
