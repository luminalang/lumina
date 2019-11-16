use crate::env::Environment;
use crate::ir;
use crate::parser::{ParseError, ParseFault, Parser, Type};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

mod builder;
mod checker;
mod fsource;
pub mod generics;
use fsource::FunctionSource;

//#[derive(Debug)]
pub struct IrBuilder {
    parser: Parser,
    completed: RefCell<Vec<ir::Entity>>,
    environment: Rc<Environment>,
    assigned_indexes: RefCell<HashMap<FunctionSource, usize>>,
}

impl IrBuilder {
    pub fn new(parser: Parser, env: Rc<Environment>) -> Self {
        Self {
            parser,
            environment: env,
            assigned_indexes: RefCell::default(),
            completed: RefCell::new(Vec::with_capacity(5)),
        }
    }

    fn should_replace(&self, findex: usize) -> bool {
        use std::mem::discriminant;
        match self.completed.borrow().get(findex) {
            Some(a) => discriminant(a) == discriminant(&ir::Entity::Unique),
            None => true,
        }
    }

    pub fn start_type_checker(
        self,
        fid: usize,
        funcname: &str,
        params: &[Type],
    ) -> Result<(Vec<ir::Entity>, usize), ParseError> {
        let (funcid, newfid, generics) = match self
            .find_matching_function(fid, fid, funcname, params)
            .map_err(|e| {
                e.to_err(0)
                    .with_source_load(&self.environment, &self.parser.modules[fid].module_path)
            }) {
            Ok(a) => a,
            Err(e) => return Err(e.with_parser(self.parser)),
        };

        let func = &self.parser.modules[fid].functions[funcid];
        let actual_return_value =
            match self.type_check(&func.body, &FunctionSource::from((fid, funcid))) {
                Ok(t) => t,
                Err(e) => return e.with_parser(self.parser).into(),
            };
        if actual_return_value != func.returns && func.returns != Type::Nothing {
            return ParseFault::FnTypeReturnMismatch(Box::new(func.clone()), actual_return_value)
                .to_err(func.body.source_index)
                .with_source_load(&self.environment, &self.parser.modules[fid].module_path)
                .with_parser(self.parser)
                .into();
        }
        let findex = self.gen_id(Cow::Owned(FunctionSource::from((newfid, funcid))));
        let entry = self.token_to_ir(&FunctionSource::from((newfid, funcid)), &func.body.inner);
        self.complete(findex, entry);

        Ok((
            self.completed.into_inner(),
            self.assigned_indexes.borrow()[&FunctionSource::from((newfid, funcid))],
        ))
    }

    fn type_check_function(
        &self,
        fid: usize,
        ident: &str,
        params: &[Type],
    ) -> Result<Type, ParseError> {
        let (funcid, newfid, generics) = self
            .find_matching_function(fid, fid, ident, params)
            .map_err(|e| {
                e.to_err(0)
                    .with_source_load(&self.environment, &self.parser.modules[fid].module_path)
            })?;

        // let func = &self.parser.modules[newfid].functions[funcid];
        let source = if generics.has_generics() {
            let mut new = self.parser.modules[fid].functions[funcid].clone();
            generics.replace_all(&mut new);
            FunctionSource::Owned(newfid, new)
        } else {
            FunctionSource::from((newfid, funcid))
        };
        let actual_return_value = self.type_check(&source.body(&self.parser), &source)?;
        if actual_return_value != *source.returns(&self.parser) {
            return ParseFault::FnTypeReturnMismatch(
                Box::new(source.func(&self.parser).clone()), // TODO: Clone can be avoided fairly easily
                actual_return_value,
            )
            .to_err(source.func(&self.parser).body.source_index)
            .into();
        }

        match self.try_get_id(&source) {
            None => {
                let findex = self.gen_id(Cow::Borrowed(&source));
                let entry = self.token_to_ir(&source, &source.func(&self.parser).body.inner);
                self.complete(findex, entry);
            }
            Some(findex) => {
                if self.should_replace(findex) {
                    let entry = self.token_to_ir(&source, &source.func(&self.parser).body.inner);
                    self.complete(findex, entry);
                }
            }
        }

        /*
        match self.try_get_id(FunctionSource::from((newfid, funcid))) {
            None => {
                // TODO: This is invalid if we've got generics.
                let findex = self.gen_id(newfid, funcid);
                let entry = self.token_to_ir(newfid, funcid, &source.func(&self.parser).body.inner);
                self.complete(findex, entry);
            }
            Some(findex) => {
                if self.should_replace(findex) {
                    let entry =
                        self.token_to_ir(newfid, funcid, &source.func(&self.parser).body.inner);
                    self.complete(findex, entry);
                }
            }
        }
        */

        Ok(actual_return_value)
    }
}
