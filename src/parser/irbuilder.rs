use crate::env::Environment;
use crate::ir;
use crate::parser::{ParseError, ParseFault, Parser, Type};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

mod builder;
mod checker;
pub mod generics;

//#[derive(Debug)]
pub struct IrBuilder {
    parser: Parser,
    completed: RefCell<Vec<ir::Entity>>,
    environment: Rc<Environment>,
    assigned_indexes: RefCell<HashMap<(usize, usize), usize>>,
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

    fn gen_id(&self, fid: usize, funcid: usize) -> usize {
        let mut indexes = self.assigned_indexes.borrow_mut();
        match indexes.get(&(fid, funcid)) {
            None => {
                let new_index = indexes.len();
                indexes.insert((fid, funcid), new_index);
                new_index
            }
            Some(existing) => *existing,
        }
    }
    fn try_get_id(&self, fid: usize, funcid: usize) -> Option<usize> {
        self.assigned_indexes.borrow().get(&(fid, funcid)).copied()
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
        let actual_return_value = match self.type_check(&func.body, fid, funcid, &generics) {
            Ok(t) => t,
            Err(e) => return e.with_parser(self.parser).into(),
        };
        let decoded_return = generics.decoded(&func.returns).map_err(|e| e.to_err(0))?;
        if actual_return_value != decoded_return && func.returns != Type::Nothing {
            return ParseFault::FnTypeReturnMismatch(Box::new(func.clone()), actual_return_value)
                .to_err(func.body.source_index)
                .with_source_load(&self.environment, &self.parser.modules[fid].module_path)
                .with_parser(self.parser)
                .into();
        }
        let findex = self.gen_id(newfid, funcid);
        let entry = self.token_to_ir(newfid, funcid, &func.body.inner);
        self.complete(findex, entry);

        Ok((
            self.completed.into_inner(),
            self.assigned_indexes.borrow()[&(newfid, funcid)],
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

        let func = &self.parser.modules[newfid].functions[funcid];
        let actual_return_value = self.type_check(&func.body, newfid, funcid, &generics)?;
        if actual_return_value != generics.decoded(&func.returns).map_err(|e| e.to_err(0))? {
            return ParseFault::FnTypeReturnMismatch(Box::new(func.clone()), actual_return_value)
                .to_err(func.body.source_index)
                .into();
        }

        match self.try_get_id(newfid, funcid) {
            None => {
                let findex = self.gen_id(newfid, funcid);
                let entry = self.token_to_ir(newfid, funcid, &func.body.inner);
                self.complete(findex, entry);
            }
            Some(findex) => {
                dbg!(&self.assigned_indexes.borrow(), &self.completed.borrow());
                if self.should_replace(findex) {
                    let entry = self.token_to_ir(newfid, funcid, &func.body.inner);
                    self.complete(findex, entry);
                }
            }
        }

        Ok(actual_return_value)
    }
}
