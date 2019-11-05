use crate::env::Environment;
use crate::ir;
use crate::parser::{ParseError, ParseFault, Parser, Type};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

mod builder;
mod checker;

#[derive(Debug)]
pub struct IrBuilder {
    parser: Parser,
    completed: RefCell<Vec<ir::Entity>>,
    environment: Rc<Environment>,
    assigned_indexes: RefCell<HashMap<(usize, usize), usize>>,
}

/*
We're gonna make the typechecker also append to irbuilder.completed.
How? Not quite sure. But I think we can just wrap type_check_function
*/

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
        // TODO: Recursive data pattern will forever typecheck
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

        if self
            .assigned_indexes
            .borrow()
            .get(&(newfid, funcid))
            .is_none()
        {
            // let entry = self.token_to_ir()

            // TODO: Maybe I can inline by pop'ing of the self.completed and self.assigned_indexes?
            // We are working in reverse so that'd make a lot of sense and as long as we're not
            // multithreadded when doing this it should be fine.
            //
            // We actually can't pop off assigned_indexes though since it's a hashmap.
            //
            // Perhaps we shouldn't even pop it off. Some other function will probably use it so
            // might as well let it stay I guess.

            let entry = self.token_to_ir(newfid, funcid, &func.body.inner);
            let findex = self.gen_id(newfid, funcid);
            self.complete(findex, entry);
        }

        Ok(actual_return_value)
    }
}
