use super::FunctionBuilder;
use crate::error::Leaf;
use crate::identifier::r#type::{BaseType, Type};
use crate::parser::error::ParseError;
use crate::parser::index::Index;
use crate::parser::tokenizer::token::{Key, Token};
use crate::parser::tokenizer::Tracked;
use crate::parser::ParseModule;

pub struct Checker<'a> {
    index: &'a Index,
    modules: &'a [ParseModule],
    function: (usize, usize),
    generics: Vec<Type>,
    tbuf: Option<&'a [Tracked<Token>]>,
}

impl<'a> Checker<'a> {
    pub fn new(index: &'a Index, modules: &'a [ParseModule]) -> Self {
        Self {
            index,
            modules,
            function: (0, 0),
            generics: Vec::new(),
            tbuf: None,
        }
    }
    pub fn dive_func(&self, fid: usize, funcid: usize) -> Self {
        Self {
            index: self.index,
            modules: self.modules,
            generics: Vec::new(),
            function: (fid, funcid),
            tbuf: None,
        }
    }
    pub fn dive_tbuf(&self, tbuf: &'a [Tracked<Token>]) -> Self {
        Self {
            index: self.index,
            modules: self.modules,
            generics: Vec::new(),
            function: self.function,
            tbuf: Some(tbuf),
        }
    }

    pub fn validate_func(mut self, given: &[Type]) -> Result<(), ParseError> {
        // Check parameter amount and infer generic types
        for (i, t) in given.iter().enumerate() {
            match self.func().header.parameters.get(i) {
                // TODO: This code is just awful, definitely need to redo this part. Must've been
                // like 4am or something.
                Some(Type::Generic(n)) => {
                    let n = *n;
                    match self.generics.get(n as usize) {
                        None => self.generics.insert(n as usize, t.clone()),
                        Some(expected) => {
                            if expected != t {
                                let func = self.func();
                                return Err(ParseError::new(
                                    func.file.clone(),
                                    Leaf::FuncTypeMismatch(
                                        func.header.clone(),
                                        expected.clone(),
                                        t.clone(),
                                    ),
                                ));
                            }
                        }
                    }
                }
                Some(expected) => {
                    if expected != t {
                        let func = self.func();
                        return Err(ParseError::new(
                            func.file.clone(),
                            Leaf::FuncTypeMismatch(
                                func.header.clone(),
                                expected.clone(),
                                t.clone(),
                            ),
                        ));
                    }
                }
                None => {
                    return Err(ParseError::new(
                        self.func().file.clone(),
                        Leaf::ToManyParams(self.func().header.clone(), t.clone()),
                    )
                    .with_linen(self.func().line))
                }
            }
        }

        // Anything can be converted to nothing
        if self.func().header.returns == Type::Base(BaseType::Nothing) {
            return self.run(0);
        }
        // Make sure it returns the correct type
        let given = match self.discover_type(&self.func().tokens[0]) {
            Found(t) => t,
            // Except for when we don't care
            UnsafeValid => return self.run(0),
        };
        if given != self.func().header.returns {
            return Err(ParseError::new(
                self.func().file.clone(),
                Leaf::FuncRetWrongType(self.func().header.clone(), given.clone()),
            ));
        }
        // Verify typing of the function body
        self.run(0)
    }

    fn run(&mut self, i: usize) -> Result<(), ParseError> {
        if i >= self.tbuf().len() {
            return Ok(());
        }
        let token = &self.tbuf()[i];
        match &token.inner {
            Token::Function(fid, funcid) => {
                let fid = *fid;
                let funcid = *funcid;

                // TODO: We're getting the func from modules twice
                let func = &self.modules[fid].functions[funcid];
                let (_walked, params) = self.check_params(i + 1, func.header.parameters.len())?;
                self.dive_func(fid, funcid)
                    .validate_func(&params)
                    .map(|_| ())
            }
            Token::TrackedRuntimeList(list) => {
                let first_type = self.discover_type(&list[0]).unwrap();

                for entity in list.iter() {
                    // This list contains inlined code, lets check that as well
                    if let Token::TrackedGroup(entity_contents) = &entity.inner {
                        self.dive_tbuf(entity_contents)
                            .run(0)
                            .map_err(|e| e.with_linen(entity.position))?;
                    }
                    // Does this entity in the list have the same type as the others?
                    let this_type = self.discover_type(entity).unwrap();
                    if this_type != first_type {
                        return Err(ParseError::new(
                            self.func().file.clone(),
                            Leaf::ListTypeMismatch(first_type, this_type),
                        )
                        .with_linen(entity.position));
                    }
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn check_params(&self, mut i: usize, amount: usize) -> Result<(usize, Vec<Type>), ParseError> {
        let old = i;
        let mut param_types: Vec<Type> = Vec::with_capacity(amount);
        loop {
            if param_types.len() >= amount {
                break;
            }
            let token = &self.tbuf()[i];
            match &token.inner {
                Token::V(v) => param_types.push(v.into()), // TODO: For equality operators this is wrong
                Token::ParamValue(n) => param_types.push(self.func().header.parameters[*n].clone()),
                Token::TrackedRuntimeList(list) => {
                    self.dive_tbuf(list).run(0)?;
                    let first_type = self.discover_type(&list[0]).unwrap();

                    for entity in list.iter() {
                        let this_type = self.discover_type(entity).unwrap();
                        if this_type != first_type {
                            return Err(ParseError::new(
                                self.func().file.clone(),
                                Leaf::ListTypeMismatch(first_type, this_type),
                            )
                            .with_linen(entity.position));
                        }
                    }

                    param_types.push(first_type);
                }
                Token::WhereValue(n) => {
                    let where_buf = &self.func().wheres[*n];
                    let t = match self.discover_type(&where_buf[0]) {
                        Found(t) => t,
                        UnsafeValid => Type::UnsafeValid,
                    };
                    self.dive_tbuf(&where_buf).run(0)?;
                    param_types.push(t);
                }
                Token::TrackedGroup(tbuf) => {
                    let t = match self.discover_type(&tbuf[0]) {
                        Found(t) => t,
                        UnsafeValid => Type::UnsafeValid,
                    };
                    self.dive_tbuf(tbuf).run(0)?;
                    param_types.push(t);
                }
                Token::TrackedIfStatement(branches, else_do) => {
                    let mut types = Vec::with_capacity(branches.len() + 1);

                    // Conditional branches
                    for (cond, action) in branches.iter() {
                        self.dive_tbuf(cond).run(0)?;
                        let t = self.discover_type(&action[0]);
                        self.dive_tbuf(action).run(0)?;
                        types.push(t.unwrap());
                    }
                    // Else branch
                    types.push(self.discover_type(&else_do[0]).unwrap());
                    self.dive_tbuf(&else_do).run(0)?;

                    if !all_same(&types) {
                        return Err(ParseError::new(
                            self.func().file.clone(),
                            Leaf::IfStatementTypeMismatch(types),
                        ));
                    }
                }
                Token::Function(fid, funcid) => {
                    // Is this enough? I don't think it is right. I need to go through the
                    // parameters. The problem is that we're stateless and I cannot index forward
                    // the parent.
                    // Actually nevermind, Because I'm in check_params. I can safely assume that
                    // this function has no parameters.
                    let mut c = self.dive_func(*fid, *funcid);
                    let t = c.func().header.returns.clone();
                    c.run(0)?;
                    param_types.push(t);
                }
                Token::Key(Key::PrimitiveExit) => param_types.push(Type::UnsafeValid),
                Token::BridgedFunction(_) => param_types.push(Type::UnsafeValid),
                Token::Group(_) => panic!(),
                _ => {}
            }
            i += 1;
        }
        // We have all the parameters we're supposed to get. Lets make sure there's not another one
        // coming.
        let next = &self.tbuf().get(i);
        if let Some(token) = next.map(|v| &v.inner) {
            // Operators are allowed to come afterwards
            if let Token::Key(Key::Operator(_)) = token {
            } else {
                let func = self.func();
                return Err(ParseError::new(
                    func.file.clone(),
                    Leaf::ToManyParams(
                        func.header.clone(),
                        self.discover_type(next.unwrap()).unwrap(),
                    ),
                )
                .with_linen(next.unwrap().position));
                //panic!("ERROR_TODO: Extra token after all required parameters. TODO: This error might or might not give lots of false positives, lets find out!\nOh btw the one I found was {:?}", token);
            }
        }

        Ok((i - old, param_types))
    }

    fn tbuf(&self) -> &[Tracked<Token>] {
        self.tbuf
            .unwrap_or(&self.modules[self.function.0].functions[self.function.1].tokens)
    }
    fn func(&self) -> &FunctionBuilder {
        &self.modules[self.function.0].functions[self.function.1]
    }

    fn discover_type(&self, t: &Tracked<Token>) -> DiscoverResult {
        match &t.inner {
            Token::V(v) => Found(v.into()),
            Token::LambdaValue(_lid) => panic!("Lambda type from token is like not a thing atm"),
            Token::ParamValue(n) => Found(self.func().header.parameters[*n].clone()),
            Token::WhereValue(n) => self.discover_type(&self.func().wheres[*n][0]),
            Token::BridgedFunction(_) => UnsafeValid,
            Token::TrackedGroup(tbuf) => self.discover_type(&tbuf[0]),
            Token::TrackedIfStatement(branches, _else) => self.discover_type(&branches[0].1[0]),
            Token::TrackedRuntimeList(list) => self.discover_type(&list[0]),
            Token::Recurse => DiscoverResult::Found(self.func().header.returns.clone()),
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

fn all_same<T: PartialEq>(list: &[T]) -> bool {
    if list.is_empty() {
        return true;
    }
    let first = &list[0];
    list.iter().all(|item| *item == *first)
}

use DiscoverResult::*;
#[derive(Debug)]
enum DiscoverResult {
    Found(Type),
    UnsafeValid,
}

impl DiscoverResult {
    fn unwrap(self) -> Type {
        match self {
            Found(t) => t,
            UnsafeValid => Type::UnsafeValid,
        }
    }
}
