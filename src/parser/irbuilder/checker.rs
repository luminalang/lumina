use super::generics::*;
use super::IrBuilder;
use crate::parser::{Inlined, ParseError, ParseFault, RawToken, Token, Type, PRELUDE_FID};

impl IrBuilder {
    pub fn type_check(
        &self,
        token: &Token,
        fid: usize,
        funcid: usize,
        generics: &Generics,
    ) -> Result<Type, ParseError> {
        let r#type = match &token.inner {
            RawToken::Inlined(inlined) => match inlined {
                Inlined::Int(_) => Type::Int,
                Inlined::Float(_) => Type::Float,
                Inlined::Bool(_) => Type::Bool,
                Inlined::Nothing => Type::Nothing,
            },
            RawToken::Unimplemented => generics
                .decoded(&self.parser.modules[fid].functions[funcid].returns)
                .unwrap(),
            RawToken::ByPointer(box t) => {
                match &t.inner {
                    RawToken::Identifier(ident) => {
                        let func = &self.parser.modules[fid].functions[funcid];
                        if let Some(paramid) = func.get_parameter(ident) {
                            let param = func.get_parameter_type(paramid);
                            if let Type::Function(_) = param {
                                generics
                                    .decoded(param)
                                    .map_err(|e| e.to_err(t.source_index))?
                            } else {
                                panic!("ET: the value {:?} cannot be passed as closure", param)
                            }
                        } else {
                            unimplemented!();
                        }
                    }
                    RawToken::ExternalIdentifier(entries) => {
                        let newfid = self.parser.modules[fid]
                            .imports
                            .get(&entries[0])
                            .ok_or_else(|| {
                                ParseFault::ModuleNotImported(entries[0].clone())
                                    .to_err(token.source_index)
                            })?;
                        unimplemented!();
                    }
                    // TODO: Parameterized()
                    _ => panic!("{:?} cannot be passed as closure", t.inner), // ET? I think body.rs already handles faults here
                }
            }
            RawToken::RustCall(_bridged_id, r#type) => r#type.clone(),
            RawToken::FirstStatement(entries) => {
                for entry in entries[0..entries.len() - 1].iter() {
                    self.type_check(entry, fid, funcid, generics)?;
                }
                self.type_check(entries.last().unwrap(), fid, funcid, generics)?
            }
            RawToken::Parameterized(box entry, params, p_types) => {
                let mut param_types = match p_types.try_borrow_mut() {
                    Ok(a) => a,
                    Err(_) => {
                        // If it's already borrowed then that means that this is a recursive call.
                        // Therefore we can assume that it's already being type checked!
                        return Ok(generics
                            .decoded(&self.find_return_type(fid, &p_types.borrow(), &entry.inner))
                            .unwrap());
                    }
                };
                if param_types.is_empty() {
                    for param in params.iter() {
                        param_types.push(
                            generics
                                .decoded(&self.type_check(param, fid, funcid, generics)?)
                                .map_err(|e| e.to_err(token.source_index))?,
                        )
                    }
                }
                drop(param_types);
                match &entry.inner {
                    RawToken::Identifier(ident) => self
                        .type_check_function(fid, ident, &p_types.borrow())
                        .map_err(|e| e.fallback(token.source_index))?,
                    RawToken::ExternalIdentifier(entries) => {
                        let newfid =
                            match self.parser.modules[fid].imports.get(&entries[0]).copied() {
                                Some(fid) => fid,
                                None => {
                                    return ParseFault::ModuleNotImported(entries[0].clone())
                                        .to_err(token.source_index)
                                        .into()
                                }
                            };
                        self.type_check_function(newfid, &entries[1], &p_types.borrow())
                            .map_err(|e| e.fallback(token.source_index))?
                    }
                    RawToken::RustCall(_bridged_id, r#type) => r#type.clone(),
                    _ => panic!("{:#?} cannot take parameters", entry.inner),
                }
            }
            RawToken::Identifier(ident) => {
                {
                    let func = &self.parser.modules[fid].functions[funcid];
                    if let Some(paramid) = func.get_parameter(ident) {
                        return Ok(generics
                            .decoded(&func.get_parameter_type(paramid).clone())
                            .unwrap());
                    };
                }

                // This is only for leaf constants. Since other functions will be RawToken::Parameterized
                self.type_check_function(fid, ident, &[])?
            }
            RawToken::ExternalIdentifier(entries) => {
                let newfid = match self.parser.modules[fid].imports.get(&entries[0]).copied() {
                    Some(fid) => fid,
                    None => {
                        return ParseFault::ModuleNotImported(entries[0].clone())
                            .to_err(token.source_index)
                            .into()
                    }
                };
                self.type_check_function(newfid, &entries[1], &[])?
            }

            RawToken::IfExpression(expr) => {
                let mut expect_type = None;
                for (cond, eval) in expr.branches.iter() {
                    let cv = self.type_check(cond, fid, funcid, generics)?;
                    if cv != Type::Bool {
                        panic!(
                            "ET: Condition must result in true or false, but I got {:?}",
                            cv
                        );
                    }
                    let ev = self.type_check(eval, fid, funcid, generics)?;
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
                let ev = self.type_check(&expr.else_branch, fid, funcid, generics)?;
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
            RawToken::List(entries) => {
                let mut of_t: Option<Type> = None;
                for (i, entry) in entries.iter().enumerate() {
                    let r#type = self.type_check(entry, fid, funcid, generics)?;
                    match &of_t {
                        Some(t) => {
                            if *t != r#type {
                                return ParseFault::ListEntryTypeMismatch(r#type, t.clone(), i)
                                    .to_err(entry.source_index)
                                    .into();
                            }
                        }
                        None => of_t = Some(r#type),
                    }
                }
                Type::List(Box::new(of_t.unwrap_or_else(|| Type::Generic(0))))
            }
            _ => panic!("Cannot discover type of {:#?}", token),
        };
        Ok(r#type)
    }

    pub fn find_matching_function(
        &self,
        self_fid: usize,
        fid: usize,
        funcname: &str,
        params: &[Type],
    ) -> Result<(usize, usize, Generics), ParseFault> {
        let module = &self.parser.modules[fid];
        let variants = match module.function_ids.get(funcname) {
            None => {
                return if fid == self_fid && fid != PRELUDE_FID {
                    // Wasn't found, Try prelude
                    match self.find_matching_function(PRELUDE_FID, PRELUDE_FID, funcname, params)
                        // Switch out the PRELUDE fid's with this file's fids since it makes
                        // more sense to assume local over prelude.
                        // .map_err(|_| ParseFault::FunctionNotFound(funcname.to_string(), fid))
                    {
                        Ok(a) => Ok(a),
                        Err(e) => match e {
                            ParseFault::FunctionNotFound(name, _prelude) => Err(ParseFault::FunctionNotFound(name, fid)),
                            ParseFault::FunctionVariantNotFound(name, params, prelude) => Err(ParseFault::FunctionVariantNotFound(name, params, prelude)),
                            _ => unreachable!(),
                        }
                    }
                } else {
                    Err(ParseFault::FunctionNotFound(funcname.to_string(), self_fid))
                };
            }
            Some(variants) => variants,
        };

        // Exact match?
        if let Some(funcid) = variants.get(params).copied() {
            return Ok((funcid, fid, Generics::empty()));
        };

        // Maybe there's a generic match?
        if let Some((funcid, generics)) = generic_search(variants, params) {
            return Ok((funcid, fid, generics));
        };

        Err(ParseFault::FunctionVariantNotFound(
            funcname.to_string(),
            params.to_vec(),
            fid,
        ))
    }

    fn find_return_type(&self, fid: usize, params: &[Type], t: &RawToken) -> Type {
        let me = &self.parser.modules[fid];
        match t {
            RawToken::Identifier(ident) => {
                me.functions[me.function_ids[ident][params]].returns.clone()
            }
            RawToken::ExternalIdentifier(entries) => {
                let newfid = me.imports[&entries[0]];
                let newme = &self.parser.modules[newfid];
                newme.functions[newme.function_ids[&entries[1]][params]]
                    .returns
                    .clone()
            }
            _ => unimplemented!(),
        }
    }
}
