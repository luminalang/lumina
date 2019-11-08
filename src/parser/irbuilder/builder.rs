use super::IrBuilder;
use crate::ir;
use crate::parser::{Inlined, RawToken, Type, PRELUDE_FID};

impl IrBuilder {
    pub fn token_to_ir(&self, fid: usize, funcid: usize, t: &RawToken) -> ir::Entity {
        match t {
            RawToken::Parameterized(box takes, has, param_types) => {
                let params = has
                    .iter()
                    .map(|t| self.token_to_ir(fid, funcid, &t.inner))
                    .collect::<Vec<ir::Entity>>();
                match &takes.inner {
                    RawToken::Identifier(ident) => {
                        let ptypes: &[Type] = &param_types.borrow();
                        dbg!(ident, ptypes);
                        let (newfid, newfuncid) = self.parser.modules[fid]
                            .function_ids
                            .get(ident.as_str())
                            .map(|a| (fid, super::generics::generic_search(a, ptypes).unwrap().0))
                            .unwrap_or_else(|| {
                                (
                                    PRELUDE_FID,
                                    self.parser.modules[PRELUDE_FID].function_ids[ident.as_str()]
                                        [ptypes],
                                )
                            });
                        let findex = self.gen_id(newfid, newfuncid);
                        ir::Entity::FunctionCall(findex as u32, params)
                    }
                    RawToken::ExternalIdentifier(entries) => {
                        let ptypes: &[Type] = &param_types.borrow();
                        let newfid = self.parser.modules[fid].imports[&entries[0]];
                        let funcid = self.parser.modules[newfid].function_ids[&entries[1]][ptypes];
                        let findex = self.gen_id(fid, funcid);
                        ir::Entity::FunctionCall(findex as u32, params)
                    }
                    RawToken::RustCall(id, _returns_type) => ir::Entity::RustCall(*id, params),
                    _ => panic!("{:?} cannot take parameters", takes.inner),
                }
            }
            // Either constant or parameter
            RawToken::Identifier(ident) => {
                let func = &self.parser.modules[fid].functions[funcid];
                for (i, param) in func.parameter_names.iter().enumerate() {
                    if ident == param {
                        return ir::Entity::Parameter(i as u16);
                    }
                }
                let no_params: &[Type] = &[];
                let newfuncid = self.parser.modules[fid].function_ids[ident][no_params];
                self.token_to_ir(
                    fid,
                    newfuncid,
                    &self.parser.modules[fid].functions[newfuncid].body.inner,
                )
            }
            // External Constant
            RawToken::ExternalIdentifier(entries) => {
                let newfid = self.parser.modules[fid].imports[&entries[0]];
                let no_params: &[Type] = &[];
                let newfuncid = self.parser.modules[newfid].function_ids[&entries[1]][no_params];
                self.token_to_ir(
                    newfid,
                    newfuncid,
                    &self.parser.modules[fid].functions[newfuncid].body.inner,
                )
            }
            RawToken::Inlined(inlined) => match &inlined {
                Inlined::Int(n) => ir::Entity::Inlined(ir::Value::Int(*n)),
                Inlined::Float(n) => ir::Entity::Inlined(ir::Value::Float(*n)),
                Inlined::Bool(b) => ir::Entity::Inlined(ir::Value::Bool(*b)),
                _ => unimplemented!(),
            },
            RawToken::FirstStatement(entries) => ir::Entity::FirstStatement(ir::First::from(
                entries
                    .iter()
                    .map(|e| self.token_to_ir(fid, funcid, &e.inner))
                    .collect::<Vec<ir::Entity>>(),
            )),
            RawToken::IfExpression(expr) => {
                let mut buf = Vec::with_capacity((expr.branches.len() * 2) + 1);
                buf.push(self.token_to_ir(fid, funcid, &expr.else_branch.inner));
                for (cond, eval) in expr.branches.iter() {
                    buf.push(self.token_to_ir(fid, funcid, &cond.inner));
                    buf.push(self.token_to_ir(fid, funcid, &eval.inner));
                }
                ir::Entity::IfExpression(ir::If::from(buf))
            }
            RawToken::Unimplemented => ir::Entity::Unimplemented,
            RawToken::List(entries) => ir::Entity::List(
                entries
                    .iter()
                    .map(|t| self.token_to_ir(fid, funcid, &t.inner))
                    .collect(),
            ),
            _ => unimplemented!("{:?}", t),
        }
    }

    pub fn complete(&self, findex: usize, entity: ir::Entity) {
        let mut stack = self.completed.borrow_mut();
        if findex > stack.len() {
            stack.resize(findex, ir::Entity::Unique)
        }
        stack.insert(findex, entity);
    }
}
