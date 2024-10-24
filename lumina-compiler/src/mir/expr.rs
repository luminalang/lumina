use super::func::InstCall;
use super::tcheck::SameAsCheck;
use super::Verify;
use crate::prelude::*;
use lumina_typesystem::IType;

impl<'a, 's> Verify<'a, 's> {
    pub fn type_check_expr(&mut self, expr: Tr<&hir::Expr<'s>>) -> Tr<IType> {
        let module = self.module();

        trace!("checking expression {}", expr.value);

        match expr.value {
            hir::Expr::Call(call, tanot, params) => {
                let ptypes = self.type_check_params(params);
                let lhs = ptypes.last();
                let instcall = self.type_of_callable(expr.span, lhs, call, params.len(), tanot);
                self.type_check_call(expr.span, instcall, ptypes)
            }
            hir::Expr::Pass(call, tanot, params) => {
                let ptypes = self.type_check_params(params);
                self.type_check_pass(expr.span, call, tanot, ptypes).0
            }
            hir::Expr::PassFnptr(key, tanot) => match self.type_of_func(expr.span, *key, tanot) {
                InstCall::Instantiated(instinfo) => {
                    let ptypes = instinfo.ptypes.iter().map(|t| t.value.clone()).collect();
                    let ret = instinfo.ret.value.clone();
                    self.current.push_inst(expr.span, Some(instinfo));
                    IType::fn_pointer(ptypes, ret)
                }
                InstCall::DirectRecursion | InstCall::CircularRecursion(_) => {
                    todo!("function pointer from function currently inferring recursively")
                }
                _ => unreachable!(),
            },
            hir::Expr::PassExpr(inner) => self.type_check_pass_expr((**inner).as_ref()),
            hir::Expr::Cast(expr_, ty) => {
                let ty_of_expr = self.type_check_expr((**expr_).as_ref());
                self.current.casts_and_matches.push_back(ty_of_expr);
                ty.value.clone()
            }
            hir::Expr::BuiltinOp(op, params) => {
                let exp = match *op {
                    "&&" | "||" => IType::bool(),
                    _ => panic!("unknown builtin operator: {op}"),
                };
                let lhs = self.type_check_expr(params[0].as_ref());
                let rhs = self.type_check_expr(params[1].as_ref());

                self.type_check_and_emit(lhs.as_ref(), (&exp).tr(expr.span));
                self.type_check_and_emit(rhs.as_ref(), (&exp).tr(expr.span));

                IType::bool()
            }
            hir::Expr::Access(rvar, object, field) => {
                let fieldvar = self.vars().add_field(*rvar, (**field).tr(field.span));

                let objty = self.type_check_expr((**object).as_ref());
                let exp = IType::infer(*rvar).tr(field.span);

                self.type_check_and_emit(objty.as_ref(), exp.as_ref());
                IType::infer(fieldvar)
            }
            hir::Expr::TupleAccess(object, i) => {
                let objty = self.type_check_expr((**object).as_ref());
                self.check_tuple_index(objty.as_ref(), *i)
            }
            hir::Expr::Record(var, ty, modify, fields) => {
                let fvars = fields
                    .iter()
                    .map(|(name, _)| self.vars().add_field(*var, *name))
                    .collect::<Vec<_>>();

                if let Some(ty) = ty {
                    assert!(modify.is_none());
                    self.type_check_and_emit(
                        IType::infer(*var).tr(expr.span).as_ref(),
                        ty.as_ref(),
                    );
                };

                if let Some(bind) = modify.as_deref() {
                    let ty = self.type_of(*bind).cloned();
                    let exp = IType::infer(*var).tr(expr.span);
                    let (ty, exp) = (ty.as_ref(), exp.as_ref());
                    self.type_check_and_emit(ty, exp);
                }

                for ((name, expr), fvar) in fields.iter().zip(fvars) {
                    let ty = self.type_check_expr(expr.as_ref());
                    let fty = IType::infer(fvar).tr(name.span);
                    self.type_check_and_emit(ty.as_ref(), fty.as_ref());
                }

                IType::infer(*var)
            }
            hir::Expr::Tuple(elems) => {
                let types = elems
                    .iter()
                    .map(|expr| self.type_check_expr(expr.as_ref()).value)
                    .collect();

                IType::tuple(types)
            }
            hir::Expr::Match(on, branches) => {
                let on = self.type_check_expr((**on).as_ref());

                let clen = self.current.casts_and_matches.len();
                self.current
                    .casts_and_matches
                    .push_back(IType::poison().tr(on.span)); // hackily reserve a slot

                let mut pchecker = SameAsCheck::new("match expressions");
                let mut echecker = SameAsCheck::new("match patterns");
                pchecker.include(self.type_system(), on);

                for (pat, expr) in branches.iter() {
                    let pat_ty = self.type_check_pat(pat.as_ref());
                    pchecker.include(self.type_system(), pat_ty);
                    let expr_ty = self.type_check_expr(expr.as_ref());
                    echecker.include(self.type_system(), expr_ty);
                }

                let on_ty = pchecker.finalize(module, self.ty_formatter(), &self.hir.sources);
                self.current.casts_and_matches[clen] = on_ty;

                let match_eval_ty =
                    echecker.finalize(module, self.ty_formatter(), &self.hir.sources);

                match_eval_ty.value
            }
            hir::Expr::Array(elems, len) => {
                let inner = self.type_check_same("list elements", None, elems, |this, elem| {
                    this.type_check_expr(elem.as_ref())
                });

                self.current.casts_and_matches.push_back(inner.clone());

                IType::array(**len, inner.value)
            }
            hir::Expr::GenericArray(elems, generic) => {
                let inner = self.type_check_same("list elements", None, elems, |this, elem| {
                    this.type_check_expr(elem.as_ref())
                });

                self.current.casts_and_matches.push_back(inner.clone());

                IType::const_array(**generic, inner.value)
            }
            hir::Expr::List(elems, ivar) => {
                let list = self.items.list_default;

                let ivar = Some((*ivar).tr(expr.span));
                let inner = self.type_check_same("list elements", ivar, elems, |this, elem| {
                    this.type_check_expr(elem.as_ref())
                });

                IType::list(list, vec![inner.value])
            }
            hir::Expr::Lit(lit) => match lit {
                hir::Literal::Bool(_) => IType::bool(),
                hir::Literal::Int(_, _, var) => IType::infer(*var),
                hir::Literal::Float(_) => IType::f64(),
                hir::Literal::String(_) => {
                    let record = self.items.pinfo.string;
                    IType::string(record, vec![])
                }
            },
            hir::Expr::Poison => IType::poison(),
        }
        .tr(expr.span)
    }

    pub fn type_check_params<'e, 'es: 'e + 's>(
        &mut self,
        iter: impl IntoIterator<Item = &'e Tr<hir::Expr<'es>>>,
    ) -> Vec<Tr<IType>> {
        iter.into_iter()
            .map(|expr| self.type_check_expr(expr.as_ref()))
            .collect()
    }

    fn type_check_pass_expr(&mut self, expr: Tr<&hir::Expr<'s>>) -> IType {
        match expr.value {
            hir::Expr::PassExpr(inner) => self.type_check_pass_expr((**inner).as_ref()),
            _ => {
                let ty = self.type_check_expr(expr);
                ty.value
            }
        }
    }

    pub fn type_check_call(
        &mut self,
        span: Span,
        instcall: InstCall,
        params: Vec<Tr<IType>>,
    ) -> IType {
        match instcall {
            InstCall::LocalCall(espan, ptypes, ret, _) => {
                if params.len() != ptypes.len() {
                    self.error("invalid parameter amount")
                        .eline(
                            span,
                            format!("expected {} got {}", ptypes.len(), params.len()),
                        )
                        .emit();
                }

                warn!("TODO: merge errors of call");
                for (got, exp) in params.iter().zip(&ptypes) {
                    self.type_check_and_emit(got.as_ref(), exp.tr(espan));
                }

                ret
            }
            InstCall::Local(ty) if params.is_empty() => ty.value,
            InstCall::Local(ty) => {
                // TODO: this is insufficient. We need a `type_system().is_poison(_)` method.
                // That method should also be used before any direct comparisons with `==` in TS if
                // we still have those.
                if matches!(ty.value, IType::Simple("poison")) {
                    return IType::poison();
                }

                let got = self.ty_formatter().fmt(&*ty);
                self.error("type mismatch")
                    .eline(params[0].span, "parameter given to non-closure")
                    .iline(span, format!("attempted to apply this {got} as a closure"))
                    .emit();

                IType::poison()
            }
            InstCall::Instantiated(instinfo) => {
                self.type_check_and_emit_application(span, &params, &instinfo.ptypes);

                let ret = instinfo.ret.value.clone();
                self.current.push_inst(span, Some(instinfo));

                ret
            }
            InstCall::DirectRecursion => {
                let instinfo = self.inst_direct_recursion(span);
                self.type_check_and_emit_application(span, &params, &instinfo.ptypes);

                let ret = instinfo.ret.value.clone();
                self.current.push_inst(span, Some(instinfo));

                ret
            }
            InstCall::CircularRecursion(key) => {
                let instinfo = self.inst_indirect_recursion(span, key);
                self.type_check_and_emit_application(span, &params, &instinfo.ptypes);

                let ret = instinfo.ret.value.clone();
                self.current.push_inst(span, Some(instinfo));

                ret
            }
            InstCall::TypeDependentFailure => {
                self.current.push_inst(span, None);
                IType::poison()
            }
        }
    }
}
