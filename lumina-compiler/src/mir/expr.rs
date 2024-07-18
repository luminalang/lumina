use super::func::InstCall;
use super::tcheck::SameAsCheck;
use super::Verify;
use crate::prelude::*;
use lumina_typesystem::{Container, IType, Prim, Type};

impl<'a, 's> Verify<'a, 's> {
    pub fn type_check_expr(&mut self, expr: Tr<&hir::Expr<'s>>) -> Tr<IType> {
        let module = self.module();

        trace!("checking expression {}", expr.value);

        match expr.value {
            hir::Expr::Call(call, tanot, params) => {
                let ptypes = params
                    .iter()
                    .map(|p| self.type_check_expr(p.as_ref()))
                    .collect::<Vec<_>>();
                let lhs = ptypes.last();
                let instcall = self.type_of_callable(expr.span, lhs, call, params.len(), tanot);
                self.type_check_call(expr.span, instcall, ptypes)
            }
            hir::Expr::Pass(call, tanot, params) => {
                let ptypes = params
                    .iter()
                    .map(|p| self.type_check_expr(p.as_ref()))
                    .collect();
                self.type_check_pass(expr.span, call, tanot, ptypes)
            }
            hir::Expr::PassExpr(inner) => self.type_check_pass_expr((**inner).as_ref()),
            hir::Expr::Cast(expr_, ty) => {
                let ty_of_expr = self.type_check_expr((**expr_).as_ref());
                self.current.casts.push_back(ty_of_expr);
                ty.value.clone()
            }
            hir::Expr::Access(rvar, object, field) => {
                let fieldvar = self.vars().add_field(*rvar, (**field).tr(field.span));

                let objty = self.type_check_expr((**object).as_ref());
                let exp = IType::InferringRecord(*rvar).tr(field.span);

                let ok = self.type_check_and_emit(objty.as_ref(), exp.as_ref());
                if ok {
                    IType::Field(*rvar, fieldvar)
                } else {
                    Prim::Poison.into()
                }
            }
            hir::Expr::Record(var, ty, modify, fields) => {
                for (name, _) in fields.iter() {
                    self.vars().add_field(*var, *name);
                }

                if let Some(ty) = ty {
                    assert!(modify.is_none());
                    self.assign_ty_to_rvar(expr.span, *var, ty.as_ref());
                };

                if let Some(bind) = modify.as_deref() {
                    let ty = self.type_of(*bind).cloned();
                    let exp = IType::InferringRecord(*var).tr(expr.span);
                    let (ty, exp) = (ty.as_ref(), exp.as_ref());
                    self.type_check_and_emit(ty, exp);
                }

                for (varfield, (vname, expr)) in self.vars().iter_var_fields(*var).zip(fields) {
                    let exp = IType::Field(*var, varfield).tr(vname.span);
                    let name = self.vars().name_of_field(*var, varfield);
                    assert_eq!(name, *vname);
                    let ty = self.type_check_expr(expr.as_ref());
                    self.type_check_and_emit(ty.as_ref(), exp.as_ref());
                }

                IType::InferringRecord(*var)
            }
            hir::Expr::Tuple(elems) => {
                let types = elems
                    .iter()
                    .map(|expr| self.type_check_expr(expr.as_ref()).value)
                    .collect();
                IType::Container(Container::Tuple(types))
            }
            hir::Expr::Match(on, branches) => {
                let on = self.type_check_expr((**on).as_ref());

                let mut pchecker = SameAsCheck::new("match expressions");
                let mut echecker = SameAsCheck::new("match patterns");
                pchecker.include(self.type_system(), on);

                for (pat, expr) in branches.iter() {
                    let pat_ty = self.type_check_pat(pat.as_ref());
                    pchecker.include(self.type_system(), pat_ty);
                    let expr_ty = self.type_check_expr(expr.as_ref());
                    echecker.include(self.type_system(), expr_ty);
                }

                let _on_ty = pchecker.finalize(module, self.ty_formatter(), &self.hir.sources);
                let match_eval_ty =
                    echecker.finalize(module, self.ty_formatter(), &self.hir.sources);

                match_eval_ty.value
            }
            hir::Expr::List(elems, ivar) => {
                let list = self.items.list_default;

                let mut checker = SameAsCheck::new("list elements");
                if elems.is_empty() {}

                for elem in elems {
                    let ty = self.type_check_expr(elem.as_ref());
                    checker.include(self.type_system(), ty);
                }

                // Include the inner typevar to make sure it's assigned post this point
                checker.include(self.type_system(), IType::Var(*ivar).tr(expr.span));

                let inner = checker.finalize(self.module(), self.ty_formatter(), &self.hir.sources);

                IType::List(list, vec![inner.value])
            }
            hir::Expr::Lit(lit) => match lit {
                hir::Literal::Bool(_) => Prim::Bool.into(),
                hir::Literal::Int(_, _, var) => IType::Var(*var),
                hir::Literal::Float(_) => Prim::Float.into(),
                hir::Literal::String(_) => {
                    let record = self.items.pinfo.string;
                    IType::defined(record, vec![])
                }
            },
            hir::Expr::Poison => Prim::Poison.into(),
        }
        .tr(expr.span)
    }

    fn type_check_params<'e, 'es: 'e + 's>(
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

    fn type_check_call(&mut self, span: Span, instcall: InstCall, params: Vec<Tr<IType>>) -> IType {
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
                dbg!(&ty, &params);
                todo!();
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
            InstCall::CircularRecursion { .. } => {
                todo!()
            }
            InstCall::TypeDependentFailure => {
                self.current.push_inst(span, None);
                IType::poison()
            }
        }
    }
}
