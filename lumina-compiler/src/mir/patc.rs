use super::func::InstInfo;
use super::Verify;
use crate::prelude::*;

use lumina_typesystem::{IType, Ty};

impl<'a, 's> Verify<'a, 's> {
    pub fn type_check_pat(&mut self, pat: Tr<&hir::Pattern<'s>>) -> Tr<IType> {
        trace!("checking pattern {}", pat.value);

        match pat.value {
            hir::Pattern::Int(_, nvar) => IType::infer(*nvar),
            hir::Pattern::Char(_) => IType::u8(),
            hir::Pattern::Any => IType::infer(self.vars().var(pat.span)),
            hir::Pattern::Bind(bind, inner) => {
                let ty = self.type_check_pat((&**inner).tr(pat.span));
                self.new_bind_as(*bind, ty.clone());
                ty.value
            }
            hir::Pattern::Constructor(type_, var, params) => {
                let params = params
                    .iter()
                    .map(|p| self.type_check_pat(p.as_ref()))
                    .collect::<Vec<_>>();
                let (finst, ptypes, returns) = self.type_of_variant(pat.span, *type_, *var);
                let instinfo = InstInfo::new(type_.0, finst, ptypes, returns.clone());
                self.type_check_and_emit_application(pat.span, &params, &instinfo.ptypes);
                returns.value
            }
            hir::Pattern::Record(var, ty, fields) => {
                let fvars = fields
                    .iter()
                    .map(|(name, _, _)| self.vars().add_field(*var, *name))
                    .collect::<Vec<_>>();

                if let Some(ty) = ty {
                    self.type_check_and_emit(IType::infer(*var).tr(pat.span).as_ref(), ty.as_ref());
                }

                for ((name, bind, pat), fvar) in fields.iter().zip(fvars) {
                    let ty = self.type_check_pat(pat.as_ref());
                    let fty = IType::infer(fvar).tr(name.span);
                    self.type_check_and_emit(ty.as_ref(), fty.as_ref());
                    self.new_bind_as(*bind, fty);
                }

                IType::infer(*var)
            }
            hir::Pattern::Tuple(elems) => {
                let types = elems
                    .iter()
                    .map(|p| self.type_check_pat(p.as_ref()).value)
                    .collect();

                IType::tuple(types)
            }

            hir::Pattern::Array(elems, len) => {
                let inner = self.type_check_same("array elements", None, elems, |this, elem| {
                    this.type_check_pat(elem.as_ref())
                });

                IType::array(**len, inner.value)
            }
            hir::Pattern::GenericArray(elems, generic) => {
                let inner = self.type_check_same("array elements", None, elems, |this, elem| {
                    this.type_check_pat(elem.as_ref())
                });

                IType::const_array(**generic, inner.value)
            }

            // TODO: since we decided to desugar patterns early on; we can't use the same
            // `type_check_same_type` as with expressions. That's rather unfortunate.
            //
            // Since MIR is split into two steps. We could technically fix this?
            // no reason why we can't just lower it into a tree on the HIR -> MIR step instead.
            hir::Pattern::Cons(pats, inner_var) => {
                let list = self.items.list_default;

                let value = self.type_check_pat(pats[0].as_ref());
                let exp = IType::infer(*inner_var).tr(pat.span);
                self.type_check_and_emit(value.as_ref(), exp.as_ref());

                let ty = self.type_check_pat(pats[1].as_ref());
                let exp = IType::list(list, vec![IType::infer(*inner_var)]).tr(pats[0].span);
                self.type_check_and_emit(ty.as_ref(), exp.as_ref());

                ty.value
            }

            hir::Pattern::Nil(inner) => {
                let list = self.items.list_default;
                IType::list(list, vec![IType::infer(*inner)])
            }
            hir::Pattern::Bool(_) => IType::bool(),
            hir::Pattern::String(spats) => {
                for (i, spat) in spats.iter().enumerate() {
                    let is_last = || i == spats.len() - 1;
                    match spat {
                        hir::StringPattern::Literal(_) => {}
                        hir::StringPattern::Extractor(extractor) => {
                            let params = self.type_check_params(&extractor.params);
                            self.type_check_pass(
                                pat.span,
                                &extractor.call,
                                &extractor.tanot,
                                params,
                            );
                            if let Some(bind) = extractor.bind {
                                let ty = Ty::string(self.items.pinfo.string, vec![]);
                                self.new_bind_as(bind.value, ty.tr(bind.span));
                            }
                        }
                        hir::StringPattern::Wildcard(bind) => {
                            let ty = if is_last() {
                                IType::string(self.items.pinfo.string, vec![])
                            } else {
                                Ty::u8()
                            };
                            self.new_bind_as(bind.value, ty.tr(bind.span))
                        }
                    }
                }

                let record = self.items.pinfo.string;
                IType::defined(record, vec![])
            }
            hir::Pattern::Poison => todo!(),
        }
        .tr(pat.span)
    }

    pub fn type_check_pat_params(
        &mut self,
        ptypes: &Map<key::Param, Tr<IType>>,
        pats: &[Tr<hir::Pattern<'s>>],
    ) {
        for (p, exp) in pats.iter().zip(ptypes.values()) {
            let ty = self.type_check_pat(p.as_ref());
            self.type_check_and_emit(ty.as_ref(), exp.as_ref());
        }
    }
}
