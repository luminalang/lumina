use super::func::InstInfo;
use super::Verify;
use crate::prelude::*;

use lumina_typesystem::IType;

impl<'a, 's> Verify<'a, 's> {
    pub fn type_check_pat(&mut self, pat: Tr<&hir::Pattern<'s>>) -> Tr<IType> {
        trace!("checking pattern {}", pat.value);

        match pat.value {
            hir::Pattern::Int(_, nvar) => IType::infer(*nvar),
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
                let instinfo = InstInfo::new(type_.module, finst, ptypes, returns.clone());
                self.type_check_and_emit_application(pat.span, &params, &instinfo.ptypes);
                returns.value
            }
            hir::Pattern::Record(var, ty, fields) => {
                for (name, _, _) in fields.iter() {
                    self.vars().add_field(*var, *name);
                }

                if let Some(ty) = ty {
                    self.assign_ty_to_rvar(pat.span, *var, ty.as_ref());
                }

                for (varfield, (vname, bind, pat)) in self.vars().iter_var_fields(*var).zip(fields)
                {
                    let exp = IType::inffield(*var, varfield).tr(vname.span);
                    let name = self.vars().name_of_field(*var, varfield);
                    assert_eq!(name, *vname);
                    let ty = self.type_check_pat(pat.as_ref());
                    self.type_check_and_emit(ty.as_ref(), exp.as_ref());
                    self.new_bind_as(*bind, exp);
                }

                IType::infrecord(*var)
            }
            hir::Pattern::Tuple(elems) => {
                let types = elems
                    .iter()
                    .map(|p| self.type_check_pat(p.as_ref()).value)
                    .collect();
                IType::tuple(types)
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
                for spat in spats {
                    if let hir::StringPattern::BindWhile(_, call) = spat {
                        let tanot = hir::TypeAnnotation::new();
                        let instcall = self.type_of_callable(pat.span, None, call, 1, &tanot);
                        // TODO: use `char` langitem instead of byte
                        let ptypes = vec![IType::u8().tr(pat.span)];
                        let ret = self.type_check_call(pat.span, instcall, ptypes);
                        self.type_check_and_emit(
                            (&IType::bool()).tr(pat.span),
                            (&ret).tr(pat.span),
                        );
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
