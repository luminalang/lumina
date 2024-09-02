use super::{
    Container, Forall, Generic, GenericKind, IType, Inference, IntConstraint, IntSize, Ty,
    TypeSystem,
};
use derive_new::new;
use lumina_util::Spanned;
use tracing::{info, trace};

#[derive(new)]
pub struct Finalizer<'a, 's> {
    pub ts: TypeSystem<'a, 's>,
    pub default_to_generic: Option<&'a mut Forall<'s, Inference>>,
}

impl IntConstraint {
    pub fn to_default_type(self, default_int_size: u8) -> IntSize {
        let mut intsize = IntSize::minimum_for(true, self.max as u128);
        if intsize.bits() < default_int_size {
            intsize = IntSize::new(true, default_int_size);
        }
        intsize
    }
}

impl<'a, 's> Finalizer<'a, 's> {
    pub fn infer_all_unknown_types(&mut self) {
        info!("inferring all {} vars", self.ts.env.vars.len());

        // Infer remaining records and ints
        for var in self.ts.env.vars.keys() {
            // Finalize as a record
            if !self.ts.env.vars[var].fields.is_empty() {
                let span = self.ts.env.vars[var].span;

                match self.ts.env.vars[var].assignment.as_ref() {
                    Some(ty) => match &ty.value {
                        Ty::Container(Container::Defined(_, _), _) => {
                            #[cfg(debug_assertions)]
                            for (_, fvar, _) in &self.ts.env.vars[var].fields {
                                assert_ne!(
                                    self.ts.env.vars[*fvar].assignment, None,
                                    "{var} was assigned without updating fields"
                                );
                            }

                            continue;
                        }
                        _ => panic!("non-record defined to {var} with fields. Is this allowed?"),
                    },
                    None => match self.ts.find_record_by_fields(var).as_slice() {
                        [record] => {
                            trace!("inferring record {var} as {record}");
                            let params = self.ts.new_record_type_params(span, *record);
                            self.ts.assign_record_to_rvar(span, var, *record, params);
                            continue;
                        }
                        _ambigious_or_empty => {
                            trace!("poisoning {var} because of record inference failing");
                            let ty = Ty::poison();
                            self.ts.env.vars[var].assignment = Some(ty.tr(span));
                            continue;
                        }
                    },
                }
            }

            let vinfo = &mut self.ts.env.vars[var];

            // Finalize as an int
            if let Some(intcon) = vinfo.int_constraint {
                if vinfo.assignment.is_some() {
                    continue;
                }

                let intsize = intcon.to_default_type(self.ts.default_int_size);
                let ty = IType::Int(intsize);
                vinfo.assignment = Some(ty.tr(vinfo.span));
            }
        }

        // Default other vars
        for (var, vinfo) in self.ts.env.vars.iter_mut() {
            if vinfo.assignment.is_some() {
                continue;
            }

            trace!("defaulting {var} without constraints");

            let ty = if vinfo.lift_to_generic {
                let forall = self.default_to_generic.as_deref_mut().unwrap();
                Ty::Generic(Generic::new(
                    forall.implicitly_declare().0,
                    GenericKind::Entity,
                ))
            } else {
                Ty::tuple(vec![])
            };

            vinfo.assignment = Some(ty.tr(vinfo.span));
        }
    }
}
