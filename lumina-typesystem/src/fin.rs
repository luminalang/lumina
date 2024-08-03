use super::{
    tenv, Constraint, Container, Downgrade, Forall, ForeignInst, Generic, GenericData, GenericKind,
    GenericMapper, IType, Inference, IntConstraint, IntSize, RecordError, RecordVar,
    RecordVarField, Static, Transformer, Ty, Type, TypeSystem, Var, M,
};
use derive_new::new;
use itertools::Itertools;
use key::{LinearFind, Map};
use lumina_key as key;
use lumina_util::{Span, Spanned, Tr};
use std::fmt;
use tracing::error;
use tracing::info;
use tracing::trace;

#[derive(new)]
pub struct Finalizer<'a, 'f, 's> {
    ts: TypeSystem<'a, 's>,
    forall: &'f mut Forall<'s, Inference>,
    lforalls: &'f mut Map<key::Lambda, Forall<'s, Inference>>,

    default_intsize: IntSize,

    implicits: bool,

    #[new(default)]
    pub errors: Vec<FinError<'s>>,
}

pub enum FinError<'s> {
    FieldNotFound(M<key::Record>, Tr<&'s str>),
    NotRecord(Span, Type, Vec<Tr<&'s str>>),
    Record(RecordError<'s>),
}

impl<'a, 'f, 's> Transformer<Inference> for Finalizer<'a, 'f, 's> {
    type Output = Static;

    fn special(&mut self, inf: &Inference) -> Ty<Self::Output> {
        match inf {
            Inference::Var(var) => self.var(*var),
            Inference::Record(rvar) => match self.record(*rvar) {
                Some((key, params)) => Ty::defined(key, params),
                None => Type::poison(),
            },
            Inference::Field(rvar, varfield) => self.field(*rvar, *varfield),
        }
    }

    fn generic(&mut self, generic: Generic) -> Ty<Self::Output> {
        Ty::Generic(generic)
    }
}

impl<'a, 'f, 's> Finalizer<'a, 'f, 's> {
    pub fn record(&mut self, var: RecordVar) -> Option<(M<key::Record>, Vec<Type>)> {
        let rdata = &mut self.ts.env.records[var];

        match rdata.assignment.clone() {
            tenv::RecordAssignment::Ok(key, params) => {
                if !rdata.verified {
                    rdata.verified = true;
                    for name in rdata.fields.values() {
                        if self.ts.fnames[key].find(|n| *n == *name).is_none() {
                            self.errors.push(FinError::FieldNotFound(key, *name));
                        }
                    }
                }
                let params = self.transforms(&params);
                Some((key, params))
            }
            tenv::RecordAssignment::Redirect(var) => self.record(var),
            tenv::RecordAssignment::NotRecord(ty) => {
                let fields = rdata.fields.values().cloned().collect();
                let span = rdata.span;
                let ty = self.transform(&ty);
                self.errors.push(FinError::NotRecord(span, ty, fields));
                None
            }
            tenv::RecordAssignment::Unknown(rerror) => {
                if !rdata.verified {
                    rdata.verified = true;
                    self.errors.push(FinError::Record(rerror));
                }
                None
            }
            tenv::RecordAssignment::None => {
                let span = rdata.span;
                self.ts.force_record_inference(span, var);
                self.record(var)
            }
        }
    }

    pub fn var(&mut self, var: Var) -> Type {
        let idata = &self.ts.env.vars[var];
        match idata.assignment.clone() {
            None => self.default_var(var),
            Some(ty) => self.transform(&*ty),
        }
    }

    pub fn forall(
        &mut self,
        lambda: Option<key::Lambda>,
    ) -> (GenericKind, &mut Forall<'s, Inference>) {
        match lambda {
            None => (GenericKind::Entity, &mut *self.forall),
            Some(lkey) => (GenericKind::Lambda(lkey), &mut self.lforalls[lkey]),
        }
    }

    fn default_var(&mut self, var: Var) -> Type {
        trace!("attempting to default {var}");

        let span = self.ts.env.vars[var].span;

        match self.ts.env.vars[var].int_constraint {
            Some(IntConstraint { min }) => {
                let ty = match min {
                    Some((signed, n)) => {
                        Type::Int(IntSize::minimum_for(signed, n).max(self.default_intsize))
                    }
                    None => Type::Int(self.default_intsize),
                };

                self.ts.env.assign(var, Downgrade.transform(&ty).tr(span));
                ty
            }
            None if self.implicits => {
                let (kind, forall) = (GenericKind::Entity, &mut *self.forall);

                let (generic, _) = forall.implicitly_declare();

                info!("defaulting {var} to generic {}", generic,);

                let generic = Generic::new(generic, kind);
                self.ts.env.assign(var, IType::Generic(generic).tr(span));
                Type::Generic(generic)
            }
            None => {
                self.ts.env.assign(var, IType::tuple(vec![]).tr(span));
                Type::tuple(vec![])
            }
        }
    }

    pub fn field(&mut self, var: RecordVar, field: RecordVarField) -> Type {
        match self.record(var) {
            Some((key, params)) => {
                let rdata = &self.ts.env.records[var];
                let name = &rdata.fields[field];
                match self.ts.fnames[key].find(|n| *n == *name) {
                    Some(fieldkey) => {
                        let finst =
                            GenericMapper::from_types(GenericKind::Entity, params.iter().cloned());
                        let ty = &self.ts.ftypes[key][fieldkey];
                        (&finst).transform(&ty)
                    }
                    // error will be created by a separate check
                    None => Type::poison(),
                }
            }
            None => Type::poison(),
        }
    }

    pub fn num_constraints(&mut self) -> Vec<(Span, Option<(bool, u128)>, Tr<Type>)> {
        self.ts
            .env
            .vars
            .keys()
            .filter_map(|var| match self.ts.env.vars[var].int_constraint {
                Some(IntConstraint { min }) => {
                    let vdata = &self.ts.env.vars[var];
                    let span = vdata.span;

                    let ty = match vdata.assignment.clone() {
                        Some(ty) => self.transform(&ty).tr(ty.span),
                        None => {
                            let ty = Type::Int(self.default_intsize);
                            self.ts
                                .env
                                .assign(var, IType::Int(self.default_intsize).tr(span));
                            ty.tr(span)
                        }
                    };

                    Some((span, min, ty))
                }
                None => None,
            })
            .collect()
    }

    pub fn constraints(&mut self) -> Vec<(Span, Type, Constraint<Static>)> {
        let cons = std::mem::take(&mut self.ts.env.constraint_checks);
        cons.into_iter()
            .map(|(var, con)| {
                let span = self.ts.env.vars[var].span;
                let type_ = self.var(var);
                let constraint = Constraint {
                    params: self.transforms(&con.params),
                    span: con.span,
                    trait_: con.trait_,
                };
                (span, type_, constraint)
            })
            .collect()
    }
}
