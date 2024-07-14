use super::{
    implicitly_declare_generic, tenv, Bitsize, Constraint, Container, Forall, ForeignInst, Generic,
    GenericData, GenericKind, IType, IntConstraint, Prim, RecordError, RecordVar, RecordVarField,
    Type, TypeSystem, Var, M,
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
    forall: &'f mut Forall<'s, IType>,
    lforalls: &'f mut Map<key::Lambda, Forall<'s, IType>>,

    implicits: bool,

    #[new(default)]
    pub errors: Vec<FinError<'s>>,
}

pub enum FinError<'s> {
    FieldNotFound(M<key::Record>, Tr<&'s str>),
    Record(RecordError<'s>),
}

impl<'a, 'f, 's> Finalizer<'a, 'f, 's> {
    pub fn apply(&mut self, ty: &IType) -> Type {
        match ty {
            IType::Container(con) => self.apply_container(con),
            IType::List(type_, params) | IType::Defined(type_, params) => {
                Type::Defined(*type_, self.apply_types(params))
            }

            IType::Generic(generic) => Type::Generic(*generic),
            IType::Self_ => Type::Self_,
            IType::Var(var) => self.var(*var),
            IType::Prim(prim) => Type::Prim(*prim),

            IType::InferringRecord(var) => self
                .record(*var)
                .map(|(key, params)| Type::Defined(key.map(key::TypeKind::Record), params))
                .unwrap_or_else(|| Type::Prim(Prim::Poison)),

            IType::Field(var, field) => self.field(*var, *field),
        }
    }

    pub fn apply_types(&mut self, ty: &[IType]) -> Vec<Type> {
        ty.iter().map(|ty| self.apply(ty)).collect()
    }

    fn apply_container(&mut self, con: &Container<IType>) -> Type {
        match con {
            Container::Func(kind, ptypes, returns) => Container::Func(
                *kind,
                self.apply_types(ptypes),
                Box::new(self.apply(&returns)),
            ),
            Container::Tuple(elems) => Container::Tuple(self.apply_types(elems)),
            Container::Pointer(inner) => Container::Pointer(Box::new(self.apply(inner))),
        }
        .into()
    }

    pub fn lower_constraints_of<'o>(
        &mut self,
        forall: &Forall<'s, IType>,
        rename: impl Fn(&'s str) -> &'o str,
    ) -> Forall<'o, Type> {
        forall
            .values()
            .map(|gdata| GenericData {
                name: rename(gdata.name),
                params: gdata.params,
                trait_constraints: gdata
                    .trait_constraints
                    .iter()
                    .map(|constraint| Constraint {
                        span: constraint.span,
                        trait_: constraint.trait_,
                        params: self.apply_types(&constraint.params),
                    })
                    .collect(),
            })
            .collect()
    }

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
                let params = self.apply_types(&params);
                Some((key, params))
            }
            tenv::RecordAssignment::Redirect(var) => self.record(var),
            tenv::RecordAssignment::NotRecord(_) => {
                todo!("i think we're meant to error on this as well? or is this just poison");
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
            Some(ty) => self.apply(&*ty),
        }
    }

    pub fn forall(&mut self, lambda: Option<key::Lambda>) -> (GenericKind, &mut Forall<'s, IType>) {
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
                let prim = match min {
                    Some((signed, n)) => Prim::Int(
                        true,
                        Bitsize::minimum_for(signed, n).max(Bitsize::default()),
                    ),
                    None => Prim::Int(true, Bitsize::default()),
                };

                self.ts.env.assign(var, IType::from(prim).tr(span));
                prim.into()
            }
            None if self.implicits => {
                let (kind, forall) = (GenericKind::Entity, &mut *self.forall);

                let (generic, _) = implicitly_declare_generic(forall);

                info!("defaulting {var} to generic {}", generic,);

                let generic = Generic::new(generic, kind);
                self.ts.env.assign(var, IType::Generic(generic).tr(span));
                Type::Generic(generic)
            }
            None => {
                self.ts
                    .env
                    .assign(var, IType::Container(Container::Tuple(vec![])).tr(span));

                Container::Tuple(vec![]).into()
            }
        }
    }

    pub fn field(&mut self, var: RecordVar, field: RecordVarField) -> Type {
        match self.record(var) {
            Some((key, params)) => {
                let rdata = &self.ts.env.records[var];
                let name = &rdata.fields[field];
                match self.ts.fnames[key].find(|n| *n == *name) {
                    Some(fieldkey) => self.ts.env.inst(&params, |finst| {
                        let ty = &self.ts.ftypes[key][fieldkey];
                        finst.apply(&ty)
                    }),
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
                        Some(ty) => self.apply(&ty).tr(ty.span),
                        None => {
                            let prim = Prim::Int(true, Bitsize::default());
                            self.ts.env.assign(var, IType::Prim(prim).tr(span));
                            Type::from(prim).tr(span)
                        }
                    };

                    Some((span, min, ty))
                }
                None => None,
            })
            .collect()
    }

    pub fn constraints(&mut self) -> Vec<(Span, Type, Constraint<Type>)> {
        let cons = std::mem::take(&mut self.ts.env.constraint_checks);
        cons.into_iter()
            .map(|(var, con)| {
                let span = self.ts.env.vars[var].span;
                let type_ = self.var(var);
                let constraint = Constraint {
                    params: self.apply_types(&con.params),
                    span: con.span,
                    trait_: con.trait_,
                };
                (span, type_, constraint)
            })
            .collect()
    }

    pub fn inst(&mut self, inst: &ForeignInst<Var>) -> ConcreteInst {
        inst.map(|var| self.var(*var))
    }
}

pub type ConcreteInst = ForeignInst<Type>;

impl ConcreteInst {
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Container(con) => Type::Container(con.map(|t| self.apply(t))),
            Type::Prim(prim) => Type::Prim(*prim),
            Type::Generic(generic) => match generic.kind {
                GenericKind::Lambda(_) => panic!("concrete instantiation of lambda"),
                GenericKind::Entity => self.generics[generic.key].clone(),
                GenericKind::Parent => self.pgenerics[generic.key].clone(),
            },
            Type::List(kind, params) | Type::Defined(kind, params) => {
                Type::Defined(*kind, self.applys(params))
            }
            Type::Self_ => match self.self_.clone() {
                Some(ty) => ty,
                None => Type::Self_,
            },
        }
    }

    pub fn applys<'a, F: FromIterator<Type>>(&self, tys: impl IntoIterator<Item = &'a Type>) -> F {
        tys.into_iter().map(|t| self.apply(t)).collect()
    }
}

impl fmt::Display for ConcreteInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.pgenerics
            .iter()
            .map(|(generic, ty)| format!("{generic} as {ty}"))
            .format(", ")
            .fmt(f)?;

        if !self.pgenerics.is_empty() && !self.generics.is_empty() {
            write!(f, ", ")?;
        }

        self.generics
            .iter()
            .map(|(generic, ty)| format!("{generic} as {ty}"))
            .format(", ")
            .fmt(f)?;

        if let Some(ty) = self.self_.as_ref() {
            if self.pgenerics.is_empty() && self.generics.is_empty() {
                write!(f, "self as {ty}")
            } else {
                write!(f, ", self as {ty}")
            }?;
        }

        Ok(())
    }
}
