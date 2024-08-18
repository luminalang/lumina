use super::{
    Constraint, Forall, Generic, GenericKind, IType, Inference, RecordAssignment, Span, Static,
    TEnv, Ty, Var,
};
use derive_new::new;
use itertools::Itertools;
use key::{Map, M};
use lumina_key as key;
use lumina_util::{Spanned, Tr};
use std::fmt;

/// Transforms types by mapping generics to new types
#[derive(Clone, PartialEq, Eq, Hash, new)]
pub struct GenericMapper<T> {
    pub generics: Vec<(Generic, Ty<T>)>,
    pub self_: Option<Ty<T>>,
}

/// Instantiates foreign static types
pub struct ForeignInst<'a, 's, T> {
    pub mapper: GenericMapper<T>,
    pub env: &'a mut TEnv<'s>,
}

/// Instantiates foreign functions that have no inferred their types statically
pub struct CircularInst<'a, 's, T> {
    mapper: GenericMapper<T>,
    from: &'a mut TEnv<'s>,
    to: &'a mut TEnv<'s>,
    to_forall: &'a mut Forall<'s, Inference>,
    failures: Vec<Span>,
}

impl<T: Clone> GenericMapper<T> {
    pub fn inst<'a, 's>(env: &'a mut TEnv<'s>) -> ForeignInst<'a, 's, T> {
        ForeignInst { mapper: GenericMapper::new(vec![], None), env }
    }

    pub fn to_defined<K: Into<key::TypeKind>>(&self, kind: GenericKind, key: M<K>) -> Ty<T> {
        Ty::defined(key, self.to_types(kind))
    }

    pub fn to_types(&self, kind: GenericKind) -> Vec<Ty<T>> {
        self.types(kind).cloned().collect()
    }

    pub fn types(&self, kind: GenericKind) -> impl Iterator<Item = &Ty<T>> + '_ {
        self.generics
            .iter()
            .filter(move |(g, _)| g.kind == kind)
            .map(|(_, ty)| ty)
    }

    pub fn assignments_by_kind(
        &self,
        kind: GenericKind,
    ) -> impl Iterator<Item = (key::Generic, &Ty<T>)> + '_ {
        self.generics
            .iter()
            .filter_map(move |(generic, ty)| (generic.kind == kind).then_some((generic.key, ty)))
    }

    pub fn from_types(kind: GenericKind, types: impl Iterator<Item = Ty<T>>) -> Self {
        let generics = types
            .enumerate()
            .map(|(i, t)| (Generic::new(key::Generic(i as u32), kind), t.clone()))
            .collect();

        Self::new(generics, None)
    }

    pub fn replicate_entity_forall<'s>(&mut self, forall: &Forall<'s, T>) {
        for key in forall.generics.keys() {
            let generic = Generic::new(key, GenericKind::Entity);
            self.push(generic, Ty::Generic(generic));
        }
    }

    pub fn clear_assignments_of_kind(&mut self, kind: GenericKind) {
        self.generics.retain(|(gen, _)| gen.kind != kind)
    }

    pub fn apply(&self, generic: Generic) -> Ty<T> {
        match self.find(generic) {
            None => panic!("generic not found: {generic}"),
            Some(ty) => ty.clone(),
        }
    }

    pub fn find(&self, generic: Generic) -> Option<&Ty<T>> {
        self.generics
            .iter()
            .find_map(|(g, ty)| (*g == generic).then_some(ty))
    }

    pub fn push(&mut self, generic: Generic, ty: Ty<T>) {
        assert!(self.generics.iter().all(|(g, _)| *g != generic));
        self.generics.push((generic, ty))
    }

    pub fn map<U>(&self, mut f: impl FnMut(&Ty<T>) -> Ty<U>) -> GenericMapper<U> {
        GenericMapper {
            generics: self
                .generics
                .iter()
                .map(|(generic, ty)| (*generic, f(ty)))
                .collect(),
            self_: self.self_.as_ref().map(f),
        }
    }
}
impl GenericMapper<Inference> {
    fn as_var(&self, generic: Generic) -> Var {
        match self.find(generic).unwrap() {
            Ty::Special(Inference::Var(var)) => *var,
            _ => unreachable!(),
        }
    }

    pub fn circular<'a, 's>(
        from: &'a mut TEnv<'s>,
        to: &'a mut TEnv<'s>,
        to_forall: &'a mut Forall<'s, Inference>,
        span: Span,
    ) -> CircularInst<'a, 's, Inference> {
        let generics = to_forall
            .generics
            .keys()
            .map(|key| {
                let generic = Generic::new(key, GenericKind::Entity);
                (generic, Ty::infer(from.var(span)))
            })
            .collect();

        let self_ = to.self_.as_ref().map(|_| from.var(span)).map(Ty::infer);

        let mut inst = CircularInst {
            mapper: GenericMapper::new(generics, self_),
            from,
            to,
            to_forall,
            failures: vec![],
        };

        for key in inst.to_forall.generics.keys() {
            let generic = Generic::new(key, GenericKind::Entity);
            let var = inst.mapper.as_var(generic);

            for con in inst.to_forall.generics[key].trait_constraints.clone() {
                let con = inst.transform_constraint(&con);
                inst.from.constraint_checks.push((var, con));
            }
        }

        inst
    }
}

impl<'a, 's> ForeignInst<'a, 's, Inference> {
    pub fn with_self(mut self, span: Span) -> Self {
        self.mapper.self_ = Some(Ty::infer(self.env.var(span)));
        self
    }

    /// we handle each instantiation in two steps so that constraints containing later generics will work properly
    pub fn forall<U>(mut self, kind: GenericKind, span: Span, forall: &Forall<'s, U>) -> Self {
        for (key, _) in &forall.generics {
            let generic = Generic::new(key, kind);
            let ty = IType::infer(self.env.var(span));
            self.mapper.push(generic, ty);
        }
        self
    }

    pub fn icons(self, kind: GenericKind, forall: &Forall<'s, Inference>) -> Self {
        for (key, gdata) in &forall.generics {
            let generic = Generic::new(key, kind);
            let var = self.mapper.as_var(generic);
            for con in &gdata.trait_constraints {
                let con = DirectRecursion(&self.mapper).transform_constraint(con);
                self.env.push_iconstraint(var, con);
            }
        }
        self
    }

    pub fn cons(self, kind: GenericKind, forall: &Forall<'s, Static>) -> Self {
        for (key, gdata) in &forall.generics {
            let generic = Generic::new(key, kind);
            let var = self.mapper.as_var(generic);
            for con in &gdata.trait_constraints {
                let con = (&self.mapper).transform_constraint(con);
                self.env.push_iconstraint(var, con);
            }
        }
        self
    }

    pub fn into_mapper(self) -> GenericMapper<Inference> {
        self.mapper
    }
}

pub trait Transformer<T> {
    type Output;

    fn special(&mut self, _: &T) -> Ty<Self::Output> {
        unreachable!();
    }
    fn self_(&mut self) -> Ty<Self::Output> {
        Ty::Simple("self")
    }
    fn generic(&mut self, generic: Generic) -> Ty<Self::Output>;

    fn transform(&mut self, ty: &Ty<T>) -> Ty<Self::Output> {
        match ty {
            Ty::Container(container, params) => Ty::Container(*container, self.transforms(params)),
            Ty::Int(size) => Ty::Int(*size),
            Ty::Simple("self") => self.self_(),
            Ty::Simple(name) => Ty::Simple(*name),
            Ty::Special(spec) => self.special(spec),
            Ty::Generic(gen) => self.generic(*gen),
        }
    }

    fn transform_spanned(&mut self, ty: Tr<&Ty<T>>) -> Tr<Ty<Self::Output>> {
        self.transform(&ty).tr(ty.span)
    }

    fn transforms(&mut self, params: &[Ty<T>]) -> Vec<Ty<Self::Output>> {
        params.iter().map(|ty| self.transform(ty)).collect()
    }

    fn transforms_spanned<F>(&mut self, params: &[Tr<Ty<T>>]) -> F
    where
        F: FromIterator<Tr<Ty<Self::Output>>>,
    {
        params
            .iter()
            .map(|ty| self.transform_spanned(ty.as_ref()))
            .collect()
    }

    fn transform_typing(
        &mut self,
        ptypes: &Map<key::Param, Tr<Ty<T>>>,
        ret: &Tr<Ty<T>>,
    ) -> (Vec<Tr<Ty<Self::Output>>>, Tr<Ty<Self::Output>>) {
        (
            ptypes
                .values()
                .map(|v| self.transform_spanned(v.as_ref()))
                .collect(),
            self.transform_spanned(ret.as_ref()),
        )
    }

    fn transform_constraint(&mut self, con: &Constraint<T>) -> Constraint<Self::Output> {
        Constraint {
            span: con.span,
            trait_: con.trait_,
            params: self.transforms(&con.params),
        }
    }
}

pub struct Downgrade;

impl Transformer<Static> for Downgrade {
    type Output = Inference;

    fn generic(&mut self, generic: Generic) -> Ty<Self::Output> {
        Ty::Generic(generic)
    }
}

impl<O: Clone> Transformer<Static> for &GenericMapper<O> {
    type Output = O;

    fn self_(&mut self) -> Ty<Self::Output> {
        self.self_.clone().unwrap_or_else(|| Ty::Simple("self"))
    }

    fn generic(&mut self, generic: Generic) -> Ty<Self::Output> {
        self.apply(generic)
    }
}

pub struct DirectRecursion<'a>(pub &'a GenericMapper<Inference>);

impl<'a> Transformer<Inference> for DirectRecursion<'a> {
    type Output = Inference;

    fn special(&mut self, inf: &Inference) -> Ty<Self::Output> {
        Ty::Special(*inf)
    }

    fn generic(&mut self, generic: Generic) -> Ty<Self::Output> {
        // Since recursion calls can only occur from within the function that's being called, any
        // generics not included in the mapping are assumed to be of the parent which may be
        // treated as being in the same scope.
        //
        // This is so that lambda's will not try to re-instantiate the pgenerics of their
        // impl/trait block.
        match self.0.find(generic) {
            None => {
                assert!(!matches!(generic.kind, GenericKind::Lambda(_)));
                Ty::Generic(generic)
            }
            Some(ty) => ty.clone(),
        }
    }
}

impl<'a, 's> Transformer<Inference> for CircularInst<'a, 's, Inference> {
    type Output = Inference;

    fn special(&mut self, inf: &Inference) -> Ty<Self::Output> {
        match inf {
            Inference::Var(var) => {
                let vdata = self.to.get(*var);
                match vdata.value {
                    Some(ty) => {
                        let ty = ty.cloned();
                        self.transform(&ty)
                    }
                    None => {
                        self.failures.push(vdata.span);
                        Ty::poison()
                    }
                }
            }
            Inference::Record(record) => match self.to.get_record(*record).clone() {
                RecordAssignment::Ok(key, params) => IType::defined(key, params),
                RecordAssignment::Redirect(rvar) => {
                    let ty = IType::infrecord(rvar);
                    self.transform(&ty)
                }
                RecordAssignment::NotRecord(ty) => self.transform(&ty),
                RecordAssignment::Unknown(_) => Ty::poison(),
                RecordAssignment::None => {
                    self.failures.push(self.to.get_record_span(*record));
                    Ty::poison()
                }
            },
            Inference::Field(record, _) => {
                self.failures.push(self.to.get_record_span(*record));
                Ty::poison()
            }
        }
    }

    fn generic(&mut self, generic: Generic) -> Ty<Self::Output> {
        self.mapper.apply(generic)
    }
}

impl<'a, 's> CircularInst<'a, 's, Inference> {
    pub fn finalize(self) -> (GenericMapper<Inference>, Vec<Span>) {
        todo!();
    }
}

impl<T: fmt::Display> fmt::Display for GenericMapper<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.generics.is_empty() {
            Ok(())
        } else {
            write!(
                f,
                "({})",
                self.generics
                    .iter()
                    .format_with(", ", |(g, t), f| f(&format_args!("{g} -> {t}")))
            )
        }
    }
}

impl<T: fmt::Display> fmt::Debug for GenericMapper<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{self}",)
    }
}
