use derive_more::From;
use derive_new::new;
use key::{Map, ModMap, M};
use lumina_key as key;
use lumina_util::{Span, Tr};
use std::collections::HashMap;

mod bitsize;
pub use bitsize::Bitsize;

mod fin;
pub use fin::{ConcreteInst, FinError, Finalizer};

mod constr;
mod fmt;

mod generic;
pub use generic::{
    implicitly_declare_generic, Constraint, Forall, Generic, GenericData, GenericKind,
    IMPLICT_GENERIC_NAMES,
};

mod tenv;
use tenv::RecordVarField;
pub use tenv::{IntConstraint, RecordAssignment, RecordError, RecordVar, TEnv, Var, VarInfo};

mod iquery;
pub use iquery::{Compatibility, ImplIndex};

#[derive(new)]
pub struct TypeSystem<'a, 's> {
    pub env: &'a mut TEnv<'s>,
    pub lambda: Option<key::Lambda>,
    pub records: &'a ModMap<key::Record, (Tr<&'s str>, Forall<'s, Type>)>,
    pub ftypes: &'a ModMap<key::Record, Map<key::RecordField, Tr<Type>>>,
    pub fnames: &'a ModMap<key::Record, Map<key::RecordField, Tr<&'s str>>>,
    pub field_lookup: &'a HashMap<&'s str, Vec<M<key::Record>>>,
}

mod check;
pub use check::CheckResult;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Container<T> {
    Func(FuncKind, Vec<T>, Box<T>),
    Tuple(Vec<T>),
    Pointer(Box<T>),
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub enum Prim {
    Int(bool, Bitsize),
    Float,
    Poison,
    Bool,
    Never,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum FuncKind {
    Closure,
    FnPointer,
}

#[derive(Clone, Debug, From)]
pub enum IType {
    #[from]
    Container(Container<Self>),
    #[from]
    Prim(Prim),
    #[from]
    Generic(Generic),

    Self_,

    Defined(M<key::TypeKind>, Vec<Self>),
    List(M<key::TypeKind>, Vec<Self>),
    InferringRecord(RecordVar),

    // infered types
    #[from]
    Var(Var),

    // Record(RecordVar),
    Field(RecordVar, RecordVarField),
}

#[derive(Clone, Debug, From, PartialEq, Eq)]
pub enum Type {
    #[from]
    Container(Container<Self>),
    #[from]
    Prim(Prim),
    #[from]
    Generic(Generic),

    Defined(M<key::TypeKind>, Vec<Self>),
    List(M<key::TypeKind>, Vec<Self>),

    Self_,
}

impl<T> Container<T> {
    pub fn map<U>(&self, mut f: impl FnMut(&T) -> U) -> Container<U> {
        match self {
            Container::Func(kind, ptypes, returns) => {
                let ret = Box::new(f(&returns));
                Container::Func(*kind, ptypes.iter().map(f).collect(), ret)
            }
            Container::Tuple(elems) => Container::Tuple(elems.iter().map(f).collect()),
            Container::Pointer(inner) => Container::Pointer(Box::new(f(*&inner))),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct DoesNotHaveField<'s> {
    span: Span,
    field: Tr<&'s str>,
}

type FInst = ForeignInst<Var>;

impl<'a, 's> TypeSystem<'a, 's> {
    #[track_caller]
    pub fn assign_pgeneric(&mut self, inst: &FInst, generic: key::Generic, ty: Tr<IType>) {
        let var = inst.pgenerics[generic];
        self.raw_assign(var, ty)
    }

    #[track_caller]
    pub fn assign_generic(&mut self, inst: &FInst, generic: key::Generic, ty: Tr<IType>) {
        let var = inst.generics[generic];
        self.raw_assign(var, ty)
    }

    #[track_caller]
    pub fn assign_self(&mut self, inst: &ForeignInst<Var>, ty: Tr<IType>) {
        let var = inst.self_.unwrap();
        self.raw_assign(var, ty)
    }

    fn raw_assign(&mut self, var: Var, ty: Tr<IType>) {
        let idata = &mut self.env.vars[var];
        assert!(idata.assignment.is_none(), "Type variable already assigned");
        idata.assignment = Some(ty);
    }
}

#[derive(Clone, Debug)]
pub struct ForeignInst<T> {
    pub generics: Map<key::Generic, T>,
    pub pgenerics: Map<key::Generic, T>,
    pub self_: Option<T>,
}

impl<T> ForeignInst<T> {
    pub fn new<'a, 's>(
        env: &'a mut TEnv<'s>,
        lambda: Option<key::Lambda>,
    ) -> ForeignInstBuilder<'a, 's> {
        ForeignInstBuilder {
            env,
            lambda,
            inst: ForeignInst { self_: None, generics: Map::new(), pgenerics: Map::new() },
        }
    }

    pub fn map<U>(&self, mut f: impl FnMut(&T) -> U) -> ForeignInst<U> {
        ForeignInst {
            generics: self.generics.values().map(&mut f).collect(),
            pgenerics: self.pgenerics.values().map(&mut f).collect(),
            self_: self.self_.as_ref().map(f),
        }
    }
}

impl ForeignInst<Var> {
    pub fn to_itype(&self, key: M<key::TypeKind>) -> IType {
        let params = self.generics.values().copied().map(IType::Var).collect();
        IType::Defined(key, params)
    }
}

pub struct ForeignInstBuilder<'a, 's> {
    lambda: Option<key::Lambda>,
    env: &'a mut TEnv<'s>,

    inst: ForeignInst<Var>,
}

impl<'a, 's> ForeignInstBuilder<'a, 's> {
    pub fn with_self(mut self, span: Span) -> Self {
        self.inst.self_ = Some(self.env.var(span, self.lambda));
        self
    }

    pub fn annotate_self(self, ty: Tr<IType>) -> Self {
        self.env.vars[self.inst.self_.unwrap()].assignment = Some(ty);
        self
    }

    pub fn forall<T>(mut self, span: Span, forall: &Forall<'s, T>) -> Self {
        for k in forall.keys() {
            debug_assert_eq!(k, self.inst.generics.push(self.env.var(span, self.lambda)));
        }
        self
    }

    pub fn parent<T>(mut self, span: Span, forall: &Forall<'s, T>) -> Self {
        for k in forall.keys() {
            debug_assert_eq!(k, self.inst.pgenerics.push(self.env.var(span, self.lambda)));
        }
        self
    }

    pub fn forall_cons(self, forall: &Forall<'s, Type>) -> Self {
        for k in forall.keys() {
            let var = self.inst.generics[k];
            for con in &forall[k].trait_constraints {
                let con = map_con(con, |t| self.inst.apply(t));
                self.env.vars[var].traits.push(con);
            }
        }
        self
    }

    pub fn iforall_cons(self, forall: &Forall<'s, IType>) -> Self {
        for k in forall.keys() {
            let var = self.inst.generics[k];
            for con in &forall[k].trait_constraints {
                let con = map_con(con, |t| self.inst.applyi(t));
                self.env.vars[var].traits.push(con);
            }
        }
        self
    }

    pub fn parent_cons(self, forall: &Forall<'s, Type>) -> Self {
        for k in forall.keys() {
            let var = self.inst.pgenerics[k];
            for con in &forall[k].trait_constraints {
                let con = map_con(con, |t| self.inst.apply(t));
                self.env.vars[var].traits.push(con);
            }
        }
        self
    }

    pub fn build(self) -> ForeignInst<Var> {
        self.inst
    }
}

fn map_con<T, U>(con: &Constraint<T>, f: impl FnMut(&T) -> U) -> Constraint<U> {
    Constraint {
        span: con.span,
        trait_: con.trait_,
        params: con.params.iter().map(f).collect(),
    }
}

fn inst_constraint<'s, T>(con: &Constraint<T>, f: impl FnMut(&T) -> IType) -> Constraint<IType> {
    let params = con.params.iter().map(f).collect();
    Constraint { span: con.span, trait_: con.trait_, params }
}

impl<T: Into<IType> + Clone> ForeignInst<T> {
    pub fn apply(&self, ty: &Type) -> IType {
        match ty {
            Type::Container(con) => IType::Container(con.map(|t| self.apply(t))),
            Type::Prim(prim) => IType::Prim(*prim),
            Type::Generic(generic) => self.generic(*generic),
            Type::Defined(key, params) => {
                IType::Defined(*key, params.iter().map(|t| self.apply(t)).collect())
            }
            Type::List(key, params) => {
                IType::List(*key, params.iter().map(|t| self.apply(t)).collect())
            }
            Type::Self_ => self
                .self_
                .as_ref()
                .map(|var| var.clone().into())
                .unwrap_or(IType::Self_),
        }
    }

    pub fn applyi(&self, ty: &IType) -> IType {
        match ty {
            IType::Container(con) => IType::Container(con.map(|ty| self.applyi(ty))),
            IType::Generic(generic) => self.generic(*generic),
            IType::Defined(kind, params) => {
                IType::Defined(*kind, params.iter().map(|ty| self.applyi(ty)).collect())
            }
            IType::List(kind, params) => {
                IType::List(*kind, params.iter().map(|ty| self.applyi(ty)).collect())
            }
            IType::Self_ => self
                .self_
                .as_ref()
                .map(|var| var.clone().into())
                .unwrap_or(IType::Self_),
            other => other.clone(),
        }
    }

    pub fn generic(&self, generic: Generic) -> IType {
        match generic.kind {
            GenericKind::Lambda(_) => todo!(),
            GenericKind::Entity => self.generics[generic.key].clone().into(),
            GenericKind::Parent => self.pgenerics[generic.key].clone().into(),
        }
    }
}
