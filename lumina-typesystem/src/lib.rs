use derive_new::new;
use itertools::Itertools;
use key::{Map, ModMap, M};
use lumina_key as key;
use lumina_util::{Highlighting, Span, Tr};
use std::collections::HashMap;
use std::fmt;

mod transform;
pub use transform::{
    CircularInst, DirectRecursion, Downgrade, ForeignInst, GenericMapper, Transformer,
};

mod intsize;
pub use intsize::IntSize;

mod fin;
pub use fin::{FinError, Finalizer};

mod generic;
pub use generic::{Constraint, Forall, Generic, GenericData, GenericKind, IMPLICT_GENERIC_NAMES};

mod tenv;
pub use tenv::{
    IntConstraint, RecordAssignment, RecordError, RecordVar, RecordVarField, TEnv, Var, VarInfo,
};

mod iquery;
pub use iquery::{Compatibility, ConcreteType, ImplIndex};

#[derive(new)]
pub struct TypeSystem<'a, 's> {
    pub env: &'a mut TEnv<'s>,
    pub records: &'a ModMap<key::Record, (Tr<&'s str>, Forall<'s, Static>)>,
    pub ftypes: &'a ModMap<key::Record, Map<key::RecordField, Tr<Type>>>,
    pub fnames: &'a ModMap<key::Record, Map<key::RecordField, Tr<&'s str>>>,
    pub field_lookup: &'a HashMap<&'s str, Vec<M<key::Record>>>,
}

mod check;
pub use check::CheckResult;

/// A generalised and customizable `Type` type with visitor API's
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Ty<T> {
    // Types which contain other types
    Container(Container, Vec<Self>),

    Generic(Generic),
    Int(IntSize),

    // Types that are only equal to themselves
    Simple(&'static str),

    // Special variants that are often transformed.
    Special(T),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Container {
    FnPointer,
    Closure,
    Tuple,
    Pointer,
    Defined(M<key::TypeKind>),
    List(M<key::TypeKind>),
}

impl<T> Ty<T> {
    pub fn as_trait(self) -> Result<(M<key::Trait>, Vec<Self>), Self> {
        match self {
            Ty::Container(
                Container::Defined(M { module, value: key::TypeKind::Trait(key) })
                | Container::List(M { module, value: key::TypeKind::Trait(key) }),
                params,
            ) => Ok((module.m(key), params)),
            other => Err(other),
        }
    }

    pub fn tuple(elems: Vec<Self>) -> Self {
        Self::Container(Container::Tuple, elems)
    }

    pub fn defined<K: Into<key::TypeKind>>(key: M<K>, params: Vec<Self>) -> Self {
        let key = key.map(K::into);
        Self::Container(Container::Defined(key), params)
    }
    pub fn list<K: Into<key::TypeKind>>(key: M<K>, params: Vec<Self>) -> Self {
        let key = key.map(K::into);
        Self::Container(Container::List(key), params)
    }

    pub fn fn_pointer(mut params: Vec<Self>, ret: Self) -> Self {
        params.push(ret);
        Self::Container(Container::FnPointer, params)
    }
    pub fn closure(mut params: Vec<Self>, ret: Self) -> Self {
        params.push(ret);
        Self::Container(Container::Closure, params)
    }

    pub fn pointer(ty: Self) -> Self {
        let params = vec![ty];
        Self::Container(Container::Pointer, params)
    }

    pub fn u8() -> Self {
        Self::int(false, 8)
    }
    pub fn u8_pointer() -> Self {
        Self::pointer(Self::Int(IntSize::new(false, 8)))
    }

    pub fn int(signed: bool, size: u8) -> Self {
        Self::Int(IntSize::new(signed, size))
    }

    pub fn f64() -> Self {
        Self::Simple("f64")
    }

    pub fn f32() -> Self {
        Self::Simple("f32")
    }

    pub fn poison() -> Self {
        Self::Simple("poison")
    }

    pub fn bool() -> Self {
        Self::Simple("bool")
    }

    pub fn self_() -> Self {
        Self::Simple("self")
    }
}

impl Ty<Inference> {
    pub fn infer(var: Var) -> Self {
        Self::Special(Inference::Var(var))
    }

    pub fn infrecord(rvar: RecordVar) -> Self {
        Self::Special(Inference::Record(rvar))
    }

    pub fn inffield(rvar: RecordVar, field: RecordVarField) -> Self {
        Self::Special(Inference::Field(rvar, field))
    }
}

pub type IType = Ty<Inference>;
pub type Type = Ty<Static>;

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Static;

impl fmt::Display for Static {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        "static".fmt(f)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Inference {
    Var(Var),
    Record(RecordVar),
    Field(RecordVar, RecordVarField),
}

impl Container {
    pub fn fmt<T>(
        &self,
        elems: &[T],
        format: impl Fn(&T) -> String,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        match self {
            Container::FnPointer => Self::fmt_func("fnptr", elems, format, f),
            Container::Closure => Self::fmt_func("fn", elems, format, f),
            Container::Tuple => Self::fmt_tuple(elems, format, f),
            Container::Pointer => write!(f, "*{}", format(&elems[0])),
            Container::List(key) | Container::Defined(key) => {
                Self::fmt_defined(key, elems, format, f, true)
            }
        }
    }

    pub fn fmt_defined<T>(
        header: impl fmt::Display,
        params: &[T],
        format: impl Fn(&T) -> String,
        f: &mut fmt::Formatter,
        paren: bool,
    ) -> fmt::Result {
        if params.is_empty() {
            write!(f, "{header}")
        } else {
            let params = params.iter().format_with(" ", |p, f| f(&format(p)));
            if paren {
                write!(f, "({header} {})", params)
            } else {
                write!(f, "{header} {}", params)
            }
        }
    }

    pub fn fmt_tuple<T>(
        elems: &[T],
        format: impl Fn(&T) -> String,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        write!(
            f,
            "({})",
            elems.iter().format_with(", ", |elem, f| f(&format(elem)))
        )
    }

    pub fn fmt_func<T>(
        h: &str,
        elems: &[T],
        format: impl Fn(&T) -> String,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        let open = '('.symbol();
        let close = ')'.symbol();
        if elems.len() == 1 {
            write!(f, "{h}{open}{}{close}", format(&elems[0]))
        } else {
            write!(
                f,
                "{h}{open}{} {} {}{close}",
                elems[..elems.len() - 1]
                    .iter()
                    .format_with(", ", |elem, f| f(&format(elem))),
                "->".symbol(),
                format(&elems[0])
            )
        }
    }
}

impl<T: fmt::Display> fmt::Display for Ty<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ty::Container(cont, params) => cont.fmt(params, |t| t.to_string(), f),
            Ty::Generic(generic) => write!(f, "{generic}"),
            Ty::Int(size) => write!(f, "{size}"),
            Ty::Simple(name) => name.fmt(f),
            Ty::Special(special) => special.fmt(f),
        }
    }
}

impl fmt::Display for Inference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Inference::Var(var) => var.fmt(f),
            Inference::Record(record) => record.fmt(f),
            Inference::Field(record, field) => write!(f, "{record}.{field}"),
        }
    }
}

impl<T: fmt::Display> fmt::Debug for Ty<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}
