use derive_new::new;
use itertools::Itertools;
use key::{MMap, Map, M};
use lumina_key as key;
use lumina_util::{Highlighting, Ignored, Span, Spanned, Tr};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::fmt;
use tracing::trace;

mod transform;
pub use transform::{
    CircularInst, DirectRecursion, Downgrade, ForeignInst, GenericMapper, Transformer, Upgrade,
};

#[cfg(test)]
mod tests;

mod fin;
pub use fin::Finalizer;

mod intsize;
pub use intsize::IntSize;

mod generic;
pub use generic::{
    ConstGeneric, Constraint, Forall, Generic, GenericData, GenericKind, IMPLICT_GENERIC_NAMES,
};

mod tenv;
pub use tenv::{IntConstraint, TEnv, Var};

mod iquery;
pub use iquery::{Compatibility, ConcreteType, ImplIndex};
pub(crate) use iquery::{GetForall, GetImplData};

mod check;
pub use check::ConstraintError;

#[derive(new)]
pub struct TypeSystem<'a, 's> {
    env: &'a mut TEnv<'s>,
    default_int_size: u8,
    fnames: &'a MMap<key::Record, Map<key::Field, Tr<&'s str>>>,
    ftypes: &'a MMap<key::Record, Map<key::Field, Tr<Type>>>,
    records: &'a MMap<key::Record, (Tr<&'s str>, Forall<'s, Static>)>,
    field_lookup: &'a HashMap<&'s str, Vec<M<key::Record>>>,
}

impl<'a, 's> TypeSystem<'a, 's> {
    fn find_record_by_fields(&self, rvar: Var) -> SmallVec<[M<key::Record>; 4]> {
        let fields = &self.env.vars[rvar].fields;

        let mut possibilities = SmallVec::<[_; 4]>::new();

        for (name, _var, _) in fields {
            let records = self
                .field_lookup
                .get(**name)
                .map(Vec::as_slice)
                .unwrap_or(&[]);

            for &r in records {
                match possibilities
                    .iter_mut()
                    .find_map(|(rec, count)| (*rec == r).then_some(count))
                {
                    Some(count) => *count += 1,
                    None => possibilities.push((r, 1)),
                }
            }
        }

        let Some(largest) = possibilities.iter().map(|(_, c)| *c).max() else {
            return SmallVec::new();
        };

        possibilities
            .into_iter()
            .filter_map(|(record, c)| (c == largest).then_some(record))
            .collect()
    }

    fn try_get_rvar(&mut self, rvar: Var) -> Option<(M<key::Record>, &[IType])> {
        match self.env.vars[rvar].assignment.as_ref() {
            Some(ty) => match &ty.value {
                Ty::Container(Container::Defined(M(module, kind), _), _) => {
                    if let key::TypeKind::Record(record) = kind {
                        // Re-borrow to circumvent limitation of brwck
                        let Ty::Container(_, params) =
                            &self.env.vars[rvar].assignment.as_ref().unwrap().value
                        else {
                            unreachable!();
                        };
                        Some((record.inside(*module), params))
                    } else {
                        None
                    }
                }
                Ty::Special(var) => return self.try_get_rvar(*var),
                _ => panic!("I don't think we're ever gonna assign a non-record to the record"),
            },
            None => self.try_infer_record_by_fields(rvar),
        }
    }

    fn try_infer_record_by_fields(&mut self, rvar: Var) -> Option<(M<key::Record>, &[IType])> {
        let record = match self.find_record_by_fields(rvar).as_slice() {
            [record] => *record,
            _ => return None,
        };

        let span = self.env.vars[rvar].span;
        let params = self.new_record_type_params(span, record);
        self.assign_record_to_rvar(span, rvar, record, params);

        let Ty::Container(Container::Defined(_, _), type_params) =
            &self.env.vars[rvar].assignment.as_ref().unwrap().value
        else {
            unreachable!();
        };

        Some((record, type_params))
    }

    fn new_record_type_params(&mut self, span: Span, record: M<key::Record>) -> Vec<IType> {
        (0..self.records[record].1.generics.len())
            .map(|_| self.env.var(span))
            .map(IType::Special)
            .collect::<Vec<_>>()
    }

    /// Assign a record to the rvar and check all the previously used fields with the now known
    /// concrete field types.
    pub fn assign_record_to_rvar(
        &mut self,
        span: Span,
        rvar: Var,
        record: M<key::Record>,
        type_params: Vec<IType>,
    ) {
        let ty = Ty::defined(record, type_params.clone());
        trace!("{rvar} => {ty}");
        self.env.vars[rvar].assignment = Some(ty.tr(span));

        // Now that we know the type, we can go back and type check any of the fields we previously
        // accepted as correct without knowing the real record type.
        for i in 0..self.env.vars[rvar].fields.len() {
            let (fname, var, mismatch) = self.env.vars[rvar].fields[i];
            assert!(mismatch.is_none());

            if let Some(ty) = self.inst_field(record, &type_params, *fname) {
                // We can't use normal unify because then it'll use `field_of` as a shortcut
                if let Some(previous) = self.env.vars[var].assignment.clone() {
                    let ok = self.unify(previous.span, &*previous, &ty);
                    if !ok {
                        self.env.vars[rvar].fields[i].2 = Some(tenv::FieldMismatch);
                        // Overwrite the incorrect var with the real type
                        self.env.vars[var].assignment = Some(ty.tr(fname.span));
                    }
                } else {
                    self.env.assign(var, ty.tr(fname.span));
                }
            }
        }
    }

    pub fn inst_field<T: Clone>(
        &self,
        record: M<key::Record>,
        params: &[Ty<T>],
        fname: &str,
    ) -> Option<Ty<T>> {
        let field = self.fnames[record]
            .iter()
            .find_map(|(k, n)| (**n == fname).then_some(k))?;

        let inst = GenericMapper::from_types(GenericKind::Entity, params.iter().cloned());

        let fty = &self.ftypes[record][field];
        Some((&inst).transform(fty))
    }

    // Retrieve a known field type if a record can/has been inferred
    fn try_get_field(&mut self, fname: Tr<&'s str>, rvar: Var) -> Option<(M<key::Record>, IType)> {
        let (record, type_params) = self.try_get_rvar(rvar)?;
        let inst = GenericMapper::from_types(GenericKind::Entity, type_params.iter().cloned());

        let field = self.fnames[record]
            .iter()
            .find_map(|(k, n)| (**n == *fname).then_some(k))?;

        let fty = &self.ftypes[record][field];
        Some((record, (&inst).transform(fty)))
    }

    fn try_get_field_if_is_field(&mut self, var: Var) -> Option<(M<key::Record>, IType)> {
        self.env.vars[var]
            .field_of
            .and_then(|(fname, rvar)| self.try_get_field(fname, rvar))
    }
}

/// A generalised and customizable `Type` type with visitor API's
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Ty<T> {
    // Types which contain other types
    Container(Container, Vec<Self>),

    Generic(Generic),
    Int(IntSize),

    Const(ConstValue),

    // Types that are only equal to themselves
    Simple(&'static str),

    // Special variants that are often transformed.
    Special(T),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum ConstValue {
    Usize(u64),
    Bool(bool),
    Char(char),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Container {
    FnPointer,
    Closure,
    Tuple,
    Pointer,
    Array,
    Defined(M<key::TypeKind>, Ignored<Lang>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Lang {
    String,
    List,
    None,
}

impl<T> Ty<T> {
    pub fn as_trait(self) -> Result<(M<key::Trait>, Vec<Self>), Self> {
        match self {
            Ty::Container(Container::Defined(M(module, key::TypeKind::Trait(key)), _), params) => {
                Ok((key.inside(module), params))
            }
            other => Err(other),
        }
    }

    pub fn tuple(elems: Vec<Self>) -> Self {
        Self::Container(Container::Tuple, elems)
    }

    pub fn defined<K: Into<key::TypeKind>>(key: M<K>, params: Vec<Self>) -> Self {
        let key = key.map(K::into);
        Self::Container(Container::Defined(key, Ignored::new(Lang::None)), params)
    }
    pub fn list<K: Into<key::TypeKind>>(key: M<K>, params: Vec<Self>) -> Self {
        let key = key.map(K::into);
        Self::Container(Container::Defined(key, Ignored::new(Lang::List)), params)
    }
    pub fn array(size: u64, inner: Self) -> Self {
        let size = Ty::Const(ConstValue::Usize(size));
        Self::Container(Container::Array, vec![inner, size])
    }
    pub fn const_array(generic: Generic, inner: Self) -> Self {
        let size = Ty::Generic(generic);
        Self::Container(Container::Array, vec![inner, size])
    }
    pub fn string<K: Into<key::TypeKind>>(key: M<K>, params: Vec<Self>) -> Self {
        let key = key.map(K::into);
        Self::Container(Container::Defined(key, Ignored::new(Lang::String)), params)
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

    pub const fn u8() -> Self {
        Self::int(false, 8)
    }
    pub fn u8_pointer() -> Self {
        Self::pointer(Self::Int(IntSize::new(false, 8)))
    }

    pub const fn int(signed: bool, size: u8) -> Self {
        Self::Int(IntSize::new(signed, size))
    }

    pub const fn f64() -> Self {
        Self::Simple("f64")
    }

    pub const fn f32() -> Self {
        Self::Simple("f32")
    }

    pub const fn poison() -> Self {
        Self::Simple("poison")
    }

    pub const fn bool() -> Self {
        Self::Simple("bool")
    }

    pub const fn self_() -> Self {
        Self::Simple("self")
    }
}

impl Ty<Inference> {
    pub fn infer(var: Var) -> Self {
        Self::Special(var)
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

pub type Inference = Var;

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
            Container::Array => write!(f, "[{}; {}]", format(&elems[0]), format(&elems[1])),
            Container::Defined(_, Ignored { inner: Lang::String }) => write!(f, "string"),
            Container::Defined(_, Ignored { inner: Lang::List }) => {
                write!(f, "[{}]", elems.iter().map(format).format(", "))
            }
            Container::Defined(key, Ignored { inner: Lang::None }) => {
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
            let params = elems[..elems.len() - 1]
                .iter()
                .format_with(", ", |elem, f| f(&format(elem)));
            let ret = format(elems.last().unwrap());
            let arrow = "->".symbol();
            if h == "" {
                write!(f, "{params} {arrow} {ret}")
            } else {
                write!(f, "{h}{open}{params} {arrow} {ret}{close}",)
            }
        }
    }
}

impl<T: fmt::Display> fmt::Display for Ty<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ty::Container(cont, params) => cont.fmt(params, |t| t.to_string(), f),
            Ty::Generic(generic) => write!(f, "{generic}"),
            Ty::Const(const_) => write!(f, "{const_}"),
            Ty::Int(size) => write!(f, "{size}"),
            Ty::Simple(name) => name.fmt(f),
            Ty::Special(special) => special.fmt(f),
        }
    }
}

impl<T: fmt::Display> fmt::Debug for Ty<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for ConstValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConstValue::Usize(n) => n.fmt(f),
            ConstValue::Bool(b) => b.fmt(f),
            ConstValue::Char(c) => c.fmt(f),
        }
    }
}
