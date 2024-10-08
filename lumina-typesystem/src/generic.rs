use super::{IntSize, Ty};
use derive_more::{Index, IndexMut};
use itertools::Itertools;
use lumina_key as key;
use lumina_key::{Map, M};
use lumina_util::Span;
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Generic {
    pub key: key::Generic,
    pub kind: GenericKind,
}

impl Generic {
    pub fn new(key: key::Generic, kind: impl Into<GenericKind>) -> Self {
        Generic { key, kind: kind.into() }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum GenericKind {
    Lambda(key::Lambda),
    Entity,
    // The `impl` or `trait` blocks generics
    Parent,
}

#[derive(Clone)]
pub struct GenericData<'s, T> {
    pub name: &'s str,
    pub trait_constraints: Vec<Constraint<T>>,
    pub const_: Option<ConstGeneric>,
}

/// Mainly exists so we can have proper array types in the stdlib
#[derive(Clone, Copy)]
pub enum ConstGeneric {
    Int(IntSize),
    Bool,
    Char,
}

impl<'s, T> GenericData<'s, T> {
    pub fn new(name: &'s str) -> Self {
        Self { name, const_: None, trait_constraints: vec![] }
    }

    pub fn map<'o, O>(
        &self,
        mut f: impl FnMut(Span, &Ty<T>) -> Ty<O>,
        rename: impl Fn(&'s str) -> &'o str,
    ) -> GenericData<'o, O> {
        GenericData {
            name: rename(self.name),
            const_: self.const_,
            trait_constraints: self
                .trait_constraints
                .iter()
                .map(|constraint| Constraint {
                    span: constraint.span,
                    trait_: constraint.trait_,
                    params: constraint
                        .params
                        .iter()
                        .map(|ty| f(constraint.span, ty))
                        .collect(),
                })
                .collect(),
        }
    }
}

#[derive(Index, IndexMut, Clone)]
pub struct Forall<'s, T> {
    pub generics: Map<key::Generic, GenericData<'s, T>>,
}

impl<'s, T> Forall<'s, T> {
    pub fn new(cap: usize) -> Self {
        Forall { generics: Map::with_capacity(cap) }
    }

    pub fn from_names(iter: impl Iterator<Item = &'s str>) -> Self {
        Self { generics: iter.map(GenericData::new).collect() }
    }

    pub fn push(&mut self, name: &'s str) -> key::Generic {
        self.generics.push(GenericData::new(name))
    }

    pub fn names(&self) -> impl Iterator<Item = &'s str> + '_ {
        self.generics.values().map(|gdata| gdata.name)
    }

    pub fn to_types<O>(&self, kind: GenericKind) -> Vec<Ty<O>> {
        self.generics
            .keys()
            .map(|key| Generic::new(key, kind))
            .map(Ty::Generic)
            .collect()
    }

    pub fn find(&self, name: &str) -> Option<key::Generic> {
        self.generics
            .iter()
            .find_map(|(key, gdata)| (gdata.name == name).then_some(key))
    }

    pub fn implicitly_declare(&mut self) -> (key::Generic, &mut GenericData<'s, T>) {
        let mut iter = IMPLICT_GENERIC_NAMES.char_indices();

        loop {
            let Some((i, c)) = iter.next() else {
                panic!(
                    "ran out of available implicit generic names.
                 what cursed code-generation monstrosity are you making?"
                );
            };

            let name = &IMPLICT_GENERIC_NAMES[i..i + c.len_utf8()];
            if self.find(name).is_some() {
                continue;
            }

            let key = self.push(name);

            return (key, &mut self[key]);
        }
    }

    pub fn map<'o, O>(
        &self,
        mut f: impl FnMut(Span, &Ty<T>) -> Ty<O>,
        rename: impl Fn(key::Generic, &'s str) -> &'o str + Clone,
    ) -> Forall<'o, O> {
        Forall {
            generics: self
                .generics
                .iter()
                .map(|(key, gdata)| gdata.map(&mut f, |name| rename(key, name)))
                .collect(),
        }
    }
}

impl<'s, T: Clone> Forall<'s, T> {
    pub fn name_by_key(key: key::Generic, _: &'s str) -> &'static str {
        if key.0 < 48 {
            &IMPLICT_GENERIC_NAMES[key.0 as usize..key.0 as usize + 1]
        } else {
            "_"
        }
    }

    pub fn rename_to_keys(&self) -> Forall<'static, T> {
        self.map(|_, ty| ty.clone(), Forall::<()>::name_by_key)
    }
}

pub const IMPLICT_GENERIC_NAMES: &str =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXZαβγδεζηθικλμνξοπρστυφχψω0123456789";

#[derive(Clone)]
pub struct Constraint<T> {
    pub span: Span,
    pub trait_: M<key::Trait>,
    pub params: Vec<Ty<T>>,
}

impl<T: fmt::Display> fmt::Display for Constraint<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.params.is_empty() {
            self.trait_.fmt(f)
        } else {
            write!(f, "({} {})", self.trait_, self.params.iter().format(" "))
        }
    }
}

impl fmt::Display for Generic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}·{}", self.kind, self.key)
    }
}

impl fmt::Display for GenericKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GenericKind::Entity => "".fmt(f),
            GenericKind::Lambda(lambda) => lambda.fmt(f),
            GenericKind::Parent => "impl".fmt(f),
        }
    }
}

impl<'s, T: fmt::Display> fmt::Debug for Forall<'s, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl<'s, T: fmt::Display> fmt::Display for Forall<'s, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.generics.is_empty() {
            "∀. ".fmt(f)
        } else {
            write!(
                f,
                "∀{} . ",
                self.generics.keys().format_with(" ", |key, f| f(&key))
            )
        }
    }
}

impl fmt::Display for ConstGeneric {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConstGeneric::Int(size) => write!(f, "const {size}"),
            ConstGeneric::Bool => write!(f, "const bool"),
            ConstGeneric::Char => write!(f, "const char"),
        }
    }
}
