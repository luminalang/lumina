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

#[derive(Clone, Debug)]
pub struct GenericData<'s, T> {
    pub name: &'s str,
    pub params: usize,
    pub trait_constraints: Vec<Constraint<T>>,
}

impl<'s, T> GenericData<'s, T> {
    pub fn new(name: &'s str) -> Self {
        Self { name, params: 0, trait_constraints: vec![] }
    }

    pub fn set_params(&mut self, n: usize) {
        self.params = n;
    }
}

pub type Forall<'s, T> = Map<key::Generic, GenericData<'s, T>>;

pub fn implicitly_declare_generic<'f, 's, T>(
    forall: &'f mut Forall<'s, T>,
) -> (key::Generic, &'f mut GenericData<'s, T>) {
    let mut iter = IMPLICT_GENERIC_NAMES.char_indices();

    loop {
        let Some((i, c)) = iter.next() else {
            panic!(
                "ran out of available implicit generic names.
                 what cursed code-generation monstrosity are you making?"
            );
        };

        let name = &IMPLICT_GENERIC_NAMES[i..i + c.len_utf8()];
        if forall.values().any(|generic| generic.name == name) {
            continue;
        }

        let key = forall.push(GenericData::new(name));

        return (key, &mut forall[key]);
    }
}

pub const IMPLICT_GENERIC_NAMES: &str =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXZαβγδεζηθικλμνξοπρστυφχψω0123456789";

#[derive(Clone, Debug)]
pub struct Constraint<T> {
    pub span: Span,
    pub trait_: M<key::Trait>,
    pub params: Vec<T>,
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
