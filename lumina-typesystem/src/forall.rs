use super::KnownType;
use derive_new::new;
use lumina_key as key;
use lumina_key::Map;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Deserialize, Serialize, Debug)]
pub enum GenericTag {
    Type,
    Trait,
    Impl,
    Func,
    Lambda(key::Lambda),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, new)]
pub struct TaggedGeneric {
    pub tag: GenericTag,
    #[new(default)]
    pub const_: Option<ConstGenericKind>,
    pub key: key::Generic,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum ArrayLen {
    Int(i64),
    Generic(TaggedGeneric),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ConstGenericKind {
    Int,
}

pub fn implicitly_declare<Ident: fmt::Display + fmt::Debug>(
    fenv: &mut ForallEnv<Ident>,
    tag: GenericTag,
) -> TaggedGeneric {
    const IMPLICIT_GENERIC_NAMES: &str =
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXZαβγδεζηθικλμνξοπρστυφχψω0123456789";

    let forall = fenv.get_mut(&tag).expect("generic scope not declared");

    let mut iter = IMPLICIT_GENERIC_NAMES.char_indices();

    loop {
        let Some((i, c)) = iter.next() else {
            panic!(
                "ran out of available implicit generic names.
                 are you sure that whatever you're trying to do is worth it?"
            );
        };

        let name = &IMPLICIT_GENERIC_NAMES[i..i + c.len_utf8()];
        if forall.names.values().any(|n| n.as_str() == name) {
            continue;
        }

        return declare(fenv, tag, name);
    }
}

pub type ForallEnv<Ident> = HashMap<GenericTag, Forall<Ident>>;

pub fn declare<Ident: fmt::Display + fmt::Debug>(
    fenv: &mut ForallEnv<Ident>,
    tag: GenericTag,
    name: impl Into<String>,
) -> TaggedGeneric {
    let Some(forall) = fenv.get_mut(&tag) else {
        panic!("{tag:?} is not declared in fenv");
    };
    let key = forall.constraints.push(vec![]);
    assert_eq!(key, forall.names.push(name.into()));
    TaggedGeneric::new(tag, key)
}

pub fn add_constraint<Ident>(
    fenv: &mut ForallEnv<Ident>,
    generic: TaggedGeneric,
    con: (Ident, Map<key::Generic, KnownType<Ident>>),
) {
    fenv.get_mut(&generic.tag).unwrap().constraints[generic.key].push(con);
}

pub type Constraints<Ident> = Vec<(Ident, Map<key::Generic, KnownType<Ident>>)>;

#[derive(Clone, Debug, Serialize)]
pub struct Forall<Ident> {
    pub constraints: Map<key::Generic, Constraints<Ident>>,
    pub names: Map<key::Generic, String>,
}

impl<I> Default for Forall<I> {
    fn default() -> Self {
        Self { constraints: Map::default(), names: Map::default() }
    }
}

impl<Ident> Forall<Ident> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_names(names: &Map<key::Generic, String>) -> Self {
        Self {
            names: names.clone(),
            constraints: names.keys().map(|_| vec![]).collect(),
        }
    }

    pub fn find(&self, name: &str) -> Option<key::Generic> {
        self.names
            .iter()
            .find_map(|(generic, n)| (name == *n).then_some(generic))
    }
}

pub fn find_generic<Ident>(
    fenv: &ForallEnv<Ident>,
    tag: GenericTag,
    name: &str,
) -> Option<TaggedGeneric> {
    if let GenericTag::Lambda(lambda) = tag {
        if let Some(key) = fenv[&tag].find(name) {
            return Some(TaggedGeneric::new(GenericTag::Lambda(lambda), key));
        }
    }

    [GenericTag::Func, GenericTag::Type, GenericTag::Impl]
        .iter()
        .find_map(|scope| {
            fenv.get(scope)
                .and_then(|forall| forall.find(name).map(|key| TaggedGeneric::new(*scope, key)))
        })
}

impl fmt::Debug for TaggedGeneric {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}·{}", self.tag, self.key)
    }
}

impl fmt::Display for TaggedGeneric {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.tag {
            GenericTag::Func => self.key.fmt(f),
            _ => write!(f, "{}·{}", self.tag, self.key),
        }
    }
}

impl fmt::Display for GenericTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenericTag::Impl => "impl".fmt(f),
            GenericTag::Trait => "trait".fmt(f),
            GenericTag::Type | GenericTag::Func => "".fmt(f),
            GenericTag::Lambda(lambda) => lambda.fmt(f),
        }
    }
}

impl fmt::Display for ArrayLen {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArrayLen::Int(n) => write!(f, "{n}"),
            ArrayLen::Generic(g) => write!(f, "{g}"),
        }
    }
}
