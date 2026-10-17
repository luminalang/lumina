use std::fmt;

#[cfg(test)]
mod tests;

use itertools::Itertools;
use key::{Map, entity_impl};
use lumina_key as key;
use lumina_typesystem::{IntSize, KnownType};

mod range;
use range::Range;

mod inverter;

mod merge;
use merge::reachable;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct DeduplicatedStringCheck(u32);
key::entity_impl!(DeduplicatedStringCheck, "strcheck");

pub type IsReachable = bool;

pub const LIST_NIL: key::Variant = key::Variant(0);
pub const LIST_CONS: key::Variant = key::Variant(1);

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct TailExpr(u32);
entity_impl!(TailExpr, "branch-expr");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct IfGuard(u32);
entity_impl!(IfGuard, "if-guard");

#[derive(Debug)]
pub struct Match<Expr> {
    pub root: DecTree,
    pub tails: Map<TailExpr, Expr>,
    pub guards: Map<IfGuard, Expr>,
}

impl<Expr> Match<Expr> {
    pub fn new() -> Match<Expr> {
        Match {
            root: DecTree::Unreached,
            tails: Map::new(),
            guards: Map::new(),
        }
    }

    pub fn add_if_guard(&mut self, expr: Expr) -> IfGuard {
        self.guards.push(expr)
    }

    pub fn add_tail(&mut self, expr: Expr) -> TailExpr {
        self.tails.push(expr)
    }

    pub fn add_branch(&mut self, just_branch: DecTree) -> merge::Reachable {
        self.root.merge(just_branch)
    }
}

/// A decision tree where the depth of the data structure also represents each point of the pattern
/// that can be bound to a value.
#[derive(Clone, Debug)]
pub enum DecTree {
    // Non-branching chains
    Record {
        fields: usize,
        next: Box<Self>,
    },
    Tuple {
        elems: usize,
        next: Box<Self>,
    },
    Array {
        elems: usize,
        next: Box<Self>,
    },

    // Branching chains
    IfGuarded {
        guard: IfGuard,
        true_next: Box<Self>,
        false_next: Box<Self>,
    },
    Variants {
        branching: Branching<key::Variant>,
    },
    List {
        branching: Branching<key::Variant>,
    },
    Ints {
        size: IntSize,
        branching: Branching<Range>,
    },
    Bools {
        branching: Branching<bool>,
    },
    Strings {
        branching: Branching<DeduplicatedStringCheck>,
        wildcard_next: Option<Box<Self>>,
    },

    // We can't eagerly expand all wildcards by type since recursive data types exist.
    //
    // But whenever possible, we avoid using `Wildcard` in favor of expanding the real form of the pattern.
    Wildcard {
        next: Box<Self>,
    },

    // Special
    Unreached,
    Tail(TailExpr),
    Error,
}

impl DecTree {
    pub fn wildcard(next: Self) -> Self {
        Self::Wildcard { next: Box::new(next) }
    }

    pub fn tuple(elems: usize, next: Self) -> Self {
        Self::Tuple { elems, next: Box::new(next) }
    }
}

type Params = usize;

pub trait BranchingKey {
    fn into_dectree(self) -> DecTree;
}

impl From<Branching<bool>> for DecTree {
    fn from(val: Branching<bool>) -> Self {
        DecTree::Bools { branching: val }
    }
}

impl From<Branching<key::Variant>> for DecTree {
    fn from(val: Branching<key::Variant>) -> Self {
        DecTree::Variants { branching: val }
    }
}

impl From<Branching<DeduplicatedStringCheck>> for DecTree {
    fn from(val: Branching<DeduplicatedStringCheck>) -> Self {
        DecTree::Strings { branching: val, wildcard_next: None }
    }
}

#[derive(Clone, Debug)]
pub struct Branching<K> {
    branches: Vec<(K, Params, DecTree)>,
    full_size: Option<usize>,
}

impl<K: PartialEq> Branching<K> {
    fn with(mut self, key: K, next: DecTree) -> Self {
        for (bkey, _bparams, bnext) in self.branches.iter_mut() {
            if *bkey == key {
                bnext.merge(next);
                return self;
            }
        }

        panic!("Branching was initialised without full size coverage");
    }
}

impl Branching<bool> {
    pub fn bools() -> Self {
        let branches = [true, false]
            .into_iter()
            .map(|bool| (bool, 0, DecTree::Unreached))
            .collect();
        Self { branches, full_size: Some(2) }
    }

    pub fn bool(self, key: bool, next: DecTree) -> Self {
        self.with(key, next)
    }
}

impl Branching<DeduplicatedStringCheck> {
    pub fn strings() -> Self {
        Self { branches: vec![], full_size: None }
    }

    pub fn string(mut self, key: DeduplicatedStringCheck, next: DecTree) -> Self {
        for (bkey, _bparams, bnext) in self.branches.iter_mut() {
            if *bkey == key {
                bnext.merge(next);
                return self;
            }
        }

        self.branches.push((key, 0, next));
        self
    }
}

impl Branching<key::Variant> {
    pub fn variants(variants: impl IntoIterator<Item = (key::Variant, usize)>) -> Self {
        let branches: Vec<_> = variants
            .into_iter()
            .map(|(variant, params)| (variant, params, DecTree::Unreached))
            .collect();

        Self { full_size: Some(branches.len()), branches }
    }

    pub fn list() -> Self {
        Self::variants([(LIST_NIL, 0), (LIST_CONS, 2)])
    }

    pub fn variant(self, key: key::Variant, next: DecTree) -> Self {
        self.with(key, next)
    }

    pub fn into_list_dectree(self) -> DecTree {
        DecTree::List { branching: self }
    }
}

impl Branching<Range> {
    fn ints_with_next(min: i128, max: i128, next: DecTree) -> Self {
        let full = Range::new(range::Constraints { min, max }, min, max);
        Self { branches: vec![(full, 0, next)], full_size: None }
    }

    pub fn ints(min: i128, max: i128) -> Self {
        let full = Range::new(range::Constraints { min, max }, min, max);

        Self {
            branches: vec![(full, 0, DecTree::Unreached)],
            full_size: None,
        }
    }

    pub fn range(mut self, r: std::ops::Range<i128>, next: DecTree) -> Self {
        self.merge_int_at(0, r.start, r.end, next);
        self
    }

    pub fn into_ints_dectree(self, size: IntSize) -> DecTree {
        DecTree::Ints { branching: self, size }
    }
}

/// All tails are treated as identical
impl PartialEq for DecTree {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Record { fields: l_fields, next: l_next },
                Self::Record { fields: r_fields, next: r_next },
            ) => l_fields == r_fields && l_next == r_next,
            (
                Self::Tuple { elems: l_elems, next: l_next },
                Self::Tuple { elems: r_elems, next: r_next },
            ) => l_elems == r_elems && l_next == r_next,
            (
                Self::Array { elems: l_elems, next: l_next },
                Self::Array { elems: r_elems, next: r_next },
            ) => l_elems == r_elems && l_next == r_next,
            (
                Self::IfGuarded {
                    guard: l_guard,
                    true_next: l_true_next,
                    false_next: l_false_next,
                },
                Self::IfGuarded {
                    guard: r_guard,
                    true_next: r_true_next,
                    false_next: r_false_next,
                },
            ) => l_guard == r_guard && l_true_next == r_true_next && l_false_next == r_false_next,
            (
                Self::Variants { branching: l_branching },
                Self::Variants { branching: r_branching },
            ) => l_branching == r_branching,
            (Self::List { branching: l_branching }, Self::List { branching: r_branching }) => {
                l_branching == r_branching
            }
            (
                Self::Ints { size: l_size, branching: l_next },
                Self::Ints { size: r_size, branching: r_next },
            ) => l_size == r_size && l_next == r_next,
            (Self::Bools { branching: l_branching }, Self::Bools { branching: r_branching }) => {
                l_branching == r_branching
            }
            (Self::Wildcard { next: l_next }, Self::Wildcard { next: r_next }) => l_next == r_next,
            (Self::Tail(_), Self::Tail(_)) => true,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl<K: PartialEq> PartialEq for Branching<K> {
    fn eq(&self, other: &Self) -> bool {
        self.branches == other.branches && self.full_size == other.full_size
    }
}

impl fmt::Display for DecTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecTree::Record { fields, next } => {
                write!(
                    f,
                    "Record(nfields={fields})\n  {}",
                    next.to_string().split('\n').format("\n  ")
                )
            }
            DecTree::Tuple { elems, next } => {
                write!(
                    f,
                    "Tuple(nelems={elems})\n  {}",
                    next.to_string().split('\n').format("\n  ")
                )
            }
            DecTree::Array { elems, next } => todo!(),
            DecTree::IfGuarded { guard, true_next, false_next } => {
                write!(
                    f,
                    "{guard}\n  true_next:\n    {}\n  true_false:\n    {}",
                    true_next.to_string().split('\n').format("\n    "),
                    false_next.to_string().split('\n').format("\n    ")
                )
            }
            DecTree::Variants { branching } => branching.fmt(f),
            DecTree::List { branching } => branching.fmt(f),
            DecTree::Ints { branching: next, .. } => next.fmt(f),
            DecTree::Bools { branching } => branching.fmt(f),
            DecTree::Strings { branching, wildcard_next } => match wildcard_next {
                Some(wc_next) => {
                    write!(
                        f,
                        "{branching}\n_:\n  {}",
                        wc_next.to_string().split('\n').format("\n  ")
                    )
                }
                None => branching.fmt(f),
            },
            DecTree::Wildcard { next } => {
                write!(f, "_:\n  {}", next.to_string().split('\n').format("\n  "))
            }
            DecTree::Unreached => "UNREACHED".fmt(f),
            DecTree::Tail(tail_expr) => tail_expr.fmt(f),
            DecTree::Error => "ERROR".fmt(f),
        }
    }
}

impl<K: fmt::Display> fmt::Display for Branching<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (k, params, next) in &self.branches {
            writeln!(
                f,
                "{k}(nparams={params}):\n  {}",
                next.to_string().split('\n').format("\n  ")
            )?;
        }

        Ok(())
    }
}
