// We're gonna try making a proper decision tree with reachability and exahustability checking
// again.
//
// Here's the plan:
//
// We create a tree and we merge stuff into it.
//
// Do we want to initialise it from a type?
//
// I believe with our new system the type will be statically known by the point of where we do this.
//
// So; that should be possible.
//
// One of the things that was annoying before were the tails. I think we want more control of the
// tails here. We want some kind of table.
//
// We also want `if` to be a thing this time
//
// Hm... in old implementations, we for some reason realised that expanding completely is
// impossible.
//
// Although; I'm unsure why that was the case. However; we were forced to make it expand lazily.
//
// Does the type not contain enough information or something?
//
//  NO: The problem was that trees can be infinitely large. That's why we needed it to be lazy.
//
// FOR RANGES: Perhaps we don't merge ranges?
//
// hm, no we do want to merge ranges for the sake of performance regardless.

// OK SO: We're gonna initialise from type. Therefore; we only need to be able to merge.
//
// Hm... perhaps we should let the user write the merge code? As that will be from a custom Pattern type.
//
// Oh we do have `KnownType` from here. So; we can use that.
//
// OR: We let the user handle conversion of `hir::Pattern -> DecTree` and from there we do `DecTree <> DecTree`?
// Ye, because I think we'll need that anyways?

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
struct DeduplicatedStringCheck(u32);
key::entity_impl!(DeduplicatedStringCheck, "strcheck");

pub type IsReachable = bool;

pub const LIST_NIL: key::Variant = key::Variant(0);
pub const LIST_CONS: key::Variant = key::Variant(1);

#[derive(Clone, Copy, PartialEq, Eq)]
struct TailExpr(u32);
entity_impl!(TailExpr, "branch-expr");

#[derive(Clone, Copy, PartialEq, Eq)]
struct IfGuard(u32);
entity_impl!(IfGuard, "if-guard");

struct Match<Expr, TypeKey> {
    root: DecTree,
    tails: Map<TailExpr, Expr>,
    guards: Map<IfGuard, Expr>,
    pat_type: KnownType<TypeKey>,
}

impl<Expr, TypeKey: Clone + fmt::Display> Match<Expr, TypeKey> {
    pub fn new(pat_type: KnownType<TypeKey>) -> Match<Expr, TypeKey> {
        Match {
            root: DecTree::Unreached,
            pat_type,
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
#[derive(Clone)]
enum DecTree {
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
        next: Branching<Range>,
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

#[derive(Clone)]
struct Branching<K> {
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
    fn bools() -> Self {
        let branches = [true, false]
            .into_iter()
            .map(|bool| (bool, 0, DecTree::Unreached))
            .collect();
        Self { branches, full_size: Some(2) }
    }

    fn bool(self, key: bool, next: DecTree) -> Self {
        self.with(key, next)
    }
}

impl Branching<DeduplicatedStringCheck> {
    fn strings() -> Self {
        Self { branches: vec![], full_size: None }
    }

    fn string(mut self, key: DeduplicatedStringCheck, next: DecTree) -> Self {
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
    fn variants(variants: impl IntoIterator<Item = (key::Variant, usize)>) -> Self {
        let branches: Vec<_> = variants
            .into_iter()
            .map(|(variant, params)| (variant, params, DecTree::Unreached))
            .collect();

        Self { full_size: Some(branches.len()), branches }
    }

    fn list() -> Self {
        Self::variants([(LIST_NIL, 0), (LIST_CONS, 2)])
    }

    fn variant(self, key: key::Variant, next: DecTree) -> Self {
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

    fn ints(min: i128, max: i128) -> Self {
        let full = Range::new(range::Constraints { min, max }, min, max);

        Self {
            branches: vec![(full, 0, DecTree::Unreached)],
            full_size: None,
        }
    }

    fn range(mut self, r: std::ops::Range<i128>, next: DecTree) -> Self {
        self.merge_int_at(0, r.start, r.end, next);
        self
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
                Self::Ints { size: l_size, next: l_next },
                Self::Ints { size: r_size, next: r_next },
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
            DecTree::Ints { next, .. } => next.fmt(f),
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
