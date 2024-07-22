use crate::prelude::*;
use lumina_typesystem::{Bitsize, Type};
use std::collections::VecDeque;

mod range;
pub use range::Range;

mod init;
pub use init::Init;

mod merge;
pub use merge::Merge;

mod missing;
pub use missing::{MissingFormatter, MissingGeneration, MissingPattern};

#[cfg(test)]
mod tests;

/// A decision tree where the depth of the data structure also represents each point of the pattern
/// that can be bound to a value.
#[derive(Clone, Debug)]
pub enum DecTree<Tail> {
    // Non-branching chains
    Record {
        record: M<key::Record>,
        params: Vec<Type>,
        fields: usize,
        next: Box<Self>,
    },
    Tuple {
        elems: usize,
        next: Box<Self>,
    },

    // Branching chains
    Sum {
        sum: M<key::Sum>,
        params: Vec<Type>,
        next: Branching<key::SumVariant, Tail>,
    },
    List {
        next: Branching<key::SumVariant, Tail>,
        ty: Type,
    },
    Ints {
        bitsize: Bitsize,
        signed: bool,
        next: Branching<Range, Tail>,
    },
    Bools(Branching<bool, Tail>),

    // We can't eagerly expand all wildcards by type since recursive data types exist
    Wildcard {
        ty: Type,
        next: Box<Self>,
    },

    // Values that can be pattern matched against with wildcards but can't be destructed
    //
    // Such as function pointers, trait objects, pointers, etc.
    Opaque {
        ty: Type,
        next: Box<Self>,
    },

    End(TreeTail<Tail>),
}

#[derive(Clone, Debug)]
pub enum TreeTail<Tail> {
    Poison,
    Unreached(VecDeque<Type>),
    Reached(PointTable, VecDeque<Type>, Tail),
}

pub const LIST_CONS: key::SumVariant = key::SumVariant(0);
pub const LIST_NIL: key::SumVariant = key::SumVariant(1);

/// Maps binds (created by wildcards) to depth points of the decision tree
#[derive(Debug, Clone, new, PartialEq)]
pub struct PointTable {
    pub binds: Vec<(key::Bind, usize)>,
}

type Params = u32;

pub trait BranchKey: std::fmt::Display {}

impl BranchKey for key::SumVariant {}
impl BranchKey for bool {}
impl BranchKey for Range {}

#[derive(Debug, Clone)]
pub struct Branching<K: BranchKey, Tail> {
    pub branches: Vec<(K, DecTree<Tail>)>,
}

impl<Tail> DecTree<Tail> {
    pub fn for_each_tail_mut(&mut self, f: &mut impl FnMut(TreeTail<Tail>) -> DecTree<Tail>) {
        match self {
            DecTree::Wildcard { next, .. } => next.for_each_tail_mut(f),
            DecTree::Record { next, .. } => next.for_each_tail_mut(f),
            DecTree::Tuple { next, .. } => next.for_each_tail_mut(f),
            DecTree::Sum { next, .. } => next.for_each_tail_mut(f),
            DecTree::Bools(next) => next.for_each_tail_mut(f),
            DecTree::Opaque { next, .. } => next.for_each_tail_mut(f),
            DecTree::List { next, .. } => next.for_each_tail_mut(f),
            DecTree::Ints { next, .. } => next.for_each_tail_mut(f),
            DecTree::End(_) => take_mut::take(self, |tree| match tree {
                DecTree::End(tail) => f(tail),
                _ => unreachable!(),
            }),
        }
    }

    pub fn for_each_tail(&self, f: &mut impl FnMut(&TreeTail<Tail>)) {
        match self {
            DecTree::Wildcard { next, .. } => next.for_each_tail(f),
            DecTree::Record { next, .. } => next.for_each_tail(f),
            DecTree::Tuple { next, .. } => next.for_each_tail(f),
            DecTree::Sum { next, .. } => next.for_each_tail(f),
            DecTree::Bools(next) => next.for_each_tail(f),
            DecTree::Opaque { next, .. } => next.for_each_tail(f),
            DecTree::List { next, .. } => next.for_each_tail(f),
            DecTree::Ints { next, .. } => next.for_each_tail(f),
            DecTree::End(tail) => f(tail),
        }
    }

    fn unreached() -> Self {
        DecTree::End(TreeTail::Unreached(VecDeque::new()))
    }

    fn lazy<const N: usize>(types: [Type; N]) -> Self {
        Self::End(TreeTail::Unreached(types.into()))
    }

    fn bump_tail_table_points(&mut self, by: usize) {
        self.for_each_tail_mut(&mut |tail| match tail {
            TreeTail::Reached(mut table, excess, tail) => {
                table.binds.iter_mut().for_each(|(_, point)| *point += by);
                DecTree::End(TreeTail::Reached(table, excess, tail))
            }
            tail => DecTree::End(tail),
        });
    }
}

impl<K: BranchKey, Tail> Branching<K, Tail> {
    fn singleton(key: K, next: DecTree<Tail>) -> Self {
        Branching { branches: vec![(key, next)] }
    }

    fn for_each_tail_mut(&mut self, f: &mut impl FnMut(TreeTail<Tail>) -> DecTree<Tail>) {
        self.branches
            .iter_mut()
            .for_each(|(_, next)| next.for_each_tail_mut(&mut *f))
    }

    fn for_each_tail(&self, f: &mut impl FnMut(&TreeTail<Tail>)) {
        self.branches
            .iter()
            .for_each(|(_, next)| next.for_each_tail(&mut *f))
    }
}

impl<K: BranchKey, Tail: std::fmt::Display> Branching<K, Tail> {
    fn fmt(&self, header: impl std::fmt::Display, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use std::fmt::Display;

        self.branches
            .iter()
            .format_with("\n", |(var, next), f| {
                f(&format_args!(
                    "{header} {var}\n    {}",
                    next.to_string().lines().format("\n    ")
                ))
            })
            .fmt(f)
    }
}

impl<Tail: std::fmt::Display> std::fmt::Display for DecTree<Tail> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            DecTree::Wildcard { next, .. } => {
                write!(f, "_\n  {}", next.to_string().lines().format("\n  "))
            }
            DecTree::Record { record, fields, next, .. } => {
                write!(
                    f,
                    "{record}(fields = {fields})\n  {}",
                    next.to_string().lines().format("\n  ")
                )
            }
            DecTree::Tuple { elems, next } => {
                write!(
                    f,
                    "tuple(len = {elems})\n  {}",
                    next.to_string().lines().format("\n  ")
                )
            }
            DecTree::Sum { sum, next, .. } => next.fmt(*sum, f),
            DecTree::List { next, .. } => next.fmt("list", f),
            DecTree::Ints { bitsize, signed, next } => {
                let c = signed.then_some('i').unwrap_or('u');
                next.fmt(format!("{c}{bitsize}"), f)
            }
            DecTree::Bools(next) => next.fmt("bool", f),
            DecTree::Opaque { ty, next } => {
                write!(f, "{ty}\n  {}", next.to_string().lines().format("\n  "))
            }
            DecTree::End(tail) => tail.fmt(f),
        }
    }
}

impl<Tail: std::fmt::Display> std::fmt::Display for TreeTail<Tail> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TreeTail::Poison => "<poison>".fmt(f),
            TreeTail::Unreached(types) => write!(f, "unreached: {}", types.iter().format(", ")),
            TreeTail::Reached(table, excess, tail) => {
                let mut tail = tail.to_string();
                if tail.contains('\n') {
                    tail = format!("\n  {}", tail.lines().format("\n  "));
                }

                write!(
                    f,
                    "{{\n  table = [{table:?}],\n  excess = [{}]\n  tail = {tail}\n}}",
                    excess.iter().format(", "),
                )
            }
        }
    }
}
