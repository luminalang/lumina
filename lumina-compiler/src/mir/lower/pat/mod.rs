use super::Callable;
use crate::prelude::*;
use lumina_typesystem::{IntSize, Type};
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
        next: Branching<key::Variant, Tail>,
    },
    List {
        next: Branching<key::Variant, Tail>,
        ty: Type,
    },
    String {
        next: Branching<StrChecks, Tail>,
        wildcard_next: Box<Self>,
    },
    Ints {
        intsize: IntSize,
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
pub struct StrChecks {
    pub checks: Vec<StrCheck>,
}

#[derive(Clone, Debug)]
pub enum StrCheck {
    Literal(M<key::ReadOnly>),
    TakeExcess,
    TakeByte,
    // fn(u8 -> bool) -> Maybe string
    TakeWhile(Callable, Vec<mir::Expr>),
    // string -> (a, string)
    TakeBySplit(Callable, Type, Vec<mir::Expr>),
}

#[derive(Clone, Debug)]
pub enum TreeTail<Tail> {
    Poison,
    Unreached(VecDeque<Type>),
    Reached(PointTable, VecDeque<Type>, Tail),
}

pub const LIST_CONS: key::Variant = key::Variant(0);
pub const LIST_NIL: key::Variant = key::Variant(1);

/// Maps binds (created by wildcards) to depth points of the decision tree
#[derive(Debug, Clone, new, PartialEq)]
pub struct PointTable {
    pub binds: Vec<(key::Bind, usize)>,
}

pub trait BranchKey: std::fmt::Display {}

impl BranchKey for key::Variant {}
impl BranchKey for bool {}
impl BranchKey for StrChecks {}
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
            DecTree::String { next, wildcard_next } => {
                next.for_each_tail_mut(f);
                wildcard_next.for_each_tail_mut(f);
            }
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
            DecTree::String { next, wildcard_next } => {
                next.for_each_tail(f);
                wildcard_next.for_each_tail(f);
            }
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
            DecTree::Ints { intsize, next } => next.fmt(intsize, f),
            DecTree::Bools(next) => next.fmt("bool", f),
            DecTree::String { next, wildcard_next } => {
                next.fmt("string", f)?;
                write!(
                    f,
                    "string _\n  {}",
                    wildcard_next.to_string().lines().format("\n  ")
                )
            }
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

impl std::fmt::Display for StrChecks {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.checks
            .iter()
            .map(|check| match check {
                StrCheck::Literal(name) => name.to_string(),
                StrCheck::TakeExcess | StrCheck::TakeByte => format!("_"),
                StrCheck::TakeWhile(call, params) => {
                    format!("#({call} {})", params.iter().format(" "))
                }
                StrCheck::TakeBySplit(call, _, params) => {
                    format!("#({call} {})", params.iter().format(" "))
                }
            })
            .format(" ")
            .fmt(f)
    }
}
