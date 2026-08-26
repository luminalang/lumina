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

use key::{Map, entity_impl};
use lumina_key as key;
use lumina_typesystem::{IntSize, KnownType};

mod range;
use range::Range;

mod missing;

pub type IsReachable = bool;

#[derive(Clone, Copy, PartialEq, Eq)]
struct TailExpr(u32);
entity_impl!(TailExpr, "branch-expr");

#[derive(Clone, Copy, PartialEq, Eq)]
struct IfGuard(u32);
entity_impl!(IfGuard, "if-guard");

struct Match<Expr, TypeKey> {
    root: DecTree<TypeKey>,
    tails: Map<TailExpr, Expr>,
    guards: Map<IfGuard, Expr>,
    pat_type: KnownType<TypeKey>,
}

impl<Expr, TypeKey: Clone> Match<Expr, TypeKey> {
    pub fn new(pat_type: KnownType<TypeKey>) -> Match<Expr, TypeKey> {
        Match {
            root: todo!(),
            // root: DecTree::Unreached(pat_type.clone()),
            pat_type,
            tails: Map::new(),
            guards: Map::new(),
        }
    }

    pub fn add_tail(&mut self, expr: Expr) -> TailExpr {
        self.tails.push(expr)
    }

    // pub fn branch(&mut self, pat: &, tail: TailExpr)
}

// TODO: Oh right, since we made this its own crate it doesn't have access to `hir::Pattern`.
//
// But; maybe that's a good thing?
//
// eh, we'll figure that out later.

/// A decision tree where the depth of the data structure also represents each point of the pattern
/// that can be bound to a value.
enum DecTree<TypeKey> {
    // Non-branching chains
    Record {
        record: TypeKey,
        params: Map<key::Generic, KnownType<TypeKey>>,
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
        vartype: TypeKey,
        params: Map<key::Generic, KnownType<TypeKey>>,
        next: Branching<key::Variant, TypeKey>,
    },
    List {
        ty: TypeKey,
        next: Branching<key::Variant, TypeKey>,
    },
    // This one is trickier, let's leave it for now
    // String {},
    Ints {
        size: IntSize,
        next: Branching<Range, TypeKey>,
    },
    Bools {
        next: Branching<bool, TypeKey>,
    },

    // We can't eagerly expand all wildcards by type since recursive data types exist.
    //
    // But whenever possible, we avoid using `Wildcard` in favor of expanding the real form of the pattern.
    Wildcard {
        ty: KnownType<TypeKey>,
        next: Box<Self>,
    },

    // Special
    // Unreached(KnownType<TypeKey>),
    Tail(),
}

struct Branching<K, TypeKey> {
    branches: Vec<(K, DecTree<TypeKey>)>,
}
