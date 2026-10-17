use std::ops::Not;

use super::*;

// Inverts a decision tree to show only unreachable paths
struct Inverter {}

impl DecTree {
    pub fn invert_to_missing(&self) -> Option<DecTree> {
        Inverter {}.tree(self)
    }
}

impl Inverter {
    fn tree(&mut self, tree: &DecTree) -> Option<DecTree> {
        match tree {
            DecTree::Record { fields, next } => self
                .tree(next)
                .map(|next| DecTree::Record { fields: *fields, next: Box::new(next) }),
            DecTree::Tuple { elems, next } => self
                .tree(next)
                .map(|next| DecTree::Tuple { elems: *elems, next: Box::new(next) }),
            DecTree::Array { elems, next } => self
                .tree(next)
                .map(|next| DecTree::Array { elems: *elems, next: Box::new(next) }),
            DecTree::IfGuarded { false_next, .. } => self.tree(false_next),
            DecTree::Variants { branching } => {
                self.branches_then(branching, |_, branching| DecTree::Variants { branching })
            }
            DecTree::List { branching } => {
                self.branches_then(branching, |_, branching| DecTree::List { branching })
            }
            DecTree::Ints { size, branching: next } => self.branches_then(next, |_, next| {
                DecTree::Ints { size: *size, branching: next }
            }),
            DecTree::Bools { branching } => {
                self.branches_then(branching, |_, branching| DecTree::Bools { branching })
            }
            DecTree::Strings { wildcard_next, .. } => match wildcard_next {
                Some(wc_next) => self
                    .tree(wc_next)
                    .map(|next| DecTree::Wildcard { next: Box::new(next) }),
                None => Some(DecTree::Unreached),
            },
            DecTree::Wildcard { next } => self
                .tree(next)
                .map(|next| DecTree::Wildcard { next: Box::new(next) }),
            DecTree::Unreached => Some(DecTree::Unreached),
            DecTree::Tail(_) => None,
            DecTree::Error => None,
        }
    }

    fn branches_then<K: Clone>(
        &mut self,
        branching: &Branching<K>,
        wrap: impl Fn(&mut Self, Branching<K>) -> DecTree,
    ) -> Option<DecTree> {
        let inverted_branches = branching
            .branches
            .iter()
            .filter_map(|(k, params, next)| self.tree(next).map(|next| (k.clone(), *params, next)))
            .collect::<Vec<_>>();

        inverted_branches.is_empty().not().then(|| {
            wrap(
                self,
                Branching { full_size: branching.full_size, branches: inverted_branches },
            )
        })
    }

    fn next_thens(
        &mut self,
        next: &DecTree,
        wrap: impl Fn(&mut Self, DecTree) -> Vec<DecTree>,
    ) -> Vec<DecTree> {
        self.tree(next)
            .into_iter()
            .flat_map(|next| wrap(self, next))
            .collect()
    }
}
