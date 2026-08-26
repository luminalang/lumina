use std::collections::VecDeque;

use super::*;

struct Generator<TypeKey, D: DataSource<TypeKey>> {
    a: std::marker::PhantomData<TypeKey>,
    data_source: D,
}

trait DataSource<TypeKey> {
    fn max_variant(&self, ty: &TypeKey) -> key::Variant;
}

#[derive(Clone)]
enum Constructor<TypeKey> {
    Record(TypeKey),
    Variant(TypeKey, key::Variant),
    Bool(bool),
    Tuple(usize),
    Array(usize),
    IfGuarded(bool),
    Wildcard(KnownType<TypeKey>),
}

struct Missing<TypeKey> {
    branches: Vec<VecDeque<Constructor<TypeKey>>>,
}

impl<TypeKey: Clone> Missing<TypeKey> {
    fn with(mut self, constructor: Constructor<TypeKey>) -> Self {
        for branch in &mut self.branches {
            branch.push_front(constructor.clone());
        }

        self
    }

    fn and(mut self, missing: Self) -> Self {
        self.branches.extend(missing.branches);
        self
    }

    fn singleton(constructor: Constructor<TypeKey>) -> Self {
        Self { branches: vec![[constructor].into()] }
    }

    fn none() -> Self {
        Self { branches: vec![] }
    }
}

// I thinkt he smartest approach is to generate the missing patterns on the callback if we at full
// depth encounter something unfilled?
//
// I think we might've overcomplicated stuff last time. We don't need the *exact* missing patterns,
// ideally, we'd show `_` more.

impl<TypeKey: Clone, D: DataSource<TypeKey>> Generator<TypeKey, D> {
    fn run(&mut self, tree: &DecTree<TypeKey>) -> Missing<TypeKey> {
        match tree {
            DecTree::Record { record, next, .. } => {
                self.run(next).with(Constructor::Record(record.clone()))
            }
            DecTree::Tuple { elems, next } => self.run(next).with(Constructor::Tuple(*elems)),
            DecTree::Array { elems, next } => self.run(next).with(Constructor::Array(*elems)),
            DecTree::IfGuarded { guard, true_next, false_next } => self
                .run(true_next)
                .with(Constructor::IfGuarded(true))
                .and(self.run(false_next).with(Constructor::IfGuarded(false))),
            DecTree::Variants { vartype, params, next } => self.run_variants(vartype, params, next),
            DecTree::List { ty, next } => todo!(),
            DecTree::Ints { size, next } => todo!(),
            DecTree::Bools { next } => self.run_bools(next),
            DecTree::Wildcard { ty, next } => {
                self.run(next).with(Constructor::Wildcard(ty.clone()))
            }
            DecTree::Tail() => Missing::none(),
        }
    }

    fn run_variants(
        &mut self,
        ty: &TypeKey,
        params: &Map<key::Generic, KnownType<TypeKey>>,
        branching: &Branching<key::Variant, TypeKey>,
    ) -> Missing<TypeKey> {
        let max = self.data_source.max_variant(ty);

        let missing_variants = max.0 - branching.branches.len() as u32;
        let mut report = match missing_variants {
            0 => Missing::none(),
            1 => {
                let variant = (0..=max.0)
                    .map(key::Variant)
                    .find(|var| branching.branches.iter().all(|(k, _)| k != var))
                    .unwrap();

                Missing::singleton(Constructor::Variant(ty.clone(), variant))
            }
            _ => {
                return Missing::singleton(Constructor::Wildcard(KnownType::Defined(
                    ty.clone(),
                    params.clone(),
                )));
            }
        };

        for (variant, next) in branching.branches.iter() {
            let of_this_variant = self
                .run(next)
                .with(Constructor::Variant(ty.clone(), *variant));

            report = report.and(of_this_variant);
        }

        report
    }

    fn run_bools(&mut self, branching: &Branching<bool, TypeKey>) -> Missing<TypeKey> {
        // TODO: If they're missing, then there can still be more patterns afterwards no?
        //
        // How will that get handled, won't they be missing from the formatting?
        //
        // I think we did something more stateful last time with a queue we pop from?
        //
        // and then we had like a rest_are_missing thing maybe?

        let truthy = branching
            .branches
            .iter()
            .find(|(k, _)| *k)
            .map(|(_, next)| self.run(next).with(Constructor::Bool(true)))
            .unwrap_or_else(|| Missing::singleton(Constructor::Bool(true)));

        let falsely = branching
            .branches
            .iter()
            .find(|(k, _)| *k)
            .map(|(_, next)| self.run(next).with(Constructor::Bool(false)))
            .unwrap_or_else(|| Missing::singleton(Constructor::Bool(false)));

        truthy.and(falsely)
    }
}
