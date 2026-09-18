use super::*;
use tracing::trace;

pub type Reachable = bool;

pub mod reachable {
    pub const YES: bool = true;
    pub const NO: bool = false;
}

impl DecTree {
    pub(crate) fn merge(&mut self, mut new: DecTree) -> Reachable {
        trace!("merging\n{new}\n -- into --\n{self}");

        match new {
            DecTree::Wildcard { next } => {
                return self.merge_wildcard_into_self(*next);
            }
            DecTree::Unreached => return reachable::NO,
            _ => {}
        }

        match self {
            DecTree::Record { next, fields } => match new {
                DecTree::Record { next: new_next, fields: new_fields } => {
                    assert_eq!(
                        new_fields, *fields,
                        "merged records are off different sizes"
                    );
                    next.merge(*new_next)
                }
                _ => panic!("cannot merge decision trees of different types"),
            },
            DecTree::Tuple { elems, next } => match new {
                DecTree::Tuple { elems: new_elems, next: new_next } => {
                    assert_eq!(new_elems, *elems, "merged tuples are off different sizes");
                    next.merge(*new_next)
                }
                _ => panic!("cannot merge decision trees of different types"),
            },
            DecTree::Array { elems, next } => match new {
                DecTree::Array { elems: new_elems, next: new_next } => {
                    assert_eq!(new_elems, *elems, "merged arrays are off different sizes");
                    next.merge(*new_next)
                }
                _ => panic!("cannot merge decision trees of different types"),
            },
            DecTree::IfGuarded { false_next, .. } => false_next.merge(new),
            DecTree::Variants { branching } => match new {
                DecTree::Variants { branching: new_branching } => {
                    branching.merge_cmp(new_branching)
                }
                _ => panic!("cannot merge decision trees of different types"),
            },
            DecTree::List { branching } => match new {
                DecTree::List { branching: new_branching } => branching.merge_cmp(new_branching),
                _ => panic!("cannot merge decision trees of different types"),
            },
            DecTree::Ints { size, next } => match new {
                DecTree::Ints { size: new_size, next: new_next } => {
                    assert_eq!(*size, new_size);
                    next.merge(new_next)
                }
                _ => panic!("cannot merge decision trees of different types"),
            },
            DecTree::Bools { branching } => match new {
                DecTree::Bools { branching: new_branching } => branching.merge_cmp(new_branching),
                _ => panic!("cannot merge decision trees of different types"),
            },
            DecTree::Strings { branching, wildcard_next } => match new {
                DecTree::Strings { branching: new_branching, wildcard_next: new_wildcard_next } => {
                    let literals_are_reachable = branching.merge_cmp(new_branching);

                    match wildcard_next {
                        None => {
                            let reachable = new_wildcard_next.is_some();
                            *wildcard_next = new_wildcard_next;
                            reachable | literals_are_reachable
                        }
                        Some(wildcard_next) => match new_wildcard_next {
                            Some(new_wildcard_next) => wildcard_next.merge(*new_wildcard_next),
                            None => literals_are_reachable,
                        },
                    }
                }
                _ => panic!("cannot merge decision trees of different types"),
            },
            DecTree::Wildcard { next } => {
                *self = Self::expanded_wildcard(&new, (**next).clone());
                self.merge(new)
            }
            DecTree::Unreached => {
                *self = new;
                reachable::YES
            }
            DecTree::Tail(_) => reachable::NO,
            DecTree::Error => reachable::YES,
        }
    }

    // Generates a tree of same type as `new` except with any parameters set to wildcard
    // and with a specified next.
    fn expanded_wildcard(new: &Self, next: Self) -> Self {
        match new {
            DecTree::Record { fields, .. } => {
                let next = (0..*fields).fold(Box::new(next), |next, _| {
                    Box::new(DecTree::Wildcard { next })
                });
                DecTree::Record { fields: *fields, next }
            }
            DecTree::Tuple { elems, .. } => {
                let next = (0..*elems).fold(Box::new(next), |next, _| {
                    Box::new(DecTree::Wildcard { next })
                });
                DecTree::Tuple { elems: *elems, next }
            }
            DecTree::Array { elems, .. } => {
                let next = (0..*elems).fold(Box::new(next), |next, _| {
                    Box::new(DecTree::Wildcard { next })
                });
                DecTree::Array { elems: *elems, next }
            }
            DecTree::IfGuarded { false_next, .. } => Self::expanded_wildcard(&false_next, next),
            DecTree::Variants { branching } => DecTree::Variants {
                branching: Branching {
                    full_size: branching.full_size,
                    branches: branching
                        .branches
                        .iter()
                        .map(|(var, params, _)| {
                            let next = (0..*params).fold(next.clone(), |next, _| {
                                DecTree::Wildcard { next: Box::new(next) }
                            });
                            (*var, *params, next)
                        })
                        .collect(),
                },
            },
            DecTree::List { .. } => Branching::list()
                .variant(LIST_CONS, next.clone())
                .variant(LIST_NIL, next)
                .into_list_dectree(),
            DecTree::Ints { size, .. } => {
                let (min, max) = (size.max_value() as i128, size.max_value() as i128);
                DecTree::Ints { next: Branching::ints_with_next(min, max, next), size: *size }
            }
            DecTree::Bools { .. } => Branching::bools()
                .bool(true, next.clone())
                .bool(false, next.clone())
                .into(),
            DecTree::Strings { .. } => todo!("we can't return an empty here..."),
            // DecTree::Unreached(_) => panic!("cannot expand Unreached"),
            DecTree::Unreached => DecTree::Wildcard { next: Box::new(next) },
            DecTree::Tail(_) => panic!("cannot expand tail"),
            DecTree::Wildcard { .. } => panic!("cannot expand wildcard"),
            // DecTree::Wildcard { .. } => DecTree::Wildcard { next: Box::new(next) },
            DecTree::Error => DecTree::Error,
        }
    }

    fn merge_wildcard_into_self(&mut self, wc_next: DecTree) -> Reachable {
        trace!("merging wildcard into\n{self}\n -- with next --\n{wc_next}");

        if let DecTree::Unreached = self {
            *self = DecTree::Wildcard { next: Box::new(wc_next) };
            return reachable::YES;
        } else if let DecTree::Wildcard { next } = self {
            return next.merge(wc_next);
        }

        let this = Self::expanded_wildcard(self, wc_next);
        self.merge(this)

        // match self {
        //     DecTree::Record { fields: elems, next }
        //     | DecTree::Tuple { elems, next }
        //     | DecTree::Array { elems, next } => {
        //         // Add additional wildcards for each element
        //         let elems_next = (0..*elems).fold(wc_next.clone(), |next, _| DecTree::Wildcard {
        //             next: Box::new(next),
        //         });
        //         next.merge_wildcard_into_self(elems_next)
        //     }
        //     DecTree::IfGuarded { false_next, .. } => {
        //         todo!("don't we want to *move* this if-guard to the end or something?");
        //         // Can't we represent ifguards statically?
        //         //
        //         // We put them alongside the DecTree instead of wrapping like this?
        //         //
        //         // No. Because; the `DecTree` shouldn't be affected by merges the same way?
        //         //
        //         // match a
        //         // | true if true -> ...
        //         // | _ -> ... // only wants to specialse into `false`?
        //         // but; I think we would still want to merge it into `{true, false}`
        //         //
        //         // So; I think a complete merge still makes sense...
        //         //
        //         // It's valid *because* the if guard *must* be the last one. But; if it must be the last one then that should
        //         // be able to be done statically?
        //         //
        //         // Perhaps we should modify `Tail` to having an optional if guard instead?

        //         // false_next.merge_wildcard_into_self(and_then)
        //     }

        //     DecTree::Variants { branching } => {
        //         Self::merge_wildcard_into_branches(branching, wc_next)
        //     }
        //     DecTree::List { branching } => Self::merge_wildcard_into_branches(branching, wc_next),
        //     DecTree::Bools { branching } => Self::merge_wildcard_into_branches(branching, wc_next),

        //     DecTree::Ints { size, .. } => {
        //         let int_branching = Branching::ints_with_next(
        //             size.min_value() as i128,
        //             size.max_value() as i128,
        //             wc_next,
        //         );

        //         let created = DecTree::Ints { size: *size, next: int_branching };

        //         self.merge(created)
        //     }
        //     DecTree::Wildcard { next } => next.merge(wc_next),
        //     DecTree::Unreached(_) => {
        //         *self = DecTree::Wildcard { next: Box::new(wc_next) };
        //         reachable::YES
        //     }
        //     DecTree::Tail(_) => reachable::NO,
        //     DecTree::Error => reachable::YES,
        // }
    }

    // NOTE: Assumes all branches exists, which is meant to hold true from construction.
    fn merge_wildcard_into_branches<K>(branching: &mut Branching<K>, wc_next: DecTree) -> bool {
        branching
            .branches
            .iter_mut()
            .fold(false, |reachable, (_, params, var_next)| {
                // Add additional wildcards for each parameter
                let variant_next = (0..*params).fold(wc_next.clone(), |next, _| {
                    DecTree::Wildcard { next: Box::new(next) }
                });
                reachable | var_next.merge_wildcard_into_self(variant_next)
            })
    }

    // fn expand_from_type(ty: &KnownType<TypeKey>, next: DecTree<TypeKey>) -> Self {
    //     match ty {
    //         KnownType::Defined(_, primary_map) => todo!(),
    //         KnownType::List(known_type) => todo!(),
    //         KnownType::Array { .. } => todo!(),
    //         KnownType::Const(const_type) => match const_type {
    //             ts::ConstType::Int(_) => DecTree::Ints {
    //                 size: todo!("`ConstType` must specify intsize?"),
    //                 next: Branching::new(None),
    //             },
    //         },
    //         KnownType::Tuple(elems) => DecTree::Tuple { elems: elems.len(), next: Box::new(next) },
    //         KnownType::Prim(prim) => match *prim {
    //             ts::Prim::Int(size) => DecTree::Ints { size, next: Branching::new(None) },
    //             ts::Prim::Bool => DecTree::Bools { branching: Branching::new(Some(2)) },
    //             ts::Prim::Float => todo!(),
    //             ts::Prim::Self_ => panic!("unresolved `Self_` primitive in pattern lowering"),
    //         },
    //         // We assume an error has already been created when the user creates the merging pattern
    //         //
    //         // PROBLEM:
    //         //
    //         // This won't work as if the RHS is a wildcard then the merge is still valid.
    //         //
    //         // I think we should just destruct RHS instead
    //         KnownType::Error
    //         | KnownType::Generic(_)
    //         | KnownType::Function { .. }
    //         | KnownType::Pointer(_) => DecTree::Error,
    //     }
    // }
}

impl<K: PartialEq> Branching<K> {
    fn merge_cmp(&mut self, new: Branching<K>) -> Reachable {
        new.branches
            .into_iter()
            .fold(reachable::NO, |reachable, (k, params, next)| {
                for (existing_k, existing_params, existing_next) in self.branches.iter_mut() {
                    if k == *existing_k {
                        assert_eq!(params, *existing_params);
                        return reachable || existing_next.merge(next);
                    }
                }

                self.branches.push((k, params, next));
                reachable::YES
            })
    }
}
