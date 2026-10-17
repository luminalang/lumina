use super::*;
use tracing::trace;

pub type Reachable = bool;

pub mod reachable {
    pub const YES: bool = true;
    pub const NO: bool = false;
}

impl DecTree {
    pub(crate) fn merge(&mut self, new: DecTree) -> Reachable {
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
            DecTree::Ints { size, branching: next } => match new {
                DecTree::Ints { size: new_size, branching: new_next } => {
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
            DecTree::IfGuarded { false_next, .. } => Self::expanded_wildcard(false_next, next),
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
                            dbg!(&next, &params);
                            (*var, *params, next)
                        })
                        .collect(),
                },
            },
            DecTree::List { .. } => Branching::list()
                .variant(
                    LIST_CONS,
                    DecTree::Wildcard {
                        //   ^^^^^^^^ `x`
                        next: Box::new(DecTree::Wildcard { next: Box::new(next.clone()) }),
                        //                      ^^^^^^^^ `xs`             ^^^^ after this list, if we're in a tuple for example
                    },
                )
                .variant(LIST_NIL, next)
                .into_list_dectree(),
            DecTree::Ints { size, .. } => {
                let (min, max) = (size.min_value() as i128, size.max_value() as i128);
                DecTree::Ints {
                    branching: Branching::ints_with_next(min, max, next),
                    size: *size,
                }
            }
            DecTree::Bools { .. } => Branching::bools()
                .bool(true, next.clone())
                .bool(false, next.clone())
                .into(),
            DecTree::Strings { .. } => todo!("we can't return an empty here..."),
            DecTree::Unreached => DecTree::Wildcard { next: Box::new(next) },
            DecTree::Tail(_) => panic!("cannot expand tail"),
            DecTree::Wildcard { .. } => panic!("cannot expand wildcard"),
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
        } else if let DecTree::Strings { wildcard_next, .. } = self {
            match wildcard_next {
                Some(previous_wc_next) => return previous_wc_next.merge(wc_next),
                None => {
                    *wildcard_next = Some(Box::new(wc_next));
                    return reachable::YES;
                }
            }
        }

        let this = Self::expanded_wildcard(self, wc_next);
        self.merge(this)
    }
}

impl<K: PartialEq> Branching<K> {
    fn merge_cmp(&mut self, new: Branching<K>) -> Reachable {
        new.branches
            .into_iter()
            .fold(reachable::NO, |reachable, (k, params, next)| {
                for (existing_k, existing_params, existing_next) in self.branches.iter_mut() {
                    if k == *existing_k {
                        assert_eq!(params, *existing_params);
                        return reachable | existing_next.merge(next);
                    }
                }

                self.branches.push((k, params, next));
                reachable::YES
            })
    }
}
