use super::{DecTree, Init, Range, TreeTail, LIST_CONS, LIST_NIL};
use lumina_key as key;
use lumina_key::M;
use std::collections::VecDeque;
use std::fmt;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum MissingPattern {
    Wildcard,
    Const(&'static str),
    Tuple(VecDeque<Self>),
    Array(VecDeque<Self>),
    Sum(M<key::Sum>, key::Variant, VecDeque<Self>),
    Record(M<key::Record>, VecDeque<Self>),

    Ints(Range),

    List {
        elems: VecDeque<Self>,
        xs: Option<Box<Self>>,
    },
}

fn construct<C>(con: C, elems: usize, mut buf: VecDeque<MissingPattern>) -> VecDeque<MissingPattern>
where
    C: Fn(VecDeque<MissingPattern>) -> MissingPattern,
{
    let mut xs = buf.split_off(elems);
    xs.push_front(con(buf));
    xs
}

fn unfold_list(
    var: key::Variant,
    mut params: VecDeque<MissingPattern>,
) -> VecDeque<MissingPattern> {
    match var {
        LIST_CONS => {
            let x = params.pop_front().unwrap();
            let xs = params.pop_front().unwrap();

            match xs {
                // [x]
                MissingPattern::List { elems, xs } if elems.is_empty() && xs.is_none() => {
                    params.push_front(MissingPattern::List { elems, xs: None })
                }
                // [x, y : xs]
                MissingPattern::List { mut elems, xs } => {
                    elems.push_front(x);
                    params.push_front(MissingPattern::List { elems, xs })
                }
                // [x : xs]
                MissingPattern::Wildcard => params.push_front(MissingPattern::List {
                    elems: [x].into(),
                    xs: Some(Box::new(MissingPattern::Wildcard)),
                }),
                _ => unreachable!("`Cons x xs` list layout violated"),
            }
        }
        LIST_NIL => {
            params.push_front(MissingPattern::List { elems: VecDeque::new(), xs: None });
        }
        _ => unreachable!(),
    }

    params
}

pub struct MissingGeneration<'a> {
    init: Init<'a>,
}

impl<'a> MissingGeneration<'a> {
    pub fn new(init: Init<'a>) -> Self {
        Self { init }
    }

    pub fn run<T: fmt::Debug>(&mut self, tree: &DecTree<T>) -> Vec<MissingPattern> {
        self.tree(tree)
            .into_iter()
            .map(|mut params| {
                let x = params.pop_front().unwrap();
                assert!(params.is_empty(), "leftover parameters: {params:?}");
                x
            })
            .collect()
    }

    fn tree<T: fmt::Debug>(&mut self, tree: &DecTree<T>) -> Vec<VecDeque<MissingPattern>> {
        match tree {
            DecTree::Wildcard { next, .. } => self
                .tree(next)
                .into_iter()
                .map(|mut params| {
                    params.push_front(MissingPattern::Wildcard);
                    params
                })
                .collect(),

            DecTree::Record { record, next, .. } => {
                let elems = self.init.ftypes[*record].len();
                self.tree(next)
                    .into_iter()
                    .map(|params| construct(|p| MissingPattern::Record(*record, p), elems, params))
                    .collect()
            }

            DecTree::Tuple { elems, next } => self
                .tree(next)
                .into_iter()
                .map(|params| construct(MissingPattern::Tuple, *elems, params))
                .collect(),

            DecTree::Array { elems, next } => self
                .tree(next)
                .into_iter()
                .map(|params| construct(MissingPattern::Array, *elems as usize, params))
                .collect(),

            // TODO: edge-case high counts
            DecTree::Sum { sum, next, .. } => next
                .branches
                .iter()
                .flat_map(|(var, next)| {
                    let elems = self.init.vtypes[*sum][*var].len();
                    self.tree(next)
                        .into_iter()
                        .map(|params| {
                            construct(|p| MissingPattern::Sum(*sum, *var, p), elems, params)
                        })
                        .collect::<Vec<_>>()
                })
                .collect(),
            DecTree::List { next, .. } => next
                .branches
                .iter()
                .flat_map(|(var, next)| {
                    self.tree(next)
                        .into_iter()
                        .map(|p| unfold_list(*var, p))
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>(),
            DecTree::Ints { next, .. } => {
                let mut next_are_eq = true;

                let missing = next
                    .branches
                    .iter()
                    .flat_map(|(range, next)| {
                        let mut missing = self.tree(next);
                        match missing.len() {
                            0 => vec![],
                            1 => {
                                missing[0].push_front(MissingPattern::Ints(range.clone()));
                                missing
                            }
                            _ if missing.iter().skip(1).all(|params| *params == missing[0]) => {
                                let mut params = missing.pop().unwrap();
                                params.push_front(MissingPattern::Ints(Range::full(range)));
                                vec![params]
                            }
                            _ => {
                                next_are_eq = false;
                                missing.iter_mut().for_each(|params| {
                                    params.push_front(MissingPattern::Ints(Range::full(range)))
                                });
                                missing
                            }
                        }
                    })
                    .collect::<Vec<_>>();

                match missing.len() {
                    0 => vec![],
                    1 => missing,
                    // TODO: I think this is a behavior we want to generalise across everything
                    _ if next_are_eq => {
                        vec![vec![MissingPattern::Wildcard; missing[0].len()].into()]
                    }
                    _ => missing,
                }
            }
            DecTree::String { wildcard_next, .. } => {
                let mut missing = self.tree(wildcard_next);
                missing
                    .iter_mut()
                    .for_each(|params| params.push_front(MissingPattern::Wildcard));
                missing
            }
            DecTree::Bools(next) => next
                .branches
                .iter()
                .flat_map(|(b, next)| {
                    let mut missing = self.tree(next);
                    for params in &mut missing {
                        let name = if *b { "true" } else { "false" };
                        params.push_front(MissingPattern::Const(name))
                    }
                    missing
                })
                .collect(),
            DecTree::Opaque { next, .. } => {
                let mut missing = self.tree(next);
                missing
                    .iter_mut()
                    .for_each(|params| params.push_front(MissingPattern::Wildcard));
                missing
            }
            DecTree::End(tail) => self.tail(tail),
        }
    }

    fn tail<T: fmt::Debug>(&mut self, tail: &TreeTail<T>) -> Vec<VecDeque<MissingPattern>> {
        match tail {
            TreeTail::Unreached(excess) => {
                vec![excess.iter().map(|_| MissingPattern::Wildcard).collect()]
            }
            _ => vec![],
        }
    }
}

impl MissingPattern {
    pub fn fmt<'a>(
        &'a self,
        name_of_var: &'a dyn Fn(M<key::Sum>, key::Variant) -> String,
        name_of_field: &'a dyn Fn(M<key::Record>, key::Field) -> String,
    ) -> MissingFormatter<'_> {
        MissingFormatter { pat: self, parenthesis: false, name_of_var, name_of_field }
    }
}

pub struct MissingFormatter<'a> {
    pat: &'a MissingPattern,
    parenthesis: bool,
    name_of_var: &'a dyn Fn(M<key::Sum>, key::Variant) -> String,
    name_of_field: &'a dyn Fn(M<key::Record>, key::Field) -> String,
}

impl<'a> MissingFormatter<'a> {
    fn params(&self, sep: &str, p: bool, params: &VecDeque<MissingPattern>) -> String {
        use itertools::Itertools;

        params
            .iter()
            .map(|pat| self.fork(pat, p))
            .format(sep)
            .to_string()
    }

    fn fork<'s, 'p: 's>(
        &'a self,
        pat: &'a MissingPattern,
        parenthesis: bool,
    ) -> MissingFormatter<'_> {
        MissingFormatter {
            pat,
            parenthesis,
            name_of_var: self.name_of_var,
            name_of_field: self.name_of_field,
        }
    }
}

impl<'a> fmt::Display for MissingFormatter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use itertools::Itertools;

        match self.pat {
            MissingPattern::Wildcard => "_".fmt(f),
            MissingPattern::Const(b) => b.fmt(f),
            MissingPattern::Tuple(elems) if elems.is_empty() => "_".fmt(f),
            MissingPattern::Tuple(elems) => write!(f, "({})", self.params(", ", false, elems)),
            MissingPattern::Array(elems) => write!(f, "[{}]", self.params(", ", false, elems)),
            MissingPattern::Sum(sum, var, params) => {
                let name = (self.name_of_var)(*sum, *var);

                if params.is_empty() {
                    name.fmt(f)
                } else if self.parenthesis {
                    write!(f, "({name} {})", self.params(" ", true, params))
                } else {
                    write!(f, "{name} {}", self.params(" ", true, params))
                }
            }
            MissingPattern::Record(record, params) => {
                write!(f, "{{")?;

                params
                    .iter()
                    .enumerate()
                    .format_with(", ", |(i, pat), f| {
                        let field = key::Field(i as u32);
                        let name = (self.name_of_field)(*record, field);
                        f(&format_args!("{name} = {}", self.fork(pat, false)))
                    })
                    .fmt(f)?;

                write!(f, "}}")
            }
            MissingPattern::Ints(range) if range.is_full() => "_".fmt(f),
            MissingPattern::Ints(range) => range.fmt(f),
            MissingPattern::List { elems, xs } => {
                write!(f, "[")?;

                elems
                    .iter()
                    .format_with(", ", |pat, f| f(&format_args!("{}", self.fork(pat, false))))
                    .fmt(f)?;

                if let Some(xs) = xs {
                    write!(f, " : {}", self.fork(xs, false))?;
                }

                write!(f, "]")
            }
        }
    }
}
