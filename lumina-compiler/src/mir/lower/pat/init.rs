use super::{range, Branching, DecTree, TreeTail, LIST_CONS, LIST_NIL};
use crate::prelude::*;
use lumina_typesystem::{Container, ForeignInst, Prim, Type};
use std::collections::VecDeque;

#[derive(new)]
pub struct Init<'a> {
    pub(super) ftypes: &'a ModMap<key::Record, Map<key::RecordField, Tr<Type>>>,
    pub(super) vtypes: &'a ModMap<key::Sum, Map<key::SumVariant, Vec<Tr<Type>>>>,
}

impl<'a> Init<'a> {
    /// Expands a full decision tree with depth of 1 from a type
    ///
    /// After the first point, it sets to `next`
    pub(super) fn reached_from_type<'s, Tail>(
        &self,
        to_expand: Type,
        next: DecTree<Tail>,
    ) -> DecTree<Tail> {
        match to_expand {
            Type::Prim(Prim::Int(signed, bitsize)) => {
                let full = range::constraints_from_bitsize(signed, bitsize).to_range();
                DecTree::Ints { bitsize, signed, next: Branching::singleton(full, next) }
            }
            ty => DecTree::Wildcard { ty, next: Box::new(next) },
        }
    }

    /// Expands a new decision tree with depth of 1 from a type
    ///
    /// After the first point, it sets to Lazy
    pub(super) fn unreached_from_type<'s, Tail>(&self, ty: &Type) -> DecTree<Tail> {
        let opaque = || {
            let ty = ty.clone();
            DecTree::Opaque { next: Box::new(DecTree::unreached()), ty }
        };

        match ty {
            Type::Prim(prim) => match prim {
                &Prim::Int(signed, bitsize) => {
                    let next = DecTree::unreached();
                    let full = range::constraints_from_bitsize(signed, bitsize).to_range();
                    DecTree::Ints { bitsize, signed, next: Branching::singleton(full, next) }
                }
                Prim::Bool => {
                    let next = || DecTree::unreached();
                    let branches = vec![(false, next()), (true, next())];
                    DecTree::Bools(Branching { branches })
                }
                Prim::Float => todo!("float patterns (if we even want to allow it?)"),
                Prim::Poison => DecTree::End(TreeTail::Poison),
                Prim::Never => todo!(),
            },
            Type::Container(cont) => match cont {
                Container::Tuple(elems) => {
                    let next = DecTree::End(TreeTail::Unreached(elems.to_vec().into()));
                    DecTree::Tuple { elems: elems.len(), next: Box::new(next) }
                }
                Container::Func(_, _, _) | Container::Pointer(_) => opaque(),
            },
            Type::Self_ | Type::Generic(_) => opaque(),
            Type::List(_, params) => {
                let inner = params.last().expect("list type without type parameters");
                let branches = vec![
                    (LIST_CONS, DecTree::lazy([inner.clone(), ty.clone()])),
                    (LIST_NIL, DecTree::unreached()),
                ];
                DecTree::List { next: Branching { branches }, ty: ty.clone() }
            }
            Type::Defined(kind, params) => match kind.value {
                key::TypeKind::Record(record) => {
                    let record = kind.module.m(record);
                    let params = params.to_vec();
                    let finst = ForeignInst::from_type_params(&params);

                    let fields = self.ftypes[record]
                        .values()
                        .map(|ty| finst.apply(&ty))
                        .collect();

                    let next = DecTree::End(TreeTail::Unreached(fields));

                    let fields = self.ftypes[record].len();

                    DecTree::Record { record, params, fields, next: Box::new(next) }
                }
                key::TypeKind::Sum(sum) => {
                    let sum = kind.module.m(sum);
                    let finst = ForeignInst::from_type_params(params);

                    let branches = self.vtypes[sum]
                        .iter()
                        .map(|(var, params)| {
                            let params = params.iter().map(|ty| finst.apply(ty)).collect();
                            let next = DecTree::End(TreeTail::Unreached(params));
                            (var, next)
                        })
                        .collect();

                    DecTree::Sum { sum, params: params.clone(), next: Branching { branches } }
                }
                key::TypeKind::Trait(_) => opaque(),
            },
        }
    }

    pub fn expand_first_then_extend_excess<Tail>(&self, mut tys: VecDeque<Type>) -> DecTree<Tail> {
        match tys.pop_front() {
            None => DecTree::unreached(),
            Some(fst) => {
                let mut tree = self.unreached_from_type(&fst);
                self.extend_tail_with_excess(&mut tree, tys);
                tree
            }
        }
    }

    pub fn extend_tail_with_excess<Tail>(&self, tree: &mut DecTree<Tail>, tys: VecDeque<Type>) {
        tree.for_each_tail_mut(&mut |tail| match tail {
            TreeTail::Unreached(mut params) => {
                params.extend(tys.clone());
                DecTree::End(TreeTail::Unreached(params))
            }
            tail => DecTree::End(tail),
        });
    }
}
