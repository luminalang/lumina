use super::{range, Branching, DecTree, TreeTail, LIST_CONS, LIST_NIL};
use crate::prelude::*;
use lumina_typesystem::{
    ConstValue, Container, GenericKind, GenericMapper, IntSize, Lang, Static, Transformer, Ty, Type,
};
use std::collections::VecDeque;

#[derive(new)]
pub struct Init<'a> {
    pub(super) ftypes: &'a MMap<key::Record, Map<key::Field, Tr<Type>>>,
    pub(super) vtypes: &'a MMap<key::Sum, Map<key::Variant, Vec<Tr<Type>>>>,
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
            Ty::Int(intsize) => {
                let full = range::Constraints::from(intsize).to_range();
                DecTree::Ints { intsize, next: Branching::singleton(full, next) }
            }
            ty => DecTree::Wildcard { ty, next: Box::new(next) },
        }
    }

    /// Expands a new decision tree with depth of 1 from a type
    ///
    /// After the first point, it sets to Lazy
    pub(super) fn unreached_from_type<'s, Tail>(&self, ty: &Type) -> DecTree<Tail> {
        match ty {
            &Ty::Int(intsize) => {
                let next = DecTree::unreached();
                let full = range::Constraints::from(intsize).to_range();
                DecTree::Ints { intsize, next: Branching::singleton(full, next) }
            }
            Ty::Const(ConstValue::Usize(_)) => {
                let next = DecTree::unreached();
                let intsize = IntSize::new(false, 64); // TODO: 32-bit targets
                let full = range::Constraints::from(intsize).to_range();
                DecTree::Ints { intsize, next: Branching::singleton(full, next) }
            }
            Ty::Const(ConstValue::Bool(_)) | Ty::Simple("bool") => {
                let next = || DecTree::unreached();
                let branches = vec![(false, next()), (true, next())];
                DecTree::Bools(Branching { branches })
            }
            Ty::Simple("f64" | "f32") => {
                todo!("float patterns (if we even want to allow it?)")
            }
            Ty::Simple("poison") => DecTree::End(TreeTail::Poison),
            Ty::Const(_) => unimplemented!("matching over const-value"),
            Ty::Container(con, elems) => self.unreached_from_tycontainer(con, elems),
            Ty::Simple("self") | Type::Generic(_) => self.unreached_opaque(ty.clone()),
            Ty::Simple(_) | Ty::Special(Static) => unreachable!(),
        }
    }

    fn unreached_opaque<Tail>(&self, ty: Type) -> DecTree<Tail> {
        DecTree::Opaque { next: Box::new(DecTree::unreached()), ty }
    }

    pub fn unreached_from_tycontainer<T>(&self, con: &Container, elems: &[Type]) -> DecTree<T> {
        match con {
            Container::Tuple => {
                let next = DecTree::End(TreeTail::Unreached(elems.to_vec().into()));
                DecTree::Tuple { elems: elems.len(), next: Box::new(next) }
            }
            Container::FnPointer | Container::Closure | Container::Pointer => {
                let ty = Type::Container(con.clone(), elems.to_vec());
                self.unreached_opaque(ty)
            }
            Container::Array => {
                let next = DecTree::End(TreeTail::Unreached([elems[0].clone()].into()));
                match &elems[1] {
                    Ty::Generic(_) => unimplemented!("matching over generic const-array"),
                    &Ty::Const(ConstValue::Usize(elems)) => {
                        DecTree::Array { elems, next: Box::new(next) }
                    }
                    _ => DecTree::End(TreeTail::Poison),
                }
            }
            &Container::Defined(M(module, kind), lang) => match *lang {
                Lang::String => DecTree::String {
                    next: Branching { branches: vec![] },
                    wildcard_next: Box::new(DecTree::End(TreeTail::Unreached(
                        elems.to_vec().into(),
                    ))),
                },
                Lang::List => {
                    let ty = Type::Container(con.clone(), elems.to_vec());
                    let inner = elems.last().expect("list type without type parameters");
                    let branches = vec![
                        (LIST_CONS, DecTree::lazy([inner.clone(), ty.clone()])),
                        (LIST_NIL, DecTree::unreached()),
                    ];
                    DecTree::List { next: Branching { branches }, ty: ty.clone() }
                }
                Lang::None => match kind {
                    key::TypeKind::Record(record) => {
                        self.unreached_record(record.inside(module), elems)
                    }
                    key::TypeKind::Sum(sum) => self.unreached_sum(sum.inside(module), elems),
                    key::TypeKind::Trait(_) => {
                        let ty = Type::Container(con.clone(), elems.to_vec());
                        self.unreached_opaque(ty)
                    }
                },
            },
        }
    }

    fn unreached_record<T>(&self, record: M<key::Record>, params: &[Type]) -> DecTree<T> {
        let finst = GenericMapper::from_types(GenericKind::Entity, params.iter().cloned());

        let fields = self.ftypes[record]
            .values()
            .map(|ty| (&finst).transform(&ty))
            .collect();

        let next = DecTree::End(TreeTail::Unreached(fields));

        let fields = self.ftypes[record].len();

        let params = params.to_vec();
        DecTree::Record { record, params, fields, next: Box::new(next) }
    }

    fn unreached_sum<T>(&self, sum: M<key::Sum>, params: &[Type]) -> DecTree<T> {
        let finst = GenericMapper::from_types(GenericKind::Entity, params.iter().cloned());

        let branches = self.vtypes[sum]
            .iter()
            .map(|(var, params)| {
                let elems = params.iter().map(|ty| (&finst).transform(ty)).collect();
                let next = DecTree::End(TreeTail::Unreached(elems));
                (var, next)
            })
            .collect();

        let params = params.to_vec();
        DecTree::Sum { sum, params, next: Branching { branches } }
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
