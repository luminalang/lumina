use super::{CallTypes, ConcreteInst, Expr, Lower};
use crate::prelude::*;
use derive_new::new;
use ibig::IBig;
use lumina_key::{entity_impl, keys};
use lumina_parser::pat::Bound;
use lumina_typesystem::{Bitsize, Prim, Type};
use lumina_util::Highlighting;
use std::fmt;

mod ints;
pub use ints::Range;

mod missing;
pub use missing::{FmtPattern, Formatter, MissingGeneration};

#[cfg(test)]
mod tests;

#[derive(Clone, Debug)]
pub enum DecTreeBranch {
    Ints(Bitsize, IBig, IBig, BranchOf<Range>),
    Tuple(Params, Box<DecTree>),
    Bools(BranchOf<bool>),
    Sum(
        ConcreteInst,
        Map<key::SumVariant, CallTypes>,
        M<key::Sum>,
        BranchOf<key::SumVariant>,
    ),
    List(M<key::TypeKind>, Type, BranchOf<ListConstr>),
    Record(M<key::Record>, Vec<Type>, Params, Box<DecTree>),
    Reached(PointToBindTranslationTable, Box<Expr>),
    Wildcard(Box<DecTree>),

    Poison,
}
pub type Params = usize;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ListConstr {
    Cons,
    Nil,
}

#[derive(Clone, Debug)]
pub struct DecTree {
    // TODO: we can probably just use the depth as a patpoint counter
    // instead of explicitly attaching it to each branch.
    pub point: PatPoint,
    pub branch: DecTreeBranch,
}

#[derive(Clone, Debug)]
pub struct BranchOf<T> {
    pub branches: Vec<(T, Params, DecTree)>,
}

impl DecTree {
    fn offset_binds(&mut self, by: u32) {
        self.point.0 += by;
        self.branch.offset_binds(by);
    }
}

impl DecTreeBranch {
    fn offset_binds(&mut self, by: u32) {
        match self {
            DecTreeBranch::Bools(branches) => branches.offset_binds(by),
            DecTreeBranch::Ints(_, _, _, branches) => branches.offset_binds(by),
            DecTreeBranch::Tuple(_, next) => next.offset_binds(by),
            DecTreeBranch::Sum(_, _, _, branches) => branches.offset_binds(by),
            DecTreeBranch::Record(_, _, _, next) => next.offset_binds(by),
            DecTreeBranch::Reached(table, _) => table.offset_binds(by),
            DecTreeBranch::Wildcard(next) => next.offset_binds(by),
            DecTreeBranch::List(_, _, branches) => branches.offset_binds(by),
            DecTreeBranch::Poison => {}
        }
    }
}

impl<T> BranchOf<T> {
    fn one(value: T, params: Params, next: DecTree) -> BranchOf<T> {
        BranchOf { branches: vec![(value, params, next)] }
    }

    fn offset_binds(&mut self, by: u32) {
        self.branches
            .iter_mut()
            .for_each(|(_, _, next)| next.offset_binds(by));
    }
}

#[derive(Clone, Default, Debug)]
pub struct PointToBindTranslationTable {
    pub map: Vec<(key::Bind, PatPoint)>,
}

impl PointToBindTranslationTable {
    fn offset_binds(&mut self, by: u32) {
        self.map.iter_mut().for_each(|(_, p)| p.0 += by)
    }
}

keys!(PatPoint . "point");

impl Default for PatPoint {
    fn default() -> Self {
        PatPoint(0)
    }
}

impl<T, I: IntoIterator<Item = (T, Params, DecTree)>> From<I> for BranchOf<T> {
    fn from(value: I) -> Self {
        BranchOf { branches: value.into_iter().collect() }
    }
}

type IsReachable = bool;
const REACHABLE: IsReachable = true;
const UNREACHABLE: IsReachable = false;

impl DecTreeBranch {}

#[derive(new)]
pub struct TreeBuilder<'p, 'l, 'a, 's> {
    #[new(default)]
    point: PatPoint,

    #[new(default)]
    table: PointToBindTranslationTable,

    #[new(default)]
    queue: Vec<(&'p hir::Pattern<'s>, Option<key::Bind>)>,

    l: &'l mut Lower<'a, 's>,

    params: &'p [Tr<hir::Pattern<'s>>],
    expr: Tr<&'p hir::Expr<'s>>,
    #[new(default)]
    current_param: usize,
    ptypes: &'p [Type],
}

fn extend_untr<'a, T>(queue: &mut Vec<(&'a T, Option<key::Bind>)>, extra: &'a [Tr<T>]) {
    extra.iter().rev().for_each(|v| queue.push((v, None)));
}

impl<'p, 'l, 'a, 's> TreeBuilder<'p, 'l, 'a, 's> {
    pub fn parameters_to_tree(
        v: &'l mut Lower<'a, 's>,
        params: &'p [Tr<hir::Pattern<'s>>],
        expr: Tr<&'p hir::Expr<'s>>,
        ptypes: &'p [Type],
    ) -> Expr {
        let mut this = Self::new(v, params, expr, ptypes);
        this.lower_tail()
    }

    pub fn new_tree(&mut self, pat: &'p hir::Pattern<'s>) -> DecTree {
        let point = self.point;
        self.point.0 += 1;

        let branch = match pat {
            hir::Pattern::Any => DecTreeBranch::Wildcard(Box::new(self.from_queue())),
            hir::Pattern::Int([start, end], var) => {
                match self.l.finalizer(|mut fin| (fin.var(*var), fin.errors)) {
                    Type::Prim(Prim::Int(signed, bitsize)) => {
                        let max = IBig::from(bitsize.maxi(signed));
                        let min = signed
                            .then(|| -max.clone())
                            .unwrap_or_else(|| IBig::from(0));

                        let range = Range {
                            start: bound_to_ibig(*start, &min),
                            end: bound_to_ibig(*end, &max),
                        };

                        let next = BranchOf::one(range, 0, self.from_queue());
                        DecTreeBranch::Ints(bitsize, min, max, next)
                    }
                    _ => DecTreeBranch::Poison,
                }
            }
            hir::Pattern::Bind(bind, next) => {
                self.table.map.push((*bind, point));
                self.point.0 -= 1; // binding doesn't produce a new point
                self.new_tree(&next).branch
            }
            hir::Pattern::Constructor(sum, variant, params) => {
                extend_untr(&mut self.queue, params);
                let instinfo = self.l.current.pop_inst_without_assertion();
                let inst = self.l.fin_inst(&instinfo.inst);
                let next = self.from_queue();
                let variant_typings = self.l.vtypes[*sum]
                    .keys()
                    .map(|var| {
                        let types = self.l.vtypes[*sum][var].clone();
                        let params = types.iter().map(|ty| inst.apply(ty)).collect();

                        let ret = Type::Defined(
                            sum.map(key::TypeKind::Sum),
                            inst.generics.values().cloned().collect(),
                        );
                        CallTypes::new(params, ret)
                    })
                    .collect();
                DecTreeBranch::Sum(
                    inst,
                    variant_typings,
                    *sum,
                    BranchOf::one(*variant, params.len(), next),
                )
            }
            hir::Pattern::Record(rvar, _, fields) => {
                match self.l.finalizer(|mut fin| (fin.record(*rvar), fin.errors)) {
                    None => DecTreeBranch::Poison,
                    Some((key, params)) => {
                        let mut count = 0;

                        for (_, fieldname) in self.l.rsolver.fnames[key].iter().rev() {
                            count += 1;
                            match fields.iter().find(|(name, _, _)| *name == *fieldname) {
                                Some((_, bind, pat)) => {
                                    self.queue.push((&**pat, Some(*bind)));
                                }
                                None => self.queue.push((&hir::Pattern::Any, None)),
                            }
                        }

                        DecTreeBranch::Record(key, params, count, Box::new(self.from_queue()))
                    }
                }
            }
            hir::Pattern::Tuple(elems) => {
                extend_untr(&mut self.queue, elems);
                DecTreeBranch::Tuple(elems.len(), Box::new(self.from_queue()))
            }
            hir::Pattern::Cons(elems, inner_var) => {
                let Some(type_) = self.l.items.list_default else {
                    return DecTree { point, branch: DecTreeBranch::Poison };
                };
                let inner = self
                    .l
                    .finalizer(|mut fin| (fin.var(*inner_var), fin.errors));

                let list_type = Type::List(type_, vec![inner.clone()]);

                let [x, xs] = &**elems;

                // The order is reveresd here because `from_queue` pops from the top
                self.queue.push((xs, None));
                self.queue.push((x, None));

                DecTreeBranch::List(
                    type_,
                    list_type,
                    BranchOf { branches: vec![(ListConstr::Cons, 2, self.from_queue())] },
                )
            }
            hir::Pattern::Nil(inner_var) => {
                let Some(type_) = self.l.items.list_default else {
                    return DecTree { point, branch: DecTreeBranch::Poison };
                };
                let inner = self
                    .l
                    .finalizer(|mut fin| (fin.var(*inner_var), fin.errors));

                let list_type = Type::List(type_, vec![inner.clone()]);

                DecTreeBranch::List(
                    type_,
                    list_type,
                    BranchOf { branches: vec![(ListConstr::Nil, 0, self.from_queue())] },
                )
            }
            hir::Pattern::Bool(b) => DecTreeBranch::Bools(BranchOf::one(*b, 0, self.from_queue())),
            hir::Pattern::String(str) => todo!("string patterns"),
            hir::Pattern::Poison => todo!("how can there be poison but no error??"),
        };

        DecTree { point, branch }
    }

    fn from_queue(&mut self) -> DecTree {
        match self.queue.pop() {
            Some((p, bind)) => {
                if let Some(bind) = bind {
                    self.table.map.push((bind, self.point));
                };
                self.new_tree(p)
            }
            None => {
                let point = std::mem::take(&mut self.point);
                let table = std::mem::take(&mut self.table);
                let tail = self.lower_tail();
                let branch = DecTreeBranch::Reached(table, Box::new(tail));
                DecTree { branch, point }
            }
        }
    }

    fn lower_tail(&mut self) -> Expr {
        match self.params.get(self.current_param) {
            None => self.l.lower_expr(self.expr),
            Some(pat) => {
                let param = key::Param(self.current_param as u32);
                self.current_param += 1;

                let dec = self.new_tree(pat);

                let on = Expr::Yield(mir::func::Local::Param(param));
                Expr::Match(Box::new(on), dec)
            }
        }
    }
}

#[derive(new)]
pub struct Merger<'a> {
    vtypes: &'a ModMap<key::Sum, Map<key::SumVariant, Vec<Tr<Type>>>>,
}

impl<'a> Merger<'a> {
    pub fn merge(&mut self, tree: &mut DecTree, other: DecTree) -> IsReachable {
        use DecTreeBranch as B;

        // assert_eq!(tree.point, other.point);

        match (&mut tree.branch, other.branch) {
            (B::Ints(_, min, max, ranges), B::Ints(_, omin, omax, oranges)) => {
                assert_eq!(*min, omin);
                assert_eq!(*max, omax);
                assert_eq!(tree.point, other.point);
                oranges
                    .branches
                    .into_iter()
                    .any(|(o, _, next)| self.merge_int(0, ranges, o, next))
            }
            (B::Tuple(params, next), B::Tuple(oparams, onext)) => {
                assert_eq!(tree.point, other.point);
                assert_eq!(*params, oparams);
                self.merge(&mut *next, *onext)
            }
            (B::Record(key, _, _, fields), B::Record(okey, _, _, ofields)) => {
                assert_eq!(tree.point, other.point);
                assert_eq!(*key, okey);
                self.merge(&mut *fields, *ofields)
            }
            (B::Poison, _) | (_, B::Poison) => true,
            (B::Bools(bools), B::Bools(obools)) => {
                assert_eq!(tree.point, other.point);
                self.merge_cmps(bools, obools, PartialEq::eq)
            }
            (B::Sum(_, _, key, variants), B::Sum(_, _, okey, ovariants)) => {
                assert_eq!(*key, okey);
                self.merge_cmps(variants, ovariants, PartialEq::eq)
            }
            (B::Reached(..), B::Reached(..)) => UNREACHABLE,
            (B::Wildcard(next), B::Wildcard(onext)) => self.merge(next, *onext),

            (B::List(key, _, variants), B::List(okey, _, ovariants)) => {
                assert_eq!(*key, okey);
                self.merge_cmps(variants, ovariants, PartialEq::eq)
            }

            (B::Wildcard(next), other) => {
                // swap the positions so that the more specialised tree becomes the existing one
                //
                // then merge the previous wildcard into that specialised tree
                //
                // then swap the references so that the specialised tree is put into `tree`
                let mut other = DecTree { point: tree.point, branch: other };
                let tnext = std::mem::replace(&mut next.branch, DecTreeBranch::Poison);
                let reachable =
                    self.merge_wildcard(&mut other, DecTree { point: next.point, branch: tnext });
                std::mem::swap(tree, &mut other);
                reachable
            }
            (_, B::Wildcard(next)) => self.merge_wildcard(tree, *next),

            _ => {
                error!("invalid pattern tree combination");

                #[cfg(test)]
                panic!("invalid pattern tree combination");

                REACHABLE
            }
        }
    }

    fn merge_cmps<T>(
        &mut self,
        bools: &mut BranchOf<T>,
        other: BranchOf<T>,
        cmp: impl Fn(&T, &T) -> bool,
    ) -> IsReachable {
        other.branches.into_iter().any(|(o, params, onext)| {
            match bools.branches.iter_mut().find(|(b, _, _)| cmp(b, &o)) {
                Some((_, _, next)) => self.merge(next, onext),
                None => {
                    bools.branches.push((o, params, onext));
                    REACHABLE
                }
            }
        })
    }

    fn merge_wildcard(&mut self, tree: &mut DecTree, wnext: DecTree) -> IsReachable {
        match &mut tree.branch {
            DecTreeBranch::Ints(bitsize, min, max, _) => {
                let branch = DecTreeBranch::Ints(
                    *bitsize,
                    min.clone(),
                    max.clone(),
                    BranchOf {
                        branches: vec![(Range { start: min.clone(), end: max.clone() }, 0, wnext)],
                    },
                );

                self.merge(tree, DecTree { point: tree.point, branch })
            }
            DecTreeBranch::List(key, inner, _) => {
                let branch = DecTreeBranch::List(
                    *key,
                    inner.clone(),
                    BranchOf {
                        branches: vec![
                            (
                                ListConstr::Cons,
                                2,
                                wildcard_chain(PatPoint(tree.point.0 + 1), 2, wnext.clone()),
                            ),
                            (ListConstr::Nil, 0, wnext),
                        ],
                    },
                );

                self.merge(tree, DecTree { point: tree.point, branch })
            }
            DecTreeBranch::Tuple(params, _) => {
                let wildcards = wildcard_chain(PatPoint(tree.point.0 + 1), *params, wnext);
                let branch = DecTreeBranch::Tuple(*params, Box::new(wildcards));
                self.merge(tree, DecTree { point: tree.point, branch })
            }
            DecTreeBranch::Bools(_) => {
                let branch = DecTreeBranch::Bools(BranchOf {
                    branches: vec![(false, 0, wnext.clone()), (true, 0, wnext)],
                });

                self.merge(tree, DecTree { point: tree.point, branch })
            }
            DecTreeBranch::Sum(_, vartypes, key, prev) => {
                return self.vtypes[*key].keys().fold(UNREACHABLE, |r, variant| {
                    let point = PatPoint(tree.point.0 + 1);
                    let params = vartypes[variant].params.len();
                    let wnext = wildcard_chain(point, params, wnext.clone());

                    match prev.branches.iter_mut().find(|(b, _, _)| *b == variant) {
                        Some((_, _, next)) => r | self.merge(next, wnext),
                        None => {
                            prev.branches.push((variant, params, wnext));
                            r | REACHABLE
                        }
                    }
                });
            }
            DecTreeBranch::Record(key, params, plen, _) => {
                let wnext = wildcard_chain(PatPoint(tree.point.0 + 1), *plen, wnext);
                let branch = DecTreeBranch::Record(*key, params.clone(), *plen, Box::new(wnext));
                self.merge(tree, DecTree { point: tree.point, branch })
            }
            DecTreeBranch::Reached(_, _) => panic!("corrupt pattern tree"),
            DecTreeBranch::Wildcard(next) => return self.merge(next, wnext),
            DecTreeBranch::Poison => return UNREACHABLE,
        }
    }
}

fn wildcard_chain(point: PatPoint, params: Params, mut wnext: DecTree) -> DecTree {
    wnext.offset_binds(params as u32);

    fn perform(point: PatPoint, params: Params, wnext: DecTree) -> DecTree {
        if params == 0 {
            wnext
        } else {
            DecTree {
                point,
                branch: DecTreeBranch::Wildcard(Box::new(wildcard_chain(
                    PatPoint(point.0 + 1),
                    params - 1,
                    wnext,
                ))),
            }
        }
    }

    perform(point, params, wnext)
}

fn bound_to_ibig(bound: Bound, excess: &IBig) -> IBig {
    match bound {
        Bound::Neg(n) => -IBig::from(n),
        Bound::Pos(n) => IBig::from(n),
        Bound::Excess => excess.clone(),
    }
}

impl fmt::Display for DecTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}@{}", self.point, self.branch)
    }
}

impl fmt::Display for DecTreeBranch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DecTreeBranch::Ints(_, _, _, branches) => branches.fmt(f),
            DecTreeBranch::Tuple(_, next) => pfmt("tuple", &next).fmt(f),
            DecTreeBranch::Bools(branches) => branches.fmt(f),
            DecTreeBranch::Sum(_, _, _, branches) => branches.fmt(f),
            DecTreeBranch::List(_, _, branches) => branches.fmt(f),
            DecTreeBranch::Record(key, _, _, next) => pfmt(key, next).fmt(f),
            DecTreeBranch::Reached(table, expr) => {
                write!(
                    f,
                    "point table [{}] {} {expr}",
                    table
                        .map
                        .iter()
                        .map(|(bind, point)| format!("{bind}={point}"))
                        .format(", "),
                    "->".symbol()
                )
            }
            DecTreeBranch::Wildcard(next) => pfmt("_", next).fmt(f),
            DecTreeBranch::Poison => "<poison>".fmt(f),
        }
    }
}

impl<T: fmt::Display> fmt::Display for BranchOf<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.branches
            .iter()
            .map(|(var, _, next)| pfmt(var, next))
            .format("\n")
            .fmt(f)
    }
}

fn pfmt(header: impl fmt::Display, then: &DecTree) -> impl fmt::Display {
    format!(
        "{header}{}\n  {}",
        ':'.symbol(),
        then.to_string().lines().format("\n  ")
    )
}

impl fmt::Display for ListConstr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ListConstr::Cons => "cons",
            ListConstr::Nil => "nil",
        }
        .fmt(f)
    }
}
