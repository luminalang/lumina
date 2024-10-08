use super::{
    super::{CallTypes, Callable},
    range, BranchKey, Branching, DecTree, Init, PointTable, StrCheck, StrChecks, TreeTail,
    LIST_CONS, LIST_NIL,
};
use crate::prelude::*;
use hir::Pattern;
use lumina_key::M as Mod;
use lumina_typesystem::{Container, GenericMapper, Static, Type, Var};
use std::collections::VecDeque;
use std::fmt::Display;
use std::{file, line};

macro_rules! reachable_as_poison {
    ($expr:expr) => {{
        tracing::warn!(
            "{}:{} | reporting reachability as true due to poison: {}",
            file!(),
            line!(),
            $expr
        );

        true
    }};
}

pub struct Merger<'h, 's, Tail, M> {
    remaining: Vec<(&'h Tr<Pattern<'s>>, Option<key::Bind>)>,
    merge: &'h mut M,
    maybe: key::M<key::Sum>,
    string: key::M<key::Record>,
    list: key::M<key::TypeKind>,
    table: PointTable,
    depth: usize,
    _tail: std::marker::PhantomData<Tail>,
}

impl<'h, 's, Tail: Display + Clone + PartialEq, M: Merge<'s, Tail>> Merger<'h, 's, Tail, M> {
    fn merge(mut self, tree: &mut DecTree<Tail>, pat: Tr<&'h Pattern<'s>>) -> IsReachable {
        trace!(
            "merging {pat} (with remaining: {}) into\n{tree}",
            self.remaining.iter().map(|(p, _)| p).format(", ")
        );

        match pat.value {
            Pattern::Bind(bind, on) => {
                self.table.binds.push((*bind, self.depth));
                return self.merge(tree, (&**on).tr(pat.span));
            }
            Pattern::Any => {
                return self.merge_any(pat.span, tree);
            }
            Pattern::Poison => return true,
            _ => {}
        }

        macro_rules! expected {
            ($exp:pat $( if $guard:expr )? => $expr:expr) => {
                match pat.value {
                    $exp $( if $guard )? => $expr,
                    _ => reachable_as_poison!(pat),
                }
            };
        }

        match tree {
            DecTree::Tuple { next, elems } => {
                expected!(
                    Pattern::Tuple(pelems) if *elems == pelems.len() =>
                    self.merge_params(next, pelems)
                )
            }
            DecTree::Array { next, elems } => {
                expected!(Pattern::Array(pelems, len) if **len == *elems =>
                    {
                        if pelems.len() == 1 && *elems != 1 {
                            unimplemented!("replicated array comparison in pattern");
                        } else {
                            self.merge_params(next, pelems)
                        }
                    }
                )
            }
            DecTree::Record { record, .. } if *record == self.string => {
                panic!("string type given as Ty::defined instead of Ty::string");
            }
            DecTree::Record { record, fields, next, .. } => {
                expected!(
                    Pattern::Record(_, _, pfields) =>
                    self.merge_record(*record, *fields, next, pfields)
                )
            }
            DecTree::Sum { sum, next, .. } => match pat.value {
                Pattern::Cons(..) if sum.map(key::TypeKind::Sum) == self.list => {
                    panic!("list sugar used on list type `List a` instead of `[a]");
                }
                Pattern::Nil(_) if sum.map(key::TypeKind::Sum) == self.list => {
                    panic!("list sugar used on list type `List a` instead of `[a]");
                }
                Pattern::Constructor(psum, var, params) if psum == sum => {
                    self.merge_cmp_var(next, *var, params)
                }
                _ => reachable_as_poison!(pat),
            },
            DecTree::Bools(next) => {
                expected!(Pattern::Bool(var) => self.merge_cmp_var(next, *var, &[]))
            }
            DecTree::List { next, .. } => match pat.value {
                Pattern::Cons(values, _) => self.merge_cmp_var(next, LIST_CONS, &**values),
                Pattern::Nil(_) => self.merge_cmp_var(next, LIST_NIL, &[]),
                Pattern::Constructor(sum, _, _) if sum.map(key::TypeKind::Sum) == self.list => {
                    panic!("concrete variants used for sugared list type");
                }
                _ => reachable_as_poison!(pat),
            },
            DecTree::Ints { intsize, next } => {
                expected!(
                    Pattern::Int(bounds, _) =>
                    self.merge_int_bounds(*intsize, next, bounds)
                )
            }
            DecTree::Opaque { .. } => reachable_as_poison!(pat),
            DecTree::String { next, wildcard_next } => {
                match pat.value {
                    Pattern::String(spats) => {
                        self.merge_strings(pat.span, spats, next, wildcard_next)
                    }
                    // Hack for allowing record patterns for strings
                    Pattern::Record(rvar, _, fields) => match self.merge.fin_record(*rvar) {
                        Some((record, params)) if record == self.string => {
                            assert!(params.is_empty());
                            if !next.branches.is_empty() {
                                panic!("cannot use a mixture of record and string patterns for string langitem");
                            }

                            let string_ty = Type::defined(record, params.clone());

                            *tree = self
                                .expand_wildcard_and_bump_tails(&string_ty, &mut **wildcard_next);

                            const STRING_FIELDS: usize = 1;
                            self.merge_record(record, STRING_FIELDS, tree, fields)
                        }
                        _ => reachable_as_poison!(pat),
                    },
                    _ => reachable_as_poison!(pat),
                }
            }
            DecTree::Wildcard { ty, next } => {
                *tree = self.expand_wildcard_and_bump_tails(ty, &mut **next);
                self.merge(tree, pat)
            }
            DecTree::End(_) => self.merge_into_tail(tree, pat),
        }
    }

    fn expand_wildcard_and_bump_tails(
        &mut self,
        ty: &Type,
        next: &mut DecTree<Tail>,
    ) -> DecTree<Tail> {
        let next = std::mem::replace(next, DecTree::End(TreeTail::Poison));

        let mut expanded_wildcard = self.merge.to_init().unreached_from_type(ty);

        // If we have `_ next` and the actual type is found to be for example `(int, int)`.
        // Then; we eagerly expand `(_, _) next` instead.
        //
        // This is normally done lazily so that we support recursive types however; we
        // want to expand here because we have `pat` which isn't a wildcard. Thereby;
        // we know we will need a deeper tree.
        expanded_wildcard.for_each_tail_mut(&mut |tail| {
            match tail {
                TreeTail::Poison => todo!(),
                TreeTail::Unreached(types) => {
                    let mut next = next.clone();

                    // Since we're expanding a wildcard in the middle of a pattern tree, we
                    // need to adjust the points on the tails to compensate.
                    next.bump_tail_table_points(types.len());

                    types_into_wildcards(types.into_iter(), next)
                }
                TreeTail::Reached(_, _, _) => {
                    unreachable!("tree_from_type shouldn't generate reached")
                }
            }
        });

        expanded_wildcard
    }

    // TODO: theoretically we could find instances of the StrChecks being identical and merge those
    // branches as well.
    fn merge_strings(
        mut self,
        span: Span,
        spats: &[hir::StringPattern<'s>],
        next: &mut Branching<StrChecks, Tail>,
        wildcard_next: &mut DecTree<Tail>,
    ) -> IsReachable {
        let checks = spats
            .iter()
            .enumerate()
            .filter_map(|(i, spat)| {
                let check = match spat {
                    hir::StringPattern::Literal(name) => {
                        let ro = self.merge.str_to_ro(name);
                        StrCheck::Literal(ro)
                    }
                    // The last wildcard will bind to excess string instead of char
                    hir::StringPattern::Wildcard(bind) if spats.len() - 1 == i => {
                        self.table.binds.push((**bind, self.depth));
                        StrCheck::TakeExcess
                    }
                    hir::StringPattern::Wildcard(bind) => {
                        self.table.binds.push((**bind, self.depth));
                        StrCheck::TakeByte
                    }
                    hir::StringPattern::Extractor(extractor) => {
                        let params = self.merge.extractor_params(&extractor.params);

                        if let Some(bind) = extractor.bind {
                            self.table.binds.push((*bind, self.depth));
                        }

                        match &extractor.call {
                            hir::Callable::Func(nfunc) => {
                                let (mapping, calltypes) = self.merge.fin_popped_inst(span)?;
                                let call = Callable::Func(M(nfunc.module, nfunc.key), mapping);
                                self.extractor_by_typing(call, calltypes, params)
                            }
                            hir::Callable::Lambda(lambda) => {
                                let (mapping, calltypes) = self.merge.fin_popped_inst(span)?;
                                let call = Callable::Lambda(*lambda, mapping);
                                self.extractor_by_typing(call, calltypes, params)
                            }
                            hir::Callable::Binding(bind) => {
                                let ty = self.merge.type_of_bind(*bind);
                                match ty {
                                    Type::Container(Container::FnPointer, elems) => {
                                        let call = Callable::Binding(*bind);
                                        let (ret, ptypes) = elems.split_last().unwrap();
                                        let ptypes = ptypes.iter().cloned().collect();
                                        let ctypes = CallTypes::new(ptypes, ret.clone());
                                        self.extractor_by_typing(call, ctypes, params)
                                    }
                                    Type::Container(Container::Closure, elems) => {
                                        let call = Callable::Binding(*bind);
                                        let (ret, ptypes) = elems.split_last().unwrap();
                                        let ptypes = ptypes.iter().cloned().collect();
                                        let ctypes = CallTypes::new(ptypes, ret.clone());
                                        self.extractor_by_typing(call, ctypes, params)
                                    }
                                    _ => {
                                        panic!("ET: invalid type for extractor");
                                    }
                                }
                            }
                            hir::Callable::Builtin(_) => {
                                panic!("builtin functions are not allowed as extractors")
                            }
                            hir::Callable::TypeDependentLookup(_) => unreachable!(),
                        }
                    }
                };
                self.depth += 1;
                Some(check)
            })
            .collect::<Vec<_>>();

        if matches!(checks.as_slice(), [StrCheck::TakeExcess]) {
            self.next(wildcard_next)
        } else {
            let mut branch_next = wildcard_next.clone();
            self.next(&mut branch_next);
            next.branches.push((StrChecks { checks }, branch_next));
            true
        }
    }

    fn extractor_by_typing(
        &mut self,
        callable: Callable,
        types: CallTypes,
        params: Vec<mir::Expr>,
    ) -> StrCheck {
        let start_at = key::Param(params.len() as u32);

        let string = Type::string(self.string, vec![]);

        match &types.params[start_at] {
            ty if *ty == string => match &types.ret {
                Type::Container(Container::Defined(key, _), elems)
                    if self.maybe.map(key::TypeKind::Sum) == *key =>
                {
                    match &elems[..] {
                        [Type::Container(Container::Tuple, elems)] => match &elems[..] {
                            [ty, str] if *str == string => {
                                return StrCheck::TakeBySplit(callable, ty.clone(), params);
                            }
                            _ => {}
                        },
                        _ => {}
                    }
                }
                _ => {}
            },
            Type::Int(size) if !size.signed && size.bits() == 8 => match &types.ret {
                Type::Simple("bool") => {
                    return StrCheck::TakeWhile(callable, params);
                }
                _ => {}
            },
            _ => {}
        }

        panic!("ET: invalid type annotation for extractor: {types:?}")
    }

    fn merge_record(
        mut self,
        record: Mod<key::Record>,
        rfields: usize,
        next: &mut DecTree<Tail>,
        fields: &'h [(Tr<&'s str>, key::Bind, Tr<hir::Pattern<'s>>)],
    ) -> IsReachable {
        for i in (0..rfields as u32).rev() {
            let field = key::Field(i);
            let name = self.merge.name_of_field(record, field);

            match fields.iter().position(|(n, _, _)| **n == name) {
                // Missing fields for record destructors are allowed
                None => {
                    static ANY: Tr<hir::Pattern<'static>> =
                        Tr::new(Span::null(), hir::Pattern::Any);

                    self.remaining.push((&ANY, None))
                }
                Some(i) => {
                    if let Some((additional, _, _)) = fields
                        .get(i + 1..)
                        .and_then(|fields| fields.iter().find(|(n, _, _)| **n == name))
                    {
                        self.merge.err_duplicate_field(fields[i].0, additional.span);
                    }

                    let (_, bind, pat) = &fields[i];
                    self.remaining.push((pat, Some(*bind)));
                }
            }
        }

        self.next(next)
    }

    // TODO: we should consider specialising `SumVariant` to make it lazily include the variants
    // themselves.
    fn merge_cmp_var<K: Eq + BranchKey>(
        self,
        next: &mut Branching<K, Tail>,
        var: K,
        pats: &'h [Tr<Pattern<'s>>],
    ) -> IsReachable {
        for (v, tree) in &mut next.branches {
            if *v == var {
                return self.merge_params(tree, pats);
            }
        }

        panic!("variant {var} was not initialised from type");
    }

    fn merge_any(self, span: Span, tree: &mut DecTree<Tail>) -> IsReachable {
        match tree {
            DecTree::Wildcard { next, .. } => self.next(next),
            DecTree::Record { fields, next, .. } => {
                let generated = vec![hir::Pattern::Any.tr(span); *fields];
                self.merge_params(next, &generated)
            }
            DecTree::Tuple { elems, next } => {
                let generated = vec![hir::Pattern::Any.tr(span); *elems];
                self.merge_params(next, &generated)
            }
            DecTree::Array { elems, next } => {
                let generated = vec![hir::Pattern::Any.tr(span); *elems as usize];
                self.merge_params(next, &generated)
            }
            DecTree::Sum { sum, next, .. } => {
                self.merge_any_into_branches(span, next, |this, var| {
                    this.merge.to_init().vtypes[*sum][*var].len()
                })
            }
            DecTree::List { next, .. } => {
                self.merge_any_into_branches(
                    span,
                    next,
                    |_, var| if *var == LIST_CONS { 2 } else { 0 },
                )
            }
            DecTree::Ints { intsize, next } => {
                let full = range::Constraints::from(*intsize);
                self.merge_int(next, full.min, full.max)
            }
            DecTree::String { wildcard_next, .. } => self.next(wildcard_next),
            DecTree::Bools(next) => self.merge_any_into_branches(span, next, |_, _| 0),
            DecTree::Opaque { next, .. } => self.next(next),
            DecTree::End(_) => self.merge_any_into_tail(span, tree),
        }
    }

    fn merge_any_into_branches<K: BranchKey>(
        mut self,
        span: Span,
        branching: &mut Branching<K, Tail>,
        params: impl Fn(&Self, &K) -> usize,
    ) -> IsReachable {
        let mut reachable = false;
        for (k, next) in &mut branching.branches {
            let params = params(&self, k);
            let this = self.fork();
            let generated = vec![hir::Pattern::Any.tr(span); params];
            reachable |= this.merge_params(next, &generated);
        }
        reachable
    }

    fn merge_any_into_tail(self, span: Span, tree: &mut DecTree<Tail>) -> IsReachable {
        let DecTree::End(tail) = tree else {
            unreachable!();
        };

        match tail {
            TreeTail::Poison => reachable_as_poison!("tree-side poison"),

            // Edge-case for the *last* part of the decision tree to not expand unecessarily
            TreeTail::Unreached(types) if self.remaining.is_empty() => {
                assert_eq!(types.len(), 1);
                let types = std::mem::take(types);
                let expr = self.merge.generate_tail();
                *tail = TreeTail::Reached(self.table, types, expr);
                true
            }

            TreeTail::Unreached(types) => {
                let mut types = std::mem::take(types);

                let to_expand = types.pop_front().unwrap();
                let next = DecTree::End(TreeTail::Unreached(types));

                *tree = self.merge.to_init().reached_from_type(to_expand, next);

                self.merge_any(span, tree)
            }

            TreeTail::Reached(_, excess, _) => {
                if excess.is_empty() {
                    assert_eq!(self.remaining.len(), 0);
                }
                let _expr = self.merge.generate_tail();
                false
            }
        }
    }

    fn merge_params(
        mut self,
        tree: &mut DecTree<Tail>,
        pats: &'h [Tr<Pattern<'s>>],
    ) -> IsReachable {
        // reversed because we want the left-most parameter to be up next
        for p in pats.iter().rev() {
            self.remaining.push((p, None));
        }

        self.next(tree)
    }

    pub(super) fn fork(&mut self) -> Merger<'_, 's, Tail, M> {
        Merger {
            remaining: self.remaining.clone(),
            merge: self.merge,
            maybe: self.maybe,
            string: self.string,
            list: self.list,
            table: self.table.clone(),
            depth: self.depth,
            _tail: std::marker::PhantomData,
        }
    }

    pub(super) fn next(mut self, tree: &mut DecTree<Tail>) -> IsReachable {
        self.depth += 1;

        match self.remaining.pop() {
            Some((pat, bind)) => {
                if let Some(bind) = bind {
                    self.table.binds.push((bind, self.depth));
                }
                self.merge(tree, pat.as_ref())
            }
            None => {
                match tree {
                    DecTree::End(tail) => match tail {
                        TreeTail::Poison => true,
                        TreeTail::Reached(_, _, _) => false,
                        TreeTail::Unreached(remaining) if remaining.is_empty() => {
                            let expr = self.merge.generate_tail();
                            trace!("generated tail: {expr}");
                            *tail = TreeTail::Reached(self.table, VecDeque::new(), expr);
                            true
                        }
                        TreeTail::Unreached(remaining) => {
                            panic!(
                                "given pattern ended too early for tree with remaining: {}",
                                remaining.iter().format(", ")
                            )
                        }
                    },
                    // Can this happen from errors? are we meant to poison?
                    _ => panic!("given pattern ended too early for tree"),
                }
            }
        }
    }

    fn merge_into_tail(self, tree: &mut DecTree<Tail>, pat: Tr<&'h Pattern<'s>>) -> IsReachable {
        let DecTree::End(tail) = tree else {
            unreachable!();
        };

        assert!(!matches!(pat.value, Pattern::Bind(..)));
        assert!(!matches!(pat.value, Pattern::Any));

        match tail {
            TreeTail::Reached(_, _, _) => {
                panic!("reached end of tree with remaining: {pat}")
            }
            TreeTail::Poison => true,
            TreeTail::Unreached(types) => {
                let types = std::mem::take(types);
                *tree = self.merge.to_init().expand_first_then_extend_excess(types);
                self.merge(tree, pat)
            }
        }
    }
}

pub trait Merge<'s, Tail: Display + Clone + PartialEq>: Sized {
    fn generate_tail(&mut self) -> Tail;

    // fn record_from_rvar(&mut self, rvar: RecordVar) -> Option<(M<key::Record>, Vec<Type>)>;
    fn to_init(&self) -> Init<'_>;

    fn name_of_field(&self, record: Mod<key::Record>, field: key::Field) -> &'s str;

    fn str_to_ro(&mut self, str: &'s str) -> M<key::ReadOnly>;

    fn err_duplicate_field(&mut self, field: Tr<&'s str>, previous: Span);

    fn fin_popped_inst(&mut self, span: Span) -> Option<(GenericMapper<Static>, CallTypes)>;
    fn fin_record(&mut self, rvar: Var) -> Option<(M<key::Record>, Vec<Type>)>;

    fn type_of_bind(&mut self, bind: key::Bind) -> Type;

    fn extractor_params(&mut self, params: &[Tr<hir::Expr<'s>>]) -> Vec<mir::Expr>;

    fn first(
        &mut self,
        string: M<key::Record>,
        maybe: M<key::Sum>,
        list: M<key::TypeKind>,
        ty: &Type,
        mut pat: Tr<&Pattern<'s>>,
    ) -> DecTree<Tail> {
        let mut table = PointTable::new(vec![]);

        // edge-case for whether it's instantly completed, we don't want to generate extra branches
        // by type if they would lead to the same destination.
        loop {
            match pat.value {
                Pattern::Any => {
                    let tail = self.generate_tail();
                    return DecTree::End(TreeTail::Reached(table, VecDeque::new(), tail));
                }
                Pattern::Bind(bind, next) => {
                    table.binds.push((*bind, 0));
                    pat.value = next;
                }
                _ => break,
            }
        }

        let mut tree = self.to_init().unreached_from_type(ty);
        trace!("initialised tree from {ty}:\n{tree}");

        let reachable = Merger {
            remaining: Vec::new(),
            merge: self,
            maybe,
            string,
            list,
            table,
            depth: 0,
            _tail: std::marker::PhantomData,
        }
        .merge(&mut tree, pat);
        assert!(reachable);

        tree
    }

    fn branch(
        &mut self,
        string: M<key::Record>,
        maybe: M<key::Sum>,
        list: M<key::TypeKind>,
        tree: &mut DecTree<Tail>,
        pat: Tr<&Pattern<'s>>,
    ) -> IsReachable {
        Merger {
            remaining: Vec::new(),
            depth: 0,
            maybe,
            string,
            list,
            table: PointTable { binds: vec![] },
            merge: self,
            _tail: std::marker::PhantomData,
        }
        .merge(tree, pat)
    }
}

pub type IsReachable = bool;

fn types_into_wildcards<Tail>(
    types: impl DoubleEndedIterator<Item = Type>,
    next: DecTree<Tail>,
) -> DecTree<Tail> {
    types
        .into_iter()
        .rev()
        .fold(next, |next, ty| DecTree::Wildcard {
            ty,
            next: Box::new(next),
        })
}
