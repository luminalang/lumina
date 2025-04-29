use super::{escape, pat, pat::Merge, Callable, FinError, Lower, MatchBranchLower};
use crate::mir::builtins;
use crate::prelude::*;
use crate::{LISTABLE_CONS, LISTABLE_NEW, LISTABLE_WITH_CAPACITY, STRINGABLE_FROM_RAW_PARTS};
use ast::NFunc;
use lumina_typesystem::{
    ConstValue, Constraint, Container, Generic, GenericKind, GenericMapper, IntSize, Static,
    Transformer, Ty, Type, Var,
};
use lumina_util::Highlighting;
use std::fmt;

#[derive(Clone, Debug)]
pub enum Expr {
    Call(Callable, Vec<Self>),
    PartiallyApplicate(Callable, Vec<Self>),
    Yield(Callable),

    TupleAccess(Box<Self>, usize),
    Access(Box<Self>, M<key::Record>, Vec<Type>, key::Field),
    Record(M<key::Record>, Vec<Type>, Vec<(key::Field, Span, Self)>),
    Array(Vec<Self>, u64, Type),
    GenericArray(Box<Self>, Generic, Type),
    ArrayAccess(Box<[Self; 2]>),

    Int(IntSize, i128),
    Bool(bool),
    Float(f64),
    ReadOnly(M<key::ReadOnly>),

    PointerToPointerCast(Box<Self>, Type),
    PointerToArrayCast(Box<Self>, u64, Type),
    PointerToGenericArrayCast(Box<Self>, Generic, Type),
    ToPointerCast(Box<Self>, IntSize, Type),
    FromPointerCast(Box<Self>, IntSize),
    IntCast(Box<Self>, IntSize, IntSize),
    ToFloatCast(Box<Self>, IntSize),
    FromFloatCast(Box<Self>, IntSize),
    ArrayLen(Box<Self>),

    ObjectCast(Box<Self>, Type, M<key::Trait>, Vec<Type>),
    MemCpy(Box<[Self; 3]>),
    Deref(Box<Self>),
    Write(Box<[Self; 2]>),
    ReflectTypeOf(Type),
    SizeOf(Type),
    AlignOf(Type),
    Alloca(Type),
    Unreachable(Type),

    Cmp(&'static str, Box<[Expr; 2]>),
    Num(&'static str, Box<[Expr; 2]>),
    IntAbs(Box<Expr>),
    ValToRef(Box<Self>),

    Tuple(Vec<Self>),
    Match(
        Box<Self>,
        pat::DecTree<key::DecisionTreeTail>,
        Map<key::DecisionTreeTail, Self>,
        Map<key::DecisionTreeTail, u16>,
    ),
    Poison,
}

impl Expr {
    pub fn bind(bind: key::Bind) -> Expr {
        Expr::Yield(Callable::Binding(bind))
    }
}

impl<'a, 's> Lower<'a, 's> {
    fn fin_inst_or_poison(
        &mut self,
        span: Span,
        f: impl FnOnce(&mut Self, GenericMapper<Static>) -> Expr,
    ) -> Expr {
        match self.current.pop_inst(span) {
            Some(instinfo) => {
                let (inst, _) = (&mut *self).fin_typing(&instinfo);
                f(self, inst)
            }
            None => Expr::Poison,
        }
    }

    pub fn lower_expr(&mut self, expr: Tr<&hir::Expr<'s>>) -> Expr {
        match &expr.value {
            hir::Expr::Call(call, tanot, params) => match call {
                hir::Callable::Func(nfunc) => {
                    let nfunc = M(nfunc.module, nfunc.key);
                    let params = self.lower_exprs(params);
                    self.fin_inst_or_poison(expr.span, |_, mapper| {
                        let call = Callable::Func(nfunc, mapper);
                        Expr::Call(call, params)
                    })
                }
                hir::Callable::Lambda(lambda) => {
                    let params = self.lower_exprs(params);
                    self.fin_inst_or_poison(expr.span, |_, mapper| {
                        let call = Callable::Lambda(*lambda, mapper);
                        Expr::Call(call, params)
                    })
                }
                hir::Callable::Binding(bind) => {
                    let ty = self.current.binds[bind].clone();
                    let ty = self.finalizer().transform(&ty);
                    match ty {
                        Type::Container(Container::FnPointer | Container::Closure, _) => {
                            let params = self.lower_exprs(params);
                            let call = Callable::Binding(*bind);
                            Expr::Call(call, params)
                        }
                        _ if params.is_empty() => Expr::Yield(Callable::Binding(*bind)),
                        _ => Expr::Poison,
                    }
                }
                hir::Callable::TypeDependentLookup(_) => {
                    let params = self.lower_exprs(params);
                    self.fin_inst_or_poison(expr.span, |this, mapper| {
                        let fkey = this.current.type_dependent_lookup.pop_front().unwrap();
                        let call = Callable::Func(fkey, mapper);
                        Expr::Call(call, params)
                    })
                }
                hir::Callable::Builtin(name) => builtins::lower(self, *name, params, tanot),
            },
            hir::Expr::BuiltinOp(op, params) => {
                let left = self.lower_expr(params[0].as_ref());
                let right = self.lower_expr(params[1].as_ref());

                Expr::Num(op, Box::new([left, right]))
            }
            hir::Expr::Pass(call, _, params) => match call {
                hir::Callable::Func(nfunc) => {
                    let params = self.lower_exprs(params);
                    self.fin_inst_or_poison(expr.span, |_, mapper| {
                        let call = Callable::Func(M(nfunc.module, nfunc.key), mapper);
                        Expr::PartiallyApplicate(call, params)
                    })
                }
                hir::Callable::Lambda(lambda) if params.is_empty() => {
                    self.fin_inst_or_poison(expr.span, |_, mapper| {
                        let call = Callable::Lambda(*lambda, mapper);
                        Expr::Yield(call)
                    })
                }
                hir::Callable::Lambda(lambda) => {
                    let params = self.lower_exprs(params);
                    self.fin_inst_or_poison(expr.span, |_, mapper| {
                        let call = Callable::Lambda(*lambda, mapper);
                        Expr::PartiallyApplicate(call, params)
                    })
                }
                hir::Callable::Binding(bind) if params.is_empty() => {
                    let ty = self.current.binds[bind].clone();
                    let ty = self.finalizer().transform(&ty);
                    match ty {
                        // Just yield the value as it's already a function
                        Ty::Container(Container::Closure | Container::FnPointer, _) => {
                            Expr::Yield(Callable::Binding(*bind))
                        }
                        _ => {
                            todo!("wrap it in a lambda to turn it *into* a function");
                        }
                    }
                }
                hir::Callable::Binding(bind) => {
                    let params = self.lower_exprs(params);
                    let call = Callable::Binding(*bind);
                    Expr::PartiallyApplicate(call, params)
                }
                hir::Callable::TypeDependentLookup(_) => todo!(),
                hir::Callable::Builtin(_) => todo!(),
            },
            hir::Expr::PassFnptr(fkey, _) => self.fin_inst_or_poison(expr.span, |_, mapper| {
                let nfunc = fkey.map(ast::NFunc::Key);
                let call = Callable::Func(nfunc, mapper);
                Expr::Yield(call)
            }),
            hir::Expr::PassExpr(inner) => {
                let to_call = self.lower_expr((**inner).as_ref());
                // inner == "#(deref v0).funcfield"
                to_call
            }
            hir::Expr::Access(rvar, object, name) => {
                let Some((key, params)) = self.transform_rvar(*rvar) else {
                    return Expr::Poison;
                };

                let object = self.lower_expr((**object).as_ref());

                let Some(field) = self.fnames[key].find(|n| n == name) else {
                    return Expr::Poison;
                };

                Expr::Access(Box::new(object), key, params, field)
            }
            hir::Expr::TupleAccess(object, i) => {
                let object = self.lower_expr((**object).as_ref());
                Expr::TupleAccess(Box::new(object), **i)
            }
            hir::Expr::Record(rvar, _, modified, fields) => {
                let Some((key, params)) = self.transform_rvar(*rvar) else {
                    return Expr::Poison;
                };

                let mut unordered_fields = Vec::with_capacity(fields.len());

                for (name, expr) in fields {
                    let expr = self.lower_expr(expr.as_ref());

                    match self.fnames[key].find(|n| n == name) {
                        Some(field) => {
                            match unordered_fields
                                .iter()
                                .find_map(|(f, span, _)| (*f == field).then_some(*span))
                            {
                                Some(previous) => {
                                    self.errors.push(FinError::DuplicateField(
                                        name.map(str::to_string),
                                        previous,
                                    ));
                                }
                                None => unordered_fields.push((field, name.span, expr)),
                            }
                        }
                        // Should've already errored by type finalization
                        None => {}
                    }
                }

                for field in self.fnames[key].keys() {
                    if unordered_fields.iter().all(|(f, _, _)| *f != field) {
                        match modified {
                            Some(bind) => {
                                let object = Expr::bind(**bind);
                                let expr =
                                    Expr::Access(Box::new(object), key, params.clone(), field);
                                unordered_fields.push((field, bind.span, expr));
                            }
                            None => {
                                let fname = self.fnames[key][field];
                                self.errors
                                    .push(FinError::MissingField(fname.to_string(), expr.span));
                                unordered_fields.push((field, expr.span, Expr::Poison));
                            }
                        }
                    }
                }

                Expr::Record(key, params, unordered_fields)
            }
            hir::Expr::Lit(lit) => self.lower_literal(expr.span, lit),
            hir::Expr::Tuple(elems) => Expr::Tuple(self.lower_exprs(elems)),
            hir::Expr::List(elems, var) => self.lower_list(elems, *var),
            hir::Expr::Array(elems, arr) => self.lower_array(expr.span, elems, *arr),
            hir::Expr::GenericArray(elems, generic) => {
                self.lower_generic_array(expr.span, elems, *generic)
            }
            hir::Expr::Match(on, branches) => {
                self.lower_match(expr.span, (**on).as_ref(), branches)
            }
            hir::Expr::Cast(expr, ty) => {
                let mexpr = self.lower_expr((**expr).as_ref());
                let ty_of_expr = self.current.casts_and_matches.pop_front().unwrap();
                let mut fin = self.finalizer();
                let ty = fin.transform(&ty);
                let ty_of_expr = fin.transform(&ty_of_expr);
                self.lower_cast(mexpr, ty_of_expr.tr(expr.span), ty)
            }
            hir::Expr::Poison => Expr::Poison,
        }
    }

    pub fn transform_rvar(&mut self, rvar: Var) -> Option<(M<key::Record>, Vec<Type>)> {
        let Ty::Container(Container::Defined(key, _), params) = self.finalizer().special(&rvar)
        else {
            return None;
        };
        let M(module, key::TypeKind::Record(rkey)) = key else {
            return None;
        };
        Some((rkey.inside(module), params))
    }

    pub(crate) fn lower_exprs(&mut self, exprs: &[Tr<hir::Expr<'s>>]) -> Vec<Expr> {
        exprs
            .iter()
            .map(|expr| self.lower_expr(expr.as_ref()))
            .collect()
    }

    fn lower_literal(&mut self, span: Span, lit: &hir::Literal<'s>) -> Expr {
        match lit {
            hir::Literal::Bool(b) => Expr::Bool(*b),
            hir::Literal::Int(neg, n, nvar) => {
                let Type::Int(intsize) = self.finalizer().special(nvar) else {
                    return Expr::Poison;
                };
                let n = *n as i128;

                Expr::Int(intsize, neg.then(|| -n).unwrap_or(n))
            }
            hir::Literal::Float(f) => Expr::Float(*f),
            hir::Literal::Char(str) => {
                let str = escape(str);
                if str.len() != 1 {
                    self.errors.push(FinError::LargeCharLiteral(span));
                    Expr::Poison
                } else {
                    let byte = str[0];
                    Expr::Int(IntSize::new(false, 8), byte as i128)
                }
            }
            hir::Literal::String(str) => {
                let ro_key = self.str_to_ro(*str);

                let len = self.read_only_table[ro_key.0].get(ro_key.1).len();

                let stringable = self.items.pinfo.stringable;
                let func = NFunc::Method(*stringable, STRINGABLE_FROM_RAW_PARTS);
                let ptr = Expr::ReadOnly(ro_key);
                let len = Expr::Int(self.target.uint(), len as i128);
                let mapper =
                    GenericMapper::new(vec![], Some(Ty::string(self.items.pinfo.string, vec![])));

                let call = Callable::Func(M(stringable.0, func), mapper);
                Expr::Call(call, vec![ptr, len])
            }
        }
    }

    fn lower_cast(&mut self, expr: mir::Expr, ty_of_expr: Tr<Type>, to: Type) -> Expr {
        let expr = Box::new(expr);
        match (&ty_of_expr.value, to) {
            (Ty::Simple("bool"), Ty::Int(tosize)) => {
                Expr::IntCast(expr, IntSize::new(false, 8), tosize)
            }
            (Ty::Int(fromsize), Ty::Simple("bool")) => {
                Expr::IntCast(expr, *fromsize, IntSize::new(false, 8))
            }
            (Ty::Int(fromsize), Ty::Int(tosize)) => Expr::IntCast(expr, *fromsize, tosize),
            (Ty::Int(intsize), Ty::Simple("f64")) => Expr::ToFloatCast(expr, *intsize),
            (Type::Simple("f64"), Type::Int(intsize)) => Expr::FromFloatCast(expr, intsize),

            // T as DynTrait
            (
                _,
                Type::Container(
                    Container::Defined(M(module, key::TypeKind::Trait(value)), _),
                    params,
                ),
            ) => {
                let trait_ = M(module, value);
                let con = Constraint { span: ty_of_expr.span, trait_, params: params.clone() };
                self.trait_object_cast_checks
                    .push((ty_of_expr.clone(), con));
                Expr::ObjectCast(expr, ty_of_expr.value, trait_, params)
            }

            // int as *a
            (Type::Int(intsize), Type::Container(Container::Pointer, mut inner)) => {
                Expr::ToPointerCast(expr, *intsize, inner.pop().unwrap())
            }

            // *a as int
            (Type::Container(Container::Pointer, _), Type::Int(intsize)) => {
                Expr::FromPointerCast(expr, intsize)
            }

            // *a as *b
            (
                Type::Container(Container::Pointer, _),
                Type::Container(Container::Pointer, mut inner),
            ) => Expr::PointerToPointerCast(expr, inner.pop().unwrap()),

            // *a as [a; n]
            (
                Type::Container(Container::Pointer, pinner),
                Type::Container(Container::Array, mut params),
            ) if pinner[0] == params[0] => {
                let inner = params.remove(0);
                match params.remove(0) {
                    Type::Generic(generic) => Expr::PointerToGenericArrayCast(expr, generic, inner),
                    Type::Const(ConstValue::Usize(len)) => {
                        Expr::PointerToArrayCast(expr, len, inner)
                    }
                    _ => panic!("invalid type parameter to array"),
                }
            }

            (_, to) => {
                self.errors.push(FinError::InvalidCast(ty_of_expr, to));
                Expr::Poison
            }
        }
    }

    fn lower_generic_array(
        &mut self,
        span: Span,
        elems: &[Tr<hir::Expr<'s>>],
        generic: Tr<Generic>,
    ) -> Expr {
        let mut elems = self.lower_exprs(elems);

        if elems.len() == 1 {
            let elem = elems.pop().unwrap();
            let inner = self.current.casts_and_matches.pop_front().unwrap();
            let inner = self.finalizer().transform(&inner);

            Expr::GenericArray(Box::new(elem), *generic, inner)
        } else {
            let got = elems.len().tr(span);
            self.errors.push(FinError::BadGenericArrayCount { got });
            Expr::Poison
        }
    }

    fn lower_array(&mut self, span: Span, elems: &[Tr<hir::Expr<'s>>], len: Tr<u64>) -> Expr {
        if elems.len() as u64 == *len || elems.len() == 1 {
            let elems = self.lower_exprs(elems);

            let inner = self.current.casts_and_matches.pop_front().unwrap();
            let inner = self.finalizer().transform(&inner);

            Expr::Array(elems, *len, inner)
        } else {
            let espan = elems
                .is_empty()
                .then_some(span)
                .unwrap_or_else(|| Span::from_elems(elems, |elem| elem.span));

            self.errors
                .push(FinError::BadArrayCount { got: elems.len().tr(espan), exp: len });

            Expr::Poison
        }
    }

    fn lower_list(&mut self, elems: &[Tr<hir::Expr<'s>>], ivar: Var) -> Expr {
        let type_ = self.items.list_default;

        let inner = self.finalizer().special(&ivar);
        let list_type = Type::list(type_, vec![inner.clone()]);

        let listable = self.items.pinfo.listable;
        let method = |m| listable.map(|trait_| NFunc::Method(trait_, m));

        let mut mapper =
            GenericMapper::from_types(GenericKind::Entity, std::iter::once(inner.clone()));
        mapper.self_ = Some(list_type.clone());

        match elems {
            [] => Expr::Call(Callable::Func(method(LISTABLE_NEW), mapper.clone()), vec![]),
            elems => {
                let new = Expr::Call(
                    Callable::Func(method(LISTABLE_WITH_CAPACITY), mapper.clone()),
                    vec![Expr::Int(self.target.int(), elems.len() as i128)],
                );

                let elems = self.lower_exprs(elems);

                // [1, 2, 3]
                // lowers to
                // Listable:Cons 1 (Listable:Cons 2 (Listable:Cons 3 new))
                elems.into_iter().rev().fold(new, |xs, x| {
                    let call = Callable::Func(method(LISTABLE_CONS), mapper.clone());
                    Expr::Call(call, vec![x, xs])
                })
            }
        }
    }

    fn lower_match(
        &mut self,
        span: Span,
        on: Tr<&hir::Expr<'s>>,
        branches: &[(Tr<hir::Pattern<'s>>, Tr<hir::Expr<'s>>)],
    ) -> Expr {
        let on = self.lower_expr(on);
        let ty = self.current.casts_and_matches.pop_front().unwrap();
        let ty = self.finalizer().transform(&ty);

        let mut iter = branches.iter();
        let (initp, inite) = iter.next().unwrap();

        let mut tails = Map::new();

        let maybe = self.items.pinfo.maybe;
        let string = self.items.pinfo.string;
        let list = self.items.list_default;
        let mut blower = MatchBranchLower::new(self, inite.as_ref(), key::DecisionTreeTail(0));
        let mut tree = blower.first(string, maybe, list, &ty, initp.as_ref());

        let Some(expr) = blower.lowered_tail else {
            warn!("poisoning due to missing tail, assuming error has already occured");
            return Expr::Poison;
        };

        tails.push(expr);

        for (pat, expr) in iter {
            info!("merging {pat} into pattern tree");

            let tailkey = tails.next_key();

            let mut blower = MatchBranchLower::new(self, expr.as_ref(), tailkey);
            let reachable = blower.branch(string, maybe, list, &mut tree, pat.as_ref());

            if !reachable {
                blower
                    .lower
                    .errors
                    .push(FinError::UnreachablePattern(pat.span));
            }

            let expr = blower.lowered_tail.take().unwrap_or(Expr::Poison);

            tails.push_as(tailkey, expr);
        }

        let missing =
            pat::MissingGeneration::new(pat::Init::new(self.ftypes, self.vtypes)).run(&tree);

        if !missing.is_empty() {
            self.errors.push(FinError::MissingPatterns(span, missing));
        }

        assert_ne!(tails.len(), 0);

        // We need to know the predecessor count to know when they should be lowered
        let mut predecessor_count: Map<_, _> = tails.values().map(|_| 0).collect();
        tree.for_each_tail(&mut |tail| match tail {
            pat::TreeTail::Reached(_, _, tail) => predecessor_count[*tail] += 1,
            _ => {}
        });

        Expr::Match(Box::new(on), tree, tails, predecessor_count)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = '('.symbol();
        let cp = ')'.symbol();
        let as_ = "as".keyword();
        let oc = '{'.symbol();
        let cc = '}'.symbol();
        let eq = '='.symbol();
        match self {
            Expr::Call(call, params) if params.is_empty() => {
                write!(f, "{call}")
            }
            Expr::Call(call, params) => {
                write!(f, "{op}{call} {}{cp}", params.iter().format(" "))
            }
            Expr::PartiallyApplicate(call, params) if params.is_empty() => {
                write!(f, "{op}{} {call}{cp}", "partial".keyword(),)
            }
            Expr::PartiallyApplicate(call, params) => {
                write!(
                    f,
                    "{op}{} {call} {}{cp}",
                    "partial".keyword(),
                    params.iter().format(" ")
                )
            }
            Expr::Yield(call) => {
                write!(f, "{call}")
            }
            Expr::Num(instr, p) => write!(f, "{op}{} {} {}{cp}", instr.keyword(), &p[0], &p[1]),
            Expr::IntAbs(n) => write!(f, "{op} {} {n}{cp}", "abs".keyword()),
            Expr::Cmp(instr, p) => write!(f, "{op}{} {} {}{cp}", instr.keyword(), &p[0], &p[1]),
            Expr::Access(object, key, _, field) => write!(f, "({object} {as_} {key}).{field}"),
            Expr::Record(record, ptypes, fields) => write!(
                f,
                "{oc} {record} {} {} {} {cc}",
                ptypes.iter().format(" "),
                '|'.symbol(),
                fields
                    .iter()
                    .map(|(k, _, v)| format!("{k} {} {v}", '='.symbol()))
                    .format(", "),
            ),
            Expr::TupleAccess(p, i) => {
                write!(f, "{op}{} {} {}{cp}", "tuple_get".keyword(), &p, &i)
            }
            Expr::ArrayAccess(p) => {
                write!(f, "{op}{} {} {}{cp}", "array_get".keyword(), &p[0], &p[1])
            }
            Expr::Array(elems, len, _) => {
                write!(f, "[{}; {len}]", elems.iter().format(", "))
            }
            Expr::GenericArray(elem, generic, _) => {
                write!(f, "[{elem}; {generic}]")
            }
            Expr::ArrayLen(p) => {
                write!(f, "{op}{} {p}{cp}", "array_len".keyword())
            }
            Expr::Int(intsize, n) => write!(f, "{n} {as_} {intsize}"),
            Expr::Bool(b) => b.fmt(f),
            Expr::Float(n) => n.fmt(f),
            Expr::ReadOnly(k) => k.fmt(f),
            Expr::Tuple(elems) => write!(f, "{op}{}{cp}", elems.iter().format(", ")),
            Expr::Match(on, tree, tails, _) => match tree {
                // edge-case for formatting `let x = y in` prettily
                pat::DecTree::End(pat::TreeTail::Reached(table, excess, key))
                    if excess.is_empty() && table.binds.len() < 2 =>
                {
                    write!(
                        f,
                        "{} {} {} {on} {}\n  {}",
                        "let".keyword(),
                        table
                            .binds
                            .get(0)
                            .map(|(bind, _)| bind.to_string())
                            .unwrap_or_else(|| String::from("_")),
                        '='.symbol(),
                        "in".keyword(),
                        tails[*key].to_string().lines().format("\n  ")
                    )
                }
                _ => {
                    write!(
                        f,
                        "{} {on}:\n{}\n  {}\n   {}",
                        "match".keyword(),
                        tree,
                        "where".keyword(),
                        tails
                            .iter()
                            .map(|(k, v)| format!(
                                "{k} ->\n     {}",
                                v.to_string().lines().format("\n     ")
                            ))
                            .format("\n   ")
                    )
                }
            },
            Expr::PointerToArrayCast(expr, len, inner) => {
                write!(f, "{op}{expr} {} [{inner}; {len}]{cp}", "as".keyword())
            }
            Expr::PointerToGenericArrayCast(expr, generic, inner) => {
                write!(f, "{op}{expr} {} [{inner}; {generic}]{cp}", "as".keyword())
            }
            Expr::IntCast(expr, _, to) => write!(f, "{op}{expr} {} {to}{cp}", "as".keyword()),
            Expr::ToFloatCast(expr, _) => {
                write!(f, "{op}{expr} {} float{cp}", "as".keyword())
            }
            Expr::FromFloatCast(expr, intsize) => {
                write!(f, "{op}{expr} {} {intsize}{cp}", "as".keyword())
            }
            Expr::ObjectCast(expr, _, tr, params) => {
                write!(
                    f,
                    "{op}{expr} {} {}{}{cp}",
                    "as".keyword(),
                    tr,
                    params.iter().format(" ")
                )
            }
            Expr::ToPointerCast(expr, _, to) | Expr::PointerToPointerCast(expr, to) => {
                write!(f, "{op}{expr} {} *{to}", "as".keyword())
            }
            Expr::FromPointerCast(expr, toint) => {
                write!(f, "{op}{expr} {} {toint}", "as".keyword())
            }
            Expr::ValToRef(val) => write!(f, "{op}{} {val}{cp}", "ref_val".keyword()),
            Expr::Deref(inner) => write!(f, "{op}{} {inner}{cp}", "deref".keyword()),
            Expr::Write(p) => write!(f, "{op}{} {} {}{cp}", "write".keyword(), &p[0], &p[1]),
            Expr::MemCpy(p) => write!(
                f,
                "{op}{} dst{eq}{} src{eq}{} count{eq}{}",
                "memcpy".keyword(),
                p[0],
                p[1],
                p[2]
            ),
            Expr::ReflectTypeOf(ty) => write!(f, "{op}{} {ty}{cp}", "type-of".keyword()),
            Expr::SizeOf(ty) => write!(f, "{op}{} {ty}{cp}", "size-of".keyword()),
            Expr::AlignOf(ty) => write!(f, "{op}{} {ty}{cp}", "align-of".keyword()),
            Expr::Alloca(ty) => write!(f, "{op}{} {ty}{cp}", "alloca".keyword()),
            Expr::Poison => "<poison>".fmt(f),
            Expr::Unreachable(_) => write!(f, "{}", "unreachable".keyword()),
        }
    }
}

impl fmt::Display for Callable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Callable::Func(nfunc, mapper) => write!(f, "{nfunc}{mapper}"),
            Callable::Lambda(lambda, mapper) => write!(f, "{lambda}{mapper}"),
            Callable::Binding(binding) => binding.fmt(f),
            Callable::Param(param) => param.fmt(f),
        }
    }
}
