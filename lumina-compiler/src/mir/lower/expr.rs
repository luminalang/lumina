use super::super::mir::func::Local;
use super::{pat, pat::Merge, ConcreteInst, FinError, Lower, MatchBranchLower};
use crate::prelude::*;
use crate::{LISTABLE_CONS, LISTABLE_NEW, LISTABLE_WITH_CAPACITY};
use ast::NFunc;
use lumina_typesystem::{Bitsize, Constraint, Container, ForeignInst, Prim, Type, Var};
use lumina_util::Highlighting;
use std::fmt;

#[derive(Clone, Debug)]
pub enum Expr {
    CallFunc(M<NFunc>, ConcreteInst, Vec<Self>),
    CallLambda(key::Lambda, ConcreteInst, Vec<Self>),
    CallLocal(Local, Vec<Self>),

    PartialFunc(M<NFunc>, ConcreteInst, Vec<Self>),
    PartialLambda(key::Lambda, ConcreteInst, Vec<Self>),
    PartialLocal(Local, Vec<Self>),

    Yield(Local),
    YieldFunc(M<NFunc>, ConcreteInst),
    YieldLambda(key::Lambda, ConcreteInst),

    Access(Box<Self>, M<key::Record>, Vec<Type>, key::RecordField),
    Record(M<key::Record>, Vec<Type>, Vec<(key::RecordField, Self)>),

    UInt(Bitsize, u128),
    Int(Bitsize, i128),
    Bool(bool),
    Float(f64),
    ReadOnly(M<key::ReadOnly>),

    IntCast(Box<Self>, (bool, Bitsize), (bool, Bitsize)),
    ObjectCast(Box<Self>, Type, M<key::Trait>, Vec<Type>),
    Deref(Box<Self>),
    Write(Box<[Self; 2]>),
    ReflectTypeOf(Type),
    SizeOf(Type),
    Abort,

    Cmp(&'static str, Box<[Expr; 2]>),
    Num(&'static str, Box<[Expr; 2]>),
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
        Expr::Yield(Local::Binding(bind))
    }
}

impl<'a, 's> Lower<'a, 's> {
    fn fin_inst_or_poison(
        &mut self,
        span: Span,
        f: impl FnOnce(&mut Self, ConcreteInst) -> Expr,
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
                    let params = self.lower_exprs(params);
                    self.fin_inst_or_poison(expr.span, |_, inst| {
                        Expr::CallFunc(nfunc.module.m(nfunc.key), inst, params)
                    })
                }
                hir::Callable::Lambda(lambda) => {
                    let params = self.lower_exprs(params);
                    self.fin_inst_or_poison(expr.span, |_, inst| {
                        Expr::CallLambda(*lambda, inst, params)
                    })
                }
                hir::Callable::Binding(bind) => {
                    let ty = self.current.binds[bind].clone();
                    let ty = self.finalizer(|mut fin| (fin.apply(&ty), fin.errors));
                    let local = Local::Binding(*bind);
                    match ty {
                        Type::Container(Container::Func(..)) => {
                            let params = self.lower_exprs(params);
                            Expr::CallLocal(local, params)
                        }
                        _ if params.is_empty() => Expr::Yield(local),
                        _ => Expr::Poison,
                    }
                }
                hir::Callable::TypeDependentLookup(_) => {
                    let params = self.lower_exprs(params);
                    self.fin_inst_or_poison(expr.span, |this, inst| {
                        let nfunc = this.current.type_dependent_lookup.pop_front().unwrap();
                        Expr::CallFunc(nfunc, inst, params)
                    })
                }
                hir::Callable::Builtin(name) => match *name {
                    "plus" => self.lower_builtin(params, |p| Expr::Num("plus", Box::new(p))),
                    "minus" => self.lower_builtin(params, |p| Expr::Num("minus", Box::new(p))),
                    "mul" => self.lower_builtin(params, |p| Expr::Num("mul", Box::new(p))),
                    "div" => self.lower_builtin(params, |p| Expr::Num("div", Box::new(p))),
                    "eq" => self.lower_builtin(params, |p| Expr::Cmp("eq", Box::new(p))),
                    "lt" => self.lower_builtin(params, |p| Expr::Cmp("lt", Box::new(p))),
                    "gt" => self.lower_builtin(params, |p| Expr::Cmp("gt", Box::new(p))),
                    "deref" => self.lower_builtin(params, |[inner]| Expr::Deref(Box::new(inner))),
                    "write" => self.lower_builtin(params, |p| Expr::Write(Box::new(p))),
                    "offset" => self.lower_builtin(params, |p| Expr::Num("plus", Box::new(p))),
                    "abort" => self.lower_builtin::<0>(params, |_| Expr::Abort),
                    "transmute" => self.lower_builtin(params, |[inner]| inner),
                    "val_to_ref" => {
                        self.lower_builtin(params, |[inner]| Expr::ValToRef(Box::new(inner)))
                    }
                    "reflect_type" => {
                        let (name, ty) = tanot.for_entity[0].clone();
                        assert_eq!(*name, "self");
                        let ty = self.finalizer(|mut fin| (fin.apply(&ty), fin.errors));
                        Expr::ReflectTypeOf(ty)
                    }
                    "size_of" => {
                        let (name, ty) = tanot.for_entity[0].clone();
                        assert_eq!(*name, "self");
                        let ty = self.finalizer(|mut fin| (fin.apply(&ty), fin.errors));
                        Expr::SizeOf(ty)
                    }
                    _ => panic!("unknown builtin: {name}"),
                },
            },
            hir::Expr::Pass(call, _, params) => match call {
                hir::Callable::Func(nfunc) => {
                    let params = self.lower_exprs(params);
                    self.fin_inst_or_poison(expr.span, |_, inst| {
                        Expr::PartialFunc(nfunc.module.m(nfunc.key), inst, params)
                    })
                }
                hir::Callable::Lambda(lambda) if params.is_empty() => {
                    self.fin_inst_or_poison(expr.span, |_, inst| Expr::YieldLambda(*lambda, inst))
                }
                hir::Callable::Lambda(lambda) => {
                    let params = self.lower_exprs(params);
                    self.fin_inst_or_poison(expr.span, |_, inst| {
                        Expr::PartialLambda(*lambda, inst, params)
                    })
                }
                hir::Callable::Binding(bind) if params.is_empty() => {
                    Expr::Yield(Local::Binding(*bind))
                }
                hir::Callable::Binding(_) => todo!(),
                hir::Callable::TypeDependentLookup(_) => todo!(),
                hir::Callable::Builtin(_) => todo!(),
            },
            hir::Expr::PassExpr(inner) => {
                let to_call = self.lower_expr((**inner).as_ref());
                // inner == "#(deref v0).funcfield"
                to_call
            }
            hir::Expr::Access(rvar, object, name) => {
                let Some((key, params)) = self.finalizer(|mut fin| (fin.record(*rvar), fin.errors))
                else {
                    return Expr::Poison;
                };
                let object = self.lower_expr((**object).as_ref());

                let Some(field) = self.rsolver.fnames[key].find(|n| n == name) else {
                    return Expr::Poison;
                };

                Expr::Access(Box::new(object), key, params, field)
            }
            hir::Expr::Record(rvar, _, modified, fields) => {
                let Some((key, params)) = self.finalizer(|mut fin| (fin.record(*rvar), fin.errors))
                else {
                    return Expr::Poison;
                };

                let mut unordered_fields = fields
                    .iter()
                    .filter_map(|(name, expr)| {
                        let expr = self.lower_expr(expr.as_ref());

                        match self.rsolver.fnames[key].find(|n| n == name) {
                            Some(field) => Some((field, expr)),
                            None => {
                                // Should've already errored by type finalization
                                // self.errors.push(FinError::FieldNotFound(key, *name));
                                None
                            }
                        }
                    })
                    .collect::<Vec<(key::RecordField, Expr)>>();

                let mut missing_fields = vec![];

                for field in self.rsolver.fnames[key].keys() {
                    if unordered_fields.iter().all(|(f, _)| *f != field) {
                        match modified {
                            Some(bind) => {
                                let object = Expr::bind(**bind);
                                let expr =
                                    Expr::Access(Box::new(object), key, params.clone(), field);
                                unordered_fields.push((field, expr));
                            }
                            None => {
                                missing_fields.push(field);
                                unordered_fields.push((field, Expr::Poison));
                            }
                        }
                    }
                }

                Expr::Record(key, params, unordered_fields)
            }
            hir::Expr::Lit(lit) => self.lower_literal(lit),
            hir::Expr::Tuple(elems) => Expr::Tuple(self.lower_exprs(elems)),
            hir::Expr::List(elems, var) => self.lower_list(elems, *var),
            hir::Expr::Match(on, branches) => {
                self.lower_match(expr.span, (**on).as_ref(), branches)
            }
            hir::Expr::Cast(expr, ty) => {
                let mexpr = self.lower_expr((**expr).as_ref());
                let ty_of_expr = self.current.casts_and_matches.pop_front().unwrap();
                let (ty, ty_of_expr) = self
                    .finalizer(|mut fin| ((fin.apply(&ty), fin.apply(&ty_of_expr)), fin.errors));
                self.lower_cast(mexpr, ty_of_expr.tr(expr.span), ty)
            }
            hir::Expr::Poison => Expr::Poison,
        }
    }

    fn lower_builtin<const N: usize>(
        &mut self,
        params: &[Tr<hir::Expr<'s>>],
        constructor: impl FnOnce([Expr; N]) -> Expr,
    ) -> Expr {
        match <[Expr; N]>::try_from(self.lower_exprs(params)) {
            Ok(params) => constructor(params),
            Err(_) => Expr::Poison,
        }
    }

    fn lower_exprs(&mut self, exprs: &[Tr<hir::Expr<'s>>]) -> Vec<Expr> {
        exprs
            .iter()
            .map(|expr| self.lower_expr(expr.as_ref()))
            .collect()
    }

    fn lower_literal(&mut self, lit: &hir::Literal<'s>) -> Expr {
        match lit {
            hir::Literal::Bool(b) => Expr::Bool(*b),
            hir::Literal::Int(neg, n, nvar) => {
                let Type::Prim(Prim::Int(signed, bitsize)) =
                    self.finalizer(|mut fin| (fin.var(*nvar), fin.errors))
                else {
                    return Expr::Poison;
                };

                match signed {
                    true => Expr::Int(bitsize, neg.then(|| -(*n as i128)).unwrap_or(*n as i128)),
                    false => {
                        assert!(!*neg);
                        Expr::UInt(bitsize, *n)
                    }
                }
            }
            hir::Literal::Float(f) => Expr::Float(*f),
            hir::Literal::String(str) => {
                let ro_key = self.str_to_ro(*str);

                let len = self.read_only_table[ro_key].0 .0.len();

                let func = self.items.pinfo.string_from_raw_parts;
                let ptr = Expr::ReadOnly(ro_key);
                let len = Expr::UInt(Bitsize::default(), len as u128); // TODO: 32-bit target
                let finst =
                    ForeignInst { generics: Map::new(), pgenerics: Map::new(), self_: None };

                Expr::CallFunc(func.map(NFunc::Key), finst, vec![ptr, len])
            }
        }
    }

    fn lower_cast(&mut self, expr: mir::Expr, ty_of_expr: Tr<Type>, to: Type) -> Expr {
        let expr = Box::new(expr);
        match (&ty_of_expr.value, to) {
            (Type::Prim(Prim::Int(froms, fromb)), Type::Prim(Prim::Int(tos, tob))) => {
                Expr::IntCast(expr, (*froms, *fromb), (tos, tob))
            }
            (Type::Prim(Prim::Int(..)), Type::Prim(Prim::Float)) => {
                todo!();
            }
            (Type::Prim(Prim::Float), Type::Prim(Prim::Int(..))) => {
                todo!();
            }
            (_, Type::Defined(M { value: key::TypeKind::Trait(value), module }, params)) => {
                let trait_ = M { value, module };
                let con = Constraint { span: ty_of_expr.span, trait_, params: params.clone() };
                self.env.push_constraint(ty_of_expr.clone(), con);
                Expr::ObjectCast(expr, ty_of_expr.value, trait_, params)
            }
            (Type::Prim(Prim::Int(signed, bitsize)), Type::Container(Container::Pointer(_))) => {
                Expr::IntCast(expr, (*signed, *bitsize), (false, Bitsize(64)))
            }
            (Type::Container(Container::Pointer(_)), Type::Prim(Prim::Int(signed, bitsize))) => {
                Expr::IntCast(expr, (false, Bitsize(64)), (signed, bitsize))
            }
            (Type::Container(Container::Pointer(_)), Type::Container(Container::Pointer(_))) => {
                // TODO: this cast does nothing. But; we still put it here because we need an expression
                Expr::IntCast(expr, (false, Bitsize(64)), (false, Bitsize(64)))
            }
            (_, to) => {
                self.errors.push(FinError::InvalidCast(ty_of_expr, to));
                Expr::Poison
            }
        }
    }

    fn lower_list(&mut self, elems: &[Tr<hir::Expr<'s>>], ivar: Var) -> Expr {
        let type_ = self.items.list_default;

        let inner = self.finalizer(|mut fin| (fin.var(ivar), fin.errors));
        let list_type = Type::List(type_, vec![inner.clone()]);

        let listable = self.items.pinfo.listable;
        let method = |m| listable.module.m(NFunc::Method(listable.value, m));

        let inst = || ConcreteInst {
            pgenerics: [inner.clone()].into_iter().collect(),
            generics: Map::new(),
            self_: Some(list_type.clone()),
        };

        match elems {
            [] => Expr::CallFunc(method(LISTABLE_NEW), inst(), vec![]),
            elems => {
                let new = Expr::CallFunc(
                    method(LISTABLE_WITH_CAPACITY),
                    inst(),
                    vec![Expr::Int(Bitsize::default(), elems.len() as i128)],
                );

                let elems = self.lower_exprs(elems);

                // [1, 2, 3]
                // lowers to
                // Listable:Cons 1 (Listable:Cons 2 (Listable:Cons 3 new))
                elems.into_iter().rev().fold(new, |xs, x| {
                    Expr::CallFunc(method(LISTABLE_CONS), inst(), vec![x, xs])
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
        let ty = self.finalizer(|mut fin| {
            let ty = fin.apply(&ty);
            (ty, fin.errors)
        });

        let mut iter = branches.iter();
        let (initp, inite) = iter.next().unwrap();

        let mut tails = Map::new();

        let mut blower = MatchBranchLower::new(self, inite.as_ref(), key::DecisionTreeTail(0));
        let mut tree = blower.first(&ty, initp.as_ref());

        let Some(expr) = blower.lowered_tail else {
            return Expr::Poison;
        };

        tails.push(expr);

        for (pat, expr) in iter {
            info!("merging {pat} into pattern tree");

            let tailkey = tails.next_key();

            let mut blower = MatchBranchLower::new(self, expr.as_ref(), tailkey);
            let reachable = blower.branch(&mut tree, pat.as_ref());

            if !reachable {
                blower
                    .lower
                    .errors
                    .push(FinError::UnreachablePattern(pat.span));
            }

            let expr = blower.lowered_tail.unwrap();
            tails.push_as(tailkey, expr);
        }

        let missing = pat::MissingGeneration::new(pat::Init::new(self.rsolver.ftypes, self.vtypes))
            .run(&tree);

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
        match self {
            Expr::CallFunc(call, inst, params) => {
                write!(f, "{op}{call}({inst}) {}{cp}", params.iter().format(" "))
            }
            Expr::CallLambda(lambda, inst, params) => {
                write!(f, "{op}{lambda}({inst}) {}{cc}", params.iter().format(" "))
            }
            Expr::CallLocal(local, params) => {
                write!(f, "{op}{local} {}{cc}", params.iter().format(" "))
            }
            Expr::PartialFunc(call, inst, params) => {
                write!(
                    f,
                    "{op}{} {call}({inst}) {}{cp}",
                    "partial".keyword(),
                    params.iter().format(" ")
                )
            }
            Expr::PartialLocal(local, params) => {
                write!(
                    f,
                    "{op}{} {local} {}{cp}",
                    "partial".keyword(),
                    params.iter().format(" ")
                )
            }
            Expr::PartialLambda(lambda, inst, params) => {
                write!(
                    f,
                    "{op}{} {lambda}({inst}) {}{cp}",
                    "partial".keyword(),
                    params.iter().format(" ")
                )
            }
            Expr::Yield(local) => write!(f, "{local}"),
            Expr::YieldFunc(func, inst) => write!(f, "#{func}({inst})"),
            Expr::YieldLambda(lkey, inst) => write!(f, "#{lkey}({inst})"),
            Expr::Num(instr, p) => write!(f, "{op}{} {} {}{cp}", instr.keyword(), &p[0], &p[1]),
            Expr::Cmp(instr, p) => write!(f, "{op}{} {} {}{cp}", instr.keyword(), &p[0], &p[1]),
            Expr::Access(object, key, _, field) => write!(f, "({object} {as_} {key}).{field}"),
            Expr::Record(record, ptypes, fields) => write!(
                f,
                "{oc} {record} {} {} {} {cc}",
                ptypes.iter().format(" "),
                '|'.symbol(),
                fields
                    .iter()
                    .map(|(k, v)| format!("{k} {} {v}", '='.symbol()))
                    .format(", "),
            ),
            Expr::UInt(bitsize, n) => write!(f, "{n} {as_} u{bitsize}"),
            Expr::Int(bitsize, n) => write!(f, "{n} {as_} i{bitsize}"),
            Expr::Bool(b) => b.fmt(f),
            Expr::Float(n) => n.fmt(f),
            Expr::ReadOnly(k) => k.fmt(f),
            Expr::Tuple(elems) => write!(f, "{op}{}{cp}", elems.iter().format(", ")),
            Expr::Match(on, tree, tails, _) => match tree {
                // edge-case for formatting `let x = y in` prettily
                pat::DecTree::End(pat::TreeTail::Reached(table, excess, key))
                    if excess.is_empty() && table.binds.len() == 1 =>
                {
                    write!(
                        f,
                        "{} {} {} {on} {}\n  {}",
                        "let".keyword(),
                        table.binds[0].0,
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
            Expr::IntCast(expr, _, to) => write!(
                f,
                "{op}{expr} {} {}{}{cp}",
                "as".keyword(),
                to.0.then_some('i').unwrap_or('u'),
                to.1
            ),
            Expr::ObjectCast(expr, _, tr, params) => {
                write!(
                    f,
                    "{op}{expr} {} {}{}{cp}",
                    "as".keyword(),
                    tr,
                    params.iter().format(" ")
                )
            }
            Expr::ValToRef(val) => write!(f, "{op}{} {val}{cp}", "ref_val".keyword()),
            Expr::Deref(inner) => write!(f, "{op}{} {inner}{cp}", "deref".keyword()),
            Expr::Write(p) => write!(f, "{op}{} {} {}{cp}", "write".keyword(), &p[0], &p[1]),
            Expr::ReflectTypeOf(ty) => write!(f, "{op}{} {ty}{cp}", "type-of".keyword()),
            Expr::SizeOf(ty) => write!(f, "{op}{} {ty}{cp}", "size-of".keyword()),
            Expr::Poison => "<poison>".fmt(f),
            Expr::Abort => write!(f, "{}", "abort".keyword()),
        }
    }
}
