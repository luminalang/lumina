use super::pat::{PatLower, Pattern};
use super::ty::{TypeAnnotation, TypeEnvInfo, TypeLower};
use super::{scope::Bindings, Lambdas};
use crate::prelude::*;
use crate::Target;
use ast::{Entity, Mod, NFunc};
use derive_more::Display;
use derive_more::From;
use lumina_parser as parser;
use lumina_typesystem::{Forall, IType, RecordVar, TEnv, Var};
use lumina_util::Highlighting;
use std::fmt;
use tracing::trace;

#[derive(new)]
#[rustfmt::skip]
pub struct ExprLower<'t, 'a, 's> {
    pub module: key::Module,
    pub ast: &'a ast::AST<'s>,

    pub type_info: &'t mut TypeEnvInfo<'s>,
    where_binds: &'a [parser::func::Declaration<'s>],

    pub target: Target,

    #[new(default)]
    pub bindings: Bindings<'s>,
    #[new(default)]
    pub lambdas: Lambdas<'s>,
}

#[derive(Clone, Debug)]
pub enum Expr<'s> {
    Call(Callable<'s>, TypeAnnotation<'s>, Vec<Tr<Self>>),
    Pass(Callable<'s>, TypeAnnotation<'s>, Vec<Tr<Self>>),
    PassFnptr(M<key::Func>, TypeAnnotation<'s>),

    PassExpr(Box<Tr<Self>>),

    Access(RecordVar, Box<Tr<Self>>, Tr<&'s str>),
    Record(
        RecordVar,
        Option<Tr<IType>>,
        Option<Tr<key::Bind>>,
        Vec<(Tr<&'s str>, Tr<Self>)>,
    ),
    Lit(Literal<'s>),
    Tuple(Vec<Tr<Self>>),
    List(Vec<Tr<Self>>, Var),
    Match(Box<Tr<Self>>, Vec<(Tr<Pattern<'s>>, Tr<Self>)>),
    Cast(Box<Tr<Self>>, Tr<IType>),
    Poison,
}

impl<'s> Expr<'s> {
    pub fn deref(expr: Tr<Expr<'s>>) -> Self {
        hir::Expr::Call("deref".into(), TypeAnnotation::new(), vec![expr])
    }

    pub fn callable(callable: impl Into<Callable<'s>>) -> Self {
        Self::Call(callable.into().into(), TypeAnnotation::new(), vec![])
    }

    pub fn let_bind(expr: Tr<Expr<'s>>, pat: Tr<Pattern<'s>>, then: Tr<Expr<'s>>) -> Self {
        Expr::Match(Box::new(expr), vec![(pat, then)])
    }
}

#[derive(From, Clone, Display, Debug)]
pub enum Callable<'s> {
    #[from]
    Func(Mod<NFunc>),
    #[from]
    Lambda(key::Lambda),
    #[from]
    Binding(key::Bind),
    TypeDependentLookup(&'s str),
    #[from]
    Builtin(&'s str),
}

#[derive(Clone, Debug)]
pub enum Literal<'s> {
    Bool(bool),
    Int(bool, u128, Var),
    Float(f64),
    String(&'s str),
}

impl<'t, 'a, 's> ExprLower<'t, 'a, 's> {
    pub fn patterns(&mut self, patterns: &[Tr<parser::Pattern<'s>>]) -> Vec<Tr<Pattern<'s>>> {
        patterns
            .iter()
            .map(|p| self.to_patlower().pat(p.as_ref()))
            .collect::<Vec<_>>()
    }

    pub fn expr(&mut self, expr: Tr<&parser::Expr<'s>>) -> Tr<Expr<'s>> {
        trace!("lowering expression {expr}");

        match expr.value {
            parser::Expr::Lit(parser::Literal::Int(neg, n)) => {
                let var = self.vars().int(expr.span);
                self.vars().hint_min_int(var, *neg, *n);

                Expr::Lit(Literal::Int(*neg, *n, var))
            }
            parser::Expr::Lit(parser::Literal::Float(n)) => Expr::Lit(Literal::Float(*n)),
            parser::Expr::Lit(parser::Literal::String(str)) => Expr::Lit(Literal::String(*str)),
            parser::Expr::Call(apath, params, _) => {
                self.callable(apath.as_ref(), params, Expr::Call)
            }
            parser::Expr::Lambda(patterns, params, body, _seal) => {
                let lambda = self.lambda(patterns, None, (**body).as_ref());
                let params = self.exprs(params);
                Expr::Call(lambda.into(), TypeAnnotation::new(), params)
            }

            // (f 0) 1
            // -------
            // let _ = #(f 0) in _ 1
            parser::Expr::CallExpr(inner, params, _) => {
                let value = Expr::PassExpr(Box::new(self.expr((**inner).as_ref()))).tr(inner.span);
                let params = self.exprs(params);

                let bind = self.bindings.declare_nameless();
                let pat = Pattern::Bind(bind, Box::new(Pattern::Any)).tr(inner.span);

                let and_then =
                    hir::Expr::Call(bind.into(), TypeAnnotation::new(), params).tr(expr.span);

                Expr::let_bind(value, pat, and_then)
            }
            parser::Expr::Operators { init, ops } => self.operators((**init).as_ref(), ops),
            parser::Expr::FieldAccess(object, name) => {
                let object = self.expr((**object).as_ref());
                let rvar = self.vars().record(object.span);
                Expr::Access(rvar, Box::new(object), *name)
            }
            parser::Expr::DotPipe(elems) => {
                let [left, right] = &**elems;
                self.dotpipe(expr.span, left.as_ref(), right.as_ref())
            }
            parser::Expr::Match(on, branches) => {
                let on = self.expr(on.as_ref().as_ref());

                let branches = branches
                    .iter()
                    .map(|(pat, then)| {
                        self.bindings.enter();
                        let pat = self.to_patlower().pat(pat.as_ref());
                        let then = self.expr(then.as_ref());
                        self.bindings.leave();
                        (pat, then)
                    })
                    .collect();

                Expr::Match(Box::new(on), branches)
            }
            parser::Expr::List(elems) => {
                let elems = self.exprs(elems);
                let ivar = self
                    .vars()
                    .var(elems.get(0).map(|elem| elem.span).unwrap_or(expr.span));
                Expr::List(elems, ivar)
            }
            parser::Expr::Tuple(elems) => {
                let elems = self.exprs(elems);
                Expr::Tuple(elems)
            }
            parser::Expr::Record { init, fields } => self.desugar_record(expr.span, init, fields),
            parser::Expr::If(elems) => {
                let cond = self.expr(elems[0].as_ref());

                let truthy = self.expr(elems[1].as_ref());
                let falsey = self.expr(elems[2].as_ref());

                let true_ = Pattern::Bool(true).tr(truthy.span);
                let false_ = Pattern::Bool(false).tr(falsey.span);

                Expr::Match(Box::new(cond), vec![(true_, truthy), (false_, falsey)])
            }
            parser::Expr::Do(elems) => {
                let [discarded, used] = self.expr_boxed(&elems);

                // let bind = self.bindings.declare_nameless();
                // let void = Pattern::Bind(bind, Box::new(Pattern::Any)).tr(expr.span);
                let void = Pattern::Any.tr(expr.span);

                Expr::let_bind(discarded, void, used)
            }
            parser::Expr::Let(pat, elems) => {
                let value = self.expr(elems[0].as_ref());

                let pat = self.to_patlower().pat(pat.as_ref());
                self.bindings.enter();
                let then = self.expr(elems[1].as_ref());
                self.bindings.leave();

                Expr::let_bind(value, pat, then)
            }
            parser::Expr::Pass(inner) => self.pass((**inner).as_ref()),
            parser::Expr::PassFptr(path) => self.pass_fnptr(expr.span, path),
            parser::Expr::CastAs(expr, ty) => {
                let expr = self.expr((**expr).as_ref());
                let ty = self.to_type_lower().ty_spanned(ty.as_ref());
                hir::Expr::Cast(Box::new(expr), ty)
            }
            parser::Expr::Poison => Expr::Poison,
        }
        .tr(expr.span)
    }

    fn pass(&mut self, inner: Tr<&parser::Expr<'s>>) -> Expr<'s> {
        match &inner.value {
            parser::Expr::Call(apath, params, seal) if *seal < 2 => {
                self.callable(apath.as_ref(), params, Expr::Pass)
            }
            parser::Expr::Call(apath, params, _) => {
                // #((f a))
                // lowers to
                // let _ = f a in #_
                let expr = self.callable(apath.as_ref(), params, Expr::Call);
                let bind = self.bindings.declare_nameless();
                let pat = Pattern::Bind(bind, Box::new(Pattern::Any)).tr(apath.span);
                let and_then =
                    Expr::Pass(bind.into(), TypeAnnotation::new(), vec![]).tr(apath.span);
                Expr::let_bind(expr.tr(apath.span), pat, and_then)
            }
            parser::Expr::Lambda(patterns, params, body, seal) if *seal == 1 => {
                assert!(params.is_empty(), "does the parser allow parameters here?");
                warn!(
                    "TODO: add type annotation to the parser and give the typing here for lambdas"
                );
                let lambda = self.lambda(patterns, None, (**body).as_ref());
                let params = self.exprs(params);
                Expr::Pass(lambda.into(), TypeAnnotation::new(), params)
            }
            parser::Expr::Lambda(_, _, _, seal) => {
                assert_ne!(*seal, 0);
                todo!("evaluate and then pass the returned value from evaluation");
            }
            parser::Expr::Pass(inner) => {
                let inner = self.expr((**inner).as_ref());
                Expr::PassExpr(Box::new(inner))
            }
            parser::Expr::CallExpr(_, _, _) => todo!(""),

            _ => {
                let lkey = self.lambda(&[], None, inner);
                Expr::Pass(lkey.into(), TypeAnnotation::new(), vec![])
            }
        }
    }

    fn pass_fnptr(&mut self, span: Span, apath: &parser::AnnotatedPath<'s>) -> Expr<'s> {
        let segments = apath.path.as_slice();
        match self.ast.lookups.resolve_func(self.module, segments) {
            Ok(Mod { key: ast::Entity::Func(ast::NFunc::Key(key)), module, .. }) => {
                let tanot = self.type_annotation(apath.tr(span), None);
                Expr::PassFnptr(module.m(key), tanot)
            }
            Ok(_) => {
                self.ast
                    .sources
                    .error("invalid function pointer")
                    .m(self.module)
                    .eline(
                        span,
                        "only non-method functions may be turned into function pointers",
                    )
                    .emit();

                Expr::Poison
            }
            Err(err) => {
                self.ast
                    .sources
                    .emit_lookup_err(span, self.module, "function", err);

                Expr::Poison
            }
        }
    }

    fn dotpipe(
        &mut self,
        span: Span,
        left: Tr<&parser::Expr<'s>>,
        right: Tr<&parser::Expr<'s>>,
    ) -> Expr<'s> {
        match right.value {
            parser::Expr::Call(apath, params, _) => {
                let left = self.expr(left);

                let path = apath.path.as_slice();

                if path.len() == 1 {
                    match path[0] {
                        "true" | "false" => panic!("ET: invalid"),
                        "_" => panic!("ET: usage of `_` is not allowed"),
                        name => {
                            if let Some(bind) = self.bindings.resolve(name) {
                                let mut params = self.exprs(params);
                                params.push(left);
                                self.forbid_type_annotation(&apath.for_segments);
                                return Expr::Call(bind.into(), TypeAnnotation::new(), params);
                            }

                            let mut params = self.exprs(params);
                            params.push(left);

                            let type_annotation = self.type_annotation(apath.as_ref(), None);

                            if let Some(lkey) =
                                self.where_binds.find(|decl| *decl.header.name == name)
                            {
                                self.bindings.reference_lambda(lkey);
                                let c = Callable::Lambda(lkey);
                                return Expr::Call(c, type_annotation, params);
                            }

                            Expr::Call(Callable::TypeDependentLookup(name), type_annotation, params)
                        }
                    }
                } else {
                    self.callable(apath.as_ref(), params, |callable, tanot, mut params| {
                        params.push(left);
                        Expr::Call(callable, tanot, params)
                    })
                }
            }
            _ => {
                self.ast
                    .sources
                    .error("invalid pipe operator")
                    .m(self.module)
                    .eline(span, "dot pipes may only be used to applicate functions")
                    .emit();

                Expr::Poison
            }
        }
    }

    fn lambda(
        &mut self,
        apatterns: &[Tr<parser::Pattern<'s>>],
        typing: Option<parser::func::Typing<'s>>,
        body: Tr<&parser::Expr<'s>>,
    ) -> key::Lambda {
        let mut tvar =
            |span| IType::infer(self.type_info.inference_mut().unwrap().var(span)).tr(span);

        let typing = match typing {
            None => hir::Typing::new(
                apatterns.iter().map(|p| tvar(p.span)).collect(),
                tvar(body.span),
            ),
            Some(typing) => {
                let mut tlower = self.to_type_lower();
                hir::Typing {
                    params: tlower.tys_spanned(&typing.ptypes),
                    returns: tlower.ty_spanned(typing.returns.as_ref()),
                }
            }
        };

        let lkey = self
            .lambdas
            .create_placeholder(body.span, Forall::new(0), typing);
        self.bindings.reference_lambda(lkey);

        self.bindings.enter();
        self.type_info.enter_lambda(lkey, Forall::new(0));

        let patterns = self.patterns(apatterns);
        let expr = self.expr(body);

        let (captures, lcaptures) = self.bindings.leave();
        let forall = self.type_info.leave_function();
        assert!(forall.generics.is_empty());

        self.lambdas
            .complete_lambda(lkey, expr, patterns, captures, lcaptures);

        lkey
    }

    pub fn vars(&mut self) -> &mut TEnv<'s> {
        self.type_info.inference_mut().unwrap()
    }

    fn operators(
        &mut self,
        init: Tr<&parser::Expr<'s>>,
        ops: &[(Tr<&'s str>, Tr<parser::Expr<'s>>)],
    ) -> Expr<'s> {
        let init = Side::Tail(init);

        let op = ops.iter().fold(init, |lhs, (op, rhs)| {
            match self.ast.lookups.resolve_func(self.module, &[**op]) {
                Ok(entity) => match entity.key {
                    Entity::Func(func) => {
                        let precedence = match func {
                            NFunc::Key(fkey) => {
                                self.ast.entities.fattributes[entity.module.m(fkey)].precedence
                            }
                            NFunc::Method(trait_, method) => {
                                let fkey =
                                    self.ast.entities.methods[entity.module.m(trait_)][method];
                                self.ast.entities.fattributes[entity.module.m(fkey)].precedence
                            }
                            NFunc::SumVar(_, _) => todo!(),
                            NFunc::Val(_) => todo!(),
                        }
                        .expect("non-operator used as operator");

                        let func = entity.map(|_| func);

                        match lhs {
                            Side::Op(mut lop) => {
                                if precedence > lop.precedence {
                                    let previous_rhs = lop.sides[1].clone();
                                    let this = OpTree {
                                        func,
                                        precedence,
                                        raw: *op,
                                        sides: Box::new([previous_rhs, Side::Tail(rhs.as_ref())]),
                                    };
                                    lop.sides[1] = Side::Op(this);
                                    Side::Op(lop)
                                } else {
                                    let this = OpTree {
                                        func,
                                        precedence,
                                        raw: *op,
                                        sides: Box::new([Side::Op(lop), Side::Tail(rhs.as_ref())]),
                                    };
                                    Side::Op(this)
                                }
                            }
                            Side::Tail(lhs) => Side::Op(OpTree {
                                func,
                                precedence,
                                raw: *op,
                                sides: Box::new([Side::Tail(lhs), Side::Tail(rhs.as_ref())]),
                            }),
                        }
                    }
                    _ => {
                        self.ast.sources.emit_wrong_entity(
                            self.module,
                            op.span,
                            **op,
                            "operator",
                            entity.key,
                        );
                        return Side::Tail((&parser::Expr::Poison).tr(op.span));
                    }
                },
                Err(err) => {
                    self.ast
                        .sources
                        .emit_lookup_err(op.span, self.module, "operator", err);
                    return Side::Tail((&parser::Expr::Poison).tr(op.span));
                }
            }
        });

        self.fold_optree_side(op).value
    }

    fn fold_optree<'o>(&mut self, op: OpTree<'o, 's>) -> Expr<'s> {
        let [left, right] = *op.sides;
        let left = self.fold_optree_side(left);
        let right = self.fold_optree_side(right);
        Expr::Call(op.func.into(), TypeAnnotation::new(), vec![left, right])
    }

    fn fold_optree_side<'o>(&mut self, side: Side<'o, 's>) -> Tr<Expr<'s>> {
        match side {
            Side::Op(op) => {
                let span = op.raw.span;
                self.fold_optree(op).tr(span)
            }
            Side::Tail(edge) => self.expr(edge),
        }
    }

    fn callable(
        &mut self,
        apath: Tr<&parser::AnnotatedPath<'s>>,
        params: &[Tr<parser::Expr<'s>>],
        to_out: impl FnOnce(Callable<'s>, TypeAnnotation<'s>, Vec<Tr<Expr<'s>>>) -> Expr<'s>,
    ) -> Expr<'s> {
        let path = apath.path.as_slice();

        match path {
            ["true"] => {
                self.forbid_type_annotation(&apath.for_segments);
                self.forbid_parameters(params);
                return Expr::Lit(Literal::Bool(true));
            }
            ["false"] => {
                self.forbid_type_annotation(&apath.for_segments);
                self.forbid_parameters(params);
                return Expr::Lit(Literal::Bool(false));
            }
            ["builtin", name] => {
                let params = self.exprs(params);
                let type_annotation = self.type_annotation(apath, None);
                return to_out(Callable::Builtin(name), type_annotation, params);
            }
            [name] => {
                if let Some(bind) = self.bindings.resolve(name) {
                    let params = self.exprs(params);
                    self.forbid_type_annotation(&apath.for_segments);
                    return to_out(bind.into(), TypeAnnotation::new(), params);
                }

                if let Some(lkey) = self.where_binds.find(|decl| *decl.header.name == *name) {
                    self.bindings.reference_lambda(lkey);
                    let params = self.exprs(params);
                    let type_annotation = self.type_annotation(apath, None);
                    return to_out(Callable::Lambda(lkey), type_annotation, params);
                }
            }
            _ => {}
        }

        match self.ast.lookups.resolve_func(self.module, path) {
            Ok(entity) => match entity.key {
                Entity::Func(nfunc) => {
                    let params = self.exprs(params);
                    let type_ = match nfunc {
                        NFunc::SumVar(key, _) => Some(entity.module.m(key::TypeKind::Sum(key))),
                        _ => None,
                    };
                    let type_annotation = self.type_annotation(apath, type_);
                    to_out(entity.map(|_| nfunc).into(), type_annotation, params)
                }
                Entity::Member(type_, name) => {
                    let module = entity.module;
                    let ty = self.ast.entities.header_of_ty(module.m(type_));
                    let tname = ty.header.name;

                    let nfunc = match type_ {
                        key::TypeKind::Trait(trait_) => {
                            let methods = &self.ast.entities.methods[module.m(trait_)];
                            match methods.find(|fkey| {
                                *self.ast.entities.fheaders[module.m(*fkey)].name == name
                            }) {
                                None => {
                                    return self
                                        .emit_member_not_found(apath.span, "trait", tname, name)
                                }
                                Some(method) => NFunc::Method(trait_, method),
                            }
                        }
                        key::TypeKind::Sum(sum) => {
                            let variants = &self.ast.entities.variant_names[module.m(sum)];
                            match variants.find(|n| **n == name) {
                                None => {
                                    return self
                                        .emit_member_not_found(apath.span, "type", tname, name)
                                }
                                Some(var) => NFunc::SumVar(sum, var),
                            }
                        }
                        key::TypeKind::Record(key) => {
                            let is_valid_field = self.ast.entities.field_names[module.m(key)]
                                .values()
                                .any(|n| **n == name);

                            return self.emit_identifier_not_found(apath.span, if is_valid_field {format!(
                                "if you're trying to use the field named `{name}` from `{tname}`, then use `.{name}`",
                            )} else {
                                format!("tried to use the type `{tname}` as a module")
                            });
                        }
                    };

                    let params = self.exprs(params);
                    let call = entity.map(|_| nfunc);
                    let type_annotation = self.type_annotation(apath, Some(call.module.m(type_)));

                    to_out(call.into(), type_annotation, params)
                }
                _ => {
                    let name = path[path.len() - 1];
                    self.ast.sources.emit_wrong_entity(
                        self.module,
                        apath.span,
                        name,
                        "function",
                        entity.key,
                    );

                    Expr::Poison
                }
            },
            Err(err) => {
                self.ast.sources.emit_lookup_err(
                    apath.span,
                    self.module,
                    "function or binding",
                    err,
                );

                Expr::Poison
            }
        }
    }

    fn emit_identifier_not_found(&self, span: Span, str: impl Into<String>) -> Expr<'s> {
        self.ast
            .sources
            .error("identifier not found")
            .m(self.module)
            .eline(span, str)
            .emit();

        Expr::Poison
    }

    fn emit_member_not_found(&self, span: Span, kind: &str, tname: &str, name: &str) -> Expr<'s> {
        self.emit_identifier_not_found(
            span,
            format!("the {kind} {tname} does not have a member named {name}"),
        )
    }

    fn emit_syntax_error<T>(&mut self, span: Span, msg: impl Into<String>) -> Option<T> {
        self.ast
            .sources
            .error("syntax error")
            .m(self.module)
            .eline(span, msg)
            .emit();

        None
    }

    fn forbid_parameters(&mut self, params: &[Tr<parser::Expr<'s>>]) {
        if let Some(p) = params.first() {
            self.ast
                .sources
                .error("syntax error")
                .m(self.module)
                .eline(p.span, "unexpected expression")
                .emit();
        }
    }

    fn forbid_type_annotation(&mut self, apath: &[(usize, parser::ty::ForallAnnotation<'s>)]) {
        if let Some((_, anot)) = apath.first() {
            let (span, _, _) = anot.assignments.first().unwrap();
            Self::emit_invalid_type_annotation(self.module, &self.ast.sources, *span);
        }
    }

    fn emit_invalid_type_annotation(module: key::Module, sources: &ast::Sources, span: Span) {
        sources
            .error("invalid type annotation")
            .m(module)
            .eline(span, "type annotations not allowed in this context")
            .emit();
    }

    fn type_annotation(
        &mut self,
        apath: Tr<&parser::AnnotatedPath<'s>>,
        type_: Option<M<key::TypeKind>>,
    ) -> TypeAnnotation<'s> {
        let len = apath.path.as_slice().len();

        let mut type_anot = TypeAnnotation::new();

        let int_size = self.target.int_size();
        let mut tlower = TypeLower::new(self.module, self.ast, int_size, &mut self.type_info);

        for (at, anot) in &apath.for_segments {
            if *at == (len - 1) {
                for (span, name, ty) in &anot.assignments {
                    let ty = tlower.ty(ty.tr(*span));
                    type_anot.for_entity.push(((*name).tr(*span), ty));
                }
                continue;
            }

            if *at == (len - 2) {
                if let Some(_) = type_ {
                    for (span, name, ty) in &anot.assignments {
                        let ty = tlower.ty(ty.tr(*span));
                        type_anot.for_type.push(((*name).tr(*span), ty));
                    }

                    continue;
                }
            }

            let span = anot.assignments[0].0;
            Self::emit_invalid_type_annotation(self.module, &self.ast.sources, span);
        }

        type_anot
    }

    pub fn to_patlower(&mut self) -> PatLower<'_, 's> {
        PatLower::new(
            self.module,
            self.ast,
            self.target.int_size(),
            self.type_info,
            &mut self.bindings,
        )
    }

    fn desugar_record(
        &mut self,
        span: Span,
        init: &parser::CurlyInit<'s>,
        fields: &parser::Fields<'s, parser::Expr<'s>>,
    ) -> Expr<'s> {
        let var = self.vars().record(span);

        match init {
            parser::CurlyInit::Construct(ty) => {
                let int_size = self.target.int_size();
                let ty = TypeLower::new(self.module, self.ast, int_size, &mut self.type_info)
                    .ty_spanned(ty.as_ref());

                let fields = self.record_fields(fields, None);

                Expr::Record(var, Some(ty), None, fields)
            }
            parser::CurlyInit::Modify(modified) => {
                let modified = self.expr(modified.as_ref());

                // edge-case to not produce `let v0 = v1 in ...` unnecessarily
                match &modified.value {
                    Expr::Call(Callable::Binding(bind), _, params) if params.is_empty() => {
                        let modified = (*bind).tr(modified.span);
                        let fields = self.record_fields(fields, Some(modified));
                        Expr::Record(var, None, Some(modified), fields)
                    }
                    _ => {
                        let modpoint = self.bindings.declare_nameless();
                        let modspan = modified.span;

                        let fields = self.record_fields(fields, Some(modpoint.tr(modspan)));

                        Expr::let_bind(
                            modified,
                            Pattern::Bind(modpoint, Box::new(Pattern::Any)).tr(modspan),
                            Expr::Record(var, None, Some(modpoint.tr(modspan)), fields).tr(span),
                        )
                    }
                }
            }
            parser::CurlyInit::None => {
                let fields = self.record_fields(fields, None);
                Expr::Record(var, None, None, fields)
            }
        }
    }

    fn expr_boxed<const N: usize>(
        &mut self,
        elems: &[Tr<parser::Expr<'s>>; N],
    ) -> [Tr<Expr<'s>>; N] {
        std::array::from_fn(|i| self.expr(elems[i].as_ref()))
    }

    fn exprs(&mut self, exprs: &[Tr<parser::Expr<'s>>]) -> Vec<Tr<Expr<'s>>> {
        exprs.iter().map(|expr| self.expr(expr.as_ref())).collect()
    }

    // { t | x, y }
    // desugars to
    // { t | x = x, y = y }
    //
    // { v ~ a.b.c @ n = n + 1 }
    // desugars to
    // { v ~ a @ '0 = { '0 ~ b @ '1 = { '1 ~ c = let n = '1.c in n + 1 } } }
    // desugars to
    // { v ~ a = let '0 = v.a in { '0 ~ b = let '1 = '0.b in { '1 ~ c = let n = '1.c in n + 1 } } }
    //
    // { v ~ a.b.c = 1 }
    // desugars to
    // { v ~ a @ '0 = { '0 ~ b @ '1 = { '1 ~ c = 1 } } }
    // desugars to
    // { v ~ a = let '0 = v.a in { '0 ~ b = let '1 = '0.b in { '1 ~ c = 1 } } }
    fn record_fields(
        &mut self,
        fields: &parser::Fields<'s, parser::Expr<'s>>,
        modified: Option<Tr<key::Bind>>,
    ) -> Vec<(Tr<&'s str>, Tr<Expr<'s>>)> {
        fields
            .iter()
            .filter_map(|field| self.record_field(field, modified))
            .collect()
    }

    fn record_field(
        &mut self,
        field: &parser::Field<'s, parser::Expr<'s>>,
        modified: Option<Tr<key::Bind>>,
    ) -> Option<(Tr<&'s str>, Tr<Expr<'s>>)> {
        match field {
            parser::Field::Value(v) => self.emit_syntax_error(v.span, "expected field name"),
            parser::Field::Punned(names) if names.len() != 1 => self.emit_syntax_error(
                names[1].span,
                "nested record fields are only allowed in patterns, not expressions",
            ),
            parser::Field::Punned(names) => match self.bindings.resolve(*names[0]) {
                Some(bind) => Some((names[0], Expr::callable(bind).tr(names[0].span))),
                None => {
                    self.emit_identifier_not_found(names[0].span, "");
                    Some((names[0], Expr::Poison.tr(names[0].span)))
                }
            },
            parser::Field::Assigned { field_path, bind, value } => match (modified, bind) {
                (None, Some(_)) => panic!("ET: cannot rebind in new record construct"),
                (None, None) if field_path.len() != 1 => {
                    panic!("ET: cannot destruct nested fields in new record construct");
                }
                (Some(modified), rebind) => {
                    Some(self.record_field_path(field_path, modified, *rebind, value.as_ref()))
                }
                (None, None) => Some((field_path[0], self.expr(value.as_ref()))),
            },
        }
    }

    fn record_field_path(
        &mut self,
        path: &[Tr<&'s str>],
        modify: Tr<key::Bind>,
        rebind: Option<Tr<&'s str>>,
        value: Tr<&parser::Expr<'s>>,
    ) -> (Tr<&'s str>, Tr<Expr<'s>>) {
        match path {
            [] => unreachable!(),
            [field] => match rebind {
                None => (*field, self.expr(value)),
                Some(name) => {
                    let bind = self.bindings.declare(*name);
                    let and_then = self.expr(value);
                    let let_bind = self.field_into_let_bind(modify, *field, bind, and_then);
                    (*field, let_bind)
                }
            },
            [field, xs @ ..] => {
                let bind = self.bindings.declare_nameless();
                let sbind = bind.tr(field.span);
                let next = self.record_field_path(xs, sbind, rebind, value);
                let nspan = next.0.span;
                let var = self.vars().record(field.span);
                // self.vars().add_field(var, nspan, next.0.value);
                let and_then = Expr::Record(var, None, Some(sbind), vec![next]).tr(nspan);
                let let_bind = self.field_into_let_bind(modify, *field, bind, and_then);
                (*field, let_bind)
            }
        }
    }

    // turns the rebind of a field into a let binding
    // ```
    // { v | x @ a = a }
    //   ^mod    ^to ^assigned_value
    // { v | x = let a = v.x in a }
    //           ^^^^^^^^^^^^^^^^ this is what gets returnt
    // ```
    // (which is desugared into a `match` over a single branch)
    //
    // also used for the desugaring of `a.b.c = ...` to create the intermediate bindings on each step
    fn field_into_let_bind(
        &mut self,
        modified: Tr<key::Bind>,
        field: Tr<&'s str>,
        to: key::Bind,
        assigned_value: Tr<hir::Expr<'s>>,
    ) -> Tr<Expr<'s>> {
        let rvar = self.vars().record(field.span);
        let letvalue = Expr::Access(
            rvar,
            Box::new(Expr::callable(modified.value).tr(modified.span)),
            field,
        )
        .tr(field.span);

        let pat = Pattern::Bind(to, Box::new(Pattern::Any)).tr(field.span);

        let span = assigned_value.span;
        Expr::let_bind(letvalue, pat, assigned_value).tr(span)
    }
}

#[derive(Clone, Debug)]
struct OpTree<'a, 's> {
    func: Mod<NFunc>,
    precedence: u32,
    raw: Tr<&'s str>,
    sides: Box<[Side<'a, 's>; 2]>,
}

#[derive(Clone, Debug)]
enum Side<'a, 's> {
    Op(OpTree<'a, 's>),
    Tail(Tr<&'a parser::Expr<'s>>),
}

impl<'s> fmt::Display for Expr<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = '('.symbol();
        let cp = ')'.symbol();
        match self {
            Expr::Call(call, anot, params) if params.is_empty() => write!(f, "{call}{anot:?}"),
            Expr::Call(call, anot, params) => {
                write!(f, "{op}{call}{anot:?} {}{cp}", params.iter().format(" "))
            }
            Expr::PassExpr(inner) => {
                write!(f, "#{op}{inner}{cp}")
            }
            Expr::Pass(call, anot, params) if params.is_empty() => write!(f, "{call}{anot:?}"),
            Expr::PassFnptr(key, anot) => write!(f, "{key}{anot:?}"),
            Expr::Pass(call, anot, params) => {
                write!(f, "{op}{call}{anot:?} {}{cp}", params.iter().format(" "))
            }
            Expr::Access(_, object, field) => write!(f, "{object}.{field}"),

            Expr::Record(_, _, Some(a), fields) => write!(
                f,
                "{{ {a} ~ {} }}",
                fields
                    .iter()
                    .format_with(", ", |(name, value), f| f(&format_args!(
                        "{name} = {value}"
                    )))
            ),
            Expr::Record(var, ty, None, fields) => write!(
                f,
                "{{ {var} {} {} | {} }}",
                "as".keyword(),
                match ty {
                    None => "_".to_string(),
                    Some(ty) => format!("{:?}", ty),
                },
                fields
                    .iter()
                    .format_with(", ", |(name, value), f| f(&format_args!(
                        "{name} = {value}"
                    )))
            ),
            Expr::Lit(lit) => write!(f, "{lit:?}"),
            Expr::Tuple(elems) => write!(f, "{op}{}{cp}", elems.iter().format(", ")),
            Expr::List(elems, _) => write!(f, "[{}]", elems.iter().format(", ")),
            Expr::Match(on, branches) => write!(
                f,
                "{} {} | {}",
                "match".keyword(),
                on,
                branches
                    .iter()
                    .map(|(pat, value)| format!("{pat} -> {value}"))
                    .format(" | ")
            ),
            Expr::Cast(expr, ty) => write!(f, "{op}{expr} as {ty}{cp}"),
            Expr::Poison => "???".fmt(f),
        }
    }
}

impl<'s> fmt::Debug for TypeAnnotation<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.for_type.is_empty() && self.for_entity.is_empty() {
            return Ok(());
        }

        write!(f, "(")?;

        for (name, ty) in self.for_type.iter() {
            write!(f, "·{name} {} {ty}, ", "as".keyword())?;
        }

        for (name, ty) in self.for_entity.iter() {
            write!(f, "{name} {} {ty}, ", "as".keyword())?;
        }

        write!(f, ")")
    }
}
