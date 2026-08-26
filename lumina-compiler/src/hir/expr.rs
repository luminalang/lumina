use super::{
    Bind, ExprId, FuncLower, Identifier, Literal, LoweringFunc, Pattern, PatternId, Tr, Typed,
    func::FieldAssignment, func::MergedFields, operator,
};
use crate::hir::TypeKey;
use crate::hir::ty::{LoweringKind as _, TypeLower};
use crate::key;
use crate::{InterResolved, symbols};
use crate::{errors, hir};
use lumina_key::EntityList;
use lumina_parser as par;
use lumina_typesystem as ts;
use lumina_typesystem::Var;
use lumina_util::{Span, Spanned};
use tracing::info;

// Various constructs are desugared into `match`.
//
// But for error messages it may be useful to know what created the `match`.
#[derive(Debug, Clone)]
pub enum MatchKind {
    Parameter(usize),
    FieldBind,
    PartialApplCapturing,
    User,
    Let,
    Do,
    If,
}

#[derive(Debug, Clone)]
pub enum Expr<'s> {
    Literal(Literal),

    // Nodes which may be applied with `Apply`
    Bind(Bind),
    Parameter(usize),
    Lambda(key::Lambda),
    Func(symbols::Origin, key::Func),
    Variant(symbols::Origin, key::Type, key::Variant),
    Val(symbols::Origin, key::Val),
    Method(symbols::Origin, key::Type, key::Method),
    Builtin(Vec<&'s str>),
    Field(symbols::Origin, key::Type, key::Field, &'s str),
    FieldUnresolved(&'s str),
    Cast(ExprId),
    // Identifiers used in dotpipes which failed to resolve but may still be type-sensitive identifiers.
    UnresolvedApplied {
        name: Tr<String>,
        params_hint: usize,
    },

    ApplyOrYield(ExprId, key::EntityList<ExprId>),

    // Invoke a partial application lambda.
    // Given parameters are partially applied, and will be captured.
    PartialApplication(key::Lambda, key::EntityList<ExprId>),

    Match(
        MatchKind,
        ExprId,
        key::EntityList<PatternId>,
        key::EntityList<ExprId>,
    ),
    As(ExprId, Tr<par::Type<'s>>),
    Array(key::EntityList<ExprId>),
    List(key::EntityList<ExprId>),
    Tuple(key::EntityList<ExprId>),
    RecordModifier {
        from: ExprId,
        fields: Vec<(&'s str, ExprId)>,
    },
    RecordConstructor {
        fields: Vec<(&'s str, ExprId)>,
    },

    // DotPipe([ExprId; 2]),
    Operators {
        init: Box<ExprId>,
        ops: Vec<(Tr<(symbols::Origin, key::Func)>, ExprId)>,
    },
    Poison,
}

enum ApplyableError<'s> {
    Circular(key::Func),
    NotFound {
        in_: key::File,
        at: &'s str,
        exists: bool,
    },
}

impl<'a, 's> FuncLower<'a, 's> {
    pub(crate) fn expr_and_typeof(&mut self, expr: Tr<&par::Expr<'s>>) -> Typed<ExprId> {
        let id = self.expr(expr);
        let ty = self.uarena.exprs[id].ty;
        let span = self.uarena.exprs[id].span;
        Typed::new(id, ty, span)
    }

    pub fn expr(&mut self, expr: Tr<&par::Expr<'s>>) -> ExprId {
        info!("expr: {expr}");

        match expr.value {
            par::Expr::Lit(lit) => self.lit(expr.span, lit),
            par::Expr::Call(apath, params)
                if apath.path.as_name() == Some("true") && params.is_empty() =>
            {
                let ty = self.tctx.define(expr.span, |env| env.prim(ts::Prim::Bool));
                self.add_typed_expr(Expr::Literal(Literal::Bool(true)), ty, expr.span)
            }
            par::Expr::Call(apath, params)
                if apath.path.as_name() == Some("false") && params.is_empty() =>
            {
                let ty = self.tctx.define(expr.span, |env| env.prim(ts::Prim::Bool));
                self.add_typed_expr(Expr::Literal(Literal::Bool(false)), ty, expr.span)
            }
            par::Expr::Call(apath, params) => match self.applyable(apath.as_ref()) {
                Ok(applyable) => {
                    let params = self.exprs(params);
                    self.apply_expr(expr.span, applyable, params)
                }
                Err(ApplyableError::Circular(called)) => {
                    self.err_circular(expr.span, called);
                    self.expr_poison(expr.span)
                }
                Err(ApplyableError::NotFound { in_, at, exists }) => {
                    self.err_func_not_found(in_, at, expr.span, exists);
                    self.expr_poison(expr.span)
                }
            },
            par::Expr::Group(inner) => return self.expr((&**inner).tr(expr.span)),
            par::Expr::DotPipe(exprs) => {
                self.dotpipe(expr.span, [exprs[0].as_ref(), exprs[1].as_ref()])
            }
            par::Expr::FieldAccess(src, field_name) => {
                let src = self.expr((**src).as_ref());
                self.unresolved_field_accessor(src, *field_name)
            }
            par::Expr::TupleAccess(src, i) => {
                let src = self.expr((**src).as_ref());
                todo!();
            }
            par::Expr::Lambda(patterns, params, expr) => {
                // let lambda = self.lambda(patterns, (**expr).as_ref());
                todo!("pretty sure we need to add the capture record to the params");
                // self.apply(lambda, params).downcast()
            }
            par::Expr::CallExpr(f, params) => {
                let f = self.expr((**f).as_ref());
                todo!();
                // self.apply(f, params).downcast()
            }
            par::Expr::Operators { init, ops } => self.operators((**init).as_ref(), ops),
            par::Expr::Match(matched, branches) => {
                self.r#match(expr.span, (**matched).as_ref(), branches)
            }
            par::Expr::CastAs(lhs, ty) => {
                let ty = self.tctx.ty(ty.as_ref());
                let lhs = self.expr((**lhs).as_ref());
                self.cast(expr.span, lhs, ty)
            }
            par::Expr::List(elems, list_length) => self.list(expr.span, elems, list_length),
            par::Expr::ListDotDot(..) => todo!("we need list lang item"),
            par::Expr::Tuple(elems) => self.tuple(expr.span, elems),
            par::Expr::Record { init, fields } => self.record(expr.span, init, fields),
            par::Expr::If(exprs) => {
                let cond = self.expr((**exprs)[0].as_ref());
                let truthy = self.expr((**exprs)[1].as_ref());
                let falsely = self.expr((**exprs)[2].as_ref());

                self.r#if(expr.span, [cond, truthy, falsely])
            }
            par::Expr::Do(exprs) => {
                let value = exprs[0].as_ref();
                let and_then = exprs[1].as_ref();
                let pat = &par::Pattern::Name(Identifier::from_raw("_"), vec![]);
                self.r#let(expr.span, pat.tr(exprs[0].span), [value, and_then])
            }
            par::Expr::Let(pat, exprs) => {
                let value = exprs[0].as_ref();
                let and_then = exprs[1].as_ref();
                self.r#let(expr.span, pat.as_ref(), [value, and_then])
            }
            par::Expr::Pass(inner) => self.pass(expr.span, (**inner).as_ref()),
            par::Expr::PassFptr(annotated_path) => todo!(),
            par::Expr::Poison => self.expr_poison(expr.span),
        }
    }

    pub fn copy(&mut self, span: Span, bind: Bind) -> ExprId {
        let ty = self.scopes.type_of(bind);
        self.add_typed_expr(Expr::Bind(bind), ty, span)
    }

    fn local_applyable(&mut self, apath: Tr<&par::AnnotatedPath<'s>>) -> Option<ExprId> {
        if let [name] = apath.path.as_slice() {
            if let Some(called) = self.scopes.resolve(name) {
                let ty = self.scopes.type_of(called);
                let expr = self.add_typed_expr(Expr::Bind(called), ty, apath.span);
                return Some(expr);
            }

            if let Some(lambda) = self.resolve_lambda(name) {
                let anotpath = apath
                    .for_segments
                    .last()
                    .map(|(_, forall_annotation)| forall_annotation);
                let lambda_expr_id = self.inst_lambda(apath.span, lambda, anotpath);
                return Some(lambda_expr_id);
            }
        }

        None
    }

    fn applyable(
        &mut self,
        apath: Tr<&par::AnnotatedPath<'s>>,
    ) -> Result<ExprId, ApplyableError<'s>> {
        let path = apath.path.as_slice();

        if let Some(call) = self.local_applyable(apath) {
            return Ok(call);
        }

        match self.find(symbols::Namespace::Functions, path) {
            InterResolved::Builtin(builtin) => {
                let call = self.applyable_builtin(apath.span, builtin, &apath.for_segments);
                Ok(call)
            }
            InterResolved::Item(origin, item) => match item {
                symbols::Item::Func(func)
                    if func == self.id && matches!(origin, symbols::Origin::Intra) =>
                {
                    let fvar = self.inst_self_recursion(apath);
                    let call = self.add_typed_expr(Expr::Func(origin, func), fvar, apath.span);
                    Ok(call)
                }
                symbols::Item::Func(func) => {
                    if let symbols::Origin::Intra = origin {
                        match self.fork(func).ensure_function_is_lowered(false) {
                            hir::func::LowerResult::CircularInference => {
                                return Err(ApplyableError::Circular(func));
                            }
                            _ => {}
                        }
                    }

                    let call = {
                        let fvar = self
                            .tctx
                            .lowering_instantiation()
                            .func_or_method(origin, func, apath);

                        self.add_typed_expr(Expr::Func(origin, func), fvar, apath.span)
                    };

                    Ok(call)
                }
                symbols::Item::Val(val) => {
                    if let symbols::Origin::Intra = origin {
                        let func = self.ast.vals[val];
                        match self.fork(func).ensure_function_is_lowered(false) {
                            hir::func::LowerResult::CircularInference => {
                                return Err(ApplyableError::Circular(func));
                            }
                            _ => {}
                        }
                    }

                    let type_ = self.tctx.lowering_instantiation().val(origin, val, apath);

                    let expr = self.add_typed_expr(Expr::Val(origin, val), type_, apath.span);
                    Ok(expr)
                }
                symbols::Item::Variant(ty, var) => {
                    let ty = TypeKey { origin, key: ty };
                    let call = {
                        let fvar = self.tctx.lowering_instantiation().variant(ty, var, apath);
                        self.add_typed_expr(Expr::Variant(origin, ty.key, var), fvar, apath.span)
                    };

                    Ok(call)
                }
                symbols::Item::Field(ty, field) => {
                    todo!();
                    // let fname = self.get_field(self.project(), ty, field);
                    // let last = path.last().unwrap();
                    // assert_eq!(&fname, last, "field renaming not supported");
                    // Ok(Expr::Field(module, ty, field, last))
                }
                symbols::Item::Method(ty, method) => {
                    let func = self.ctx().in_origin(self.project(), origin, |unit| {
                        unit.header.method(ty, method)
                    });

                    let call = {
                        let fvar = self
                            .tctx
                            .lowering_instantiation()
                            .func_or_method(origin, func, apath);

                        self.add_typed_expr(Expr::Func(origin, func), fvar, apath.span)
                    };

                    Ok(call)
                }
                symbols::Item::Type(_) => {
                    panic!("ET: Not a function")
                }
            },
            InterResolved::Module(module) => panic!("ET: "),
            InterResolved::Poison => {
                todo!();
            }
            InterResolved::NotFound { in_, at, exists } => {
                Err(ApplyableError::NotFound { in_, at, exists })
            }
        }
    }

    pub fn partial_application_lambda(
        &mut self,
        span: Span,
        f: ExprId,
        params: EntityList<ExprId>,
    ) -> ExprId {
        let lambda = self.uarena.reserve_lambda_key(self.id);

        let forall = ts::Forall::new();

        self.tctx
            .fenv
            .insert(ts::GenericTag::Lambda(lambda), forall.clone());

        // Create binds for each partially applied parameter such that they can be captured into the lambda
        self.bind_to_expr(params, |this, binds| {
            // We're now inside of the expression branch of an implicit `match` expression which
            // has turned the partially applied parameters into binds that can be captured.

            let f_type = this.uarena.exprs[f].ty;
            let ret = this
                .tctx
                .env
                .get_return_type_of_var(f_type)
                .expect("applyable yielded non_function, is it Poison?");
            this.uarena.set_lambda(this.id, lambda, None, None, ret);

            // When all binds have been created, switch to the lambda context and add them as captures.
            let previous = std::mem::replace(&mut this.tctx.scope, ts::GenericTag::Lambda(lambda));
            this.scopes.enter_lambda();
            let mut applied_params = EntityList::new();
            for bind in binds {
                let ty = this.scopes.type_of(bind);
                let span = this.tctx.env.spans[ty];
                this.uarena.lambda_mut(this.id, lambda).captures.push(bind);
                let expr = this.add_typed_expr(Expr::Bind(bind), ty, span);
                applied_params.push(expr, this.uarena.expr_pool_mut(this.id));
            }
            this.uarena.lambda_mut(this.id, lambda).captures = this.scopes.exit_lambda().1;
            this.tctx.scope = previous;

            let ty = this.tctx.define(span, |env| {
                env.unknown_function(ts::CallableKind::Closure, ret)
            });

            this.add_typed_expr(Expr::PartialApplication(lambda, applied_params), ty, span)
        })
    }

    pub fn lambda(
        &mut self,
        span: Span,
        pats: &[Tr<par::Pattern<'s>>],
        body: Tr<&par::Expr<'s>>,
    ) -> ExprId {
        let lambda = self.uarena.reserve_lambda_key(self.id);
        let previous = std::mem::replace(&mut self.tctx.scope, ts::GenericTag::Lambda(lambda));
        self.scopes.enter_lambda();

        let forall = ts::Forall::new();

        // NOTE: We assume inline lambdas can't have when bindings
        self.tctx
            .fenv
            .insert(ts::GenericTag::Lambda(lambda), forall.clone());

        let patterns = self.patterns_in_signature(pats);

        let ptypes = EntityList::from_iter(
            patterns.iter().map(|&pat| self.uarena.pats[pat].ty),
            self.tctx.var_pool_mut(),
        );

        let ret = self.tctx.define(body.span, |env| env.unknown());
        self.uarena
            .set_lambda(self.id, lambda, None, Some(ptypes), ret);

        let entrypoint = self.patterns_to_match_tree(patterns, |this, _| this.expr(body));

        let given_ret = self.uarena.exprs[entrypoint].ty;
        self.tctx.env.assign(given_ret, ret);

        self.uarena.lambda_mut(self.id, lambda).captures = self.scopes.exit_lambda().1;

        self.tctx.scope = previous;

        self.inst_lambda(span, lambda, None)
    }

    pub(crate) fn applyable_builtin(
        &mut self,
        span: Span,
        builtin: Vec<&'s str>,
        explicit: &[(usize, par::ty::ForallAnnotation<'s>)],
    ) -> ExprId {
        let Some((params, ret)) = self
            .tctx
            .lowering_instantiation()
            .builtin(span, &builtin, explicit)
        else {
            return self.expr_poison(span);
        };

        let ty = self.tctx.define(span, |env| {
            env.function(ts::CallableKind::FnPointer, params, ret)
        });

        self.add_typed_expr(Expr::Builtin(builtin), ty, span)
    }

    pub fn exprs(&mut self, exprs: &[Tr<par::Expr<'s>>]) -> EntityList<ExprId> {
        let mut list = key::EntityList::new();
        for p in exprs {
            let typed = self.expr_and_typeof(p.as_ref());
            list.push(typed.entity, &mut self.uarena.expr_pool_mut(self.id));
        }
        list
    }

    pub fn exprs_and_typeof(
        &mut self,
        exprs: &[Tr<par::Expr<'s>>],
    ) -> (EntityList<ExprId>, EntityList<Var>) {
        let exprs = self.exprs(exprs);
        let types = self.type_of_exprs(exprs.clone());
        (exprs, types)
    }

    pub fn type_of_exprs(&mut self, exprs: EntityList<ExprId>) -> EntityList<Var> {
        let mut list = EntityList::new();
        let mut iter = key::EntityIter::from(exprs);
        while let Some(expr_id) = iter.next(&self.uarena.expr_pool(self.id)) {
            let ty = self.uarena.exprs[expr_id].ty;
            list.push(ty, self.tctx.var_pool_mut());
        }
        list
    }

    fn operators(
        &mut self,
        init: Tr<&par::Expr<'s>>,
        ops: &[(Tr<&'s str>, Tr<par::Expr<'s>>)],
    ) -> ExprId {
        let init = operator::Side::Tail(init);

        let op = ops.iter().fold(init, |lhs, (op, rhs)| {
            self.handle_operator(lhs, *op, rhs.as_ref())
        });

        self.fold_optree_side(op)
    }

    pub fn apply_expr(&mut self, span: Span, f: ExprId, params: EntityList<ExprId>) -> ExprId {
        let f_type = self.uarena.exprs[f].ty;
        let cappl = self.tctx.env.apply(span, f_type);

        let mut iter = key::EntityIter::from(params);
        while let Some(p) = iter.next(&self.uarena.expr_pool(self.id)) {
            let ty = self.uarena.exprs[p].ty;
            self.tctx.env.apply_next_parameter(cappl, ty);
        }

        let ret = self.tctx.env.get_return_type(cappl);

        let apply = Expr::ApplyOrYield(f, params);
        self.add_typed_expr(apply, ret, span)
    }

    fn cast(&mut self, span: Span, lhs: ExprId, ty: Var) -> ExprId {
        self.add_typed_expr(Expr::Cast(lhs), ty, span)
    }

    fn lit(&mut self, span: Span, lit: &par::Literal<'s>) -> ExprId {
        let lit = Literal::from(lit);
        let ty = match lit {
            Literal::Int(_, _) => self.tctx.define(span, |env| env.numeric()),
            Literal::Float(_) => self.tctx.define(span, |env| env.prim(ts::Prim::Float)),
            Literal::String(_) => {
                let Some(ty) = self.string_type(span) else {
                    return self.expr_poison(span);
                };

                ty
            }
            Literal::Char(_) => {
                let size = lumina_typesystem::IntSize::new(false, 8);
                self.tctx.define(span, |env| env.int(size))
            }
            Literal::Bool(_) => unreachable!("bools are not resolved in parser"),
        };
        self.add_typed_expr(Expr::Literal(lit), ty, span)
    }

    fn r#match(
        &mut self,
        span: Span,
        matched: Tr<&par::Expr<'s>>,
        branches: &[(Tr<par::Pattern<'s>>, Tr<par::Expr<'s>>)],
    ) -> ExprId {
        let matched_expr = self.expr(matched);

        let mut plist = EntityList::new();
        let mut elist = EntityList::new();

        for (pat, expr) in branches {
            self.scopes.enter_match_branch();

            let pat = self.pat(pat.as_ref());
            let expr = self.expr(expr.as_ref());

            self.scopes.exit_match_branch();

            plist.push(pat, &mut self.uarena.pat_pool_mut(self.id));
            elist.push(expr, &mut self.uarena.expr_pool_mut(self.id));
        }

        {
            let pat_master = &self.uarena.exprs[matched_expr];
            let (sameas, _) = self
                .tctx
                .env
                .expr_sameas(pat_master.span, Some(pat_master.ty));

            let mut iter = key::EntityIter::from(plist);
            while let Some(pat) = iter.next(self.uarena.pat_pool(self.id)) {
                let var = self.uarena.pats[pat].ty;
                self.tctx.env.add_sameas_member(sameas, var);
            }
        };

        let expr_master = {
            let (sameas, expr_master) = self.tctx.env.expr_sameas(span, None);

            let mut iter = key::EntityIter::from(elist);
            while let Some(expr) = iter.next(self.uarena.expr_pool(self.id)) {
                let var = self.uarena.exprs[expr].ty;
                self.tctx.env.add_sameas_member(sameas, var);
            }

            expr_master
        };

        let match_expr = Expr::Match(MatchKind::User, matched_expr, plist, elist);
        self.add_typed_expr(match_expr, expr_master, span)
    }

    fn r#if(&mut self, span: Span, [cond, truthy, falsely]: [ExprId; 3]) -> ExprId {
        let spans = [cond, truthy, falsely].map(|expr| self.uarena.exprs[expr].span);

        let (same_as, if_expr_type) = self.tctx.env.expr_sameas(span, None);
        let truthy_ty = self.uarena.exprs[truthy].ty;
        let falsely_ty = self.uarena.exprs[falsely].ty;
        self.tctx.env.add_sameas_member(same_as, truthy_ty);
        self.tctx.env.add_sameas_member(same_as, falsely_ty);

        let ptrue = self.pat(par::Pattern::String("true", vec![]).tr(spans[1]).as_ref());
        let pfalse = self.pat(par::Pattern::String("false", vec![]).tr(spans[1]).as_ref());

        let plist = EntityList::from_slice(&[ptrue, pfalse], self.uarena.pat_pool_mut(self.id));
        let elist = EntityList::from_slice(&[truthy, falsely], self.uarena.expr_pool_mut(self.id));

        let match_expr = Expr::Match(MatchKind::If, cond, plist, elist);

        self.add_typed_expr(match_expr, if_expr_type, span)
    }

    // let x = 0 in 1
    //
    // match 0
    // | x -> 1
    fn r#let(
        &mut self,
        span: Span,
        pat: Tr<&par::Pattern<'s>>,
        [value, and_then]: [Tr<&par::Expr<'s>>; 2],
    ) -> ExprId {
        let matched_expr = self.expr(value);

        let let_pat = self.pat(pat);
        let let_and_then = self.expr(and_then);

        let [pat_type, matched_expr_type] = [
            self.uarena.pats[let_pat].ty,
            self.uarena.exprs[matched_expr].ty,
        ];
        let and_then_type = self.uarena.exprs[let_and_then].ty;

        self.tctx.env.assign(matched_expr_type, pat_type);

        let plist = EntityList::from_slice(&[let_pat], self.uarena.pat_pool_mut(self.id));
        let elist = EntityList::from_slice(&[let_and_then], self.uarena.expr_pool_mut(self.id));

        let match_expr = Expr::Match(MatchKind::Let, matched_expr, plist, elist);
        self.add_typed_expr(match_expr, and_then_type, span)
    }

    // Implicitly create `match` to turn an arbritary expression into a binding.
    pub fn bind_to_expr<F>(&mut self, exprs: EntityList<ExprId>, f: F) -> ExprId
    where
        F: FnOnce(&mut Self, Vec<Bind>) -> ExprId,
    {
        let tuple_type = {
            let elem_types = self.type_of_exprs(exprs);
            self.tctx.define(Span::null(), |env| env.tuple(elem_types))
        };

        let matched_expr = {
            // TODO: Is it okay to use Span::null here? They should never fail.
            self.add_typed_expr(Expr::Tuple(exprs), tuple_type, Span::null())
        };

        let mut expr_iter = key::EntityIter::from(exprs);
        let mut tuple_binds = key::EntityList::new();
        let mut binds = vec![];
        while let Some(expr) = expr_iter.next(self.uarena.expr_pool(self.id)) {
            let ty = self.uarena.exprs[expr].ty;
            let span = self.uarena.exprs[expr].span;
            let (pat, bind) = self.bind_to(None, span, ty, |this, _| {
                this.add_typed_pat(Pattern::Any, ty, span)
            });
            binds.push(bind);
            tuple_binds.push(pat, self.uarena.pat_pool_mut(self.id));
        }

        let let_pat = self.add_typed_pat(Pattern::Tuple(tuple_binds), tuple_type, Span::null());
        let let_expr = f(self, binds);
        let let_expr_type = self.uarena.exprs[let_expr].ty;

        let plist = EntityList::from_slice(&[let_pat], self.uarena.pat_pool_mut(self.id));
        let elist = EntityList::from_slice(&[let_expr], self.uarena.expr_pool_mut(self.id));

        let match_expr = Expr::Match(MatchKind::PartialApplCapturing, matched_expr, plist, elist);
        self.add_typed_expr(match_expr, let_expr_type, Span::null())
    }

    /// `{ point | x @ x = x + 1 }`
    ///
    /// lowers to
    ///
    /// `{ point | x = let x = point.x in x + 1 }`
    pub fn implicit_bind_accessor(
        &mut self,
        field_name: Tr<&'s str>,
        bind_name: Option<&'s str>,
        modified: ExprId,
        and_then: impl FnOnce(&mut Self, Bind) -> ExprId,
    ) -> ExprId {
        let field_accessor = self.unresolved_field_accessor(modified, field_name);
        let field_type = self.uarena.exprs[field_accessor].ty;

        let (let_pat, bind) = self.bind_to(bind_name, field_name.span, field_type, |this, _| {
            this.uarena
                .pats
                .push(Typed::new(Pattern::Any, field_type, field_name.span))
        });
        let plist = EntityList::from_slice(&[let_pat], self.uarena.pat_pool_mut(self.id));

        let and_then = and_then(self, bind);
        let elist = EntityList::from_slice(&[and_then], self.uarena.expr_pool_mut(self.id));

        let match_expr = Expr::Match(MatchKind::FieldBind, field_accessor, plist, elist);
        let and_then_type = self.uarena.exprs[and_then].ty;
        self.add_typed_expr(match_expr, and_then_type, field_name.span)
    }

    pub fn implicit_bind_if_named(
        &mut self,
        field_name: Tr<&'s str>,
        bind_name: Option<&'s str>,
        modified: ExprId,
        and_then: impl FnOnce(&mut Self) -> ExprId,
    ) -> ExprId {
        if let Some(_) = bind_name {
            self.implicit_bind_accessor(field_name, bind_name, modified, |this, _| and_then(this))
        } else {
            and_then(self)
        }
    }

    fn unresolved_field_accessor(&mut self, src: ExprId, field: Tr<&'s str>) -> ExprId {
        let src_type = self.uarena.exprs[src].ty;
        let field_type = self
            .tctx
            .define(field.span, |env| env.add_field(src_type, *field));

        self.add_typed_expr(Expr::FieldUnresolved(*field), field_type, field.span)
    }

    fn dotpipe(&mut self, span: Span, [lhs, rhs]: [Tr<&par::Expr<'s>>; 2]) -> ExprId {
        let lhs = self.expr_and_typeof(lhs);

        match &rhs.value {
            par::Expr::Call(apath, params) => {
                let mut param_exprs = self.exprs(params);
                param_exprs.push(lhs.entity, self.uarena.expr_pool_mut(self.id));

                // Prioritise local bindings over type-sensitive name resolution.
                if let Some(applyable) = self.local_applyable(apath.as_ref()) {
                    return self.apply_expr(rhs.span, applyable, param_exprs);
                }

                match apath.path.as_name() {
                    // Leave it unresolved for now and hint to the typesystem that it should trigger
                    // name resolution later during type inference.
                    Some(name) => {
                        let ty = self
                            .tctx
                            .define(rhs.span, |env| env.type_resolved_function(name));

                        let applyable = self.add_typed_expr(
                            Expr::UnresolvedApplied {
                                name: name.to_string().tr(rhs.span),
                                params_hint: params.len() + 1,
                            },
                            ty,
                            rhs.span,
                        );

                        self.apply_expr(rhs.span, applyable, param_exprs)
                    }
                    // Fallback to normal name resolution if path has multiple segments
                    None => match self.applyable(apath.as_ref()) {
                        Ok(applyable) => {
                            return self.apply_expr(rhs.span, applyable, param_exprs);
                        }
                        Err(ApplyableError::Circular(called)) => {
                            self.err_circular(span, called);
                            self.expr_poison(span)
                        }
                        Err(ApplyableError::NotFound { in_, at, exists }) => {
                            self.err_func_not_found(in_, at, span, exists);
                            return self.expr_poison(rhs.span);
                        }
                    },
                }
            }
            _ => panic!("ET: invalid dot-pipe."),
        }
    }

    fn pass(&mut self, span: Span, inner: Tr<&par::Expr<'s>>) -> ExprId {
        match *inner {
            // #(...)
            par::Expr::Group(inner) => match &**inner {
                // #(\ ...)
                par::Expr::Lambda(patterns, params, body) => {
                    assert!(params.is_empty());
                    self.lambda(span, patterns, (**body).as_ref())
                }
                // #(f ...)
                par::Expr::Call(apath, params) => {
                    let params = self.exprs(params);

                    match self.applyable(apath.as_ref()) {
                        Ok(applyable) => self.partial_application_lambda(span, applyable, params),
                        Err(ApplyableError::Circular(called)) => {
                            self.err_circular(span, called);
                            self.expr_poison(span)
                        }
                        Err(ApplyableError::NotFound { in_, at, exists }) => {
                            self.err_func_not_found(in_, at, span, exists);
                            self.expr_poison(span)
                        }
                    }
                }
                // #((f ...))
                par::Expr::Group(inner) => {
                    todo!();
                    // let inner = self.expr((&**inner).tr(to_pass.span));
                    // Expr::PassExpr(Box::new(inner))
                }
                // #((f) 0)
                par::Expr::CallExpr(inner, _) => {
                    let _inner = self.expr((**inner).as_ref());
                    todo!();
                }
                // #(0)
                _ => {
                    todo!();
                    // let lkey = self.lambda(&[], None, (&**inner).tr(to_pass.span));
                    // Expr::Pass(lkey.into(), TypeAnnotation::new(), vec![])
                }
            },
            // #f
            par::Expr::Call(apath, params) => {
                assert!(params.is_empty());
                match self.applyable(apath.as_ref()) {
                    Ok(applyable) => applyable,
                    Err(ApplyableError::Circular(called)) => {
                        self.err_circular(span, called);
                        self.expr_poison(span)
                    }
                    Err(ApplyableError::NotFound { in_, at, exists }) => {
                        self.err_func_not_found(in_, at, span, exists);
                        self.expr_poison(span)
                    }
                }
            }
            // #handlers.f
            par::Expr::FieldAccess(object, name) => {
                let object = self.expr((**object).as_ref());
                todo!();
            }
            // ##...
            par::Expr::Pass(inner) => {
                // let inner = self.pass((**inner).as_ref());
                // Expr::PassExpr(Box::new(inner.tr(to_pass.span)))
                todo!();
            }
            // #0
            _ => {
                // let lkey = self.lambda(&[], None, (&**to_pass).tr(to_pass.span));
                // Expr::Pass(lkey.into(), TypeAnnotation::new(), vec![])
                todo!();
            }
        }
    }

    fn list(
        &mut self,
        span: Span,
        elems: &[Tr<par::Expr<'s>>],
        list_length: &par::ListLength<'s>,
    ) -> ExprId {
        let elems_len = elems.len();
        let elems = self.exprs(elems);

        match list_length {
            par::ListLength::Name(name) if **name == "_" => {
                let len = self
                    .tctx
                    .define(span, |env| env.const_int(elems_len as i128));
                self.array(span, elems, len)
            }
            par::ListLength::Name(name) => {
                match ts::find_generic(&self.tctx.fenv, self.tctx.scope, **name) {
                    Some(generic) => {
                        let len = self.tctx.define(name.span, |env| env.generic(generic));
                        self.array(span, elems, len)
                    }
                    None => {
                        errors::err("invalid array")
                            .line(
                                name.span,
                                "array length must be either numeric or a const generic",
                            )
                            .emit();

                        self.expr_poison(span)
                    }
                }
            }
            par::ListLength::Exact(n) => {
                let len = self.tctx.define(span, |env| env.const_int(**n as i128));
                self.array(span, elems, len)
            }
            par::ListLength::None => {
                let (sameas, list, _) = self.tctx.env.list_sameas(span);
                self.merge_sameas(sameas, elems);

                self.add_typed_expr(Expr::List(elems), list, span)
            }
        }
    }

    fn array(&mut self, span: Span, elems: EntityList<ExprId>, len: Var) -> ExprId {
        // let len = match len {
        //     ts::ArrayLen::Int(n) => self.tctx.define(span, |env| env.const_int(n as i128)),
        //     ts::ArrayLen::Generic(generic) => self.tctx.define(span, |env| env.generic(generic)),
        // };

        let (sameas, inner) = self.tctx.env.expr_sameas(span, None);
        let ty = self.tctx.define(span, |env| env.array(inner, len));

        self.merge_sameas(sameas, elems);

        let expr = Expr::Array(elems);
        self.add_typed_expr(expr, ty, span)
    }

    fn merge_sameas(&mut self, sameas: ts::SameasUnification, elems: EntityList<ExprId>) {
        for expr in elems.as_slice(&self.uarena.expr_pool(self.id)) {
            let elem_ty = self.uarena.exprs[*expr].ty;
            self.tctx.env.add_sameas_member(sameas, elem_ty);
        }
    }

    fn tuple(&mut self, span: Span, elems: &[Tr<par::Expr<'s>>]) -> ExprId {
        let (tuple_elems, types) = self.exprs_and_typeof(elems);
        let ty = self.tctx.define(span, |env| env.tuple(types));
        self.add_typed_expr(Expr::Tuple(tuple_elems), ty, span)
    }

    fn record(
        &mut self,
        span: Span,
        init: &par::CurlyInit<'s>,
        fields: &[par::Field<'s, par::Expr<'s>>],
    ) -> ExprId {
        match init {
            par::CurlyInit::Modify(modified) => {
                let merged = MergedFields::new(fields);
                let modified = self.expr(modified.as_ref());
                self.modify_record(span, modified, merged)
            }
            par::CurlyInit::Construct(ty) => {
                let merged = MergedFields::new(fields);
                let ty = self.tctx.ty(ty.as_ref());
                self.construct_record(span, ty, merged)
            }
            par::CurlyInit::None => match self.hack_resolve_ambigious_constructor(fields) {
                None => {
                    let merged = MergedFields::new(fields);
                    let ty = self.tctx.define(span, |env| env.unknown());
                    self.construct_record(span, ty, merged)
                }
                Some(tkey) => {
                    assert_eq!(fields.len(), 1);
                    let ty = self
                        .tctx
                        .define(span, |env| env.defined(tkey, EntityList::new()));
                    self.construct_record(span, ty, MergedFields::new(&[]))
                }
            },
        }
    }

    /// `{ point ~ x @ x = x + 1 }`
    ///
    /// lowers to
    ///
    /// `{ point ~ x = let x = point.x in x + 1 }`
    fn modify_record(
        &mut self,
        span: Span,
        modified: ExprId,
        fields: MergedFields<'_, 's, par::Expr<'s>>,
    ) -> ExprId {
        // TODO: We declare a new unknown to allow the use of type-changing modifications.
        //
        // However I'm unsure how well this works.
        let ty = self.tctx.define(span, |env| env.unknown());

        let fields = fields
            .into_iter()
            .map(|(field, assigned)| {
                // let lhs = self.desugar_modify_fieldnames(modified, desugared);
                let rhs = match assigned {
                    FieldAssignment::Fields(merged_fields) => {
                        let modified = self.unresolved_field_accessor(modified, field);
                        self.modify_record(field.span, modified, merged_fields)
                    }
                    FieldAssignment::Tail(bind_to, tail_expr) => {
                        let bind_to = bind_to.map(|v| v.value);
                        self.implicit_bind_if_named(field, bind_to, modified, |this| {
                            this.assigned_or_punned(field, tail_expr)
                        })
                    }
                };

                // Declare the new constructed record to have the field and assign it
                let assigned_field_type = self
                    .tctx
                    .define(field.span, |env| env.add_field(ty, *field));

                self.tctx
                    .env
                    .assign(assigned_field_type, self.uarena.exprs[rhs].ty);

                (*field, rhs)
            })
            .collect::<Vec<(&'s str, ExprId)>>();

        let constructor = Expr::RecordModifier { from: modified, fields };
        self.add_typed_expr(constructor, ty, span)
    }

    // { Entity | point.x = 4, point.y = 5 }
    //
    // lowers to
    //
    // { Entity | point = { Point | x = 4, y = 5 } }
    fn construct_record(
        &mut self,
        span: Span,
        ty: Var,
        fields: MergedFields<'_, 's, par::Expr<'s>>,
    ) -> ExprId {
        let fields = fields
            .into_iter()
            .map(|(field, assigned)| {
                let ty = self
                    .tctx
                    .define(field.span, |env| env.add_field(ty, *field));

                let rhs = match assigned {
                    FieldAssignment::Fields(fields) => {
                        self.construct_record(field.span, ty, fields)
                    }
                    FieldAssignment::Tail(bind_to, tail_expr) => {
                        if let Some(bind_to) = bind_to {
                            errors::err("invalid bind")
                                .line(bind_to.span, "cannot bind to field of constructor")
                                .line(
                                    self.tctx.env.spans[ty],
                                    "did you mean to modify an existing record with `~`?",
                                )
                                .emit();

                            self.poison_name(field);
                        }

                        self.assigned_or_punned(field, tail_expr)
                    }
                };

                self.tctx.env.assign(ty, self.uarena.exprs[rhs].ty);

                (*field, rhs)
            })
            .collect();

        let constructor = Expr::RecordConstructor { fields };
        self.add_typed_expr(constructor, ty, span)
    }

    fn err_func_not_found(&self, in_: key::File, at: &str, span: Span, exists: bool) {
        let err = errors::err("function not found").line(
            span,
            format!("`{at}` not found in module `{}`", self.files.name(in_)),
        );

        if exists {
            err.text("hint: the function exists but is not public")
        } else {
            err
        }
        .emit();
    }

    fn err_circular(&self, span: Span, called: key::Func) {
        let this_func = &self.ast.functions[self.id].header;
        let called_func = &self.ast.functions[called].header;

        errors::err("inference error")
            .line(this_func.name.span, "type annotation needed")
            .line(
                span,
                format!(
                    "`{}` called here, but `{}`s type depends on the type of `{}`",
                    *called_func.name, *called_func.name, *this_func.name,
                ),
            )
            .emit();
    }
}
