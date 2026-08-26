use std::cmp::Ordering;

pub use super::*;
use crate::key;
use crate::{InterResolved, project::FuncDef};
use expr::MatchKind;
// use lumina_key as key;
use derive_new::new;
use lumina_key::EntityList;
use lumina_key::SecondaryMap;
use lumina_typesystem::{self as ts, Annotation, Var as Inference};
use lumina_util::Highlighting;
use owo_colors::OwoColorize;
use tracing::info_span;

impl<'a, 's> FuncLower<'a, 's> {
    pub fn new(
        ctx: Context<TranslationUnit>,
        files: &'a Files,
        ast: &'a AST<'s>,
        node: &'a ProjectNode,
        uarena: &'a mut LoweringArena<'s>,
        id: key::Func,
    ) -> Self {
        let func = &ast.functions[id];
        let file = ast.items[func.item].file;
        Self {
            files,
            ast,
            node,
            uarena,
            tctx: TypeLower::new(ctx, node.key, file, InferenceEnv::new(), GenericTag::Func),
            id,
            lambda_instantiations: vec![],
            where_binding_names: Map::new(),
            scopes: Scopes::new(),
        }
    }

    pub fn fork(&mut self, func_id: key::Func) -> FuncLower<'_, 's> {
        let ctx = self.tctx.ctx.clone();
        FuncLower::new(ctx, self.files, self.ast, self.node, self.uarena, func_id)
    }

    pub fn poison_name(&mut self, name: Tr<&'s str>) -> Bind {
        let ty = self.tctx.define(name.span, |env| env.error());
        self.scopes.declare(*name, ty)
    }

    pub fn assigned_or_punned(
        &mut self,
        name: Tr<&'s str>,
        expr: Option<&Tr<parser::Expr<'s>>>,
    ) -> ExprId {
        match expr {
            // Assigned field
            Some(field) => self.expr(field.as_ref()),

            // Punned field
            None => match self.scopes.resolve(*name) {
                Some(bind) => self.copy(name.span, bind),
                None => {
                    errors::err("identifier not found")
                        .line(name.span, "could not pun field")
                        .emit();

                    self.expr_poison(name.span)
                }
            },
        }
    }

    pub fn string_type(&mut self, span: Span) -> Option<Var> {
        let resolve = self.find(symbols::Namespace::Types, &["std", "langitem", "string"]);

        let InterResolved::Item(origin, symbols::Item::Type((type_, plen))) = resolve else {
            errors::err("string langitem not available")
                .line(span, "")
                .emit();

            return None;
        };
        assert_eq!(plen, 0, "string lang item may not take type parameters");

        let string = TypeKey { origin, key: type_ };

        let var = self
            .tctx
            .define(span, |env| env.defined(string, key::EntityList::new()));

        Some(var)
    }

    // Lower the patterns and type signature of a function, and unify their types.
    //
    // Will poison patterns/ptypes if they are not of same length.
    fn patterns_and_signature(
        &mut self,
        header: &parser::func::Header<'s>,
        allow_missing_patterns: bool,
    ) -> (Vec<PatternId>, key::EntityList<Var>, Var) {
        self.tctx.env.enter_signature();

        match &header.typing {
            Some(typing) => {
                let (params, ret) = self.tctx.typing(typing, &mut Vec::new());

                self.tctx.env.leave_signature_enter_expression();

                let pats = self.patterns_in_signature(&header.params);

                if !(pats.is_empty() && allow_missing_patterns) {
                    let (before_span, rspan) = (header.name.span, typing.returns.span);
                    self.unify_patterns_against_signature(before_span, rspan, &pats, params);
                }

                (pats, params, ret)
            }
            None => {
                let pats = self.patterns_in_signature(&header.params);

                // Set the parameter types to be that of the patterns
                let params = EntityList::from_iter(
                    pats.iter().map(|pat| self.uarena.pats[*pat].ty),
                    self.tctx.var_pool_mut(),
                );

                let ret = self.tctx.define(header.name.span, |env| env.unknown());

                self.tctx.env.leave_signature_enter_expression();

                (pats, params, ret)
            }
        }
    }

    fn unify_patterns_against_signature(
        &mut self,
        before_span: Span,
        rspan: Span,
        patterns: &[PatternId],
        ptypes: EntityList<Var>,
    ) {
        let patterns_len = patterns.len();
        let ptypes_len = ptypes.len(self.tctx.var_pool());

        let error_text = || {
            format!(
                "{} patterns, function expects {} parameters",
                patterns_len.numeric(),
                ptypes_len.numeric(),
            )
        };

        match patterns_len.cmp(&ptypes_len) {
            Ordering::Equal => {
                // Unify the pattern types with the type signature
                let mut types = key::EntityIter::from(ptypes);
                let mut patterns = patterns.iter();
                while let Some(expected) = types.next(self.tctx.var_pool()) {
                    let pat = patterns.next().unwrap();
                    let given = self.uarena.pats[*pat].ty;
                    self.tctx.env.assign(given, expected);
                }
            }
            Ordering::Less => {
                let span = patterns
                    .last()
                    .map(|&p| self.uarena.pats[p].span)
                    .unwrap_or(before_span)
                    .move_indice(1);

                errors::err("missing parameter pattern(s)")
                    .line(span, error_text())
                    .emit();

                // Use poisoned patterns instead
                for p in patterns {
                    self.uarena.pats[*p].entity = Pattern::Poison;
                }
            }
            Ordering::Greater => {
                let span = ptypes
                    .as_slice(self.tctx.var_pool())
                    .last()
                    .copied()
                    .map(|ty| self.tctx.env.spans[ty])
                    .unwrap_or_else(|| rspan.move_indice(-1));

                errors::err("missing parameter type(s)")
                    .line(span, error_text())
                    .emit();

                // Use poisoned ptypes instead
                let mut iter = key::EntityIter::from(ptypes);
                while let Some(ty) = iter.next(self.tctx.var_pool()) {
                    self.tctx.env.poison(ty);
                }
            }
        }
    }

    pub fn fold_patterns_then_entrypoint(
        &mut self,
        patterns: Vec<PatternId>,
        decl: &parser::func::Declaration<'s>,
    ) -> ExprId {
        self.patterns_to_match_tree(patterns, |this, patterns| {
            decl.body
                .as_ref()
                .map(|body| this.expr(body.expr.as_ref()))
                .unwrap_or_else(|| this.expr_poison(decl.header.name.span))
        })
    }

    // Lower the patterns and type signature of a function and its lambdas, and unify the types.
    //
    // Will poison patterns/ptypes if they are not of same length.
    //
    // Sets the `LoweringFunction` state in uarena.
    fn init_uarena_func(
        &mut self,
        func: &parser::func::Declaration<'s>,
    ) -> (Vec<PatternId>, Map<key::Lambda, Vec<PatternId>>) {
        let allow_missing_patterns = self.ast.functions[self.id].attr.extern_.is_some();

        let patterns = match self.uarena.take_preprocessed(self.id) {
            Some((gspans, sspans)) => {
                self.tctx.generic_spans.extend(gspans);
                let fdef = self.ctx().in_project_mut(self.project(), |unit| {
                    unit.header
                        .take_func(self.id)
                        .expect("Function prepassed in uarena but not header")
                });
                *self.tctx.fenv.entry(GenericTag::Func).or_default() = fdef.sig.forall;

                // Signature has already been reversed in the case of `impl` block.
                // So make sure we're not doing it again.
                assert!(!self.tctx.fenv.contains_key(&GenericTag::Impl));

                let annotation = self.tctx.reverse_forall_into_type_environment();

                let mut inst = self.tctx.instantiation(symbols::Origin::Intra, annotation);
                let params = inst.tys(&fdef.sig.params);
                let ret = inst.ty(&fdef.sig.ret);

                // Retrieve the spans we got during prepass
                let mut params_iter = key::EntityIter::from(params);
                let mut spans_iter = sspans.iter().copied();
                while let Some(var) = params_iter.next(self.tctx.var_pool()) {
                    let span = spans_iter.next().unwrap();
                    self.tctx.env.spans[var] = span;
                }
                self.tctx.env.spans[ret] = spans_iter.next().unwrap();

                self.uarena.init_func(self.id, params, ret, true);

                self.tctx.env.leave_signature_enter_expression();

                let patterns = self.patterns_in_signature(&func.header.params);

                if !(allow_missing_patterns && patterns.is_empty()) {
                    let (before_span, rspan) = (func.header.name.span, self.tctx.env.spans[ret]);
                    self.unify_patterns_against_signature(before_span, rspan, &patterns, params);
                }

                patterns
            }
            None => {
                self.uarena.semi_init_func(self.id);

                // If there was no type annotation we may get the signature type variables from the patterns instead.
                let (pats, params, ret) =
                    self.patterns_and_signature(&func.header, allow_missing_patterns);

                let (when, lambdas) = func.when_bindings();
                self.tctx.as_static(
                    |tctx| {
                        tctx.lower_and_include_when_constraints(when);
                        tctx.lower_and_include_lambda_when_constraints(lambdas);
                    },
                    "when constraints",
                );

                self.uarena.init_func(self.id, params, ret, false);

                pats
            }
        };

        let lpatterns =
            self.init_uarena_lambdas(func.body.as_ref().map(|body| body.where_binds.as_slice()));

        self.tctx.scope = GenericTag::Func;

        (patterns, lpatterns)
    }

    fn init_uarena_lambdas(
        &mut self,
        where_binds: Option<&[parser::func::Declaration<'s>]>,
    ) -> Map<key::Lambda, Vec<PatternId>> {
        let Some(where_binds) = where_binds else {
            return Map::new();
        };

        let mut lambdas = Map::with_capacity(where_binds.len());

        for decl in where_binds.iter() {
            let lambda = self.uarena.reserve_lambda_key(self.id);

            self.scopes.enter_lambda();
            self.tctx.scope = GenericTag::Lambda(lambda);
            self.tctx.fenv.entry(self.tctx.scope).or_default();
            let (pats, params, ret) = self.patterns_and_signature(&decl.header, false);

            let parked_scope = self.scopes.exit_lambda();

            self.uarena
                .set_lambda(self.id, lambda, Some(parked_scope), Some(params), ret);

            assert_eq!(lambda, self.where_binding_names.push(decl.header.name));
            assert_eq!(lambda, lambdas.push(pats));
        }

        self.tctx.scope = GenericTag::Func;

        lambdas
    }

    pub fn lower_impl_method(
        &mut self,
        impl_: key::Impl,
        ierrors: &mut r#impl::Errors,
    ) -> LowerResult {
        let ctx = self.ctx().clone();
        let iforall = self.tctx.fenv.entry(GenericTag::Impl).or_default();

        self.tctx
            .generic_spans
            .extend(&self.uarena.impls[impl_].generic_spans);

        let self_ = ctx.in_project(self.node.key, |unit| {
            *iforall = unit.header.implementations[impl_].forall.clone();
            unit.header.implementations[impl_].self_.clone()
        });

        let anot = self.tctx.reverse_forall_into_type_environment();

        let var = self
            .tctx
            .instantiation(symbols::Origin::Intra, anot)
            .ty(&self_);

        self.tctx.env.self_ = Some(var);

        self.lower_function(Some(ierrors))
    }

    /// Assume function is not lowered and lower it
    fn lower_function(&mut self, ierrors: Option<&mut r#impl::Errors>) -> LowerResult {
        // Skip trait methods that have already been lowered by pre-pass and have no expressions.
        if self.uarena.is_trait_method(self.id) {
            return LowerResult::AlreadyLowered;
        }

        let decl = &self.ast.functions[self.id];
        let file = self.ast.items[decl.item].file;

        info!("function is not lowered, lowering {}", *decl.header.name);
        let previous_file = self.files.switch_errors_file(file);

        let (patterns, lpatterns) = self.init_uarena_func(decl);

        self.tctx.env.leave_signature_enter_expression();

        let trait_template_cappl = self.apply_trait_template(self.id);

        // Apply the implementation methods parameters to the trait template.
        if let Some(appl) = trait_template_cappl {
            let mut iter = key::EntityIter::from(self.uarena.params(self.id, None));
            while let Some(ty) = iter.next(self.tctx.var_pool()) {
                self.tctx.env.apply_next_parameter(appl, ty);
            }
            let trait_ret = self.tctx.env.get_return_type(appl);
            self.tctx
                .env
                .assign(self.uarena.ret(self.id, None), trait_ret);
        }

        let entrypoint = self.fold_patterns_then_entrypoint(patterns, decl);

        for (lambda, patterns) in lpatterns {
            let ldata = self.uarena.lambda_mut(self.id, lambda);
            let (binds, captures) = ldata.parked_scope.take().unwrap();

            // Switch the current scope back to the lambda
            self.tctx.scope = GenericTag::Lambda(lambda);
            self.scopes.re_enter_lambda(binds, captures);

            let entrypoint = self.fold_patterns_then_entrypoint(patterns, decl);

            // Finalize the lambda in the function lowering context.
            self.uarena.lambda_mut(self.id, lambda).captures = self.scopes.exit_lambda().1;
            self.uarena.lambda_mut(self.id, lambda).entrypoint = Some(entrypoint);
        }
        self.tctx.scope = GenericTag::Func;

        let ret = self.uarena.ret(self.id, None);
        self.tctx
            .env
            .assign_return(self.uarena.exprs[entrypoint].ty, ret);

        self.hack_allow_non_capturing_lambda_as_fnptr();

        info!("Finalizing types for {}", *decl.header.name);

        let (static_types, impl_errors) = self.tctx.type_check_and_finalize(
            self.ast,
            self.node,
            self.files,
            self.uarena,
            trait_template_cappl,
            self.id,
        );

        if let Some(ierrors) = ierrors {
            let method = self
                .ctx()
                .in_project(self.project(), |unit| {
                    unit.header.resolve_impl_method(ierrors.impl_, self.id)
                })
                .unwrap();
            ierrors.trait_check_errors(impl_errors, method, trait_template_cappl);
            ierrors.spans = std::mem::replace(
                &mut self.tctx.env.spans,
                SecondaryMap::with_default(Span::null()),
            );
        }

        // Update the function type signature with the now known static types
        self.finalize_func_in_headerfile(&static_types, false);

        // Set the lowering state to Lowered
        self.uarena
            .finish_func(self.id, static_types, Some(entrypoint));

        if let Some(file) = previous_file {
            self.files.switch_errors_file(file);
        }

        LowerResult::Lowered
    }

    fn finalize_func_in_headerfile(&mut self, vars: &Map<Var, Type>, has_body: bool) {
        let name = self.ast.functions[self.id].header.name.to_string();
        let (params, ret) = match &self.uarena.funcs[self.id] {
            LoweringFunc::Lowering { params, ret, .. } => (params, ret),
            _ => panic!(
                "finalize_func_in_headerfile must be balled before LoweringArena finalization"
            ),
        };

        let forall = self.tctx.take_forall(GenericTag::Func);

        let sig = FuncSig {
            forall,
            params: params
                .as_slice(self.tctx.var_pool())
                .iter()
                .map(|var| vars[*var].clone())
                .collect(),
            ret: vars[*ret].clone(),
        };

        self.ctx().in_project_mut(self.node.key, |unit| {
            let prec = self.ast.functions[self.id]
                .attr
                .precedence
                .unwrap_or(DEFAULT_OPERATOR_PRECEDENCE);

            unit.header
                .set_func(self.file(), self.id, name, sig, None, prec, has_body)
        })
    }

    /// Start the lower of this function if it hasn't been lowered.
    ///
    /// To finish, it may need to lower other functions in this unit as dependencies for inference.
    pub fn ensure_function_is_lowered(&mut self, force_prepassed: bool) -> LowerResult {
        let decl = &self.ast.functions[self.id];
        let item = &self.ast.items[decl.item];

        let span = info_span!(
            "ensuring function is lowered",
            item = *decl.header.name,
            file = self
                .files
                .path(item.file)
                .file_name()
                .unwrap()
                .to_str()
                .unwrap_or("")
        );
        let _handle = span.enter();

        if let Some(_) = self.ast.method_member_mapping[self.id] {
            panic!("Implementation function called directly instead of through trait");
        }

        // Since functions are lowered recursively to enable top-level type inference, we must
        // check whether this function was already lowered because of a prior function.
        match &self.uarena.funcs[self.id] {
            LoweringFunc::Lowering { preprocessed: true, .. } | LoweringFunc::Prepassed { .. }
                if !force_prepassed =>
            {
                return LowerResult::AlreadyLowered;
            }

            LoweringFunc::SemiLowering { .. } | LoweringFunc::Lowering { .. } => {
                return LowerResult::CircularInference;
            }

            LoweringFunc::Lowered { .. } => return LowerResult::AlreadyLowered,

            // `lower_function` sets the state to Lowering after
            // lowering the type signature
            LoweringFunc::Method { .. }
            | LoweringFunc::Pending
            | LoweringFunc::Prepassed { .. } => {}
        }

        self.tctx.fenv.entry(GenericTag::Func).or_default();

        self.lower_function(None)
    }

    // { Type }
    //
    // is allowed as sugar for
    //
    // { Type | }
    //
    // when no fields named `Type` exist in scope
    pub fn hack_resolve_ambigious_constructor<T>(
        &mut self,
        fields: &[parser::Field<'s, T>],
    ) -> Option<TypeKey> {
        if let [field] = fields {
            if field.value.is_none() && field.bind.is_none() {
                if let [name] = field.field_names.as_slice() {
                    if let InterResolved::Item(module, symbols::Item::Type((ty, _))) =
                        self.find(symbols::Namespace::Fields, &[**name])
                    {
                        return Some(TypeKey { origin: module, key: ty });
                    }
                }
            }
        }

        None
    }

    fn hack_allow_non_capturing_lambda_as_fnptr(&mut self) {
        for (lambda, ty) in self.lambda_instantiations.drain(..) {
            if self.uarena.lambda(self.id, lambda).captures.is_empty() {
                self.tctx.set_closure_to_fnptr(ty);
            }
        }
    }

    pub fn inst_self_recursion(&mut self, apath: Tr<&parser::AnnotatedPath<'s>>) -> Var {
        let mut inst = self.tctx.lowering_instantiation();
        let anot = inst.self_recursion(apath).finish();

        let (params, ret) = self
            .tctx
            .instantiation(symbols::Origin::Intra, anot)
            .self_recursion(self.uarena, self.id);

        let kind = ts::CallableKind::FnPointer;

        self.tctx
            .define(apath.span, |env| env.function(kind, params, ret))
    }

    pub fn inst_lambda(
        &mut self,
        span: Span,
        lambda: key::Lambda,
        pathanot: Option<&parser::ty::ForallAnnotation<'s>>,
    ) -> ExprId {
        let lambda_sig = self.uarena.lambda_mut(self.id, lambda);

        let anot = self
            .tctx
            .lowering_instantiation()
            .lambda(span, lambda, pathanot)
            .finish();

        let mut inst = self.tctx.instantiation(symbols::Origin::Intra, anot);
        let params = lambda_sig
            .params
            .clone()
            .expect("cannot instantiate partial application lambda as explicit lambda");
        let params = inst.entities(params);
        let ret = inst.ty(&lambda_sig.ret);

        let kind = ts::CallableKind::Closure;

        let ty = self
            .tctx
            .define(span, |env| env.function(kind, params, ret));

        self.lambda_instantiations.push((lambda, ty));

        self.add_typed_expr(Expr::Lambda(lambda), ty, span)
    }

    pub fn patterns_in_signature(&mut self, pats: &[Tr<parser::Pattern<'s>>]) -> Vec<PatternId> {
        pats.iter()
            .map(|p| self.pat(p.as_ref()))
            .collect::<Vec<_>>()
    }

    /// Lowers a list of patterns into a nested matches, one for each pattern.
    ///
    /// It is assumed that each pattern is meant to be infallible. Though those checks are not made
    /// here.
    ///
    /// `then_tail` is ran to generate the expression in the match branch for the last pattern.
    pub fn patterns_to_match_tree<F>(&mut self, patterns: Vec<PatternId>, then_tail: F) -> ExprId
    where
        F: FnOnce(&mut Self, Vec<PatternId>) -> ExprId,
    {
        let in_lambda = self.tctx.lambda();
        let expected_types = self.uarena.params(self.id, in_lambda).clone();

        let tail = then_tail(self, patterns.clone());

        info!("folding already lowered patterns into match tree");
        patterns
            .iter()
            .enumerate()
            .rev()
            .fold(tail, |tail, (i, pat)| {
                let pspan = self.uarena.pats[*pat].span;
                let expected = expected_types.get(i, self.tctx.var_pool()).unwrap();
                let destructed = self.add_typed_expr(Expr::Parameter(i), expected, pspan);

                let mut expr_list = key::EntityList::new();
                let mut pat_list = key::EntityList::new();

                pat_list.push(*pat, self.uarena.pat_pool_mut(self.id));
                expr_list.push(tail, self.uarena.expr_pool_mut(self.id));

                let match_ = Expr::Match(MatchKind::Parameter(i), destructed, pat_list, expr_list);

                let tail_ty = self.uarena.exprs[tail].ty;
                self.add_typed_expr(match_, tail_ty, pspan)
            })
    }

    pub fn expr_poison(&mut self, span: Span) -> ExprId {
        let ty = self.tctx.define(span, |env| env.error());
        self.uarena.exprs.push(Typed::new(Expr::Poison, ty, span))
    }
    pub fn pat_poison(&mut self, span: Span) -> PatternId {
        let ty = self.tctx.define(span, |env| env.error());
        self.uarena.pats.push(Typed::new(Pattern::Poison, ty, span))
    }

    pub fn ctx(&self) -> Context<TranslationUnit> {
        self.tctx.ctx.clone()
    }
    pub fn file(&self) -> key::File {
        self.tctx.file
    }
    pub fn project(&self) -> key::Project {
        self.tctx.project
    }

    pub fn find<'p>(&self, ns: symbols::Namespace, path: &[&'p str]) -> InterResolved<'p> {
        let origin = self.project();
        self.tctx
            .ctx
            .find(origin, None, self.file(), ns, path, true)
    }

    pub fn add_typed_expr(&mut self, expr: Expr<'s>, ty: Var, span: Span) -> ExprId {
        trace!("{} as {} = {:?}", self.uarena.exprs.next_key(), ty, &expr);
        self.uarena.exprs.push(Typed::new(expr, ty, span))
    }

    pub fn add_typed_pat(&mut self, pat: Pattern<'s>, ty: Var, span: Span) -> PatternId {
        self.uarena.pats.push(Typed::new(pat, ty, span))
    }

    pub fn get_field(&self, p: key::Project, ty: key::Type, field: key::Field) -> String {
        self.ctx()
            .in_project(p, |unit| match &unit.header.typedefs[ty] {
                TypeDef::Struct { fnames, .. } => fnames[field].clone(),
                _ => unreachable!(),
            })
    }

    pub fn resolve_lambda(&self, name: &str) -> Option<lumina_key::Lambda> {
        self.where_binding_names
            .iter()
            .find_map(|(lambda, n)| (**n == name).then_some(lambda))
    }

    // { t | x, y }
    // desugars to
    // { t | x = x, y = y }
    //
    // { t | a.b.c @ value = 0 }
    // desugars to
    // { t | a = { b = { c @ value = 0 } } }
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
    pub fn desugar_record_field<T, U>(
        &mut self,
        field: &parser::Field<'s, T>,
        mut lower_value: impl FnMut(&mut Self, T) -> U,
        mut from_name: impl FnMut(&mut Self, Tr<&'s str>) -> U,
    ) {
        let (last, field_names) = field.field_names.split_first().unwrap();

        let rhs = match field.value.as_ref() {
            Some(_) => todo!(),
            None => from_name(self, *last),
        };

        field_names.iter().rev().fold(rhs, |assigned, name| {
            todo!();
        });

        // TODO: record *modification* does not exist in patterns. Which I think
        // changes how this desugar needs to be done?

        todo!();
    }

    fn desugar_record_modification(
        &mut self,
        field: &parser::Field<'s, parser::Expr<'s>>,
        modified: Tr<Bind>,
    ) -> ExprId {
        // let (last, field_names) = field.field_names.split_first().unwrap();

        // let rhs = match field.value.as_ref() {
        //     Some(_) => todo!(),
        //     None => {
        //         todo!();
        //     }
        // };

        // field_names.iter().rev().fold(rhs, |assigned, name| {
        //     todo!();
        // });

        match field.field_names.as_slice() {
            [] => unreachable!(),
            [field] => {
                todo!();
                // todo!("are we *sure* that a parser::Field -> parser::Field desugar wouldn't work?");
                // a
                // I kinda want to try it again
                //
                // what if we take the expander as a parameter instead?

                // let bind = self.bindings.declare(*name);
                // let and_then = self.expr(value);
                // let let_bind = self.field_into_let_bind(modify, *field, bind, and_then);
                // (*field, let_bind)
            }
            // { entity ~ point.x = 5 }
            //
            // lowers to
            //
            // { entity ~ point @ point = { point | x = 5 } }
            [name, xs @ ..] => {
                // let ty = todo!();
                // let bind = match field.bind {
                //     // Some(bind_name) => self.scopes.declare(*bind_name, ty),
                //     // None => self.scopes.declare_nameless(name.span),
                // };

                todo!();
            }
        }
    }
}

pub enum LowerResult {
    CircularInference,
    Lowered,
    AlreadyLowered,
    Skipped,
}

fn func_lower_body<'s>(
    flower: &mut FuncLower<'_, 's>,
    decl: &parser::func::Declaration<'s>,
    patterns: Vec<PatternId>,
    // lpatterns: Map<key::Lambda, Vec<PatternId>>,
) -> ExprId {
    assert_eq!(None, flower.tctx.lambda());
    todo!();
    // Initalise the type annotations of the lambdas
    // if let Some(body) = decl.body.as_ref() {
    //     for (i, decl) in body.where_binds.iter().enumerate() {
    //         let lambda = key::Lambda(i as u32);
    //         assert_eq!(lambda, flower.where_bindings.push(decl.header.name));

    //         let previous = std::mem::replace(&mut flower.tctx.scope, GenericTag::Lambda(lambda));
    //         todo!("new pattern fallback stuff");
    //         // TODO: Do we initialise everything at once in one method instead?

    //         // let (params, ret) = flower.tctx.optional_typing(&decl.header);

    //         // assert_eq!(
    //         //     lambda,
    //         //     flower.func().lambdas.push(LoweringLambda::new(
    //         //         // this is a temporary forall which will be overwritten from
    //         //         // the one in tctx at finalization
    //         //         ts::Forall::new(),
    //         //         params,
    //         //         Some(ret)
    //         //     ))
    //         // );

    //         // flower.tctx.scope = previous;
    //     }
    // }

    // // TODO: Ditto for lambdas on patterns

    // // Fold function pattern parameters into a match tree
    // flower.patterns_to_match_tree(&decl.header.params, |this: &mut FuncLower<'_, 's>, _| {
    //     if let Some(body) = decl.body.as_ref() {
    //         let entrypoint = this.expr(body.expr.as_ref());

    //         for (i, decl) in body.where_binds.iter().enumerate() {
    //             let lkey = key::Lambda::from_u32(i as u32);
    //             this.tctx.scope = GenericTag::Lambda(lkey);
    //             let entrypoint = func_lower_body(this, decl);
    //             todo!("remake this");
    //             // this.func().lambdas[lkey].entrypoint = Some(entrypoint);
    //         }
    //         this.tctx.scope = GenericTag::Func;

    //         entrypoint
    //     } else {
    //         this.expr_poison(decl.header.name.span)
    //     }
    // })
}

pub struct Scopes<'s> {
    scopes: Vec<Scope<'s>>,

    // spans: Map<Bind, Span>,
    // TODO: We can turn this into EntityList if we do manual indice trickery
    types: Map<Bind, Var>,
}

enum Scope<'s> {
    Plain(Vec<(&'s str, Bind)>),

    Lambda {
        // The implicit capture parameter
        // captures: Bind,
        // Mapping of binds from upper scope to implicit capture field indice
        capture_map: Vec<Bind>,
    },
}

impl<'s> Scopes<'s> {
    fn new() -> Self {
        Self { scopes: vec![Scope::Plain(vec![])], types: Map::new() }
    }

    pub fn enter_lambda(&mut self) {
        self.scopes.push(Scope::Lambda { capture_map: vec![] });
        self.scopes.push(Scope::Plain(vec![]));
    }
    pub fn re_enter_lambda(&mut self, binds: Vec<(&'s str, Bind)>, capture_map: Vec<Bind>) {
        self.scopes.push(Scope::Lambda { capture_map });
        self.scopes.push(Scope::Plain(binds));
    }

    pub fn enter_match_branch(&mut self) {
        self.scopes.push(Scope::Plain(vec![]));
    }
    pub fn exit_match_branch(&mut self) -> Vec<(&'s str, Bind)> {
        let Some(Scope::Plain(names)) = self.scopes.pop() else {
            panic!("Unexpected scope order");
        };

        names
    }

    pub fn exit_lambda(&mut self) -> (Vec<(&'s str, Bind)>, Vec<Bind>) {
        let Some(Scope::Plain(names)) = self.scopes.pop() else {
            panic!("Lambda is missing a plain scope");
        };

        match self.scopes.pop() {
            Some(Scope::Lambda { capture_map }) => (names, capture_map),
            _ => panic!("scopes were entered/exited in an inconsistent order"),
        }
    }

    pub fn declare_nameless(&mut self, var: Var) -> Bind {
        self.types.push(var)
    }

    pub fn declare(&mut self, name: &'s str, ty: Var) -> Bind {
        let bind = self.types.push(ty);
        self.current_mut().push((name, bind));
        bind
    }

    fn current_mut(&mut self) -> &mut Vec<(&'s str, Bind)> {
        match self.scopes.last_mut().unwrap() {
            Scope::Plain(items) => items,
            Scope::Lambda { .. } => {
                unreachable!("Lambda is missing a plain scope")
            }
        }
    }

    pub fn resolve(&mut self, name: &str) -> Option<Bind> {
        Self::resolve_in(&mut self.scopes, name)
    }

    pub fn type_of(&self, bind: Bind) -> Var {
        self.types[bind]
    }

    fn resolve_in(scopes: &mut [Scope<'s>], name: &str) -> Option<Bind> {
        let (x, xs) = scopes.split_last_mut()?;

        match x {
            Scope::Plain(items) => items
                .iter()
                .find_map(|(n, bind)| (*n == name).then_some(*bind))
                .or_else(|| Self::resolve_in(xs, name)),

            Scope::Lambda { capture_map, .. } => Self::resolve_in(xs, name).map(|bind| {
                if !capture_map.contains(&bind) {
                    capture_map.push(bind);
                }
                bind
            }),
        }
    }
}

// `{ a.x = 4, a.y = 4 }`
//
// desugars to
//
// `{ a = { x = 4, y = 4 } }`
pub struct MergedFields<'a, 's, T> {
    assignments: Vec<(Tr<&'s str>, FieldAssignment<'a, 's, T>)>,
}

pub enum FieldAssignment<'a, 's, T> {
    Fields(MergedFields<'a, 's, T>),
    Tail(Option<Tr<&'s str>>, Option<&'a Tr<T>>),
}

impl<'a, 's, T> MergedFields<'a, 's, T> {
    pub fn new(fields: &'a [parser::Field<'s, T>]) -> Self {
        let mut this = MergedFields { assignments: Vec::with_capacity(fields.len()) };

        for field in fields {
            this.merge(&field.field_names, field.bind, field.value.as_ref());
        }

        this
    }

    pub fn iter(&self) -> impl Iterator<Item = (Tr<&'s str>, &FieldAssignment<'a, 's, T>)> {
        self.assignments.iter().map(|(name, field)| (*name, field))
    }
    pub fn into_iter(self) -> impl Iterator<Item = (Tr<&'s str>, FieldAssignment<'a, 's, T>)> {
        self.assignments.into_iter()
    }

    fn merge(&mut self, names: &[Tr<&'s str>], bind: Option<Tr<&'s str>>, tail: Option<&'a Tr<T>>) {
        let (first, xs) = names.split_first().unwrap();

        match self
            .assignments
            .iter_mut()
            .find(|(name, _)| *name == *first)
        {
            Some((_, FieldAssignment::Fields(field_assignments))) => {
                field_assignments.merge(xs, bind, tail);
            }
            // { point.x = 1, point.x = 2 }
            // { x = 1, x = 2 }
            Some((previous, FieldAssignment::Tail(_, _))) => {
                errors::err("invalid field assignment")
                    .line(first.span, "")
                    .line(previous.span, "same field already assigned here")
                    .emit();
            }
            None => {
                let next = FieldAssignment::include(xs, bind, tail);
                self.assignments.push((*first, next))
            }
        }
    }
}

impl<'a, 's, T> FieldAssignment<'a, 's, T> {
    fn include(names: &[Tr<&'s str>], bind: Option<Tr<&'s str>>, tail: Option<&'a Tr<T>>) -> Self {
        match names {
            [] => FieldAssignment::Tail(bind, tail),
            [name, xs @ ..] => {
                let assignments = [(*name, Self::include(xs, bind, tail))].into();
                FieldAssignment::Fields(MergedFields { assignments: assignments })
            }
        }
    }
}
