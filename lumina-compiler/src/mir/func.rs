use super::{lower, tyfmt::TyFmtState, Current, LangItems, ReadOnlyBytes, Verify};
use crate::prelude::*;
use crate::{ProjectInfo, Target};
use ast::NFunc;
use either::Either;
use hir::HIR;
use lumina_typesystem::{
    Compatibility, Constraint, ConstraintError, Container, DirectRecursion, Finalizer, Forall,
    Generic, GenericKind, GenericMapper, IType, ImplIndex, Inference, Static, TEnv, Transformer,
    Ty, Type, TypeSystem,
};
use lumina_util::Highlighting;
use std::fmt;
use tracing::info_span;

#[derive(Clone, new, Debug)]
pub struct InstInfo {
    pub module: key::Module,
    pub inst: GenericMapper<Inference>,
    pub ptypes: Vec<Tr<IType>>,
    pub ret: Tr<IType>,
}

pub enum FunctionStatus {
    Done(lower::Function),
    Extern {
        link_name: String,
        typing: lower::ConcreteTyping,
    },
    InCallStack(Current),
    Pending,
    Lowering,
}

impl FunctionStatus {
    #[track_caller]
    pub fn as_done(&self) -> &lower::Function {
        match self {
            FunctionStatus::Done(func) => func,
            _ => panic!("as_done called for pending/lowering function"),
        }
    }

    #[track_caller]
    pub fn as_typing(&self) -> &lower::ConcreteTyping {
        match self {
            FunctionStatus::Done(func) => &func.typing,
            FunctionStatus::Extern { typing, .. } => typing,
            _ => panic!("as_typing called for pending/lowering function"),
        }
    }

    pub fn take_in_callstack(&mut self) -> Current {
        let this = std::mem::replace(self, FunctionStatus::Lowering);
        match this {
            FunctionStatus::InCallStack(current) => current,
            _ => panic!("not in callstack"),
        }
    }
}

impl<'a, 's> Verify<'a, 's> {
    pub fn start_at(
        pinfo: ProjectInfo,
        target: Target,
        hir: &'a HIR<'s>,
        fields: &'a Map<key::Module, HashMap<&'s str, Vec<M<key::Record>>>>,
        tenvs: &mut MMap<key::Func, TEnv<'s>>,
        iquery: &ImplIndex,
        funcs: &'a mut MMap<key::Func, FunctionStatus>,
        rotable: &'a mut MMap<key::ReadOnly, (ReadOnlyBytes, Type)>,
        func: M<key::Func>,
    ) {
        match &funcs[func] {
            FunctionStatus::Done(_) => {}
            FunctionStatus::Extern { .. } => {}
            FunctionStatus::InCallStack(_) | FunctionStatus::Lowering => {
                panic!("function is being lowered but from nowhere")
            }
            FunctionStatus::Pending => {
                let (pforall, fdef) = match &hir.funcs[func] {
                    hir::FuncDefKind::Defined(fdef) => (&Forall::new(0), fdef),
                    hir::FuncDefKind::TraitDefaultMethod(tr, _, _, fdef) => {
                        let pforall = &hir.traits[*tr].1;
                        (pforall, fdef)
                    }
                    hir::FuncDefKind::ImplMethod(impl_, fdef) => {
                        let pforall = &hir.impls[*impl_];
                        (pforall, fdef)
                    }
                    hir::FuncDefKind::InheritedDefault(_, _) => {
                        todo!(
                            "I think we do want to generate the `Done` form by copying the other"
                        );
                    }
                    hir::FuncDefKind::TraitHeader(_, forall, typing) => {
                        let forall = forall.rename_to_keys();
                        let typing =
                            lower::ConcreteTyping::from_hir(forall, typing, |ty| (*ty).clone());

                        let function =
                            lower::Function::new(typing, Map::new(), Map::new(), mir::Expr::Poison);
                        funcs[func] = FunctionStatus::Done(function);
                        return;
                    }
                    hir::FuncDefKind::Extern { link_name, typing } => {
                        let typing = Self::lower_extern(typing);
                        funcs[func] =
                            FunctionStatus::Extern { link_name: link_name.clone(), typing };
                        return;
                    }
                };

                funcs[func] = FunctionStatus::Lowering;

                let current = Current::new(func, fdef.lambdas.keys());

                let langs = LangItems::new(fdef.list, pinfo);
                let mut lower = Verify::new(
                    &hir, iquery, tenvs, langs, funcs, rotable, target, current, fields, pforall,
                    fdef,
                );

                let function = lower.lower_func();
                funcs[func] = FunctionStatus::Done(function);
            }
        }
    }

    // Returns `None` if already lowering
    fn query_func(&mut self, func: M<key::Func>) -> Result<(), bool> {
        match &self.funcs[func] {
            FunctionStatus::Done(_) => Ok(()),
            FunctionStatus::Extern { .. } => Ok(()),
            FunctionStatus::Lowering => Err(true),
            FunctionStatus::InCallStack(_) => Err(false),
            FunctionStatus::Pending => {
                match &self.hir.funcs[func] {
                    hir::FuncDefKind::Extern { link_name, typing } => {
                        let typing = Self::lower_extern(typing);

                        self.funcs[func] =
                            FunctionStatus::Extern { link_name: link_name.clone(), typing };

                        return Ok(());
                    }
                    other => {
                        // Change child function from pending to lowering
                        self.funcs[func] = FunctionStatus::Lowering;

                        // Take the current function and mark it as being in the callstack
                        let previous_fdef = self.fdef;
                        self.fdef = other.as_defined();
                        let previous = std::mem::replace(
                            &mut self.current,
                            Current::new(func, self.fdef.lambdas.keys()),
                        );
                        let pfkey = previous.fkey;
                        self.funcs[pfkey] = FunctionStatus::InCallStack(previous);

                        info!(
                            "lowering func due to inference requirements func // {}",
                            self.hir.func_names[func]
                        );

                        // Lower the child function (that's now set to current)
                        assert_eq!(func, self.current.fkey);
                        let function = self.lower_func();
                        self.funcs[func] = FunctionStatus::Done(function);

                        // Switch back to what used to be the current function
                        self.current = self.funcs[pfkey].take_in_callstack();
                        self.fdef = previous_fdef;

                        Ok(())
                    }
                }
            }
        }
    }

    fn lower_extern(typing: &hir::Typing<Type>) -> lower::ConcreteTyping {
        lower::ConcreteTyping::from_hir(Forall::new(0), typing, |t| (**t).clone())
    }

    fn lower_func(&mut self) -> lower::Function {
        let module = self.hir.sources.name_of_module(self.current.fkey.0);
        let entity = *self.hir.func_names[self.current.fkey];

        let _span = info_span!("lowering function", module, entity);
        info!("typing is: {}", &self.fdef.typing);
        let _handle = _span.enter();

        let module = self.current.fkey.0;

        trace!("type checking parameters");
        self.type_check_pat_params(&self.fdef.typing.params, &self.fdef.params);

        trace!("type checking expression");
        let ret = self.type_check_expr(self.fdef.expr.as_ref());
        let ret = ret.as_ref();
        let expected = self.fdef.typing.returns.as_ref();
        self.type_check_and_emit(ret, expected);

        let fkey = self.current.fkey;

        for lkey in self.fdef.lambdas.keys() {
            self.current.lambda = Some(lkey);
            let typing = &self.fdef.lambdas.typings[lkey];
            let params = &self.fdef.lambdas.params[lkey];
            let expr = self.fdef.lambdas.bodies[lkey].as_ref();

            trace!("type checking lambda parameters for {lkey}");
            self.type_check_pat_params(&typing.params, params);

            trace!("type checking lambda expr for {lkey}");
            let ret = self.type_check_expr(expr);

            let ret = ret.as_ref();
            let expected = typing.returns.as_ref();
            self.type_check_and_emit(ret, expected);
        }

        let mut forall = self.fdef.forall.borrow_mut();
        let lforalls = self.fdef.lambdas.foralls.borrow_mut();

        // Finalize all types by defaulting and resolving any remaining inference
        let ts = self.hir.type_system(
            &mut self.tenvs[fkey],
            self.target,
            &self.field_lookup[module],
        );
        let default_to_generic = match &self.hir.funcs[fkey] {
            hir::FuncDefKind::Defined(_) => Some(&mut *forall),
            _ => None,
        };
        Finalizer::new(ts, default_to_generic).infer_all_unknown_types();

        // Finalization pass transforming HIR to MIR
        let mut finalization = lower::Lower::new(
            &self.tenvs[fkey],
            &mut self.read_only_table,
            &mut self.current,
            self.items,
            &self.hir.fnames,
            &self.hir.field_types,
            &self.hir.variant_types,
            self.target,
        );
        finalization.current.lambda = None;

        let typing = finalization.typing(&forall, &self.fdef.typing);
        info!("typing finalised to: {typing}");

        let expr = finalization.patterns_and_expr(
            &typing.params,
            &self.fdef.params,
            self.fdef.expr.as_ref(),
        );
        info!("expr finalised to:\n  {expr}");

        let lambdas: Map<key::Lambda, lower::Lambda> = {
            let lambdas = &self.fdef.lambdas;

            lambdas
                .typings
                .iter()
                .map(|(lkey, typing)| {
                    finalization.current.lambda = Some(lkey);

                    let typing = finalization.typing(&lforalls[lkey], &typing);
                    info!("{lkey} typing finalised to: {typing}");

                    let expr = finalization.patterns_and_expr(
                        &typing.params,
                        &lambdas.params[lkey],
                        lambdas.bodies[lkey].as_ref(),
                    );
                    info!("{lkey} expr finalised to:\n  {expr}");

                    lower::Lambda::new(typing, expr)
                })
                .collect()
        };

        drop(forall);
        let trait_object_cast_checks = finalization.trait_object_cast_checks;

        for error in finalization.errors.into_iter() {
            let tfmt = self.ty_formatter();
            let sources = &self.hir.sources;
            lower::emit_fin_error(sources, module, tfmt, error);
        }

        self.check_all_constraints(&lambdas, &typing, trait_object_cast_checks);

        let lcaptures = self.fdef.lambdas.captures.clone();

        let mut function = lower::Function::new(typing, lambdas, lcaptures, expr);
        function.no_mangle = self.fdef.no_mangle;

        function
    }

    fn check_all_constraints(
        &mut self,
        lambdas: &Map<key::Lambda, lower::Lambda>,
        typing: &lower::ConcreteTyping,
        casts: Vec<(Tr<Type>, Constraint<Static>)>,
    ) {
        let fkey = self.current.fkey;

        // HACK: We substitute `Self` in implementation methods so we don't need to handle them
        // BUT we forgot that default trait methods exist of where we can't substitute. So; we
        // now need to partially track `Self` regardless making the effort of substituting
        // `Self` useless.
        let in_trait = match &self.hir.funcs[fkey] {
            hir::FuncDefKind::TraitDefaultMethod(key, _, _, _) => {
                let params = &self.hir.traits[*key].1;
                Some((*key, params))
            }
            _ => None,
        };
        let pforall = self.pforall;
        let lhs_forall = &|kind| match kind {
            GenericKind::Lambda(lkey) => &lambdas[lkey].typing.forall,
            GenericKind::Entity => &typing.forall,
            GenericKind::Parent => pforall,
        };
        let get_impl_data = &|ikey: M<key::Impl>| {
            let impltor = &*self.hir.impltors[ikey];
            let iforall = &self.hir.impls[ikey];
            let (trait_, trait_params) = &self.hir.itraits[ikey];
            (*trait_, iforall, impltor, trait_params.as_slice())
        };

        for (ty, constraint) in casts {
            Compatibility::constraint(
                self.iquery,
                in_trait,
                get_impl_data,
                lhs_forall,
                &ty,
                &constraint,
            );
        }

        let env = &mut self.tenvs[self.current.fkey];
        for conerr in self
            .hir
            .type_system(env, self.target, &self.field_lookup[fkey.0])
            .check_all_constraints(self.iquery, in_trait, lhs_forall, get_impl_data)
        {
            match conerr {
                ConstraintError::GotInt { span, expected, .. } => {
                    let exp = self.ty_formatter().fmt(&expected);
                    self.emit_type_mismatch(span, "", "integer", exp);
                }
                ConstraintError::IntConstantNegativeUnsigned(_, _) => todo!(),
                ConstraintError::IntConstantTooLarge(_, _, _) => todo!(),
                ConstraintError::Trait(ty, con) if con.trait_ == self.items.pinfo.listable => {
                    let tfmt = self.ty_formatter();
                    let got = tfmt.clone().fmt(&*ty);
                    let exp = format!("[{}]", tfmt.fmt(&con.params[0]));
                    self.emit_type_mismatch(ty.span, "", got, exp);
                }
                ConstraintError::Trait(ty, con) => {
                    self.error("constraint not met")
                        .eline(
                            ty.span,
                            format!(
                                "`{}` does not implement `{}`",
                                self.ty_formatter().fmt(&*ty),
                                self.ty_formatter().fmt((con.trait_, con.params.as_slice()))
                            ),
                        )
                        .emit();
                }
                ConstraintError::FieldType { exp, got, at } => {
                    let tfmt = self.ty_formatter();
                    self.emit_type_mismatch(at, "", tfmt.clone().fmt(&got), tfmt.fmt(exp));
                }
                ConstraintError::RecordNotFound(fields) => self
                    .error("record not found")
                    .eline(
                        fields[0].span,
                        format!(
                            "no record in scope with the fields `{{{}}}`",
                            fields.iter().format(", ")
                        ),
                    )
                    .emit(),
                ConstraintError::RecordAmbigous(span, records, fields) => self
                    .error("record not found")
                    .eline(
                        span,
                        format!(
                            "known to have the fields {{{}}}",
                            fields.iter().format(", ")
                        ),
                    )
                    .text(format!(
                        "which could infer to {}",
                        records
                            .iter()
                            .map(|record| self
                                .ty_formatter()
                                .fmt((*record, &[] as &[Type]))
                                .to_string())
                            .format(" or ")
                    ))
                    .emit(),
            }
        }
    }

    #[track_caller]
    pub fn new_bind_as(&mut self, key: key::Bind, ty: Tr<IType>) {
        self.current.binds.insert(key, ty);
    }

    pub fn type_of(&mut self, bind: key::Bind) -> Tr<&IType> {
        self.current.binds[&bind].as_ref()
    }

    pub fn vars(&mut self) -> &mut TEnv<'s> {
        &mut self.tenvs[self.current.fkey]
    }

    // TODO: we're probably gonna buffer errors instead?
    //
    // or perhaps buffer them on a per-function basis to then attach context on the callback for
    // less verbosity?
    pub fn error(&self, name: &'static str) -> ast::ErrorBuilder<'a> {
        self.hir.sources.error(name).m(self.current.fkey.0)
    }
    pub fn warning(&self, name: &'static str) -> ast::ErrorBuilder<'a> {
        self.hir.sources.warning(name).m(self.current.fkey.0)
    }

    pub fn ty_formatter(&'a self) -> TyFmtState<'a, 's> {
        let forall = self.fdef.forall.borrow().names().collect();
        let pforall = self.pforall.names().collect();
        let isize = self.target.int_size();
        let env = &self.tenvs[self.current.fkey];
        TyFmtState::new(self.hir, env, isize, forall, pforall)
    }

    pub fn module(&self) -> key::Module {
        self.current.fkey.0
    }

    pub fn lambda(&self) -> Option<key::Lambda> {
        self.current.lambda
    }

    pub fn inst_direct_recursion(&mut self, span: Span) -> InstInfo {
        let forall = self.fdef.forall.borrow();
        let typing = &self.fdef.typing;

        let mut inst = GenericMapper::inst(&mut self.tenvs[self.current.fkey])
            .forall(GenericKind::Entity, span, &forall)
            .icons(GenericKind::Entity, &forall);

        if inst.env.self_.is_some() {
            inst = inst.with_self(span);
        }

        let mapper = inst.into_mapper();

        let (ptypes, returns) = typing.map(|ty| DirectRecursion(&mapper).transform(*ty));

        let module = self.module();
        InstInfo::new(module, mapper, ptypes, returns.clone())
    }

    pub fn inst_indirect_recursion(&mut self, span: Span, key: M<key::Func>) -> InstInfo {
        let target = self.hir.funcs[key].as_defined();
        let mut forall = target.forall.borrow_mut();
        let [from, to] = self.tenvs.get_both([self.current.fkey, key]);
        let mut cinst = GenericMapper::circular(from, to, &mut *forall, span);

        let (ptypes, returns) = target.typing.map(|ty| cinst.transform(*ty));

        let (inst, failures) = cinst.finalize();
        for fspan in failures {
            self.hir
                .sources
                .error("inference failure")
                .m(key.0)
                .eline(fspan, "")
                .m(self.current.fkey.0)
                .iline(span, "caused by this recursive call")
                .emit();
        }

        InstInfo::new(key.0, inst, ptypes, returns.clone())
    }

    pub fn type_system(&mut self) -> TypeSystem<'_, 's> {
        let env = &mut self.tenvs[self.current.fkey];
        let fields = &self.field_lookup[self.current.fkey.0];
        self.hir.type_system(env, self.target, fields)
    }

    pub fn module_of_type(&mut self, ty: Tr<&IType>) -> Option<key::Module> {
        match &ty.value {
            IType::Simple(_) => None,
            IType::Container(Container::Defined(key, _), _) => Some(key.0),
            IType::Special(var) => self
                .type_system()
                .try_get_known_type(*var)
                .and_then(|ty| self.module_of_type(ty.as_ref())),
            IType::Container(Container::Pointer, _) => self.hir.lookups.find_lib("std", "ptr"),
            IType::Int(size) => self.hir.lookups.find_lib("std", "math").and_then(|math| {
                match self
                    .hir
                    .lookups
                    .resolve_module(math, &[size.to_string().as_str()])
                {
                    Ok(ast::Mod { key: ast::Entity::Module(module), .. }) => Some(module),
                    _ => {
                        warn!("skipping resolving the stdlib std:math:{size} because it is not available");
                        None
                    }
                }
            }),
            _ => None,
        }
    }

    pub fn type_of_variant(
        &mut self,
        span: Span,
        sum: M<key::Sum>,
        var: key::Variant,
    ) -> (GenericMapper<Inference>, Vec<Tr<IType>>, Tr<IType>) {
        let forall = &self.hir.sums[sum].1;
        let vars = self.vars();

        let finst = GenericMapper::inst(vars)
            .forall(GenericKind::Entity, span, forall)
            .cons(GenericKind::Entity, forall)
            .into_mapper();

        let ptypes = self.hir.variant_types[sum][var]
            .iter()
            .map(|ty| (&finst).transform(&**ty).tr(ty.span))
            .collect::<Vec<_>>();

        let key = sum.map(key::TypeKind::Sum);
        let returns = finst.to_defined(GenericKind::Entity, key);

        (finst, ptypes, returns.tr(span))
    }

    pub fn ty_as_callable(&mut self, span: Span, ty: Tr<IType>, params: usize) -> InstCall {
        match self.type_system().call_as_function(span, &ty, params) {
            Some((kind, ptypes, returns)) => InstCall::LocalCall(span, ptypes, returns, kind),
            None if params == 0 => InstCall::Local(ty.value.tr(span)),
            None => InstCall::Local(ty),
        }
    }

    pub fn type_check_pass(
        &mut self,
        span: Span,
        call: &hir::Callable<'s>,
        tanot: &hir::TypeAnnotation<'s>,
        params: Vec<Tr<IType>>,
    ) -> (IType, IType) {
        let lhs = params.last();
        match self.type_of_callable(span, lhs, call, params.len(), tanot) {
            InstCall::LocalCall(_, ptypes, returns, cont) => {
                let (applicated, xs) = ptypes.split_at(params.len());
                params.iter().zip(applicated).for_each(|(g, e)| {
                    self.type_check_and_emit(g.as_ref(), e.tr(span));
                });

                let mut remaining = xs.to_vec();
                remaining.push(returns.clone());
                (IType::Container(cont, remaining), returns)
            }
            InstCall::Local(_) if !params.is_empty() => todo!("ET: can not take parameters"),
            InstCall::Local(_) => todo!("turn it into a function"),
            InstCall::Instantiated(instinfo) => {
                self.partially_applicate_inst(span, instinfo, params)
            }
            InstCall::DirectRecursion => {
                let instinfo = self.inst_direct_recursion(span);
                self.partially_applicate_inst(span, instinfo, params)
            }
            InstCall::CircularRecursion { .. } => {
                todo!()
            }
            InstCall::TypeDependentFailure => (IType::poison(), IType::poison()),
        }
    }

    fn partially_applicate_inst(
        &mut self,
        span: Span,
        instinfo: InstInfo,
        params: Vec<Tr<IType>>,
    ) -> (IType, IType) {
        self.type_check_and_emit_application(span, &params, &instinfo.ptypes[..params.len()]);

        let ret = instinfo.ret.value.clone();
        let ptypes = instinfo.ptypes[params.len()..]
            .iter()
            .map(|t| t.value.clone())
            .collect();

        self.current.push_inst(span, Some(instinfo));

        (IType::closure(ptypes, ret.clone()), ret)
    }

    pub fn type_of_callable(
        &mut self,
        span: Span,
        lhs: Option<&Tr<IType>>,
        callable: &hir::Callable<'s>,
        params: usize,
        tanot: &hir::TypeAnnotation<'s>,
    ) -> InstCall {
        let m = self.module();

        match callable {
            hir::Callable::TypeDependentLookup(name) => {
                let target_module = self.module_of_type(lhs.unwrap().as_ref()).unwrap_or(m);

                // First resolve in the lhs's types module
                //
                // Then try the current module
                let result = self
                    .hir
                    .lookups
                    .resolve_func(target_module, &[*name])
                    .or_else(|error| match error {
                        ast::ImportError::NotFound(..) => self
                            .hir
                            .lookups
                            .resolve_func(m, &[*name])
                            .map_err(|_| error),
                        _ => Err(error),
                    });

                match result {
                    Ok(entity) => match entity.key {
                        ast::Entity::Func(nfunc) => {
                            self.current
                                .type_dependent_lookup
                                .push_back(M(entity.module, nfunc));
                            self.type_of_nfunc(span, M(entity.module, nfunc), tanot)
                        }
                        ast::Entity::Member(_, _) => todo!(),
                        ast::Entity::Module(_) => todo!(),
                        ast::Entity::Type(_) => {
                            self.error("invalid function")
                                .eline(span, format!("{name} is a type, not a function"))
                                .emit();

                            InstCall::TypeDependentFailure
                        }
                    },
                    Err(error) => {
                        self.hir.sources.emit_lookup_err(span, m, "function", error);
                        InstCall::TypeDependentFailure
                    }
                }
            }
            hir::Callable::Builtin(name) => match *name {
                "plus" | "minus" | "mul" | "div" => {
                    let any = Ty::infer(self.vars().var(span));
                    let ptypes = vec![any.clone(), any.clone()];
                    InstCall::LocalCall(span, ptypes, any, Container::FnPointer)
                }
                "plus_checked" | "minus_checked" | "mul_checked" | "div_checked" => {
                    let any = Ty::infer(self.vars().var(span));
                    let ptypes = vec![any.clone(), any.clone()];
                    let ret = Ty::tuple(vec![any, Ty::bool()]);
                    InstCall::LocalCall(span, ptypes, ret, Container::FnPointer)
                }
                "iabs" => {
                    let any = Ty::infer(self.vars().var(span));
                    let ptypes = vec![any.clone()];
                    InstCall::LocalCall(span, ptypes, any, Container::FnPointer)
                }
                "eq" | "lt" | "gt" => {
                    let any = Ty::infer(self.vars().var(span));
                    let ptypes = vec![any.clone(), any];
                    InstCall::LocalCall(span, ptypes, Ty::bool(), Container::FnPointer)
                }
                "deref" => {
                    let any = Ty::infer(self.vars().var(span));
                    let ptypes = vec![Ty::pointer(any.clone())];
                    InstCall::LocalCall(span, ptypes, any, Container::FnPointer)
                }
                "write" => {
                    let any = Ty::infer(self.vars().var(span));
                    let ptypes = vec![Ty::pointer(any.clone()), any.clone()];
                    let ret = Ty::tuple(vec![]);
                    InstCall::LocalCall(span, ptypes, ret, Container::FnPointer)
                }
                "offset" => {
                    let any = Ty::infer(self.vars().var(span));
                    let ptr = Ty::pointer(any.clone());
                    let ptypes = vec![ptr.clone(), Ty::Int(self.target.int())];
                    InstCall::LocalCall(span, ptypes, ptr, Container::FnPointer)
                }
                "reflect_type" => {
                    InstCall::Local(Ty::defined(self.items.pinfo.reflect_type, vec![]).tr(span))
                }
                "size_of" => {
                    // TODO: 32-bit
                    InstCall::Local(Ty::Int(self.target.uint()).tr(span))
                }
                "unreachable" => InstCall::Local(Ty::infer(self.vars().var(span)).tr(span)),
                "transmute" => {
                    let param = Ty::infer(self.vars().var(span));
                    let ptypes = vec![param];
                    let ret = Ty::infer(self.vars().var(span));
                    InstCall::LocalCall(span, ptypes, ret, Container::FnPointer)
                }
                "val_to_ref" => {
                    let any = Ty::infer(self.vars().var(span));
                    let ptr = Ty::pointer(any.clone());
                    let ptypes = vec![any];
                    let ret = ptr;
                    InstCall::LocalCall(span, ptypes, ret, Container::FnPointer)
                }
                _ => {
                    self.error("unrecognised builtin").eline(span, "").emit();
                    InstCall::Local(Ty::poison().tr(span))
                }
            },
            hir::Callable::Func(mnfunc) => {
                self.type_of_nfunc(span, M(mnfunc.module, mnfunc.key), tanot)
            }
            hir::Callable::Binding(bind) => {
                let ty = self.current.binds[bind].clone();
                self.ty_as_callable(span, ty, params)
            }
            hir::Callable::Lambda(lkey) => {
                let instinfo = self.type_of_lambda(span, *lkey);
                self.apply_tanot(tanot, &instinfo.inst, Either::Right(*lkey));
                InstCall::Instantiated(instinfo)
            }
        }
    }

    pub fn type_of_lambda(&mut self, span: Span, lkey: key::Lambda) -> InstInfo {
        let forall = &self.fdef.lambdas.foralls.borrow()[lkey];
        let typing = &self.fdef.lambdas.typings[lkey];

        let inst = GenericMapper::inst(&mut self.tenvs[self.current.fkey])
            .forall(GenericKind::Lambda(lkey), span, &forall)
            .icons(GenericKind::Lambda(lkey), &forall)
            .into_mapper();

        let (ptypes, returns) =
            DirectRecursion(&inst).transform_typing(&typing.params, &typing.returns);

        let module = self.module();
        let iinfo = InstInfo::new(module, inst, ptypes, returns.clone());

        iinfo
    }

    pub fn type_of_nfunc(
        &mut self,
        span: Span,
        M(module, key): M<NFunc>,
        tanot: &hir::TypeAnnotation<'s>,
    ) -> InstCall {
        let (mut finst, ptypes, returns) = match key {
            ast::NFunc::Key(func) => return self.type_of_func(span, func.inside(module), tanot),
            ast::NFunc::Method(trait_, method) => {
                let func = self.hir.methods[trait_.inside(module)][method];
                return self.type_of_func(span, func.inside(module), tanot);
            }
            ast::NFunc::Val(val) => {
                let func = self.hir.val_initializers[val.inside(module)];
                return self.type_of_func(span, func, tanot);
            }
            ast::NFunc::SumVar(sum, var) => self.type_of_variant(span, sum.inside(module), var),
        };

        self.apply_tanot(tanot, &mut finst, Either::Left(M(module, key)));
        let linfo = InstInfo::new(module, finst, ptypes, returns);

        InstCall::Instantiated(linfo)
    }

    pub fn type_of_func(
        &mut self,
        span: Span,
        func: M<key::Func>,
        tanot: &hir::TypeAnnotation<'s>,
    ) -> InstCall {
        match &self.hir.funcs[func] {
            hir::FuncDefKind::TraitHeader(trait_, forall, typing) => {
                let vars = &mut self.tenvs[self.current.fkey];
                let pforall = &self.hir.traits[*trait_].1;
                let finst = GenericMapper::inst(vars)
                    .with_self(span)
                    .forall(GenericKind::Entity, span, forall)
                    .forall(GenericKind::Parent, span, pforall)
                    .cons(GenericKind::Entity, forall)
                    .cons(GenericKind::Parent, pforall)
                    .into_mapper();

                let (ptypes, returns) = (&finst).transform_typing(&typing.params, &typing.returns);
                self.apply_tanot(tanot, &finst, Either::Left(func.map(NFunc::Key)));
                let Some(Ty::Special(self_var)) = finst.self_ else {
                    panic!("non-var assignment to `self` from instantiation");
                };

                let params = finst.to_types(GenericKind::Parent);
                self.vars()
                    .add_trait_constraint(self_var, Constraint { span, trait_: *trait_, params });

                let linfo = InstInfo::new(func.0, finst, ptypes, returns);

                InstCall::Instantiated(linfo)
            }
            _ => match self.query_func(func) {
                Ok(_) => {
                    let typing = self.funcs[func].as_typing();
                    let vars = &mut self.tenvs[self.current.fkey];
                    let finst = GenericMapper::inst(vars);
                    let forall = &typing.forall;

                    // Attach the impl blocks `forall` to the instantiation if this function is
                    // defined as a member to an implementation.
                    let pforall = match &self.hir.funcs[func] {
                        hir::FuncDefKind::ImplMethod(imp, _) => Some(&self.hir.impls[*imp]),
                        hir::FuncDefKind::TraitDefaultMethod(tr, _, _, _) => {
                            Some(&self.hir.traits[*tr].1)
                        }
                        _ => None,
                    };

                    let finst = match pforall {
                        Some(pforall) => finst
                            .with_self(span)
                            .forall(GenericKind::Entity, span, forall)
                            .forall(GenericKind::Parent, span, pforall)
                            .cons(GenericKind::Entity, forall)
                            .cons(GenericKind::Parent, pforall)
                            .into_mapper(),
                        None => finst
                            .forall(GenericKind::Entity, span, forall)
                            .cons(GenericKind::Entity, forall)
                            .into_mapper(),
                    };

                    // Attach the constraint from `self` if this is a method
                    if let Some(var) = &finst.self_ {
                        let (trait_, params) = match &self.hir.funcs[func] {
                            hir::FuncDefKind::TraitDefaultMethod(trait_, _, _, _) => {
                                (*trait_, finst.to_types(GenericKind::Parent))
                            }
                            hir::FuncDefKind::ImplMethod(ikey, _) => {
                                let (trait_, i_trait_params) = &self.hir.itraits[*ikey];
                                let trait_params = (&finst).transforms(i_trait_params);
                                (*trait_, trait_params)
                            }
                            hir::FuncDefKind::InheritedDefault(_, _) => todo!(),
                            _ => panic!("unexpected `self`"),
                        };

                        let Ty::Special(self_var) = var else {
                            panic!("non-var assignment to `self` from instantiation");
                        };

                        self.tenvs[self.current.fkey]
                            .add_trait_constraint(*self_var, Constraint { span, trait_, params });
                    }

                    // HACK: we need the spans from the HIR as they weren't copied to the MIR.
                    //
                    // This seemed like a good idea when all called functions where defined, but
                    // that's no longer the case.
                    let (types, ret) = match &self.hir.funcs[func] {
                        hir::FuncDefKind::TraitHeader(_, _, etyping)
                        | hir::FuncDefKind::Extern { typing: etyping, .. } => {
                            let types = typing
                                .params
                                .iter()
                                .zip(&etyping.params)
                                .map(|(p, (_, hir_ty))| (p).tr(hir_ty.span))
                                .collect::<Vec<_>>();

                            let ret = (&typing.returns).tr(etyping.returns.span);
                            (types, ret)
                        }
                        hir::FuncDefKind::ImplMethod(_, f)
                        | hir::FuncDefKind::TraitDefaultMethod(_, _, _, f)
                        | hir::FuncDefKind::Defined(f) => {
                            let types = typing
                                .params
                                .iter()
                                .zip(&f.typing.params)
                                .map(|(p, (_, hir_ty))| (p).tr(hir_ty.span))
                                .collect();

                            let ret = (&typing.returns).tr(f.typing.returns.span);
                            (types, ret)
                        }
                        hir::FuncDefKind::InheritedDefault(_, _) => unreachable!(),
                    };

                    let ptypes = types
                        .into_iter()
                        .map(|ty| (&finst).transform_spanned(ty))
                        .collect();
                    let returns = (&finst).transform_spanned(ret);

                    self.apply_tanot(tanot, &finst, Either::Left(func.map(NFunc::Key)));
                    let linfo = InstInfo::new(func.0, finst, ptypes, returns);

                    InstCall::Instantiated(linfo)
                }
                Err(true) => InstCall::DirectRecursion,
                Err(false) => InstCall::CircularRecursion(func),
            },
        }
    }

    pub fn apply_tanot(
        &mut self,
        tanot: &hir::TypeAnnotation<'s>,
        finst: &GenericMapper<Inference>,
        func: Either<M<NFunc>, key::Lambda>,
    ) -> Option<Span> {
        use Either::*;

        let ty_lookup = match func {
            Left(M(module, nfunc)) => match nfunc {
                NFunc::Val(_) => None,
                NFunc::Key(func) => match &self.hir.funcs[func.inside(module)] {
                    hir::FuncDefKind::TraitHeader(key, _, _) => Some(&self.hir.traits[*key].1),
                    _ => None,
                },
                NFunc::Method(tkey, _) => Some(&self.hir.traits[tkey.inside(module)].1),
                NFunc::SumVar(tkey, _) => Some(&self.hir.sums[tkey.inside(module)].1),
            },
            Right(_) => None,
        };

        let mut self_span = None;

        match (ty_lookup, tanot.for_type.is_empty()) {
            (_, true) => {}
            (Some(lookup), false) => {
                for (name, ty) in tanot.for_type.iter() {
                    if **name == "self" {
                        // self.type_system().assign_self(finst, tr(ty));
                        let Some(self_ty) = &finst.self_ else {
                            self.error("unknown generic")
                                .eline(name.span, "type has no `self` parameter")
                                .emit();

                            continue;
                        };

                        self.type_check_and_emit((ty).tr(name.span), (self_ty).tr(name.span));

                        self_span = Some(name.span);
                        continue;
                    }

                    let Some(generic) = lookup.find(**name) else {
                        // TODO: merge multiple unknown generics
                        self.error("unknown generic").eline(name.span, "").emit();
                        continue;
                    };

                    let exp = finst
                        .find(Generic::new(generic, GenericKind::Parent))
                        .unwrap();

                    self.type_check_and_emit((ty).tr(name.span), (exp).tr(name.span));
                }
            }
            (None, false) => panic!("ET: invalid generic annotation"),
        }

        match func {
            Left(M(_, NFunc::SumVar(..) | NFunc::Val(_))) | Right(_)
                if !tanot.for_entity.is_empty() =>
            {
                panic!("ET: can not annotate")
            }
            _ => {}
        }

        for (name, ty) in tanot.for_entity.iter() {
            let generic = match func {
                Left(M(module, nfunc)) => match nfunc {
                    NFunc::Key(func) => match &self.hir.funcs[func.inside(module)] {
                        hir::FuncDefKind::ImplMethod(_, fdef) | hir::FuncDefKind::Defined(fdef) => {
                            fdef.forall.borrow().find(**name)
                        }
                        hir::FuncDefKind::TraitDefaultMethod(_, forall, _, _)
                        | hir::FuncDefKind::TraitHeader(_, forall, _) => forall.find(**name),
                        hir::FuncDefKind::InheritedDefault(_, _) => unreachable!(),
                        hir::FuncDefKind::Extern { .. } => {
                            panic!("generics for extern function")
                        }
                    },
                    NFunc::Method(trait_, method) => {
                        let func = self.hir.methods[trait_.inside(module)][method];
                        let forall = &self.hir.funcs[func.inside(module)]
                            .as_defined()
                            .forall
                            .borrow();
                        forall.find(**name)
                    }
                    NFunc::SumVar(_, _) => todo!(),
                    NFunc::Val(_) => todo!(),
                },
                Right(lkey) => self.fdef.lambdas.foralls.borrow()[lkey].find(**name),
            };

            match generic {
                Some(generic) => {
                    let exp = finst
                        .find(Generic::new(generic, GenericKind::Entity))
                        .unwrap();

                    self.type_check_and_emit((ty).tr(name.span), (exp).tr(name.span));
                }
                None => {
                    let err = self
                        .error("generic not found")
                        .m(self.module())
                        .eline(name.span, "the function does not have this generic");

                    if **name == "self"
                        || ty_lookup.map(|forall| forall.find(**name).is_some()) == Some(true)
                    {
                        err.text(
                            "the type this function is a member of does however have that generic",
                        )
                    } else {
                        err
                    }
                    .emit()
                }
            }
        }

        self_span
    }
}

#[derive(Debug)]
pub enum InstCall {
    LocalCall(Span, Vec<IType>, IType, Container),
    Local(Tr<IType>),
    Instantiated(InstInfo),
    DirectRecursion,
    CircularRecursion(M<key::Func>),
    TypeDependentFailure,
}

impl fmt::Display for FunctionStatus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionStatus::Done(func) => func.fmt(f),
            FunctionStatus::Extern { link_name, typing } => {
                write!(
                    f,
                    "{} {typing} {} {link_name}",
                    "as".keyword(),
                    "extern".keyword()
                )
            }
            FunctionStatus::InCallStack(_) => "<current>".fmt(f),
            FunctionStatus::Pending => "<pending>".fmt(f),
            FunctionStatus::Lowering => "<lowering>".fmt(f),
        }
    }
}
