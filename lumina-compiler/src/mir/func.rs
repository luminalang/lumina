use super::{lower, tyfmt::TyFmtState, Current, LangItems, RSolver, ReadOnlyBytes, Verify};
use crate::prelude::*;
use crate::{ProjectInfo, Target};
use ast::NFunc;
use either::Either;
use hir::HIR;
use lumina_typesystem::{
    Constraint, Container, DirectRecursion, Finalizer, Forall, Generic, GenericKind, GenericMapper,
    IType, ImplIndex, Inference, IntSize, RecordAssignment, RecordVar, TEnv, Transformer, Ty, Type,
    TypeSystem,
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

// #[derive(From, Debug, Clone, Copy, Display)]
// pub enum Local {
//     #[from]
//     Param(key::Param),
//     #[from]
//     Binding(key::Bind),
// }

impl<'a, 's> Verify<'a, 's> {
    pub fn start_at(
        pinfo: ProjectInfo,
        target: Target,
        hir: &'a HIR<'s>,
        fields: &'a Map<key::Module, HashMap<&'s str, Vec<M<key::Record>>>>,
        tenvs: &mut ModMap<key::Func, TEnv<'s>>,
        iquery: &ImplIndex,
        funcs: &'a mut ModMap<key::Func, FunctionStatus>,
        rotable: &'a mut ModMap<key::ReadOnly, (ReadOnlyBytes, Type)>,
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
                let rsolver = RSolver::new(fields, &hir.records, &hir.field_types, &hir.fnames);
                let mut lower = Verify::new(
                    &hir, iquery, tenvs, rsolver, langs, funcs, rotable, target, current, pforall,
                    fdef,
                );

                let function = lower.lower_func();
                funcs[func] = FunctionStatus::Done(function);
            }
        }
    }

    pub fn type_system(&mut self) -> TypeSystem<'_, 's> {
        let fkey = self.current.fkey;
        self.rsolver
            .as_typesystem(fkey.module, &mut self.tenvs[fkey])
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
        let module = self.hir.sources.name_of_module(self.current.fkey.module);
        let entity = *self.hir.func_names[self.current.fkey];

        let _span = info_span!("lowering function", module, entity);
        info!("typing is: {}", &self.fdef.typing);
        let _handle = _span.enter();

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
        let mut lforalls = self.fdef.lambdas.foralls.borrow_mut();

        // Finalization pass transforming HIR to MIR
        let mut finalization = lower::Lower::new(
            &mut self.tenvs[fkey],
            &mut self.read_only_table,
            &mut forall,
            &mut lforalls,
            &mut self.current,
            true,
            self.items,
            self.rsolver,
            &self.hir.vnames,
            &self.hir.variant_types,
            self.target,
        );

        let typing = finalization.lower_func_typing(&self.fdef.typing);
        info!("typing finalised to: {typing}");

        finalization.implicits = false;
        finalization.lower_lambda_typings(self.fdef.lambdas.typings.iter());

        finalization.current.lambda = None;
        let expr = finalization.patterns_and_expr(
            &typing.params,
            &self.fdef.params,
            self.fdef.expr.as_ref(),
        );
        info!("expr finalised to:\n  {expr}");
        let (lambdas, errors) = finalization
            .lower_lambda_expressions(&self.fdef.lambdas.bodies, &self.fdef.lambdas.params);

        let dint_size = self.target.int();
        let ts = self.type_system();
        let mut fin = Finalizer::new(ts, &mut forall, &mut lforalls, dint_size, false);
        let num_cons = fin.num_constraints();
        let constraints = fin.constraints();

        let finerrors = fin.errors.into_iter();
        drop(forall);

        for error in errors.into_iter().chain(finerrors.map(lower::FinError::TS)) {
            let tfmt = self.ty_formatter();
            let sources = &self.hir.sources;
            lower::emit_fin_error(sources, self.module(), tfmt, &self.hir.records, error);
        }

        for (span, min, ty) in num_cons {
            let intsize = min
                .map(|(i, num)| IntSize::minimum_for(i, num).max(dint_size))
                .unwrap_or(dint_size);

            let num_type = Type::Int(intsize);

            match ty.value {
                Type::Int(int) => {
                    if let Some((neg, m)) = min {
                        let within = if neg {
                            int.min_value() as i128 >= m as i128
                        } else {
                            int.max_value() as u128 >= m
                        };

                        let err = |msg| {
                            self.error("integer not within bounds")
                                .eline(span, msg)
                                .emit()
                        };

                        if !within {
                            err(format!(
                                "{}{m} does not fit in {int}",
                                neg.then_some("-").unwrap_or(""),
                            ))
                        }

                        if !int.signed && neg {
                            err(format!("can not use negative numbers with {int}"))
                        }
                    }
                }
                Type::Simple("poison") => {}
                _ => {
                    let got = self.ty_formatter().fmt(&num_type);
                    let exp = self.ty_formatter().fmt(&*ty);
                    self.emit_type_mismatch(span, "", got, exp)
                }
            }
        }

        let pforall = self.pforall;

        for (span, ty, con) in constraints {
            let get_forall = &|gkind| match gkind {
                GenericKind::Lambda(lkey) => &lambdas[lkey].typing.forall,
                GenericKind::Entity => &typing.forall,
                GenericKind::Parent => &pforall,
            };

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

            let hit = lumina_typesystem::Compatibility::constraint(
                self.iquery,
                in_trait,
                &|ikey| {
                    let impltor = &self.hir.impltors[ikey];
                    let iforall = &self.hir.impls[ikey];
                    let (trait_, trait_params) = &self.hir.itraits[ikey];
                    (*trait_, iforall, impltor, trait_params)
                },
                &get_forall,
                &ty,
                &con,
            );

            if !hit {
                self.error("constraint not met")
                    .eline(
                        span,
                        format!(
                            "`{}` does not implement `{}`",
                            self.ty_formatter().fmt(&ty),
                            self.ty_formatter().fmt((con.trait_, con.params.as_slice()))
                        ),
                    )
                    .emit();
            }
        }

        let lcaptures = self.fdef.lambdas.captures.clone();

        let mut function = lower::Function::new(typing, lambdas, lcaptures, expr);
        function.no_mangle = self.fdef.no_mangle;

        function
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
        self.hir.sources.error(name).m(self.current.fkey.module)
    }
    pub fn warning(&self, name: &'static str) -> ast::ErrorBuilder<'a> {
        self.hir.sources.warning(name).m(self.current.fkey.module)
    }

    pub fn ty_formatter(&'a self) -> TyFmtState<'a, 's> {
        let forall = self.fdef.forall.borrow().names().collect();
        let pforall = self.pforall.names().collect();
        TyFmtState::new(self.hir, &self.tenvs[self.current.fkey], forall, pforall)
    }

    pub fn module(&self) -> key::Module {
        self.current.fkey.module
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
                .m(key.module)
                .eline(fspan, "")
                .m(self.current.fkey.module)
                .iline(span, "caused by this recursive call")
                .emit();
        }

        InstInfo::new(key.module, inst, ptypes, returns.clone())
    }

    pub fn module_of_type(&mut self, ty: Tr<&IType>) -> Option<key::Module> {
        match &ty.value {
            IType::Simple(_) => None,
            IType::Container(
                Container::List(key) | Container::Defined(key) | Container::String(key),
                _,
            ) => Some(key.module),
            IType::Special(Inference::Var(var)) => {
                let vinfo = self.vars().get(*var);
                match vinfo.value {
                    Some(ty) => {
                        let ty = ty.cloned();
                        self.module_of_type(ty.as_ref())
                    }
                    None => Some(self.module()),
                }
            }
            IType::Special(Inference::Record(record)) => {
                let env = &mut self.tenvs[self.current.fkey];
                match env.get_record(*record) {
                    RecordAssignment::Ok(key, _) => Some(key.module),
                    RecordAssignment::Redirect(var) => {
                        let ty = IType::infrecord(*var).tr(ty.span);
                        self.module_of_type(ty.as_ref())
                    }
                    RecordAssignment::NotRecord(t) => {
                        let t = t.clone().tr(ty.span);
                        self.module_of_type(t.as_ref())
                    }
                    RecordAssignment::Unknown(_) => None,
                    RecordAssignment::None => None,
                }
            }
            IType::Special(Inference::Field(rvar, rfield)) => {
                let fname = self.tenvs[self.current.fkey].name_of_field(*rvar, *rfield);
                self.module_of_field_by_name(ty.span, *rvar, fname)
            }
            IType::Container(Container::Pointer, _) => self.hir.lookups.find_lib(&["std", "ptr"]),
            IType::Int(_) => self.hir.lookups.find_lib(&["std", "math"]),
            _ => None,
        }
    }

    fn module_of_field_by_name(
        &mut self,
        span: Span,
        rvar: RecordVar,
        fname: Tr<&'s str>,
    ) -> Option<key::Module> {
        let env = &mut self.tenvs[self.current.fkey];

        match env.get_record(rvar).clone() {
            lumina_typesystem::RecordAssignment::Ok(key, params) => self
                .type_system()
                .inst_field_by_type_params(key, fname, &params)
                .and_then(|field_ty| self.module_of_type((&field_ty).tr(span))),
            lumina_typesystem::RecordAssignment::Redirect(var) => {
                self.module_of_field_by_name(span, var, fname)
            }
            lumina_typesystem::RecordAssignment::NotRecord(_) => None,
            lumina_typesystem::RecordAssignment::Unknown(_) => None,
            lumina_typesystem::RecordAssignment::None => None,
        }
    }

    pub fn type_of_variant(
        &mut self,
        span: Span,
        sum: M<key::Sum>,
        var: key::SumVariant,
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
        match self.type_system().call_as_function(ty.as_ref(), params) {
            Some((kind, ptypes, returns)) => InstCall::LocalCall(span, ptypes, returns, kind),
            None if params == 0 => InstCall::Local(ty.value.tr(span)),
            None => panic!("ET: can not take parameters (or do we err on this later?)"),
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
                                .push_back(entity.module.m(nfunc));
                            self.type_of_nfunc(span, entity.module.m(nfunc), tanot)
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
                self.type_of_nfunc(span, mnfunc.module.m(mnfunc.key), tanot)
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
        M { module, value }: M<NFunc>,
        tanot: &hir::TypeAnnotation<'s>,
    ) -> InstCall {
        let (mut finst, ptypes, returns) = match value {
            ast::NFunc::Key(func) => return self.type_of_func(span, module.m(func), tanot),
            ast::NFunc::Method(trait_, method) => {
                let func = self.hir.methods[module.m(trait_)][method];
                return self.type_of_func(span, module.m(func), tanot);
            }
            ast::NFunc::Val(val) => {
                let func = self.hir.val_initializers[module.m(val)];
                return self.type_of_func(span, func, tanot);
            }
            ast::NFunc::SumVar(sum, var) => self.type_of_variant(span, module.m(sum), var),
        };

        self.apply_tanot(tanot, &mut finst, Either::Left(module.m(value)));
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
                let Some(Ty::Special(Inference::Var(self_var))) = finst.self_ else {
                    panic!("non-var assignment to `self` from instantiation");
                };

                let params = finst.to_types(GenericKind::Parent);
                self.vars()
                    .push_iconstraint(self_var, Constraint { span, trait_: *trait_, params });

                let linfo = InstInfo::new(func.module, finst, ptypes, returns);

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

                        let Ty::Special(Inference::Var(self_var)) = var else {
                            panic!("non-var assignment to `self` from instantiation");
                        };

                        self.tenvs[self.current.fkey]
                            .push_iconstraint(*self_var, Constraint { span, trait_, params });
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
                    let linfo = InstInfo::new(func.module, finst, ptypes, returns);

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
            Left(nfunc) => match nfunc.value {
                NFunc::Val(_) => None,
                NFunc::Key(func) => match &self.hir.funcs[nfunc.module.m(func)] {
                    hir::FuncDefKind::TraitHeader(key, _, _) => Some(&self.hir.traits[*key].1),
                    _ => None,
                },
                NFunc::Method(tkey, _) => Some(&self.hir.traits[nfunc.module.m(tkey)].1),
                NFunc::SumVar(tkey, _) => Some(&self.hir.sums[nfunc.module.m(tkey)].1),
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
            Left(M { value: NFunc::SumVar(..) | NFunc::Val(_), .. }) | Right(_)
                if !tanot.for_entity.is_empty() =>
            {
                panic!("ET: can not annotate")
            }
            _ => {}
        }

        for (name, ty) in tanot.for_entity.iter() {
            let generic = match func {
                Left(M { module, value }) => match value {
                    NFunc::Key(func) => match &self.hir.funcs[module.m(func)] {
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
                        let func = self.hir.methods[module.m(trait_)][method];
                        let forall = &self.hir.funcs[module.m(func)].as_defined().forall.borrow();
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

    pub fn assign_ty_to_rvar(&mut self, span: Span, var: RecordVar, ty: Tr<&IType>) {
        let result = self.type_system().ascribe_record(ty, span, var);
        self.emit_type_errors((ty, IType::infrecord(var).tr(span).as_ref(), result));
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
