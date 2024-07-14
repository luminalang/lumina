use super::{lower, tyfmt::TyFmtState, Current, LangItems, RSolver, ReadOnlyBytes, Verify, MIR};
use crate::prelude::*;
use crate::ProjectInfo;
use ast::{NFunc, AST};
use derive_more::{Display, From};
use either::Either;
use hir::HIR;
use lumina_typesystem::{
    Bitsize, Container, Finalizer, Forall, ForeignInst, FuncKind, Generic, GenericKind, IType,
    ImplIndex, Prim, RecordError, RecordVar, TEnv, Type, TypeSystem, Var,
};
use lumina_util::Highlighting;
use owo_colors::OwoColorize;
use std::fmt;
use tracing::info_span;

#[derive(Clone, new, Debug)]
pub struct InstInfo {
    pub module: key::Module,
    pub inst: ForeignInst<Var>,
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

#[derive(From, Debug, Clone, Copy, Display)]
pub enum Local {
    #[from]
    Param(key::Param),
    #[from]
    Binding(key::Bind),
}

fn resolve_listable<'s>(lookups: &ast::Lookups<'s>, module: key::Module) -> Option<M<key::Trait>> {
    lookups
        .resolve_type(module, &["std", "list", "Listable"])
        .map(|entity| match entity.key {
            ast::Entity::Type(key::TypeKind::Trait(key)) => entity.module.m(key),
            _ => panic!("core lang-item failure: Listable must be a trait"),
        })
        .ok()
}

impl<'a, 's> Verify<'a, 's> {
    pub fn start_at(
        pinfo: ProjectInfo,
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
                    hir::FuncDefKind::Defined(fdef) => (&Forall::new(), fdef),
                    hir::FuncDefKind::TraitDefaultMethod(tr, fdef) => {
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
                        let typing = lower::ConcreteTyping {
                            forall: forall
                                .values()
                                .map(|gdata| lumina_typesystem::GenericData {
                                    name: "_",
                                    ..gdata.clone()
                                })
                                .collect(),
                            params: typing.params.values().map(|t| t.value.clone()).collect(),
                            returns: typing.returns.value.clone(),
                        };
                        let function =
                            lower::Function::new(typing, Map::new(), Map::new(), mir::Expr::Poison);
                        funcs[func] = FunctionStatus::Done(function);
                        return;
                    }
                    hir::FuncDefKind::Extern { link_name, typing } => {
                        let typing = lower::ConcreteTyping {
                            forall: Forall::new(),
                            params: typing.params.values().map(|t| t.value.clone()).collect(),
                            returns: typing.returns.value.clone(),
                        };
                        funcs[func] =
                            FunctionStatus::Extern { link_name: link_name.clone(), typing };
                        return;
                    }
                };

                let emit_bad_param_count = |span, msg| {
                    hir.sources
                        .error(msg)
                        .m(func.module)
                        .eline(
                            span,
                            format!(
                                "function expects {} parameters from it's type annotation",
                                fdef.typing.params.len()
                            ),
                        )
                        .emit()
                };

                match fdef.typing.params.len().cmp(&fdef.params.len()) {
                    std::cmp::Ordering::Equal => {}
                    std::cmp::Ordering::Less => {
                        let span = fdef.params.last().unwrap().span;
                        emit_bad_param_count(span, "excess parameter pattern")
                    }
                    std::cmp::Ordering::Greater => {
                        let span = fdef.typing.params.last().unwrap().1.span;
                        emit_bad_param_count(span, "missing parameter pattern")
                    }
                }

                funcs[func] = FunctionStatus::Lowering;

                let current = Current::new(func, fdef.lambdas.keys());

                let ProjectInfo { listable, reflect_type, string, .. } = pinfo;

                let langs = LangItems::new(fdef.list, listable, reflect_type, string);
                let flookup = &fields[func.module];
                let rsolver = RSolver::new(flookup, &hir.records, &hir.field_types, &hir.fnames);
                let mut lower = Verify::new(
                    &hir, iquery, tenvs, rsolver, langs, funcs, rotable, current, pforall, fdef,
                );

                let function = lower.lower_func();
                funcs[func] = FunctionStatus::Done(function);
            }
        }
    }

    pub fn type_system(&mut self) -> TypeSystem<'_, 's> {
        let fkey = self.current.fkey;
        self.rsolver.as_typesystem(&mut self.tenvs[fkey])
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
                        let typing = lower::ConcreteTyping {
                            forall: Forall::new(),
                            params: typing.params.values().map(|t| t.value.clone()).collect(),
                            returns: typing.returns.value.clone(),
                        };
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
            self.iquery,
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

        let mut fin = Finalizer::new(self.type_system(), &mut forall, &mut lforalls, false);
        let num_cons = fin.num_constraints();
        let constraints = fin.constraints();

        for error in errors
            .into_iter()
            .chain(fin.errors.into_iter().map(lower::FinError::TS))
        {
            lower::emit_fin_error(&self.hir.sources, self.module(), &self.hir.records, error);
        }

        drop(forall);

        for (span, min, ty) in num_cons {
            let minbit = min
                .map(|(i, num)| Bitsize::minimum_for(i, num).max(Bitsize::default()))
                .unwrap_or_else(Bitsize::default);
            let exp = Type::Prim(Prim::Int(true, minbit));

            match ty.value {
                Type::Prim(Prim::Int(signed, bitsize)) => {
                    if let Some((neg, m)) = min {
                        let within = if neg {
                            bitsize.mini(signed) >= m
                        } else {
                            bitsize.maxi(signed) >= m
                        };

                        if !within {
                            self.error("integer not within bounds")
                                .eline(
                                    span,
                                    format!(
                                        "{}{m} does not fit in {}{bitsize}",
                                        neg.then_some("-").unwrap_or(""),
                                        signed.then_some("i").unwrap_or("u"),
                                    ),
                                )
                                .emit()
                        }

                        if !signed && neg {
                            self.error("integer not within bounds")
                                .eline(
                                    span,
                                    format!("can not use negative numbers with u{bitsize}"),
                                )
                                .emit();
                        }
                    }
                }
                Type::Prim(Prim::Poison) => {}
                _ => self.emit_type_mismatch(
                    span,
                    "",
                    self.ty_formatter().fmt(&*ty),
                    self.ty_formatter().fmt(&exp),
                ),
            }
        }

        for (span, ty, con) in constraints {
            let hit = lumina_typesystem::Compatibility::constraint(
                self.iquery,
                |generic| match generic.kind {
                    GenericKind::Lambda(lkey) => {
                        &lambdas[lkey].typing.forall[generic.key].trait_constraints
                    }
                    GenericKind::Entity => &typing.forall[generic.key].trait_constraints,
                    GenericKind::Parent => todo!("???"),
                },
                &|mut comp, exp, ikey, trtp| {
                    let impltor = &self.hir.impltors[ikey];
                    let (_, trait_params) = &self.hir.itraits[ikey];
                    comp.cmps(trait_params, trtp) && comp.cmp(&*impltor, exp)
                },
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

        lower::Function::new(typing, lambdas, lcaptures, expr)
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
        let forall = self
            .fdef
            .forall
            .borrow()
            .values()
            .map(|gdata| gdata.name)
            .collect();
        let pforall = self.pforall.values().map(|gdata| gdata.name).collect();
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

        let inst = ForeignInst::<Var>::new(&mut self.tenvs[self.current.fkey])
            // TODO: using `with_self` for all recursive functions is questionable.
            .with_self(span)
            .forall(span, &forall)
            .iforall_cons(&forall)
            .build();

        let ptypes = typing
            .params
            .values()
            .map(|ty| inst.applyi(ty).tr(ty.span))
            .collect::<Vec<_>>();
        let returns = inst.applyi(&typing.returns).tr(typing.returns.span);

        let module = self.module();
        InstInfo::new(module, inst, ptypes, returns.clone())
    }

    pub fn get_list(&self, span: Span) -> Option<M<key::TypeKind>> {
        self.fdef.list.or_else(|| {
            self.error("missing list lang item")
                .m(self.module())
                .eline(span, "")
                .emit();
            None
        })
    }

    pub fn module_of_type(&mut self, ty: Tr<&IType>) -> Option<key::Module> {
        match &ty.value {
            IType::List(key, _) | IType::Defined(key, _) => Some(key.module),
            IType::InferringRecord(rvar) => {
                let env = &mut self.tenvs[self.current.fkey];
                match env.get_record(*rvar) {
                    lumina_typesystem::RecordAssignment::Ok(key, _) => Some(key.module),
                    lumina_typesystem::RecordAssignment::Redirect(var) => {
                        let ty = IType::InferringRecord(*var).tr(ty.span);
                        self.module_of_type(ty.as_ref())
                    }
                    lumina_typesystem::RecordAssignment::NotRecord(t) => {
                        let t = t.clone().tr(ty.span);
                        self.module_of_type(t.as_ref())
                    }
                    lumina_typesystem::RecordAssignment::Unknown(_) => None,
                    lumina_typesystem::RecordAssignment::None => None,
                }
            }
            IType::Var(var) => {
                let vinfo = self.vars().get(*var);
                match vinfo.value {
                    Some(ty) => {
                        let ty = ty.cloned();
                        self.module_of_type(ty.as_ref())
                    }
                    None => Some(self.module()),
                }
            }
            IType::Field(rvar, rfield) => {
                todo!("check if it's been assigned");
                // if it has then use whatever helper we need to instantiate the field
                // if not then fall back to using the unknown field map
            }
            _ => None,
        }
    }

    pub fn type_of_variant(
        &mut self,
        span: Span,
        sum: M<key::Sum>,
        var: key::SumVariant,
    ) -> (ForeignInst<Var>, Vec<Tr<IType>>, Tr<IType>) {
        let forall = &self.hir.sums[sum].1;
        let vars = self.vars();

        let finst = ForeignInst::<Var>::new(vars)
            // .with_self(span)
            .forall(span, forall)
            .forall_cons(forall)
            .build();

        let ptypes = self.hir.variant_types[sum][var]
            .iter()
            .map(|ty| finst.apply(&**ty).tr(ty.span))
            .collect();

        let key = sum.map(key::TypeKind::Sum);
        let returns = finst.to_itype(key);

        (finst, ptypes, returns.tr(span))
    }

    pub fn ty_as_callable(&mut self, span: Span, ty: Tr<IType>, params: usize) -> InstCall {
        match self.type_system().call_as_function(ty.as_ref(), params) {
            Some(Ok((kind, ptypes, returns))) => InstCall::LocalCall(span, ptypes, returns, kind),
            Some(Err(err)) => {
                panic!("ET: ");
            }
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
    ) -> IType {
        let lhs = params.last();
        match self.type_of_callable(span, lhs, call, params.len(), tanot) {
            InstCall::LocalCall(_, ptypes, returns, kind) => {
                let (applicated, xs) = ptypes.split_at(params.len());
                params.iter().zip(applicated).for_each(|(g, e)| {
                    self.type_check_and_emit(g.as_ref(), e.tr(span));
                });

                IType::Container(Container::Func(kind, xs.to_vec(), Box::new(returns)))
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
            InstCall::TypeDependentFailure => IType::poison(),
        }
    }

    fn partially_applicate_inst(
        &mut self,
        span: Span,
        instinfo: InstInfo,
        params: Vec<Tr<IType>>,
    ) -> IType {
        self.type_check_and_emit_application(span, &params, &instinfo.ptypes[..params.len()]);

        let ret = instinfo.ret.value.clone();
        let ptypes = instinfo.ptypes[params.len()..]
            .iter()
            .map(|t| t.value.clone())
            .collect();

        self.current.push_inst(span, Some(instinfo));

        // let kind = if params.len() == 0 {
        //     FuncKind::FnPointer
        // } else {
        //     FuncKind::Closure
        // };
        //
        // Let's not infer this anymore. In the future we're gonna add explicit syntax for raw
        // pointers.
        //
        // It's a niche systems-level use-case regardless so; we shouldn't bother ordinary users
        // with it.
        let kind = FuncKind::Closure;

        IType::Container(Container::Func(kind, ptypes, Box::new(ret)))
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
                        ast::Entity::Type(_) => todo!(),
                    },
                    Err(error) => {
                        self.hir.sources.emit_lookup_err(span, m, "function", error);
                        InstCall::TypeDependentFailure
                    }
                }
            }
            hir::Callable::Builtin(name) => match *name {
                "plus" | "minus" | "mul" | "div" => {
                    let any = IType::Var(self.vars().var(span));
                    let ptypes = vec![any.clone(), any.clone()];
                    InstCall::LocalCall(span, ptypes, any, FuncKind::FnPointer)
                }
                "eq" | "lt" | "gt" => {
                    let any = IType::Var(self.vars().var(span));
                    let ptypes = vec![any.clone(), any];
                    InstCall::LocalCall(span, ptypes, Prim::Bool.into(), FuncKind::FnPointer)
                }
                "deref" => {
                    let any = IType::Var(self.vars().var(span));
                    let ptypes = vec![Container::Pointer(Box::new(any.clone())).into()];
                    InstCall::LocalCall(span, ptypes, any, FuncKind::FnPointer)
                }
                "write" => {
                    let any = IType::Var(self.vars().var(span));
                    let ptypes = vec![
                        Container::Pointer(Box::new(any.clone())).into(),
                        any.clone(),
                    ];
                    let ret = IType::Container(Container::Tuple(vec![]));
                    InstCall::LocalCall(span, ptypes, ret, FuncKind::FnPointer)
                }
                "offset" => {
                    let any = IType::Var(self.vars().var(span));
                    let ptr: IType = Container::Pointer(Box::new(any.clone())).into();
                    let ptypes = vec![ptr.clone(), Prim::Int(true, Bitsize::default()).into()];
                    InstCall::LocalCall(span, ptypes, ptr, FuncKind::FnPointer)
                }
                "reflect_type" => {
                    InstCall::Local(IType::defined(self.items.reflect_type, vec![]).tr(span))
                }
                "abort" => InstCall::Local(IType::Var(self.vars().var(span)).tr(span)),
                "transmute" => {
                    let param = IType::Var(self.vars().var(span));
                    let ptypes = vec![param];
                    let ret = IType::Var(self.vars().var(span));
                    InstCall::LocalCall(span, ptypes, ret, FuncKind::FnPointer)
                }
                "val_to_ref" => {
                    let any = IType::Var(self.vars().var(span));
                    let ptr = Container::Pointer(Box::new(any.clone())).into();
                    let ptypes = vec![any];
                    let ret = ptr;
                    InstCall::LocalCall(span, ptypes, ret, FuncKind::FnPointer)
                }
                _ => {
                    self.error("unrecognised builtin").eline(span, "").emit();
                    InstCall::Local(IType::Prim(Prim::Poison).tr(span))
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

        let inst = ForeignInst::<Var>::new(&mut self.tenvs[self.current.fkey])
            .forall(span, &forall)
            .iforall_cons(&forall)
            .build();

        let ptypes = typing
            .params
            .values()
            .map(|ty| inst.applyi(ty).tr(ty.span))
            .collect::<Vec<_>>();
        let returns = inst.applyi(&typing.returns).tr(typing.returns.span);

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
                let finst = ForeignInst::<Var>::new(vars)
                    .with_self(span)
                    .parent(span, pforall)
                    .forall(span, forall)
                    .forall_cons(forall)
                    .parent_cons(pforall)
                    .build();

                let (ptypes, returns) = apply_finst(
                    &finst,
                    typing.params.values().map(|t| t.as_ref()),
                    typing.returns.as_ref(),
                );
                self.apply_tanot(tanot, &finst, Either::Left(func.map(NFunc::Key)));

                let linfo = InstInfo::new(func.module, finst, ptypes, returns);

                InstCall::Instantiated(linfo)
            }
            _ => match self.query_func(func) {
                Ok(_) => {
                    let typing = self.funcs[func].as_typing();
                    let vars = &mut self.tenvs[self.current.fkey];
                    let finst = ForeignInst::<Var>::new(vars);
                    let forall = &typing.forall;

                    // Attach the impl blocks `forall` to the instantiation of this function is
                    // defined as a member to an implementation.
                    let pforall = match &self.hir.funcs[func] {
                        hir::FuncDefKind::ImplMethod(imp, _) => Some(&self.hir.impls[*imp]),
                        hir::FuncDefKind::TraitDefaultMethod(tr, _) => {
                            Some(&self.hir.traits[*tr].1)
                        }
                        _ => None,
                    };

                    let finst = match pforall {
                        Some(pforall) => finst
                            .with_self(span)
                            .forall(span, forall)
                            .parent(span, pforall)
                            .forall_cons(forall)
                            .parent_cons(pforall)
                            .build(),
                        None => finst.forall(span, forall).forall_cons(forall).build(),
                    };

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
                        | hir::FuncDefKind::TraitDefaultMethod(_, f)
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

                    let (ptypes, returns) = apply_finst(&finst, types.into_iter(), ret);
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
        finst: &ForeignInst<Var>,
        func: Either<M<NFunc>, key::Lambda>,
    ) {
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

        match (ty_lookup, tanot.for_type.is_empty()) {
            (_, true) => {}
            (Some(lookup), false) => {
                for (name, ty) in tanot.for_type.iter() {
                    let tr = |t: &IType| t.clone().tr(name.span);

                    if **name == "self" {
                        self.type_system().assign_self(finst, tr(ty));
                        continue;
                    }

                    let Some(generic) = lookup.find(|n| n.name == **name) else {
                        // TODO: merge multiple unknown generics
                        self.error("unknown generic")
                            .m(self.module())
                            .eline(name.span, "")
                            .emit();
                        continue;
                    };

                    self.type_system().assign_pgeneric(&finst, generic, tr(ty));
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
                            fdef.forall.borrow().find(|gdata| gdata.name == **name)
                        }
                        hir::FuncDefKind::TraitDefaultMethod(_, fdef) => todo!(),
                        hir::FuncDefKind::TraitHeader(_, forall, _) => {
                            forall.find(|gdata| gdata.name == **name)
                        }
                        hir::FuncDefKind::InheritedDefault(_, _) => unreachable!(),
                        hir::FuncDefKind::Extern { .. } => {
                            panic!("generics for extern function")
                        }
                    },
                    NFunc::Method(trait_, method) => {
                        let func = self.hir.methods[module.m(trait_)][method];
                        let forall = &self.hir.funcs[module.m(func)].as_defined().forall.borrow();
                        forall.find(|gdata| gdata.name == **name)
                    }
                    NFunc::SumVar(_, _) => todo!(),
                    NFunc::Val(_) => todo!(),
                },
                Right(lkey) => {
                    self.fdef.lambdas.foralls.borrow()[lkey].find(|gdata| gdata.name == **name)
                }
                _ => unreachable!(),
            };

            match generic {
                Some(generic) => {
                    self.type_system()
                        .assign_generic(finst, generic, (ty.clone()).tr(name.span));
                }
                None => {
                    let err = self
                        .error("generic not found")
                        .m(self.module())
                        .eline(name.span, "the function does not have this generic");

                    if **name == "self"
                        || ty_lookup
                            .map(|forall| forall.find(|gdata| gdata.name == **name).is_some())
                            == Some(true)
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
    }

    pub fn assign_ty_to_rvar(&mut self, span: Span, var: RecordVar, ty: Tr<&IType>) {
        let result = self.type_system().ascribe_record(ty, span, var);
        self.emit_type_errors((ty, IType::InferringRecord(var).tr(span).as_ref(), result));
    }

    pub fn string_langitem(&mut self, span: Span) -> IType {
        match self.fdef.string {
            Some(key) => IType::Defined(key, vec![]),
            None => {
                self.error("missing lang item")
                    .eline(span, "string lang item is not set for this module")
                    .emit();

                Prim::Poison.into()
            }
        }
    }
}

fn apply_finst<'t, 's>(
    finst: &ForeignInst<Var>,
    types: impl Iterator<Item = Tr<&'t Type>>,
    ret: Tr<&'t Type>,
) -> (Vec<Tr<IType>>, Tr<IType>) {
    let ptypes = types.map(|p| finst.apply(*p).tr(p.span)).collect();
    let returns = finst.apply(*ret).tr(ret.span);
    (ptypes, returns)
}

#[derive(Debug)]
pub enum InstCall {
    LocalCall(Span, Vec<IType>, IType, FuncKind),
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
