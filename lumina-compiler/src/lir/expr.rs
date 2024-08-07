use super::*;
use crate::{TRAIT_OBJECT_DATA_FIELD, VTABLE_FIELD};
use ssa::Value;
use std::cmp::Ordering;

impl<'a> FuncLower<'a> {
    pub fn expr_to_flow(&mut self, expr: &mir::Expr) {
        trace!("lowering expression {expr}");

        match expr {
            // TODO: we also want to edge-case tail calls here
            _ => {
                let value = self.expr_to_value(expr);
                self.ssa().return_(value);
            }
        }
    }

    fn yield_to_value(&mut self, local: mir::Local) -> Value {
        match local {
            mir::Local::Param(pid) => {
                let offset = self.current.captures.unwrap_or(0);
                self.ssa()
                    .get_block_param(ssa::Block::entry(), pid.0 + offset as u32)
                    .value()
            }
            mir::Local::Binding(bind) => self.current.bindmap[&bind],
        }
    }

    pub fn expr_to_value(&mut self, expr: &mir::Expr) -> Value {
        trace!("lowering {expr}");

        match expr {
            mir::Expr::CallFunc(func, inst, params) => self.call_nfunc(*func, inst, params),
            mir::Expr::CallLambda(lambda, inst, params) => {
                let params = self.params_to_values(params);
                let (mfunc, mut captures) = self.morphise_lambda(*lambda, inst);
                let returns = self.lir.functions[mfunc].returns.clone();

                captures.extend(params);

                self.ssa().call(mfunc, captures, returns)
            }
            mir::Expr::PartialLambda(lambda, inst, partials) => {
                let (mfunc, mut captures) = self.morphise_lambda(*lambda, inst);

                let partials = self.params_to_values(partials);
                captures.extend(partials);

                self.partially_applicate_func(mfunc, captures)
            }
            mir::Expr::PartialLocal(local, partials) => {
                let Value::V(cap) = self.yield_to_value(*local) else {
                    panic!("partial call of non-closure");
                };
                let partials = self.params_to_values(partials);
                self.partially_applicate_closure(cap, partials)
            }
            mir::Expr::PartialFunc(func, inst, partials) => match self.resolve_nfunc(*func, inst) {
                ResolvedNFunc::Static(mfunc, _) => {
                    let partials = self.params_to_values(partials);
                    self.partially_applicate_func(mfunc, partials)
                }
                ResolvedNFunc::Sum { tmap, var, ty, .. } => {
                    let partials = self.params_to_values(partials);
                    let mfunc = self.monofunc_wrapper_for_sumvar(ty, tmap, var);
                    self.partially_applicate_func(mfunc, partials)
                }
                ResolvedNFunc::Extern(_, _) => todo!("partially applicating a FFI function"),
                ResolvedNFunc::Val(_, _) => todo!("partially applicating a global value"),
            },
            mir::Expr::YieldLambda(lambda, inst) => {
                let (mfunc, captures) = self.morphise_lambda(*lambda, inst);
                self.dyn_lambda(mfunc, captures)
            }
            mir::Expr::CallLocal(local, params) => {
                let params = self.params_to_values(params);
                let to_call = self.yield_to_value(*local);
                let ty = self.type_of_value(to_call);
                match ty {
                    MonoType::FnPointer(_, ret) => self.ssa().call(to_call, params, (*ret).clone()),
                    MonoType::Monomorphised(mk) => self.call_closure(mk, to_call, params),
                    _ => panic!("attempted to call {ty:#?} as a function"),
                }
            }
            mir::Expr::ValToRef(val) => match &**val {
                mir::Expr::CallFunc(M { value: ast::NFunc::Val(val), module }, _, _) => {
                    let key = module.m(*val);
                    let ty = self.lir.vals[key].clone();
                    self.ssa().val_to_ref(key, MonoType::pointer(ty))
                }
                other => panic!("non-val given to val_to_ref builtin: {other}"),
            },
            mir::Expr::Yield(local) => self.yield_to_value(*local),
            mir::Expr::YieldFunc(fkey, mapper) => {
                let tmap = self.morphise_inst(&mapper);
                let (mfunc, _) = self.call_to_mfunc(FuncOrigin::Defined(*fkey), tmap);
                Value::FuncPtr(mfunc)
            }
            mir::Expr::Access(object, record, types, field) => {
                let value = self.expr_to_value(object);

                let mut morph = to_morphization!(self.lir, self.mir, &mut self.current.tmap);

                let mk = morph.record(*record, types);

                let ty = self.lir.mono.types.type_of_field(mk, *field);
                self.ssa().field(value, mk, *field, ty)
            }
            mir::Expr::Record(record, types, fields) => {
                let mut mono = to_morphization!(self.lir, self.mir, &mut self.current.tmap);

                let ty = MonoType::Monomorphised(mono.record(*record, types));

                let values = fields
                    .iter()
                    .map(|(_, expr)| self.expr_to_value(expr))
                    .collect::<Vec<Value>>();

                let sorted = (0..fields.len() as u32)
                    .map(key::RecordField)
                    .map(|field| values[fields.iter().position(|(f, _)| *f == field).unwrap()])
                    .collect();

                self.ssa().construct(sorted, ty)
            }
            mir::Expr::Int(intsize, n) => Value::Int(*n, *intsize),
            mir::Expr::Bool(b) => Value::Int(*b as u8 as i128, IntSize::new(false, 8)),
            mir::Expr::Float(n) => Value::Float(*n),
            mir::Expr::ReadOnly(ro) => Value::ReadOnly(*ro),
            mir::Expr::Tuple(elems) => {
                let params = self.params_to_values(elems);
                self.elems_to_tuple(params, None)
            }
            mir::Expr::IntCast(expr, from, to) => {
                let inner = self.expr_to_value(&expr);

                let ty = MonoType::Int(*to);

                match from.bits().cmp(&to.bits()) {
                    Ordering::Equal => inner,
                    Ordering::Less => self.ssa().extend(inner, from.signed, ty),
                    Ordering::Greater => self.ssa().reduce(inner, ty),
                }
            }
            mir::Expr::ToFloatCast(expr, fromint) => {
                let inner = self.expr_to_value(&expr);
                self.ssa().int_to_float(inner, *fromint)
            }
            mir::Expr::FromFloatCast(expr, toint) => {
                let inner = self.expr_to_value(&expr);
                self.ssa().float_to_int(inner, *toint)
            }
            mir::Expr::Deref(inner) => {
                let inner = self.expr_to_value(&inner);
                let ty = self.type_of_value(inner).deref();
                self.ssa().deref(inner, ty)
            }
            mir::Expr::Write(elems) => {
                let [ptr, value] = self.params_to_values(&**elems).try_into().unwrap();
                self.ssa().write(ptr, value)
            }
            mir::Expr::ObjectCast(expr, weak_impltor, trait_, trait_params) => {
                let trait_ = *trait_;
                let expr = self.expr_to_value(expr);
                let impltor = self.type_of_value(expr);

                let mut morph = to_morphization!(self.lir, self.mir, &mut self.current.tmap);
                let weak_impltor = morph.apply_weak(weak_impltor);
                let weak_trait_params = morph.applys_weak::<Vec<_>>(trait_params);

                let (impl_, tmap) = self.find_implementation(
                    trait_,
                    &weak_trait_params,
                    weak_impltor.clone(),
                    impltor.clone(),
                );

                let methods = self.mir.imethods[impl_]
                    .keys()
                    .map(|method| match self.mir.imethods[impl_][method] {
                        None => todo!("default method instantiation"),
                        Some(func) => {
                            // If the methods have generics then this isn't trait-safe therefore
                            // this would've already been stopped.
                            let mut tmap = tmap.clone();
                            let mut morph = to_morphization!(self.lir, self.mir, &mut tmap);
                            let typing = self.mir.funcs[func].as_typing();
                            let typing =
                                morph.apply_typing(FuncOrigin::Method(impl_, method), typing);
                            self.lir
                                .func(self.mir, self.iquery, self.info, tmap, typing, None)
                        }
                    })
                    .collect();

                self.dyn_object(impl_, expr, methods)
            }
            mir::Expr::Match(on, tree, branches, pred) => {
                let on = self.expr_to_value(on);
                self.to_pat_lower(branches, pred).run(on, tree)
            }
            mir::Expr::ReflectTypeOf(ty) => {
                let ty =
                    to_morphization!(self.lir, self.mir, &mut self.current.tmap).apply_weak(ty);
                self.create_reflection(ty)
            }
            mir::Expr::SizeOf(ty) => {
                let ty = to_morphization!(self.lir, self.mir, &mut self.current.tmap).apply(ty);
                let size = self.lir.mono.types.size_of(&ty) / 8;
                Value::Int(
                    size as i128,
                    IntSize::new(false, self.lir.target.int_size()),
                )
            }
            mir::Expr::Cmp(cmp, params) => {
                let params = [
                    self.expr_to_value(&params[0]),
                    self.expr_to_value(&params[1]),
                ];

                let intsize = match self.type_of_value(params[0]) {
                    MonoType::Int(intsize) => intsize,
                    ty => panic!("not an int: {ty:?}"),
                };

                match *cmp {
                    "eq" => self.ssa().cmp(params, Ordering::Equal, intsize),
                    "lt" => self.ssa().cmp(params, Ordering::Less, intsize),
                    "gt" => self.ssa().cmp(params, Ordering::Greater, intsize),
                    _ => panic!("unknown comparison operator: {cmp}"),
                }
            }
            mir::Expr::Num(name, params) => {
                let [left, right] = [
                    self.expr_to_value(&params[0]),
                    self.expr_to_value(&params[1]),
                ];

                let ty = self.type_of_value(left);
                match *name {
                    "plus" => self.ssa().add(left, right, ty),
                    "minus" => self.ssa().sub(left, right, ty),
                    "mul" => self.ssa().mul(left, right, ty),
                    "div" => self.ssa().div(left, right, ty),
                    _ => panic!("unknown num builtin: {name}"),
                }
            }
            mir::Expr::Unreachable(_) => {
                self.ssa().unreachable();
                // TODO: Do we need to create a tuple of bytes with the same size as `ty` then
                // transmute? or is this fine?
                Value::Int(0, IntSize::new(false, self.lir.target.int_size()))
                // let ty = to_morphization!(self.lir, self.mir, &mut self.current.tmap).apply(ty);
                // self.ssa().unreachable(ty).value()
            }
            mir::Expr::Poison => todo!(),
        }
    }

    pub fn heap_alloc(&mut self, value: lir::Value, ty: MonoType) -> Value {
        let size = self.lir.mono.types.size_of(&ty);
        if size == 0 {
            Value::Int(0, IntSize::new(false, self.lir.target.int_size()))
        } else {
            let ptr = self.ssa().alloc(size, ty);
            self.ssa().write(ptr, value);
            ptr
        }
    }

    fn call_nfunc(
        &mut self,
        func: M<ast::NFunc>,
        inst: &GenericMapper<Static>,
        params: &[mir::Expr],
    ) -> Value {
        match self.resolve_nfunc(func, inst) {
            ResolvedNFunc::Extern(key, ret) => {
                let params = self.params_to_values(params);
                self.ssa().call_extern(key, params, ret)
            }
            ResolvedNFunc::Static(mfunc, ret) => {
                let params = self.params_to_values(params);
                self.ssa().call(mfunc, params, ret)
            }
            ResolvedNFunc::Sum { tag, payload_size, ty, .. } => {
                let params = self.params_to_values(params);
                let parameters = self.elems_to_tuple(params, Some(payload_size));

                self.ssa()
                    .construct(vec![tag, parameters.into()], MonoType::Monomorphised(ty))
            }
            ResolvedNFunc::Val(key, ty) => {
                assert!(params.is_empty(), "giving parameters to the function returnt by a static value is not yet supported");
                let v = self.ssa().val_to_ref(key, ty.clone());
                self.ssa().deref(v, ty)
            }
        }
    }

    fn call_closure(&mut self, objty: MonoTypeKey, obj: Value, params: Vec<Value>) -> Value {
        let dataptr_type = self
            .lir
            .mono
            .types
            .type_of_field(objty, TRAIT_OBJECT_DATA_FIELD);
        assert_eq!(MonoType::u8_pointer(), dataptr_type);

        let fnptr_type = self.lir.mono.types.type_of_field(objty, VTABLE_FIELD);
        let ret = fnptr_type.as_fnptr().1.clone();

        debug_assert_eq!(MonoType::u8_pointer(), dataptr_type);

        let objptr = self
            .ssa()
            .field(obj, objty, TRAIT_OBJECT_DATA_FIELD, dataptr_type);

        let fnptr = self
            .ssa()
            .field(obj, objty, VTABLE_FIELD, fnptr_type.clone());

        let param_tuple = self.elems_to_tuple(params, None);

        let call_method_params = vec![objptr, param_tuple];

        self.ssa().call(fnptr, call_method_params, ret)
    }

    pub fn find_implementation(
        &mut self,
        trait_: M<key::Trait>,
        trtp: &[Type],
        weak_impltor: Type,
        impltor: MonoType,
    ) -> (M<key::Impl>, TypeMap) {
        warn!(
            "conflicting implementations is not fully implemented. Weird auto-selections may occur"
        );

        let concrete_impltor = (&weak_impltor).try_into().ok();

        info!(
            "attempting to find `impl {trait_} {} for {}` in {}",
            trtp.iter().format(" "),
            weak_impltor,
            self.current.origin.name(self.mir)
        );

        self.iquery
            .for_each_relevant(trait_, concrete_impltor, |imp| {
                let iforall = &self.mir.impls[imp];
                let (_, trait_params) = &self.mir.itraits[imp];
                let iimpltor = &self.mir.impltors[imp];

                let mut comp = lumina_typesystem::Compatibility::new(
                    &self.iquery,
                    &|_| panic!("un-monomorphised generic in LHS"),
                    &iforall,
                    &|_| unreachable!(),
                );

                let valid = trtp
                    .iter()
                    .zip(trait_params)
                    .all(|(ty, ttp)| comp.cmp(ty, ttp))
                    && comp.cmp(&weak_impltor, iimpltor);

                valid.then(|| {
                    let mut tmap = TypeMap::new();
                    tmap.set_self(weak_impltor.clone(), impltor.clone());
                    for (generic, ty) in comp.into_assignments().generics.into_iter() {
                        let mono =
                            to_morphization!(self.lir, self.mir, &mut TypeMap::new()).apply(&ty);
                        tmap.push(generic, ty, mono);
                    }
                    (imp, tmap)
                })
            })
            .unwrap()
    }

    pub fn call_to_mfunc(&mut self, func: FuncOrigin, mut tmap: TypeMap) -> (MonoFunc, MonoType) {
        assert!(
            !matches!(func, FuncOrigin::Lambda(..)),
            "call_to_value does not handle captures"
        );

        let fdef = func.get_root_fdef(self.mir);

        trace!(
            "monomorphising typing of call fn {func} as {}\n  with mapping {tmap:?}",
            &fdef.typing
        );

        let typing =
            to_morphization!(self.lir, self.mir, &mut tmap).apply_typing(func, &fdef.typing);
        let ret = typing.returns.clone();

        info!(
            "calling function {} ({})",
            self.lir.mono.fmt(&typing),
            typing.origin.name(&self.mir)
        );

        let mfunc = self
            .lir
            .func(self.mir, self.iquery, self.info, tmap, typing, None);

        (mfunc, ret)
    }
}
