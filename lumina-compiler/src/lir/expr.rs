use super::*;
use crate::{TRAIT_OBJECT_DATA_FIELD, VTABLE_FIELD};
use lumina_typesystem::ConstValue;
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

    pub fn param_to_value(&self, pid: key::Param) -> Value {
        let offset = self.current.captures.unwrap_or(0);
        self.lir.functions[self.current.mfkey]
            .blocks
            .get_block_param(ssa::Block::entry(), pid.0 + offset as u32)
            .value()
    }

    pub fn bind_to_value(&self, bind: key::Bind) -> Value {
        self.current.bindmap[&bind]
    }

    pub fn expr_to_value(&mut self, expr: &mir::Expr) -> Value {
        trace!("lowering {expr}");

        match expr {
            mir::Expr::Call(call, params) => {
                let params = self.params_to_values(params);
                self.call(call, params)
            }
            mir::Expr::PartiallyApplicate(call, partials) => {
                let partials = self.params_to_values(partials);
                self.pass(call, partials)
            }
            mir::Expr::Yield(call) => match self.lower_callable(call) {
                Callable::Extern(fkey) => Value::ExternFuncPtr(fkey),
                Callable::Static(mfunc) => Value::FuncPtr(mfunc),
                Callable::LiftedLambda(mfunc, captures) => {
                    self.partially_applicate_func(mfunc, captures)
                }
                Callable::Val(_) => todo!("yield fnptr returnt by global value"),
                Callable::Sum { tmap, var, ty, .. } => {
                    let mfunc = self.monofunc_wrapper_for_sumvar(ty, tmap, var);
                    Value::FuncPtr(mfunc)
                }
                Callable::Local(value) => value,
            },
            mir::Expr::ValToRef(val) => match &**val {
                mir::Expr::Call(mir::Callable::Func(M(module, ast::NFunc::Val(key)), _), _) => {
                    let key = key.inside(*module);
                    let ty = self.lir.vals[key].clone();
                    self.ssa().val_to_ref(key, ty)
                }
                other => panic!("non-val given to val_to_ref builtin: {other}"),
            },

            mir::Expr::Access(object, record, types, field) => {
                let value = self.expr_to_value(object);

                let mut morph = to_morphization!(self.lir, self.mir, &mut self.current.tmap);
                let mk = morph.record(*record, types);

                let ty = self.types()[mk].as_record()[*field].clone();
                self.ssa().field(value, mk, *field, ty)
            }
            mir::Expr::Record(record, types, fields) => {
                let mut mono = to_morphization!(self.lir, self.mir, &mut self.current.tmap);
                let ty = MonoType::Monomorphised(mono.record(*record, types));

                let values = fields
                    .iter()
                    .map(|(_, _, expr)| self.expr_to_value(expr))
                    .collect::<Vec<Value>>();

                let sorted = (0..fields.len() as u32)
                    .map(key::Field)
                    .map(|field| values[fields.iter().position(|(f, _, _)| *f == field).unwrap()])
                    .collect();

                self.ssa().construct(sorted, ty)
            }
            mir::Expr::Array(elems, len, inner) => {
                let inner =
                    to_morphization!(self.lir, self.mir, &mut self.current.tmap).apply(inner);
                let arrayt = MonoType::Array(*len, Box::new(inner));

                if elems.len() as u64 != *len {
                    let value = self.expr_to_value(&elems[0]);
                    assert_eq!(elems.len(), 1);
                    self.ssa().replicate(value, *len, arrayt)
                } else {
                    let values = elems
                        .iter()
                        .map(|expr| self.expr_to_value(expr))
                        .collect::<Vec<Value>>();

                    self.ssa().construct(values, arrayt)
                }
            }
            mir::Expr::GenericArray(elem, generic, inner) => {
                let mut morph = to_morphization!(self.lir, self.mir, &mut self.current.tmap);
                let &MonoType::Const(ConstValue::Usize(len)) = morph.generic(*generic) else {
                    panic!("non-const-usize given as instantiation of const generic");
                };
                let inner = morph.apply(inner);
                let arrayt = MonoType::Array(len, Box::new(inner));

                let value = self.expr_to_value(elem);
                self.ssa().replicate(value, len, arrayt)
            }
            mir::Expr::ArrayLen(arr) => {
                let arr = self.expr_to_value(arr);
                let MonoType::Array(len, _) = self.type_of_value(arr) else {
                    panic!("not an array: {arr}");
                };
                Value::Int(len as i128, self.lir.target.uint())
            }
            mir::Expr::TupleAccess(tuple, i) => {
                let tuple = self.expr_to_value(tuple);
                let key = self.type_of_value(tuple).as_key();
                let field = key::Field(*i as u32);
                let fty = self.types()[key].as_record()[field].clone();
                self.ssa().field(tuple, key, field, fty)
            }
            mir::Expr::ArrayAccess(params) => {
                let i = self.expr_to_value(&params[0]);
                let arr = self.expr_to_value(&params[1]);
                let ty = match self.type_of_value(arr) {
                    MonoType::Array(_, inner) => *inner,
                    _ => panic!("not an array"),
                };
                self.ssa().indice(arr, i, ty)
            }
            mir::Expr::Int(intsize, n) => Value::Int(*n, *intsize),
            mir::Expr::Bool(b) => Value::Int(*b as u8 as i128, IntSize::new(false, 8)),
            mir::Expr::Float(n) => Value::Float(*n),
            mir::Expr::ReadOnly(ro) => Value::ReadOnly(*ro),
            mir::Expr::Tuple(elems) => {
                let params = self.params_to_values(elems);
                self.elems_to_tuple(params)
            }
            mir::Expr::PointerToArrayCast(expr, len, inner) => {
                let inner =
                    to_morphization!(self.lir, self.mir, &mut self.current.tmap).apply(inner);
                let v = self.expr_to_value(expr);
                self.ssa()
                    .transmute(v, MonoType::Array(*len, Box::new(inner)))
            }
            mir::Expr::PointerToGenericArrayCast(expr, generic, inner) => {
                let mut morph = to_morphization!(self.lir, self.mir, &mut self.current.tmap);
                let inner = morph.apply(inner);
                let &MonoType::Const(ConstValue::Usize(len)) = morph.generic(*generic) else {
                    panic!("invalid const-value given as generic");
                };
                let v = self.expr_to_value(expr);
                self.ssa()
                    .transmute(v, MonoType::Array(len, Box::new(inner)))
            }
            mir::Expr::PointerToPointerCast(expr, to) | mir::Expr::ToPointerCast(expr, _, to) => {
                let ty = to_morphization!(self.lir, self.mir, &mut self.current.tmap).apply(to);
                let v = self.expr_to_value(&expr);
                let fromint = match self.type_of_value(v) {
                    MonoType::Int(size) => size,
                    MonoType::Pointer(_) => IntSize::new(false, 64),
                    ty => panic!("not a pointer or int: {ty:?}"),
                };
                let v = self.int_cast(v, [fromint, IntSize::new(false, 64)]);
                self.ssa().transmute(v, MonoType::pointer(ty))
            }
            mir::Expr::FromPointerCast(expr, toint) => {
                let v = self.expr_to_value(&expr);
                let v = self.int_cast(v, [IntSize::new(false, 64), *toint]);
                self.ssa().transmute(v, MonoType::Int(*toint))
            }
            mir::Expr::IntCast(expr, from, to) => {
                let inner = self.expr_to_value(&expr);
                self.int_cast(inner, [*from, *to])
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
                let trait_params = morph.applys(trait_params);

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

                self.dyn_object(impl_, trait_params, expr, methods)
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
                let intsize = self.lir.target.int_size();
                self.ssa().size_of(ty, IntSize::new(false, intsize))
            }
            mir::Expr::Alloca(ty) => {
                let ty = to_morphization!(self.lir, self.mir, &mut self.current.tmap).apply(ty);
                self.ssa().alloca(ty)
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
            mir::Expr::IntAbs(n) => {
                let n = self.expr_to_value(&*n);
                let ty = self.type_of_value(n);
                self.ssa().abs(n, ty)
            }
            mir::Expr::Num(name, params) => {
                let [left, right] = [
                    self.expr_to_value(&params[0]),
                    self.expr_to_value(&params[1]),
                ];

                let ty = self.type_of_value(left);
                let cty = self
                    .lir
                    .mono
                    .get_or_make_tuple(vec![ty.clone(), MonoType::bool()])
                    .into();

                match *name {
                    "plus" => self.ssa().add(left, right, ty),
                    "minus" => self.ssa().sub(left, right, ty),
                    "mul" => self.ssa().mul(left, right, ty),
                    "div" => self.ssa().div(left, right, ty),
                    "plus_checked" => self.ssa().add(left, right, cty),
                    "minus_checked" => self.ssa().sub(left, right, cty),
                    "mul_checked" => self.ssa().mul(left, right, cty),
                    "div_checked" => self.ssa().div(left, right, cty),
                    _ => panic!("unknown num builtin: {name}"),
                }
            }
            mir::Expr::Unreachable(_) => {
                self.ssa().unreachable();
                // TODO: Do we need to create a tuple of bytes with the same size as `ty` then
                // transmute? or is this fine?
                Value::Int(0, self.lir.target.uint())
                // let ty = to_morphization!(self.lir, self.mir, &mut self.current.tmap).apply(ty);
                // self.ssa().unreachable(ty).value()
            }
            mir::Expr::Poison => todo!(),
        }
    }

    fn int_cast(&mut self, v: Value, [from, to]: [IntSize; 2]) -> Value {
        let ty = MonoType::Int(to);

        match from.bits().cmp(&to.bits()) {
            Ordering::Equal if from.signed != to.signed => {
                self.ssa().transmute(v, MonoType::Int(to))
            }
            Ordering::Equal => v,
            Ordering::Less => self.ssa().extend(v, from.signed, ty),
            Ordering::Greater => self.ssa().reduce(v, ty),
        }
    }

    fn call(&mut self, call: &mir::Callable, params: Vec<Value>) -> Value {
        match self.lower_callable(call) {
            Callable::Extern(fkey) => {
                let ret = self.lir.extern_funcs[&fkey].returns.clone();
                self.ssa().call_extern(fkey, params, ret)
            }
            Callable::Static(mfunc) => {
                let ret = self.lir.functions[mfunc].returns.clone();
                self.ssa().call(mfunc, params, ret)
            }
            Callable::LiftedLambda(mfunc, mut captures) => {
                let ret = self.lir.functions[mfunc].returns.clone();
                captures.extend(params);
                self.ssa().call(mfunc, captures, ret)
            }
            Callable::Val(key) => {
                assert!(params.is_empty(), "giving parameters to the function returnt by a static value is not yet supported");
                let ty = self.lir.vals[key].clone();
                let v = self.ssa().val_to_ref(key, ty.clone());
                self.ssa().deref(v, ty)
            }
            Callable::Sum { var, ty, .. } => self.ssa().variant(var, params, ty),
            Callable::Local(to_call) => {
                let ty = self.type_of_value(to_call);
                match ty {
                    MonoType::FnPointer(_, ret) => self.ssa().call(to_call, params, (*ret).clone()),
                    MonoType::Monomorphised(mk) => self.call_closure(mk, to_call, params),
                    _ => panic!("attempted to call {ty:#?} as a function"),
                }
            }
        }
    }

    pub fn pass(&mut self, call: &mir::Callable, partials: Vec<Value>) -> Value {
        match self.lower_callable(call) {
            Callable::Extern(fkey) => {
                todo!(
                    "partial application of extern functions: {}",
                    self.mir.func_names[fkey]
                );
            }
            Callable::Static(mfunc) => self.partially_applicate_func(mfunc, partials),
            Callable::LiftedLambda(mfunc, mut captures) => {
                captures.extend(partials);
                self.partially_applicate_func(mfunc, captures)
            }
            Callable::Val(_) => {
                panic!("TODO: partially applicate value returned from global");
            }
            Callable::Sum { tmap, var, ty, .. } => {
                let mfunc = self.monofunc_wrapper_for_sumvar(ty, tmap, var);
                self.partially_applicate_func(mfunc, partials)
            }
            Callable::Local(value) => {
                let Value::V(cap) = value else {
                    panic!("partial call of non-closure");
                };
                self.partially_applicate_closure(cap, partials)
            }
        }
    }

    pub fn heap_alloc(&mut self, value: lir::Value, ty: MonoType) -> Value {
        let ptr = self.ssa().alloc(ty);
        self.ssa().write(ptr, value);
        ptr
    }

    pub fn call_closure(&mut self, objty: MonoTypeKey, obj: Value, params: Vec<Value>) -> Value {
        let lir::MonoTypeData::DynTraitObject { vtable, .. } = &self.types()[objty] else {
            panic!("attempted to call non-closure as closure");
        };

        let dataptr_type = MonoType::u8_pointer();

        let fnptr_type = vtable.clone();
        let ret = fnptr_type.as_fnptr().1.clone();

        let objptr = self
            .ssa()
            .field(obj, objty, TRAIT_OBJECT_DATA_FIELD, dataptr_type);

        let fnptr = self
            .ssa()
            .field(obj, objty, VTABLE_FIELD, fnptr_type.clone());

        let mut call_method_params = vec![objptr];
        call_method_params.extend(params);

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

    pub fn call_to_mfunc(&mut self, func: FuncOrigin, mut tmap: TypeMap) -> MonoFunc {
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

        info!(
            "calling function {} ({})",
            self.lir.mono.fmt(&typing),
            typing.origin.name(&self.mir)
        );

        self.lir
            .func(self.mir, self.iquery, self.info, tmap, typing, None)
    }
}
