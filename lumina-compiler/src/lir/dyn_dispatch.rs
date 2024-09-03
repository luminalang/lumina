//! Lowering for closures, dynamic trait objects and partial application

use super::{ssa, Blocks, FuncLower, MonoFunc, MonoType, MonoTypeKey, Value, V};
use crate::prelude::*;
use either::Either;

impl<'a> FuncLower<'a> {
    // #(f 0) // where fn f as int, int, int -> float
    //
    // {
    //   data = *u8 // *{int}
    //   call = fnptr(*u8, int, int -> float)
    // }
    //  where
    //   fn call data b c as *u8, int, int -> float =
    //     let {a} = deref data in
    //       target a b c
    pub fn partially_applicate_func(&mut self, target: MonoFunc, params: Vec<Value>) -> Value {
        let target_func = &self.lir.functions[target];
        let mut types = target_func.blocks.func_params();
        let ret = target_func.returns.clone();

        let remaining = types.split_off(params.len());
        let key = (target, types);

        let capture_tuple = self.lir.mono.get_or_make_tuple(key.1.clone());

        let mfunc = match self.lir.memo_closures.get(&key) {
            Some(mfunc) => *mfunc,
            None => {
                let mut fnptr_ptypes = vec![MonoType::u8_pointer()];
                fnptr_ptypes.extend(remaining.iter().cloned());

                // TODO: remember caching

                let symbol = format!(
                    "__Partial_{target}_{}",
                    key.1.iter().map(|ty| self.ty_symbol(ty)).format(",")
                );

                let construct = |this: &mut Self, ssa: &mut Blocks| {
                    let ptr = ssa.transmute(V(0).value(), MonoType::pointer(capture_tuple.into()));
                    let data = ssa.deref(ptr, capture_tuple.into());

                    let mut applicated = this
                        .lir
                        .mono
                        .fields(capture_tuple)
                        .map(|field| {
                            let ty = this.types().type_of_field(capture_tuple, field);
                            ssa.field(data, capture_tuple, field, ty)
                        })
                        .collect::<Vec<_>>();

                    for p in ssa.params(lir::Block::entry()).skip(1) {
                        applicated.push(p.value());
                    }

                    ssa.jump(target, applicated)
                };

                let mfunc = self.create_deref_funcwrapper(symbol, construct, fnptr_ptypes, ret);
                self.lir.memo_closures.insert(key, mfunc);

                mfunc
            }
        };

        self.construct_closure(mfunc, remaining, params, capture_tuple)
    }

    // let f = #(f 0) in
    // #(f 1)
    //
    // {
    //   data = *{
    //     { data = *u8, // `0` // *{int}
    //       call = fnptr(*u8, int, int -> float)
    //     },
    //     int // `1`
    //   }
    //   call = fnptr(*u8, int -> float)
    // }
    //  where
    //   fn call data c as *u8, int -> float =
    //     let {{data, call}, b} = deref data in
    //       call data b c
    pub fn partially_applicate_closure(&mut self, target: V, mut params: Vec<Value>) -> Value {
        let inner_object = self.ssa().type_of(target).as_key();
        let inner_fnptr = self.types().as_closure_get_fnptr(inner_object);

        let (inner_ptypes, inner_ret) = inner_fnptr.as_fnptr();
        let inner_ret = inner_ret.clone();

        let mut fnptr_ptypes = Vec::with_capacity(inner_ptypes.len() - params.len());
        fnptr_ptypes.push(MonoType::u8_pointer());
        let remaining = inner_ptypes
            .iter()
            .skip(1 + params.len())
            .cloned()
            .collect::<Vec<_>>();
        fnptr_ptypes.extend(remaining.iter().cloned());

        let capture_tuple = {
            let mut types = vec![inner_object.into()];
            for v in &params {
                types.push(self.type_of_value(*v));
            }
            self.lir.mono.get_or_make_tuple(types)
        };

        let construct = |this: &mut Self, ssa: &mut Blocks| {
            use key::Field as field;

            let ptr = ssa.transmute(V(0).value(), MonoType::pointer(capture_tuple.into()));
            let data = ssa.deref(ptr, capture_tuple.into());
            let inner = ssa.field(data, capture_tuple, field(0), inner_object.into());
            let inner_data = ssa.field(inner, inner_object, field(0), MonoType::u8_pointer());
            let inner_call = ssa.field(inner, inner_object, field(1), inner_fnptr);

            let mut applicated = vec![inner_data];

            for field_ in this.lir.mono.fields(capture_tuple).skip(1) {
                let ty = this.types().type_of_field(capture_tuple, field_);
                let v = ssa.field(data, capture_tuple, field_, ty);
                applicated.push(v);
            }

            for p in ssa.params(lir::Block::entry()).skip(1) {
                applicated.push(p.value());
            }

            let v = ssa.call(inner_call, applicated, inner_ret.clone());
            ssa.return_(v)
        };

        let symbol = format!("__Partial_{target}_in_{}", self.current.mfkey);

        let mfunc =
            self.create_deref_funcwrapper(symbol, construct, fnptr_ptypes, inner_ret.clone());

        params.insert(0, target.value());
        self.construct_closure(mfunc, remaining, params, capture_tuple)
    }

    pub fn construct_closure(
        &mut self,
        mfunc: MonoFunc,
        remaining: Vec<MonoType>,
        captures: Vec<Value>,
        cap: MonoTypeKey,
    ) -> Value {
        let ret = self.lir.functions[mfunc].returns.clone();

        let object_type = to_morphization!(self.lir, self.mir, &mut self.current.tmap)
            .closure_object(self.info.closure, remaining, ret);

        let data = self.ssa().construct(captures, cap.into());
        let dataptr = self.heap_alloc(data, cap.into());
        let dataptr = self.ssa().transmute(dataptr, MonoType::u8_pointer());

        self.ssa()
            .construct(vec![dataptr, Value::FuncPtr(mfunc)], object_type.into())
    }

    // Multi-Field:
    //
    // trait Animal
    //   fn talk as self -> string
    //   fn walk as int, self -> int
    //
    // {
    //   talk: fnptr(*u8 -> string)
    //   walk: fnptr(int, *u8 -> int)
    // }
    //
    // Single-Field:
    //
    // trait Closure
    //   fn call as self, int, int -> float
    //
    // fnptr(*u8, int, int -> float)
    pub fn dyn_object(
        &mut self,
        ikey: M<key::Impl>,
        trait_params: Vec<MonoType>,
        impltorv: Value,
        methods: Map<key::Method, MonoFunc>,
    ) -> Value {
        let trait_ = self.mir.itraits[ikey].0;
        assert_ne!(trait_, self.info.closure);

        let impltor = self.type_of_value(impltorv);
        let self_positions = self.mir.trait_objects[trait_].as_ref().unwrap();

        let dataptr = self.heap_alloc(impltorv, impltor.clone());

        let target = match self.lir.memo_trait_objects.get(&ikey) {
            Some(either) => *either,
            None if methods.len() == 1 => {
                let method = key::Method(0);
                let param = self_positions[method].0;
                let target = methods[method];
                let mfunc = self.create_method_funcwrapper(trait_, param, target, impltor.clone());

                self.lir
                    .memo_trait_objects
                    .insert(ikey, Either::Right(mfunc));

                Either::Right(mfunc)
            }
            None => {
                let (fptrs, fn_types): (Vec<_>, Vec<_>) = methods
                    .iter()
                    .map(|(method, target)| {
                        let param = self_positions[method].0;
                        let mfunc =
                            self.create_method_funcwrapper(trait_, param, *target, impltor.clone());
                        let fn_ty = self.lir.mfunc_to_fnpointer_type(mfunc);
                        (Value::FuncPtr(mfunc), fn_ty)
                    })
                    .unzip();

                let vtable_type = self.lir.mono.get_or_make_tuple(fn_types);

                let vtable_val_initializer = {
                    let mut ssa = ssa::Blocks::new(0);

                    let symbol = format!(
                        "__VTable_{trait_}_for_{impltorv}_{}",
                        fptrs.iter().map(Value::as_fptr).format(",")
                    );

                    let v = ssa.construct(fptrs, vtable_type.into());
                    ssa.return_(v);

                    self.lir.push_function(symbol, ssa, vtable_type.into())
                };

                let val = self.lir.vals.push(trait_.0, vtable_type.into());
                self.lir
                    .val_initialisers
                    .insert(val, vtable_val_initializer);

                self.lir.memo_trait_objects.insert(ikey, Either::Left(val));

                Either::Left(val)
            }
        };

        match target {
            Either::Right(mfunc) => {
                let object_type = to_morphization!(self.lir, self.mir, &mut self.current.tmap)
                    .trait_object(trait_, trait_params);

                self.ssa()
                    .construct(vec![dataptr, Value::FuncPtr(mfunc)], object_type.into())
            }
            Either::Left(val) => {
                let vtable_type = self.lir.vals[val].clone();

                let object_type = to_morphization!(self.lir, self.mir, &mut self.current.tmap)
                    .trait_object(trait_, trait_params);
                let vtableptr = self.ssa().val_to_ref(val, vtable_type);

                self.ssa()
                    .construct(vec![dataptr, vtableptr], object_type.into())
            }
        }
    }

    fn create_method_funcwrapper(
        &mut self,
        trait_: M<key::Trait>,
        self_: u32,
        target: MonoFunc,
        impltor: MonoType,
    ) -> MonoFunc {
        let mut params = self.lir.functions[target].blocks.func_params();
        let ret = self.lir.functions[target].returns.clone();
        params[self_ as usize] = MonoType::u8_pointer();

        let symbol = {
            let tname = &self.mir.trait_names[trait_];
            let mname = self.mir.name_of_method(trait_, key::Method(0));
            format!("__Dyn_{tname}:{mname}_for_{}", self.ty_symbol(&impltor))
        };

        let con = |_: &mut Self, ssa: &mut Blocks| {
            let params = ssa
                .params(lir::Block::entry())
                .map(|v| match v.0 == self_ {
                    true => {
                        let ptr = ssa.transmute(v.value(), MonoType::pointer(impltor.clone()));
                        ssa.deref(ptr, impltor.clone())
                    }
                    false => v.value(),
                })
                .collect();

            ssa.jump(target, params);
        };

        self.create_deref_funcwrapper(symbol, con, params, ret)
    }

    fn create_deref_funcwrapper(
        &mut self,
        symbol: String,
        construct: impl FnOnce(&mut Self, &mut Blocks),
        ptypes: Vec<MonoType>,
        ret: MonoType,
    ) -> MonoFunc {
        let mut ssa = ssa::Blocks::new(ptypes.len() as u32);
        for ty in ptypes {
            ssa.add_block_param(lir::Block::entry(), ty);
        }

        // Construct the forwarding jump
        construct(self, &mut ssa);

        self.lir.push_function(symbol, ssa, ret)
    }
}