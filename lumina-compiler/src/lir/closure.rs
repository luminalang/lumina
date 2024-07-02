//! Lowering for Closures and partial application

use super::{ssa, FuncLower, Function, MonoFunc, MonoType, MonoTypeKey, Value};
use crate::prelude::*;

impl<'a> FuncLower<'a> {
    pub fn partially_applicate_func(&mut self, target: MonoFunc, given: Vec<lir::Value>) -> Value {
        let given_types = given.iter().map(|v| self.type_of_value(*v)).collect();
        let vtable = self.partial_application_vtable(target, given_types);
        let data = self.current.ssa.construct(given, vtable.object.into());
        self.construct_dyn_object(&vtable, data.value())
    }

    // Allocates and pairs opaque pointer for data to a vtable
    pub fn construct_dyn_object(&mut self, vtable: &VTable, data: Value) -> Value {
        let data_ptr = self.heap_alloc(data, vtable.data_type.clone());

        let vtable_ptr = self
            .current
            .ssa
            .val_to_ref(vtable.val, vtable.vtable_type.into());

        self.current
            .ssa
            .construct(
                vec![data_ptr.value(), vtable_ptr.into()],
                vtable.object.into(),
            )
            .value()
    }

    // MyTrait:target (impltor as MyTrait) 40
    //
    // lowers to
    //
    // { data = alloc impltor, vtable }
    // where
    //   val vtable = [
    //     fn _ ptr a =
    //       let object = deref ptr in MyTrait:target object a
    //   ]
    pub fn trait_impl_vtable(
        &mut self,
        trait_: M<key::Trait>,
        impltor: MonoType,
        methods: Map<key::Method, MonoFunc>,
    ) -> VTable {
        let module = self.current.origin.module();

        let data_type = impltor;

        let Some(param_pos) = self.mir.trait_objects[trait_].as_ref() else {
            panic!("{trait_} is not object safe");
        };
        let vtable_type = self.new_vtable_type(Some(param_pos), methods.values().copied());
        let object_type = self.new_object_type(trait_, vtable_type);

        if let Some(val) = self.lir.vtables.get(&object_type).copied() {
            return VTable { val, object: object_type, data_type, vtable_type };
        }

        let forwarding_funcs = methods
            .into_iter()
            .map(|(method, target)| {
                // Get the function typing by accessing the function pointer in the vtable
                let vtable_call_field = self
                    .lir
                    .types
                    .types
                    .type_of_field(vtable_type, key::RecordField(method.0));
                let (vtable_func_ptypes, ret) = vtable_call_field.as_fnptr();

                let mut blocks = ssa::Blocks::new(vtable_func_ptypes.iter().cloned().collect());

                let self_bparam = lir::BlockParam(param_pos[method].0);

                // Dereference the opaque data
                let data = blocks.deref(Value::BlockParam(self_bparam), data_type.clone());

                // Forward the parameters
                let params = (0..vtable_func_ptypes.len())
                    .map(|i| {
                        if self_bparam.0 == i as u32 {
                            data.value()
                        } else {
                            Value::BlockParam(lir::BlockParam(i as u32))
                        }
                    })
                    .collect();

                // Call the target
                blocks.jump(target, params);

                let forwarding_func = Function {
                    symbol: format!("VTable_{target}_{object_type}"),
                    blocks,
                    returns: ret.clone(),
                };

                self.lir.push_function(forwarding_func)
            })
            .collect::<Vec<_>>();

        let val = self.create_vtable_global(
            module,
            data_type.clone(),
            vtable_type,
            forwarding_funcs.into_iter(),
        );
        self.lir.vtables.insert(vtable_type, val);

        VTable { val, object: object_type, data_type, vtable_type }
    }

    // f #(target a b) // `target` takes 3 parameters
    //
    // lowers to
    //
    // { data = alloc (a, b), vtable } : fn(c -> R)
    // where           ^^^^ 'given: Vec<MonoType>'
    //   val vtable = [
    //     fn _ ptr c as *u8, c -> R =
    //       let (a, b) = deref ptr in target a b c
    //   ]
    pub fn partial_application_vtable(&mut self, target: MonoFunc, given: Vec<MonoType>) -> VTable {
        // TODO: currently these vals are declared in the module where they're first used.
        // That's a bit odd. We should probably make it so that it uses `target.module()`
        let module = self.current.origin.module();
        let given_count = given.len();

        let vtable_type = self.new_vtable_type(None, std::iter::once(target));
        let data_type = self.lir.types.get_or_make_tuple(given);
        let object_type = self.new_object_type(self.info.closure, vtable_type);

        if let Some(val) = self.lir.vtables.get(&vtable_type).copied() {
            return VTable {
                val,
                object: object_type.into(),
                data_type: data_type.into(),
                vtable_type,
            };
        }

        let vtable_call_field = self
            .lir
            .types
            .types
            .type_of_field(vtable_type, key::RecordField(0));
        let (vtable_func_ptypes, ret) = vtable_call_field.as_fnptr();

        // Create the function that's put in the vtable
        let forwarding_func = {
            let mut blocks = ssa::Blocks::new(vtable_func_ptypes.iter().cloned().collect());

            // Dereference the opaque data
            let data = blocks.deref(Value::BlockParam(lir::BlockParam(0)), data_type.into());

            let rdata = &self.lir.types.types[data_type];
            assert!(rdata.autoboxed.is_empty());

            // Flatten the values by dereferencing the opaque data into parameters for target
            let mut params = self.lir.types.types[data_type]
                .fields
                .iter()
                .map(|(field, ty)| {
                    blocks
                        .field(data.value(), data_type, field, ty.clone())
                        .value()
                })
                .collect::<Vec<_>>();

            // Append all parameters after the flattened values as additional params for target
            for (i, _) in vtable_func_ptypes.iter().skip(given_count).enumerate() {
                let pid = lir::BlockParam(i as u32 + 1); // offset to skip the capture data
                let additional = Value::BlockParam(pid);
                params.push(additional);
            }

            // Call the target
            blocks.jump(target, params);

            let forwarding_func = Function {
                symbol: format!("VTable_{target}_{object_type}"),
                blocks,
                returns: ret.clone(),
            };

            self.lir.push_function(forwarding_func)
        };

        let fns = std::iter::once(forwarding_func);
        let val = self.create_vtable_global(module, data_type.into(), vtable_type, fns);
        self.lir.vtables.insert(vtable_type, val);

        VTable {
            val,
            object: object_type.into(),
            data_type: data_type.into(),
            vtable_type,
        }
    }

    // Generate the initialiser and global for the vtable
    fn create_vtable_global(
        &mut self,
        module: key::Module,
        data_type: MonoType,
        vtable: MonoTypeKey,
        fns: impl Iterator<Item = MonoFunc>,
    ) -> M<key::Val> {
        let vtable_val_initialiser = {
            let mut blocks = ssa::Blocks::new(Map::new());

            let fn_pointers = fns.map(Value::FuncPtr).collect();
            let v = blocks.construct(fn_pointers, vtable.into());
            blocks.return_(v.value());

            let init_func = Function {
                symbol: format!("VTable_{data_type:?}_{vtable}_initialiser"),
                blocks,
                returns: vtable.into(),
            };

            self.lir.push_function(init_func)
        };

        let val = self.lir.vals.push(module, vtable.into());
        self.lir
            .val_initialisers
            .insert(val, vtable_val_initialiser);

        val
    }

    // Construct the vtable type by iterating the functions to be included while substituting
    // `self` for `*u8`. If the position of `self` isn't specified then assume first parameter.
    fn new_vtable_type(
        &mut self,
        param: Option<&Map<key::Method, key::Param>>,
        methods: impl IntoIterator<Item = MonoFunc>,
    ) -> MonoTypeKey {
        // Using tuples here could be considered dangerous as it relies on the `self.lir.vtables`
        // being used.
        let fields = methods
            .into_iter()
            .enumerate()
            .map(|(i, mfunc)| {
                let func = &self.lir.functions[mfunc];

                let mut params = func
                    .blocks
                    .params(lir::Block::entry())
                    .values()
                    .cloned()
                    .collect::<Vec<_>>();

                let opaque = param
                    .map(|methods| methods[key::Method(i as u32)])
                    .unwrap_or(key::Param(0));
                params[opaque.0 as usize] = MonoType::u8_pointer();

                MonoType::fn_pointer(params, func.returns.clone())
            })
            .collect();

        self.lir.types.get_or_make_tuple(fields)
    }

    // Create the object struct which is an opaque pointer attached to the vtable pointer
    fn new_object_type(&mut self, trait_: M<key::Trait>, vtable: MonoTypeKey) -> MonoTypeKey {
        self.lir.types.get_or_make_record(
            trait_,
            vec![MonoType::u8_pointer(), MonoType::pointer(vtable.into())],
        )
    }
}

#[derive(Clone)]
pub struct VTable {
    val: M<key::Val>,    // global for vtable
    data_type: MonoType, // the real type behind `*u8`
    vtable_type: MonoTypeKey,
    object: MonoTypeKey, // *u8+vtable
}
