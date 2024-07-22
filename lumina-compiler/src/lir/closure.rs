//! Lowering for Closures and partial application

use super::{ssa, FuncLower, Function, MonoFunc, MonoType, MonoTypeKey, Value};
use crate::prelude::*;

impl<'a> FuncLower<'a> {
    pub fn partially_applicate_func(&mut self, target: MonoFunc, given: Vec<lir::Value>) -> Value {
        info!("partially applicating {target} with {given:?}");
        let given_types = given.iter().map(|v| self.type_of_value(*v)).collect();
        let vtable = self.partial_application_vtable(target, given_types);
        let data = self.ssa().construct(given, vtable.object.into());
        self.construct_dyn_object(&vtable, data.value())
    }

    // Allocates and pairs opaque pointer for data to a vtable
    pub fn construct_dyn_object(&mut self, vtable: &VTable, data: Value) -> Value {
        let data_ptr = self.heap_alloc(data, vtable.data_type.clone());

        let vtable_ptr = self.ssa().val_to_ref(vtable.val, vtable.vtable_type.into());

        self.ssa()
            .construct(vec![data_ptr, vtable_ptr.into()], vtable.object.into())
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
        let vtable_type =
            self.new_vtable_type(VTableSelf::Method(param_pos), methods.values().copied());
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

                // Define the forwarding function
                self.lir.push_function(
                    format!("VTable_{target}_{object_type}"),
                    blocks,
                    ret.clone(),
                )
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

        let vtable_type = self.new_vtable_type(
            VTableSelf::PartialApplication(given_count),
            std::iter::once(target),
        );
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

        let target_func = &self.lir.functions[target];

        // Create the function that's put in the vtable
        let forwarding_func = {
            let mut blocks = ssa::Blocks::new(vtable_func_ptypes.iter().cloned().collect());

            let mut params = if given_count == 0 {
                vec![]
            } else {
                // Dereference the opaque data
                let data = blocks.deref(Value::BlockParam(lir::BlockParam(0)), data_type.into());

                let rdata = &self.lir.types.types[data_type];
                assert!(rdata.autoboxed.is_empty());

                // Flatten the values by dereferencing the opaque data into parameters for target
                self.lir.types.types[data_type]
                    .fields
                    .iter()
                    .map(|(field, ty)| {
                        blocks
                            .field(data.value(), data_type, field, ty.clone())
                            .value()
                    })
                    .collect::<Vec<_>>()
            };

            // Append all parameters after the flattened values as additional params for target
            for (i, _) in target_func
                .blocks
                .params(lir::Block::entry())
                .iter()
                .skip(given_count)
                .enumerate()
            {
                let pid = lir::BlockParam(i as u32 + 1); // offset to skip the capture data
                let additional = Value::BlockParam(pid);
                params.push(additional);
            }

            // Call the target
            blocks.jump(target, params);

            // Define the forwarding func
            self.lir.push_function(
                format!("VTable_({})_Closure", self.lir.functions[target].symbol),
                blocks,
                ret.clone(),
            )
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
        fns: impl Iterator<Item = MonoFunc> + Clone,
    ) -> M<key::Val> {
        let vtable_val_initialiser = {
            let mut blocks = ssa::Blocks::new(Map::new());

            let fn_pointers = fns.clone().map(Value::FuncPtr).collect();
            let v = blocks.construct(fn_pointers, vtable.into());
            blocks.return_(v.value());

            let init_name = format!(
                "VTable_{}_{}^{}_initialiser",
                self.ty_symbol(&data_type),
                fns.format(","),
                self.ty_symbol(&MonoType::Monomorphised(vtable)),
            );
            self.lir.push_function(init_name, blocks, vtable.into())
        };

        let val = self.lir.vals.push(module, vtable.into());
        self.lir
            .val_initialisers
            .insert(val, vtable_val_initialiser);

        val
    }

    // Construct the vtable type by iterating the functions to be included while substituting
    // `self` for `*u8`. If the position of `self` isn't specified then assume partial application
    // and substitute `N` parameters with a single `*u8`.
    fn new_vtable_type<'s>(
        &mut self,
        param: VTableSelf<'s>,
        methods: impl IntoIterator<Item = MonoFunc>,
    ) -> MonoTypeKey {
        // Using tuples here could be considered dangerous as it relies on the `self.lir.vtables`
        // being used.
        let fields = methods
            .into_iter()
            .enumerate()
            .map(|(i, mfunc)| {
                let func = &self.lir.functions[mfunc];

                let target_params = func.blocks.params(lir::Block::entry());

                let params = match param {
                    VTableSelf::PartialApplication(count) => {
                        let mut params = vec![MonoType::u8_pointer()];
                        params.extend(target_params.values().skip(count).cloned());
                        params
                    }
                    VTableSelf::Method(methods) => {
                        let mut params = target_params.values().cloned().collect::<Vec<_>>();
                        params[methods[key::Method(i as u32)].0 as usize] = MonoType::u8_pointer();
                        params
                    }
                };

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

enum VTableSelf<'a> {
    PartialApplication(usize),
    Method(&'a Map<key::Method, key::Param>),
}
