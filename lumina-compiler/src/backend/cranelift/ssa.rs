use super::{abi, abi::ByteOffset, Context};
use crate::prelude::*;
use cranelift::frontend::FuncInstBuilder;
use cranelift::prelude::*;
use cranelift_codegen::ir;
use cranelift_module::{FuncId, Module};
use either::Either;
use lir::{MonoFunc, MonoType, MonoTypeKey};
use lumina_typesystem::IntSize;
use lumina_util::Highlighting;
use std::cmp::Ordering;

#[derive(new)]
pub struct Translator<'c, 'a, 'f> {
    ctx: &'c mut Context<'a>,
    f: Current<'a, 'f>,
}

type Predecessors = u16;

#[derive(new)]
struct Current<'a, 'f> {
    func: &'a lir::Function,
    builder: FunctionBuilder<'f>,
    blockmap: Map<lir::Block, (Block, Predecessors)>,
    #[new(default)]
    imports: HashMap<FuncId, ir::FuncRef>,
    abi_block_types: Map<lir::Block, Map<key::Param, abi::Param>>,
    #[new(default)]
    vmap: Map<lir::V, VEntry>,
    #[new(value = "lir::Block::entry()")]
    block: lir::Block,
}

/// During the lower from LIR to CLIR, we have two categories of values.
///
/// * S_Stable
/// S_Stable values are those which are in the memory layout that's expected when stored inside
/// another struct, either behind a pointer or embedded.
///
/// * F_Stable
/// F_Stable values are those which are in the memory layout that's expected by other functions when
/// handed as a parameter. They may contain parameters pointing to the callés stack, thus may need
/// to be memcpy'd into a heap allocation if they are to escape.
///
/// * Unstable
/// Unstable values have much looser requirements and thus much greater variation in their layout
/// which is checked dynamically during the lower of a function. Unstable values are never allowed
/// to escape the function they're created in.
///
/// All S_Stable values can be treated as F_Stable.
/// All S_Stable and F_Stable values can be treated as Unstable.
#[derive(Clone, PartialEq, Eq, Debug)]
enum VEntry {
    // S_Stable
    StructHeapPointer(MonoTypeKey, Value), // Struct by-value which is implicitly passed as a pointer.
    Scalar(abi::Scalar<Value>),
    ZST,

    // F_Stable
    StructStackPointer(MonoTypeKey, Value), // Struct by-value which is implicitly passed as a pointer.
    SumPayloadStackPointer { largest: u32, ptr: Value },

    // Unstable
    StructFlat(MonoTypeKey, Map<abi::Field, VEntry>),
}

impl VEntry {
    fn direct(point: Value) -> Self {
        VEntry::Scalar(abi::Scalar { kind: abi::ScalarKind::Direct, point })
    }

    #[track_caller]
    fn as_direct(&self) -> Value {
        match self {
            VEntry::Scalar(abi::Scalar { kind: abi::ScalarKind::Direct, point }) => *point,
            other => panic!("as_direct called on non-direct ventry: {other:?}"),
        }
    }

    #[track_caller]
    fn as_intable(&self) -> (Value, abi::ScalarKind) {
        match self {
            VEntry::Scalar(abi::Scalar {
                kind: kind @ (abi::ScalarKind::Direct | abi::ScalarKind::Pointer(_)),
                point,
            }) => (*point, kind.clone()),
            other => panic!("as_intable called on non-intable ventry: {other:?}"),
        }
    }
}

impl<'a, 'f> Current<'a, 'f> {
    fn switch_to_block(&mut self, block: lir::Block) {
        let (clblock, _) = self.blockmap[block];

        info!("switching to {} (cl{clblock})", block);

        self.builder.switch_to_block(clblock);
        self.block = block;
    }

    fn type_of_value(&self, value: Value) -> Type {
        self.builder.func.dfg.value_type(value)
    }
}

impl<'c, 'a, 'f> Translator<'c, 'a, 'f> {
    fn ins(&mut self) -> FuncInstBuilder<'_, 'f> {
        self.f.builder.ins()
    }

    fn types(&self) -> &lir::Types {
        &self.ctx.lir.types
    }

    pub fn func(ctx: &'c mut Context<'a>, func: &'a lir::Function, key: MonoFunc) -> ir::Function {
        let mut func_builder_ctx = FunctionBuilderContext::new();
        let mut clfunc = ir::Function::new();

        let abi_block_types = func
            .blocks
            .blocks()
            .map(|block| {
                ctx.structs
                    .records
                    .get_abi_params(func.blocks.param_types(block))
            })
            .collect::<Map<_, _>>();

        let ptr = ctx.ptr();
        let header = &ctx.funcmap[key];

        let params = &abi_block_types[lir::Block::entry()];
        clfunc.signature = ctx
            .structs
            .signature(isa::CallConv::Fast, &params, &header.typing.ret);
        let mut builder = FunctionBuilder::new(&mut clfunc, &mut func_builder_ctx);

        let blockmap = func
            .blocks
            .blocks()
            .map(|block| {
                let clblock = builder.create_block();

                func.blocks.params(block).for_each(|v| {
                    let t = func.blocks.type_of(v);
                    let param = ctx.structs.records.abi_param(t);

                    match param {
                        abi::Param::Struct(mk) => match ctx.structs.pass_mode(mk) {
                            abi::PassBy::Pointer => {
                                builder.append_block_param(clblock, ptr);
                            }
                            abi::PassBy::Value => {
                                ctx.structs.iter_s_stable_fields(mk, &mut |scalar| {
                                    builder.append_block_param(clblock, scalar.point);
                                });
                            }
                        },
                        abi::Param::Scalar(scalar) => {
                            builder.append_block_param(clblock, scalar.point);
                        }
                        abi::Param::ZST => {}
                    }
                });

                if block == lir::Block::entry() {
                    // Attach the structret pointer as a parameter if needed
                    if let abi::Return::Struct(mk) = &header.typing.ret {
                        if let abi::PassBy::Pointer = ctx.structs.pass_mode(*mk) {
                            builder.append_block_param(clblock, ptr);
                        }
                    }
                }

                (clblock, 0)
            })
            .collect();

        Translator {
            ctx,
            f: Current::new(func, builder, blockmap, abi_block_types),
        }
        .lower_and_finalize_current();

        info!("lowered {}:\n {clfunc}", func.symbol);

        if let Err(err) = cranelift_codegen::verify_function(&clfunc, ctx.isa.as_ref()) {
            error!("cranelift_codegen verifier error:\n{err}");
        }

        clfunc
    }

    fn lower_and_finalize_current(mut self) {
        let (clblock, predecessors) = &mut self.f.blockmap[lir::Block::entry()];
        *predecessors += 1;

        self.f.builder.seal_block(*clblock);
        self.f.switch_to_block(self.f.block);
        self.block();

        self.f.builder.finalize();
    }

    fn abi_param_to_ventry(&self, abi_param: &abi::Param, input: &mut &[Value]) -> VEntry {
        match abi_param {
            abi::Param::ZST => VEntry::ZST,
            abi::Param::Scalar(scalar) => {
                let point = next_value(input);
                VEntry::Scalar(abi::Scalar { kind: scalar.kind.clone(), point })
            }
            &abi::Param::Struct(mk) => self.abi_param_struct_to_ventry(mk, None, input),
        }
    }

    fn abi_param_struct_to_ventry(
        &self,
        mk: MonoTypeKey,
        out: Option<Value>,
        input: &mut &[Value],
    ) -> VEntry {
        match self.ctx.structs.pass_mode(mk) {
            abi::PassBy::Pointer => {
                // When mapping the return values from a function, the out pointer is not passed in
                // the instruction results but rather as a known stack slot.
                let ptr = out.unwrap_or_else(|| next_value(input));

                // We are pessimistic about the origin of a struct as to not have to
                // monomorphise over memory origin.
                //
                // Setting it as a stack pointer means that it'll always be memcpy'd into
                // an allocation if escaped into a struct.
                VEntry::StructStackPointer(mk, ptr)
            }
            abi::PassBy::Value => {
                let buf = self
                    .ctx
                    .structs
                    .get(mk)
                    .fields
                    .values()
                    .map(|fv| self.abi_field_to_ventry(fv, input))
                    .collect();

                VEntry::StructFlat(mk, buf)
            }
        }
    }

    fn abi_field_to_ventry(&self, field: &abi::FieldV, input: &mut &[Value]) -> VEntry {
        match field {
            abi::FieldV::Flat(inner) => {
                let p = self.ctx.structs.records.abi_param(inner);
                self.abi_param_to_ventry(&p, input)
            }
            &abi::FieldV::AutoBoxed(mk) => {
                let ptr = next_value(input);
                VEntry::StructHeapPointer(mk, ptr)
            }
            abi::FieldV::SumPayloadPointer { sum } => {
                let ptr = next_value(input);
                let largest = self.ctx.structs.sum_payload_alloca_size(*sum);
                VEntry::SumPayloadStackPointer { largest, ptr }
            }
            &abi::FieldV::SumPayloadInline(clty) => {
                let inline = next_value(input);
                VEntry::Scalar(abi::Scalar::new(inline, abi::ScalarKind::SumInline(clty)))
            }
        }
    }

    fn block(&mut self) {
        let mut raw_params = self.f.builder.block_params(self.f.blockmap[self.f.block].0);
        let raw_params_ref: &mut &[Value] = &mut raw_params;

        // Map the block parameters to ventries
        for (i, v) in self.f.func.blocks.params(self.f.block).enumerate() {
            let param = key::Param(i as u32);
            let abi_param = self.f.abi_block_types[self.f.block][param].clone();
            let ventry = self.abi_param_to_ventry(&abi_param, raw_params_ref);
            self.f.vmap.push_as(v, ventry);
        }

        if self.f.builder.func.signature.uses_struct_return_param()
            && self.f.block == lir::Block::entry()
        {
            let _struct_return = next_value(raw_params_ref);
        }

        assert_eq!(raw_params_ref, &[]);

        for v in self.f.func.blocks.entries(self.f.block) {
            let entry = self.f.func.blocks.entry_of(v);
            let ty = self.f.func.blocks.type_of(v);
            let n = self.f.builder.func.dfg.num_values();
            trace!("at v{n} ; lowering {v} {} {entry} : {ty:?}", '='.symbol());
            let entry = self.entry(entry, ty);
            // trace!("{v} => {entry:?}");
            self.f.vmap.push_as(v, entry);
        }

        let flow = self.f.func.blocks.flow_of(self.f.block);
        trace!("lowering flow {flow}");
        self.flow(flow)
    }

    fn declare_func_in_func(&mut self, id: FuncId) -> ir::FuncRef {
        *self.f.imports.entry(id).or_insert_with(|| {
            self.ctx
                .objmodule
                .declare_func_in_func(id, &mut self.f.builder.func)
        })
    }

    fn map_call_results_to_ventry(
        &mut self,
        call: ir::Inst,
        ret: &abi::Return,
        slot: Option<(ir::StackSlot, Value)>,
    ) -> VEntry {
        let mut rvalues = self.f.builder.inst_results(call);

        match ret {
            &abi::Return::Struct(mk) => {
                let mslot = slot.map(|(_, ptr)| ptr);
                self.abi_param_struct_to_ventry(mk, mslot, &mut rvalues)
            }
            abi::Return::Scalar(scalar) => {
                assert_eq!(rvalues.len(), 1);
                VEntry::Scalar(abi::Scalar { kind: scalar.kind.clone(), point: rvalues[0] })
            }
            abi::Return::ZST => VEntry::ZST,
        }
    }

    fn create_struct_stack_slot(&mut self, size: u32, align: u8) -> (ir::StackSlot, u32) {
        let slotdata = StackSlotData::new(StackSlotKind::ExplicitSlot, size, align as u8);
        (self.f.builder.create_sized_stack_slot(slotdata), size)
    }

    fn append_rptr_if_needed(
        &mut self,
        ret: &abi::Return,
        params: &mut Vec<Value>,
    ) -> Option<(ir::StackSlot, Value)> {
        match ret {
            abi::Return::Struct(mk) => match self.ctx.structs.pass_mode(*mk) {
                abi::PassBy::Value => None,
                abi::PassBy::Pointer => {
                    let (size, align) = self.ctx.structs.size_and_align_of(&(*mk).into());
                    let (slot, _) = self.create_struct_stack_slot(size, align as u8);
                    let ptr = self.ins().stack_addr(types::I64, slot, 0);
                    params.push(ptr);
                    Some((slot, ptr))
                }
            },
            abi::Return::Scalar(_) => None,
            abi::Return::ZST => None,
        }
    }

    fn call_func(&mut self, id: FuncId, typing: abi::Typing, params: &[lir::Value]) -> VEntry {
        let mut appl = self.params(&typing.params, params);
        let slot = self.append_rptr_if_needed(&typing.ret, &mut appl);

        let fref = self.declare_func_in_func(id);
        let call = self.ins().call(fref, &appl);

        self.map_call_results_to_ventry(call, &typing.ret, slot)
    }

    fn call_func_pointer(
        &mut self,
        fptr: Value,
        typing: &abi::Typing,
        params: &[lir::Value],
    ) -> VEntry {
        let mut appl = self.params(&typing.params, params);
        let slot = self.append_rptr_if_needed(&typing.ret, &mut appl);

        let sig = self
            .ctx
            .structs
            .signature(typing.conv, &typing.params, &typing.ret);
        let sigref = self.f.builder.import_signature(sig);
        let call = self.ins().call_indirect(sigref, fptr, &appl);

        self.map_call_results_to_ventry(call, &typing.ret, slot)
    }

    fn construct_variant(
        &mut self,
        key: MonoTypeKey,
        var: key::Variant,
        values: &[lir::Value],
    ) -> VEntry {
        let lir::MonoTypeData::Sum { tag, variants, .. } = &self.ctx.structs.records[key] else {
            panic!("attempted to construct variant of non-sum");
        };

        let param_tuple = variants[var];

        let tagt = Type::int(tag.bits() as u16).unwrap();
        let tag = self.ins().iconst(tagt, var.0 as i64);
        let tagfield = VEntry::direct(tag);

        let sum_struct = self.ctx.structs.get(key);

        match sum_struct.field_map.as_slice() {
            [_tagi] => VEntry::StructFlat(key, [tagfield].into()),
            [_tagi, payloadi] => {
                let payload = match &sum_struct.fields[*payloadi] {
                    &abi::FieldV::SumPayloadPointer { sum } => {
                        let largest = self.ctx.structs.sum_payload_alloca_size(sum);
                        let ptr =
                            self.construct_record_on_stack(Some(largest), param_tuple, values);
                        VEntry::SumPayloadStackPointer { largest, ptr }
                    }
                    &abi::FieldV::SumPayloadInline(clty) => {
                        let inline = self.compress_into_inline_payload(clty, param_tuple, values);
                        VEntry::Scalar(abi::Scalar::new(inline, abi::ScalarKind::SumInline(clty)))
                    }
                    other => unreachable!("{other:?}"),
                };
                VEntry::StructFlat(key, [payload, tagfield].into())
            }
            other => unreachable!("bad field count for sum type: {other:?}"),
        }
    }

    // Creates a payload compressed scalar from a list of values
    fn compress_into_inline_payload(
        &mut self,
        clty: Type,
        param_tuple: MonoTypeKey,
        values: &[lir::Value],
    ) -> Value {
        let fields = self.construct_record_in_regs(param_tuple, values);
        match fields.as_slice() {
            [] | [VEntry::ZST] => self.ins().iconst(clty, 0),
            [VEntry::Scalar(scalar)] => {
                match scalar.kind {
                    abi::ScalarKind::Direct => {
                        let has = self.f.type_of_value(scalar.point);
                        if has == types::F64 {
                            todo!("we need to cast as well");
                        } else {
                            assert!(has.is_int());
                            self.resize_uint(scalar.point, clty)
                        }
                    }
                    abi::ScalarKind::SumInline(_) => unreachable!(),

                    // Pointers
                    _ => scalar.point,
                }
            }
            [_, _] => todo!("compress multiple small variants into one sum payload"),
            _ => panic!("cannot compress variants with 3 or more fields into inline payload"),
        }
    }

    fn decompress_inline_payload(
        &mut self,
        _from: Type,
        inline: Value,
        param_tuple: MonoTypeKey,
    ) -> Map<abi::Field, VEntry> {
        let struct_ = self.ctx.structs.get(param_tuple).clone();
        match struct_.fields.as_slice() {
            [] => Map::new(),
            [x] => match x {
                abi::FieldV::Flat(ty) => [self.cast_value_to_scalar(ty, inline)].into(),
                abi::FieldV::AutoBoxed(inner) => [VEntry::Scalar(abi::Scalar::new(
                    inline,
                    abi::ScalarKind::AutoBoxed((*inner).into()),
                ))]
                .into(),
                _ => unreachable!(),
            },
            [_, _] => todo!(),
            _ => panic!("cannot compress variants with 3 or more fields into inline payload"),
        }
    }

    fn construct_record(&mut self, key: MonoTypeKey, values: &[lir::Value]) -> VEntry {
        debug_assert_eq!(values.len(), self.ctx.structs.get(key).fields.len());

        if values.is_empty() {
            return VEntry::ZST;
        }

        match self.ctx.structs.pass_mode(key) {
            abi::PassBy::Pointer => {
                let ptr = self.construct_record_on_stack(None, key, values);
                VEntry::StructStackPointer(key, ptr)
            }
            abi::PassBy::Value => {
                let fields = self.construct_record_in_regs(key, values);
                VEntry::StructFlat(key, fields)
            }
        }
    }

    fn construct_record_on_stack(
        &mut self,
        size: Option<u32>,
        key: MonoTypeKey,
        values: &[lir::Value],
    ) -> Value {
        let (ssize, align) = self.ctx.structs.size_and_align_of(&key.into());
        let size = match size {
            Some(size) if ssize > size => panic!("alloca to small for {key}"),
            Some(size) => size,
            None => ssize,
        };

        let (slot, _) = self.create_struct_stack_slot(size, align as u8);
        let size_t = self.ctx.ptr();

        for (field, value) in values.iter().enumerate() {
            let field = key::Field(field as u32);
            let entry = self.value_to_entry(*value);

            let abi_field = self.ctx.structs.get_real_field(key, field);
            let offset = self.ctx.structs.offset_of(key, abi_field);

            let slot_addr = self.ins().stack_addr(size_t, slot, offset.0 as i32);

            self.write_entry_to_ptr(slot_addr, &entry);
        }

        self.ins().stack_addr(size_t, slot, 0)
    }

    fn construct_record_in_regs(
        &mut self,
        key: MonoTypeKey,
        values: &[lir::Value],
    ) -> Map<abi::Field, VEntry> {
        let mut fields = Map::<abi::Field, _>::with_capacity(values.len());

        for fi in self.ctx.structs.get(key).fields.keys() {
            let struct_ = self.ctx.structs.get(key);
            let fieldkey = struct_.field_map.find(|i| *i == fi).unwrap();

            let matching = values[fieldkey.0 as usize];
            let entry = self.value_to_entry(matching);

            fields.push(entry);
        }

        fields
    }

    #[track_caller]
    fn lower_block_if_last_predecessor(&mut self, block: lir::Block) {
        let (clblock, predecessors) = &mut self.f.blockmap[block];
        *predecessors += 1;

        if *predecessors == self.f.func.blocks.predecessors(block) {
            self.f.builder.seal_block(*clblock);
            self.f.switch_to_block(block);
            self.block();
        }
    }

    fn entry(&mut self, entry: &lir::Entry, ty: &MonoType) -> VEntry {
        match entry {
            lir::Entry::Copy(value) => self.value_to_entry(*value),
            lir::Entry::BlockParam(v) => self.value_to_entry(lir::Value::V(*v)),
            lir::Entry::Transmute(value) => match self.value_to_entry(*value) {
                VEntry::StructFlat(_, _)
                | VEntry::StructStackPointer(_, _)
                | VEntry::StructHeapPointer(_, _) => {
                    unimplemented!("transmuting structs");
                }
                VEntry::Scalar(scalar) => self.cast_value_to_scalar(ty, scalar.point),
                VEntry::SumPayloadStackPointer { ptr, .. } => self.cast_value_to_scalar(ty, ptr),
                VEntry::ZST => VEntry::ZST,
            },
            lir::Entry::SizeOf(ty) => {
                let size = self.ctx.structs.size_of(ty);
                let size_t = self.ctx.ptr();
                let v = self.ins().iconst(size_t, size as i64);
                VEntry::direct(v)
            }
            lir::Entry::CallStatic(mfunc, params) => {
                let fheader = &self.ctx.funcmap[*mfunc];
                self.call_func(fheader.id, fheader.typing.clone(), params)
            }
            lir::Entry::CallExtern(key, params) => {
                let fheader = &self.ctx.externmap[key];
                self.call_func(fheader.id, fheader.typing.clone(), params)
            }
            lir::Entry::CallValue(ptr, params) => {
                let ptr = self.value_to_entry(*ptr);
                match ptr {
                    VEntry::Scalar(abi::Scalar {
                        kind: abi::ScalarKind::FuncPointer(typing),
                        point,
                    }) => self.call_func_pointer(point, &typing, params),
                    _ => panic!("call to non-function"),
                }
            }
            lir::Entry::Construct(values) => match ty {
                MonoType::Monomorphised(mk) => self.construct_record(*mk, values),
                _ => panic!("cannot construct: {ty:?}"),
            },
            lir::Entry::Variant(var, values) => match ty {
                MonoType::Monomorphised(mk) => self.construct_variant(*mk, *var, values),
                _ => panic!("cannot construct: {ty:?}"),
            },
            lir::Entry::TagFromSum { of } => match self.value_to_entry(*of) {
                VEntry::StructFlat(key, scalars) => {
                    let i = self.ctx.structs.get(key).field_map[key::Field(0)];
                    match scalars[i].clone() {
                        VEntry::Scalar(scalar) => {
                            assert_eq!(scalar.kind, abi::ScalarKind::Direct);
                            VEntry::Scalar(scalar)
                        }
                        other => panic!("unexpected entry kind for tag: {other:?}"),
                    }
                }
                VEntry::StructStackPointer(key, ptr) => {
                    let (tagsize, _) = self.ctx.structs.records[key].as_sum();
                    let tagt = Type::int(tagsize.bits() as u16).unwrap();

                    let size_t = self.ctx.ptr().bytes();
                    let v = self.deref_scalar(ptr, ByteOffset(size_t), tagt);
                    VEntry::direct(v)
                }
                other => unreachable!("{other:?}"),
            },
            lir::Entry::RefStaticVal(val) => {
                let MonoType::Pointer(ty) = ty else {
                    panic!("ref_static_val into non-pointer type");
                };

                let dataid = self.ctx.val_to_globals[*val];
                let data = self
                    .ctx
                    .objmodule
                    .declare_data_in_func(dataid, &mut self.f.builder.func);

                let size_t = self.ctx.ptr();
                let ptr = self.ins().symbol_value(size_t, data);

                VEntry::Scalar(abi::Scalar::pointer(ptr, &*ty))
            }
            lir::Entry::Field { of, field, key } => match self.value_to_entry(*of) {
                VEntry::StructStackPointer(mk, ptr) | VEntry::StructHeapPointer(mk, ptr) => {
                    assert_eq!(*key, mk);

                    let rfield = self.ctx.structs.get_real_field(*key, *field);
                    let offset = self.ctx.structs.offset_of(*key, rfield);

                    self.deref_type(ptr, offset, ty)
                }
                VEntry::StructFlat(mk, fields) => {
                    assert_eq!(*key, mk);

                    let rfield = self.ctx.structs.get_real_field(*key, *field);
                    fields[rfield].clone()
                }
                other => panic!("field of non-struct: {other:?}"),
            },
            lir::Entry::CastFromSum { of } => match self.value_to_entry(*of) {
                // Sum types without a payload
                VEntry::StructFlat(key, params) if params.len() == 1 => {
                    assert_eq!(self.ctx.structs.get(key).fields.len(), 1);
                    assert_eq!(self.ctx.structs.size_of(ty), 0);
                    VEntry::ZST
                }
                VEntry::StructFlat(key, params) => {
                    let payi = self.ctx.structs.get_real_field(key, key::Field(1));
                    let payload = &params[payi];

                    match payload {
                        // Simply cast the associated type
                        //
                        // we don't use `Scalar::pointer` because it's of type `Struct` and not `*Struct` in LIR
                        &VEntry::SumPayloadStackPointer { ptr, .. } => {
                            VEntry::StructStackPointer(ty.as_key(), ptr)
                        }
                        &VEntry::Scalar(abi::Scalar {
                            point: ptr,
                            kind: abi::ScalarKind::HeapSumPointer { .. },
                        }) => VEntry::StructHeapPointer(ty.as_key(), ptr),

                        VEntry::Scalar(abi::Scalar {
                            point,
                            kind: abi::ScalarKind::SumInline(clty),
                        }) => {
                            let fields = self.decompress_inline_payload(*clty, *point, ty.as_key());
                            VEntry::StructFlat(ty.as_key(), fields)
                        }
                        _ => panic!("casting non-opaque sum payload. Should that be allowed?"),
                    }
                }
                VEntry::StructStackPointer(key, ptr) => {
                    let struct_ = self.ctx.structs.get(key);

                    let payi = self.ctx.structs.get_real_field(key, key::Field(1));
                    let poffset = self.ctx.structs.offset_of(key, payi);

                    match &struct_.fields[payi] {
                        abi::FieldV::SumPayloadPointer { .. } => {
                            let size_t = self.ctx.ptr();
                            let payload_pointer = self.deref_scalar(ptr, poffset, size_t);
                            // Since it was stored as a field, it should already be S_Stable
                            VEntry::StructHeapPointer(ty.as_key(), payload_pointer)
                        }
                        &abi::FieldV::SumPayloadInline(clty) => {
                            let inline = self.deref_scalar(ptr, poffset, clty);
                            let fields = self.decompress_inline_payload(clty, inline, ty.as_key());
                            VEntry::StructFlat(ty.as_key(), fields)
                        }
                        _ => unreachable!(),
                    }
                }
                other => unreachable!("{other:?}"),
            },
            lir::Entry::IntAdd(left, right) => {
                let [left, right] = [*left, *right].map(|v| self.value_to_entry(v).as_intable().0);
                self.checked_numop(
                    ty,
                    |this| this.ins().sadd_overflow(left, right),
                    |this| this.ins().uadd_overflow(left, right),
                    |this| this.ins().iadd(left, right),
                )
            }
            lir::Entry::IntSub(left, right) => {
                let [left, right] = [*left, *right].map(|v| self.value_to_entry(v).as_intable().0);
                self.checked_numop(
                    ty,
                    |this| this.ins().ssub_overflow(left, right),
                    |this| this.ins().usub_overflow(left, right),
                    |this| this.ins().isub(left, right),
                )
            }
            lir::Entry::IntMul(left, right) => {
                let [left, right] = [*left, *right].map(|v| self.value_to_entry(v).as_intable().0);
                self.checked_numop(
                    ty,
                    |this| this.ins().smul_overflow(left, right),
                    |this| this.ins().umul_overflow(left, right),
                    |this| this.ins().imul(left, right),
                )
            }
            lir::Entry::IntDiv(left, right) => {
                let [(left, lkind), (right, _)] =
                    [*left, *right].map(|v| self.value_to_entry(v).as_intable());
                let v = if as_int(ty).signed {
                    self.ins().sdiv(left, right)
                } else {
                    self.ins().udiv(left, right)
                };
                VEntry::Scalar(abi::Scalar { point: v, kind: lkind })
            }
            lir::Entry::IntAbs(v) => {
                let (v, kind) = self.value_to_entry(*v).as_intable();
                let v = self.ins().iabs(v);
                VEntry::Scalar(abi::Scalar { point: v, kind })
            }
            lir::Entry::IntCmpInclusive(left, cmp, right, bitsize) => {
                let [(left, _), (right, _)] =
                    [*left, *right].map(|v| self.value_to_entry(v).as_intable());
                let intty = Type::int(bitsize.bits() as u16).unwrap();
                assert_eq!(self.f.type_of_value(left), intty);
                assert_eq!(self.f.type_of_value(right), intty);

                // TODO: Unsigned comparisons ( IntCC::unsigned(self) )

                let intcc = match *cmp {
                    std::cmp::Ordering::Less => IntCC::SignedLessThan,
                    std::cmp::Ordering::Equal => IntCC::Equal,
                    std::cmp::Ordering::Greater => IntCC::SignedGreaterThan,
                };

                let v = self.ins().icmp(intcc, left, right);

                VEntry::direct(v)
            }
            lir::Entry::Reduce(v) => {
                let v = self.value_to_entry(*v).as_direct();
                let ty = Type::int(as_int(ty).bits() as u16).unwrap();
                let v = self.ins().ireduce(ty, v);
                VEntry::direct(v)
            }
            lir::Entry::ExtendSigned(v) => {
                let v = self.value_to_entry(*v).as_direct();
                let ty = Type::int(as_int(ty).bits() as u16).unwrap();
                let v = self.ins().sextend(ty, v);
                VEntry::direct(v)
            }
            lir::Entry::ExtendUnsigned(v) => {
                let v = self.value_to_entry(*v).as_direct();
                let ty = Type::int(as_int(ty).bits() as u16).unwrap();
                let v = self.ins().uextend(ty, v);
                VEntry::direct(v)
            }
            lir::Entry::FloatToInt(v, intsize) => {
                let v = self.value_to_entry(*v).as_direct();
                let int = Type::int(intsize.bits() as u16).unwrap();
                let v = if intsize.signed {
                    self.ins().fcvt_to_sint_sat(int, v)
                } else {
                    self.ins().fcvt_to_uint_sat(int, v)
                };
                VEntry::direct(v)
            }
            lir::Entry::IntToFloat(v, intsize) => {
                let v = self.value_to_entry(*v).as_direct();
                let v = if intsize.signed {
                    self.ins().fcvt_from_sint(types::F64, v)
                } else {
                    self.ins().fcvt_from_uint(types::F64, v)
                };
                VEntry::direct(v)
            }
            lir::Entry::BitAnd([left, right]) => {
                let [left, right] = [*left, *right].map(|v| self.value_to_entry(v).as_intable().0);
                let v = self.ins().band(left, right);
                VEntry::direct(v)
            }
            lir::Entry::BitNot(v) => {
                let v = self.value_to_entry(*v).as_direct();
                let v = self.ins().bnot(v);
                VEntry::direct(v)
            }
            // TODO: for ZST we should just give a hardcoded `0`
            lir::Entry::Alloc => {
                let MonoType::Pointer(ty) = ty else {
                    panic!("attempted to allocate for non-pointer");
                };
                let size = self.ctx.structs.size_of(ty);
                let ptr = self.heap_alloc(size as i128);
                VEntry::Scalar(abi::Scalar::pointer(ptr, &*ty))
            }
            lir::Entry::Dealloc { ptr } => {
                self.heap_dealloc(*ptr, 0);
                VEntry::ZST
            }
            lir::Entry::WritePtr { ptr, value } => {
                let v = self.value_to_entry(*value);
                match self.value_to_entry(*ptr) {
                    VEntry::Scalar(abi::Scalar { kind: abi::ScalarKind::Pointer(_), point }) => {
                        self.write_entry_to_ptr(point, &v)
                    }
                    entry => panic!("attempted write to non-ptr: {entry:?}"),
                }

                VEntry::ZST
            }
            lir::Entry::Deref(ptr) => {
                let (ptr, kind) = self.value_to_entry(*ptr).as_intable();

                assert!(
                    matches!(kind, abi::ScalarKind::Pointer(_)),
                    "should we allow builtin deref on int without cast?"
                );

                self.deref_type(ptr, ByteOffset(0), ty)
            }
        }
    }

    fn deref_struct_into_raw(
        &mut self,
        for_each: &mut dyn FnMut(abi::Scalar<Value>),
        key: MonoTypeKey,
        ptr: Value,
    ) {
        for field in self.ctx.structs.get(key).fields.keys() {
            let size_t = self.ctx.ptr();
            let offset = self.ctx.structs.offset_of(key, field);

            match self.ctx.structs.get(key).fields[field].clone() {
                abi::FieldV::Flat(fty) => match self.deref_scalar_or_getm(ptr, offset, &fty) {
                    Ok(scalar) => for_each(scalar),
                    Err(field_mk) => {
                        // Create a pointer to the inner struct
                        let innerp = self.ins().iadd_imm(ptr, offset.0 as i64);

                        // Then dereference fields from that inner struct
                        assert_eq!(self.ctx.structs.pass_mode(field_mk), abi::PassBy::Value);
                        self.deref_struct_into_raw(for_each, field_mk, innerp);
                    }
                },

                // Since we're reading this from a pointer, we know that the sum pointer is S_Stable.
                abi::FieldV::SumPayloadPointer { sum } => {
                    let largest = self.ctx.structs.sum_payload_alloca_size(sum);
                    let point = self.deref_scalar(ptr, offset, size_t);
                    let scalar =
                        abi::Scalar { point, kind: abi::ScalarKind::HeapSumPointer { largest } };
                    for_each(scalar);
                }
                abi::FieldV::SumPayloadInline(clty) => {
                    let point = self.deref_scalar(ptr, offset, clty);
                    let scalar = abi::Scalar { point, kind: abi::ScalarKind::SumInline(clty) };
                    for_each(scalar);
                }

                abi::FieldV::AutoBoxed(to) => {
                    let point = self.deref_scalar(ptr, offset, size_t);
                    let scalar = abi::Scalar::pointer(point, &to.into());
                    for_each(scalar);
                }
            }
        }
    }

    fn cast_value_to_scalar(&mut self, ty: &MonoType, point: Value) -> VEntry {
        match ty {
            MonoType::Int(intsize) => {
                let int = Type::int(intsize.bits() as u16).unwrap();
                VEntry::direct(self.resize_uint(point, int))
            }
            MonoType::Pointer(ty) => VEntry::Scalar(abi::Scalar::pointer(point, &ty)),
            MonoType::FnPointer(params, ret) => {
                let typing = self.ctx.structs.records.get_abi_typing(params.iter(), ret);

                VEntry::Scalar(abi::Scalar::fn_pointer(point, typing))
            }
            &MonoType::Monomorphised(key) => {
                let struct_ = self.ctx.structs.get(key);

                let mut inputs = vec![point];
                let mut outputs: Vec<Value> = vec![];

                let current = &mut self.f;

                // Call iconcat and isplit to construct correctly sized fields from the raw input
                self.ctx.structs.iter_s_stable_fields(key, &mut |scalar| {
                    let psize = current.type_of_value(point).bytes();
                    let size = scalar.point.bytes();

                    loop {
                        match psize.cmp(&size) {
                            Ordering::Equal => {
                                let matching = inputs.pop().unwrap();
                                outputs.push(matching);
                                break;
                            }
                            Ordering::Less => {
                                let r = inputs.pop().unwrap();
                                let l = inputs.pop().unwrap();
                                let bigger = current.builder.ins().iconcat(l, r);
                                inputs.push(bigger);
                            }
                            Ordering::Greater => {
                                let (l, r) = current.builder.ins().isplit(point);
                                inputs.push(r);
                                inputs.push(l);
                            }
                        }
                    }
                });

                let mut slice = outputs.as_slice();
                let fields = struct_
                    .fields
                    .values()
                    .map(|f| self.abi_field_to_ventry(f, &mut slice))
                    .collect();

                VEntry::StructFlat(key, fields)
            }
            MonoType::Float => {
                panic!("illegal transmute into float. use proper conversion method instead")
            }
            _ => panic!("illegal transmute of two differently sized types"),
        }
    }

    // S_Stable write of entry to pointer
    fn write_entry_to_ptr(&mut self, dst: Value, entry: &VEntry) {
        match entry {
            &VEntry::StructStackPointer(key, src) | &VEntry::StructHeapPointer(key, src) => {
                let inner_struct_size = self.ctx.structs.size_of(&key.into()) as i64;
                self.memcpy_struct(dst, src, inner_struct_size);
            }
            &VEntry::SumPayloadStackPointer { largest, ptr } => {
                let nptr = self.heap_alloc(largest as i128);
                self.memcpy_struct(nptr, ptr, largest as i64);
                self.ins().store(MemFlags::trusted(), nptr, dst, 0);
            }
            VEntry::StructFlat(key, flat) => self.write_fields_to_structptr(*key, &flat, dst),
            VEntry::Scalar(scalar) => {
                self.ins().store(MemFlags::trusted(), scalar.point, dst, 0);
            }

            VEntry::ZST => {}
        }
    }

    fn memcpy_struct(&mut self, dst: Value, src: Value, size: i64) {
        let size_t = self.ctx.ptr();

        let size = self.ins().iconst(size_t, size);
        let config = self.ctx.isa.frontend_config();
        self.f.builder.call_memcpy(config, dst, src, size);
    }

    fn heap_alloc(&mut self, size: i128) -> Value {
        let alloc = self.ctx.lir.alloc;
        let fheader = self.ctx.funcmap[alloc].clone();

        let size = lir::Value::Int(size, IntSize::new(true, self.ctx.ptr().bits() as u8));

        let entry = self.call_func(fheader.id, fheader.typing, &[size]);

        match entry {
            VEntry::Scalar(abi::Scalar { kind: abi::ScalarKind::Pointer(_), point }) => point,
            _ => panic!("alloc has unexpected signature: {entry:?}"),
        }
    }

    fn heap_dealloc(&mut self, ptr: lir::Value, size: i128) {
        let dealloc = self.ctx.lir.dealloc;
        let fheader = self.ctx.funcmap[dealloc].clone();
        let triple = self.ctx.isa.triple();

        let size = lir::Value::Int(
            size,
            IntSize::new(true, triple.pointer_width().unwrap().bits()),
        );

        let entry = self.call_func(fheader.id, fheader.typing, &[ptr, size]);

        match entry {
            VEntry::ZST => {}
            _ => panic!("dealloc has unexpected signature"),
        }
    }

    fn value_to_entry(&mut self, value: lir::Value) -> VEntry {
        match value {
            lir::Value::V(v) => self.f.vmap[v].clone(),
            lir::Value::ReadOnly(ronly) => {
                let dataid = self.ctx.rotable[ronly];
                let data = self
                    .ctx
                    .objmodule
                    .declare_data_in_func(dataid, &mut self.f.builder.func);

                let size_t = self.ctx.ptr();
                let ptr = self.ins().symbol_value(size_t, data);

                let ty = self.ctx.lir.read_only_table[ronly].1.clone();
                VEntry::Scalar(abi::Scalar::pointer(ptr, &ty))
            }
            lir::Value::FuncPtr(mfunc) => {
                let header = self.ctx.funcmap[mfunc].clone();
                let fref = self.declare_func_in_func(header.id);
                let ptr_type = self.ctx.ptr();
                let fptr = self.ins().func_addr(ptr_type, fref);

                VEntry::Scalar(abi::Scalar::fn_pointer(fptr, header.typing))
            }
            lir::Value::ExternFuncPtr(key) => {
                let fheader = self.ctx.externmap[&key].clone();
                let fref = self.declare_func_in_func(fheader.id);
                let ptr_type = self.ctx.ptr();
                let fptr = self.ins().func_addr(ptr_type, fref);
                VEntry::Scalar(abi::Scalar::fn_pointer(fptr, fheader.typing))
            }
            lir::Value::Int(n, bitsize) => {
                let ty = Type::int(bitsize.bits() as u16).unwrap();
                VEntry::direct(self.ins().iconst(ty, n as i64))
            }
            lir::Value::Float(_) => todo!(),
        }
    }

    fn deref_type(&mut self, ptr: Value, offset: ByteOffset, ty: &MonoType) -> VEntry {
        match self.deref_scalar_or_getm(ptr, offset, ty) {
            Ok(scalar) => VEntry::Scalar(scalar),
            Err(mk) => {
                // Create a pointer to the inner struct
                let innerp = if offset == ByteOffset(0) {
                    ptr
                } else {
                    self.ins().iadd_imm(ptr, offset.0 as i64)
                };

                match self.ctx.structs.pass_mode(mk) {
                    abi::PassBy::Pointer => VEntry::StructStackPointer(mk, innerp),
                    abi::PassBy::Value => {
                        // Let's try just not dereferencing it here and let it happen lazily
                        VEntry::StructStackPointer(mk, innerp)

                        // let buf = self.deref_struct_into_flat(mk, innerp);
                        // VEntry::StructFlat(mk, buf)
                    }
                }
            }
        }
    }

    fn deref_scalar_or_getm(
        &mut self,
        ptr: Value,
        offset: ByteOffset,
        ty: &MonoType,
    ) -> Result<abi::Scalar<Value>, MonoTypeKey> {
        let size_t = Type::int(self.ctx.structs.records.pointer_bits as u16).unwrap();

        match ty {
            MonoType::Int(intsize) => {
                let v = self.deref_scalar(ptr, offset, Type::int(intsize.bits() as u16).unwrap());
                Ok(abi::Scalar::direct(v))
            }
            MonoType::Pointer(inner) => {
                let v = self.deref_scalar(ptr, offset, size_t);
                Ok(abi::Scalar::pointer(v, &**inner))
            }
            MonoType::FnPointer(params, ret) => {
                let v = self.deref_scalar(ptr, offset, size_t);
                let typing = self.ctx.structs.records.get_abi_typing(params, ret);
                Ok(abi::Scalar::fn_pointer(v, typing))
            }
            MonoType::Float => {
                let v = self.deref_scalar(ptr, offset, types::F64);
                Ok(abi::Scalar::direct(v))
            }
            MonoType::Unreachable => unreachable!(),
            MonoType::Monomorphised(mk) => Err(*mk),
        }
    }

    fn deref_scalar(&mut self, ptr: Value, offset: ByteOffset, ty: Type) -> Value {
        self.ins()
            .load(ty, MemFlags::trusted(), ptr, offset.0 as i32)
    }

    fn block_params(&mut self, block: lir::Block, params: &[lir::Value]) -> Vec<Value> {
        let mut buf = Vec::with_capacity(params.len());
        for (p, ty) in params.iter().zip(self.f.func.blocks.param_types(block)) {
            let abi = self.ctx.structs.records.abi_param(ty);
            let entry = self.value_to_entry(*p);
            self.entry_to_f_stable(&abi, entry, &mut buf)
        }
        buf
    }

    fn flow(&mut self, flow: &lir::ControlFlow) {
        match flow {
            lir::ControlFlow::JmpFunc(mfunc, params) => {
                let fheader = self.ctx.funcmap[*mfunc].clone();
                // TODO: tail calls
                let entry = self.call_func(fheader.id, fheader.typing, params);
                self.return_(entry);
            }
            lir::ControlFlow::JmpBlock(block, params) => {
                let params = self.block_params(*block, params);

                let clblock = self.f.blockmap[*block].0;
                self.ins().jump(clblock, &params);

                self.lower_block_if_last_predecessor(*block);
            }
            lir::ControlFlow::Unreachable => {
                self.ins().trap(TrapCode::UnreachableCodeReached);
            }
            lir::ControlFlow::Return(v) => {
                let entry = self.value_to_entry(*v);
                self.return_(entry);
            }
            lir::ControlFlow::Select { value, on_true, on_false } => {
                let int = self.value_to_entry(*value).as_direct();

                let then_params = self.block_params(on_true.0, &on_true.1);
                let else_params = self.block_params(on_false.0, &on_false.1);

                let [block_then, block_else] =
                    [on_true.0, on_false.0].map(|block| self.f.blockmap[block].0);

                self.ins()
                    .brif(int, block_then, &then_params, block_else, &else_params);

                for block in [on_true.0, on_false.0] {
                    self.lower_block_if_last_predecessor(block);
                }
            }
            lir::ControlFlow::JmpTable(of, against, blocks) => {
                let indice = self.value_to_entry(*of).as_direct();

                let def = self.f.builder.create_block();

                let pool = &mut self.f.builder.func.dfg.value_lists;

                let table: Vec<ir::BlockCall> = blocks
                    .iter()
                    .map(|block| ir::BlockCall::new(self.f.blockmap[*block].0, &[], pool))
                    .collect();

                let defcall = ir::BlockCall::new(def, &[], pool);

                let jump_data = JumpTableData::new(defcall, &table);
                let table = self.f.builder.create_jump_table(jump_data);

                if !against.is_empty() {
                    unimplemented!("block params to branchs of jumpt able");
                }

                let indice = self.resize_uint(indice, types::I32);

                self.ins().br_table(indice, table);

                self.f.builder.seal_block(def);
                self.f.builder.switch_to_block(def);
                self.ins().trap(TrapCode::UnreachableCodeReached);

                for block in blocks {
                    self.lower_block_if_last_predecessor(*block);
                }
            }
            lir::ControlFlow::Empty => panic!("missing control flow in LIR block"),
        }
    }

    fn resize_uint(&mut self, n: Value, to: Type) -> Value {
        let has = self.f.type_of_value(n).bytes();

        match has.cmp(&to.bytes()) {
            std::cmp::Ordering::Equal => n,
            std::cmp::Ordering::Less => self.ins().uextend(to, n),
            std::cmp::Ordering::Greater => self.ins().ireduce(to, n),
        }
    }

    fn write_fields_to_structptr(
        &mut self,
        key: MonoTypeKey,
        fields: &Map<abi::Field, VEntry>,
        ptr: Value,
    ) {
        for (field, entry) in fields.iter() {
            let offset = self.ctx.structs.offset_of(key, field);
            let ptr = if offset != ByteOffset(0) {
                self.ins().iadd_imm(ptr, offset.0 as i64)
            } else {
                ptr
            };
            self.write_entry_to_ptr(ptr, entry);
        }
    }

    fn struct_to_fstable(
        &mut self,
        key: MonoTypeKey,
        fields: &Map<abi::Field, VEntry>,
        dst: &mut Vec<Value>,
    ) {
        match self.ctx.structs.pass_mode(key) {
            abi::PassBy::Pointer => {
                let (size, align) = self.ctx.structs.size_and_align_of(&key.into());
                let (slot, _) = self.create_struct_stack_slot(size, align as u8);
                let size_t = self.ctx.ptr();
                let ptr = self.ins().stack_addr(size_t, slot, 0);
                self.write_fields_to_structptr(key, fields, ptr);
                dst.push(ptr);
            }
            abi::PassBy::Value => self.fields_to_fstable(fields, dst),
        }
    }

    fn fields_to_fstable(&mut self, fields: &Map<abi::Field, VEntry>, dst: &mut Vec<Value>) {
        for (_, entry) in fields {
            match entry {
                // All scalars are assumed to be S_Stable
                VEntry::Scalar(scalar) => dst.push(scalar.point),
                VEntry::ZST => {}

                // We don't call struct_to_fstable because we want to embed regardless of the
                // passing mode of the child struct.
                VEntry::StructFlat(_, inner_fields) => self.fields_to_fstable(inner_fields, dst),

                // A `Struct` which we've implicitly been passing around as a pointer.
                &VEntry::StructHeapPointer(inner, ptr)
                | &VEntry::StructStackPointer(inner, ptr) => {
                    match self.ctx.structs.pass_mode(inner) {
                        abi::PassBy::Pointer => dst.push(ptr),
                        abi::PassBy::Value => {
                            self.deref_struct_into_raw(
                                &mut |scalar| dst.push(scalar.point),
                                inner,
                                ptr,
                            );
                        }
                    }
                }

                // Here we make the assumption that no sum-types which take inlined payloads will ever
                // be constructed with a pointer payload.
                VEntry::SumPayloadStackPointer { ptr, .. } => dst.push(*ptr),
            }
        }
    }

    fn fields_to_sstable(&mut self, fields: &Map<abi::Field, VEntry>, dst: &mut Vec<Value>) {
        for (_, entry) in fields {
            match entry {
                // All scalars are assumed to be S_Stable
                VEntry::Scalar(scalar) => dst.push(scalar.point),
                VEntry::ZST => {}

                VEntry::StructFlat(_, inner_fields) => {
                    self.fields_to_sstable(inner_fields, dst);
                }

                // A `Struct` which we've implicitly been passing around as a pointer.
                &VEntry::StructHeapPointer(inner, ptr)
                | &VEntry::StructStackPointer(inner, ptr) => {
                    self.deref_struct_into_raw(&mut |scalar| dst.push(scalar.point), inner, ptr);
                }

                &VEntry::SumPayloadStackPointer { largest, ptr } => {
                    let nptr = self.heap_alloc(largest as i128);
                    self.memcpy_struct(nptr, ptr, largest as i64);
                    dst.push(nptr);
                }
            }
        }
    }

    fn return_(&mut self, entry: VEntry) {
        let ret = self
            .ctx
            .structs
            .records
            .get_abi_return(&self.f.func.returns);

        match ret {
            abi::Return::Struct(mk) => match self.ctx.structs.pass_mode(mk) {
                abi::PassBy::Pointer => {
                    let dst = self
                        .f
                        .builder
                        .func
                        .special_param(ir::ArgumentPurpose::StructReturn)
                        .unwrap();

                    self.write_entry_to_ptr(dst, &entry);
                    self.ins().return_(&[]);
                }
                abi::PassBy::Value => match entry {
                    VEntry::StructHeapPointer(key, ptr) | VEntry::StructStackPointer(key, ptr) => {
                        assert_eq!(key, mk);
                        let mut buf = vec![];
                        self.deref_struct_into_raw(&mut |scalar| buf.push(scalar.point), mk, ptr);
                        self.ins().return_(&buf);
                    }
                    VEntry::StructFlat(mk_, values) if mk == mk_ => {
                        let mut flat = Vec::with_capacity(values.len());
                        self.fields_to_sstable(&values, &mut flat);
                        self.ins().return_(&flat);
                    }
                    _ => unreachable!(),
                },
            },
            abi::Return::Scalar(tscalar) => match entry {
                VEntry::Scalar(scalar) if scalar.kind == tscalar.kind => {
                    self.ins().return_(&[scalar.point]);
                }
                other => panic!("return abi mismatch: {other:?} for {tscalar:?}"),
            },
            abi::Return::ZST => {
                self.ins().return_(&[]);
            }
        }
    }

    fn params(&mut self, params: &Map<key::Param, abi::Param>, src: &[lir::Value]) -> Vec<Value> {
        let mut buf = Vec::with_capacity(src.len());
        src.iter().zip(params.iter()).for_each(|(p, (i, abi))| {
            let entry = self.value_to_entry(*p);
            trace!("{i} as {entry:?}");
            self.entry_to_f_stable(abi, entry, &mut buf)
        });
        buf
    }

    fn entry_to_f_stable(&mut self, abi: &abi::Param, entry: VEntry, dst: &mut Vec<Value>) {
        match entry {
            VEntry::StructHeapPointer(mk, ptr) | VEntry::StructStackPointer(mk, ptr) => match abi {
                abi::Param::Struct(abi_mk) => {
                    assert_eq!(*abi_mk, mk);

                    match &self.ctx.structs.pass_mode(mk) {
                        abi::PassBy::Pointer => {
                            dst.push(ptr);
                        }
                        abi::PassBy::Value => {
                            self.deref_struct_into_raw(
                                &mut |scalar| dst.push(scalar.point),
                                mk,
                                ptr,
                            );
                        }
                    }
                }
                _ => panic!("struct pointer to non-struct abi"),
            },
            VEntry::SumPayloadStackPointer { ptr, .. } => {
                dst.push(ptr);
            }
            VEntry::StructFlat(key, flat) => self.struct_to_fstable(key, &flat, dst),
            VEntry::Scalar(v) => match abi {
                abi::Param::Scalar(_) => dst.push(v.point),
                _ => {
                    println!(
                        "{}",
                        lir::ty_fmt(
                            self.ctx.structs.records,
                            &self.ctx.lir.functions[MonoFunc::from(33)]
                        )
                    );
                    println!("{}", lir::ty_fmt(self.ctx.structs.records, self.f.func));
                    panic!("scalar value {v:?} to non-scalar abi: {abi:?}")
                }
            },
            VEntry::ZST => {
                assert_eq!(abi::Param::ZST, *abi, "zst to non-zst abi");
            }
        }
    }

    fn checked_numop<S, U, N>(&mut self, ty: &MonoType, signed: S, unsigned: U, simple: N) -> VEntry
    where
        S: FnOnce(&mut Self) -> (Value, Value),
        U: FnOnce(&mut Self) -> (Value, Value),
        N: FnOnce(&mut Self) -> Value,
    {
        let with_check = match ty {
            MonoType::Monomorphised(mk) => {
                let [int, bool_] = [0, 1]
                    .map(key::Field)
                    .map(|field| self.types()[*mk].as_record()[field].clone());

                assert_eq!(bool_, MonoType::bool());
                Either::Left((*mk, as_int(&int).signed))
            }
            MonoType::Int(_) => Either::Right(abi::ScalarKind::Direct),
            MonoType::Pointer(key) => Either::Right(abi::ScalarKind::Pointer((**key).clone())),
            _ => panic!("invalid return signature for num binop: {ty:?}"),
        };

        match with_check {
            Either::Left((mk, true)) => {
                let (n, c) = signed(self);
                let fields = [n, c].into_iter().map(VEntry::direct).collect();
                VEntry::StructFlat(mk, fields)
            }
            Either::Left((mk, false)) => {
                let (n, c) = unsigned(self);
                let fields = [n, c].into_iter().map(VEntry::direct).collect();
                VEntry::StructFlat(mk, fields)
            }
            Either::Right(kind) => VEntry::Scalar(abi::Scalar { point: simple(self), kind }),
        }
    }
}

fn next_value(values: &mut &[Value]) -> Value {
    let (x, xs) = values.split_first().unwrap();
    *values = xs;
    *x
}

fn as_int(ty: &MonoType) -> IntSize {
    match ty {
        MonoType::Int(bits) => *bits,
        _ => unreachable!(),
    }
}
