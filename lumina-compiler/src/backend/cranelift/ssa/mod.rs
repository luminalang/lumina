use super::{
    layout,
    layout::{ByteOffset, FType, FieldV, PassBy, Scalar, ScalarKind},
    Context,
};
use crate::prelude::*;
use cranelift::frontend::FuncInstBuilder;
use cranelift::prelude::*;
use cranelift_codegen::ir;
use cranelift_module::{DataId, FuncId, Module};
use either::Either;
use lir::{MonoFunc, MonoType, MonoTypeKey};
use lumina_typesystem::IntSize;
use lumina_util::Highlighting;
use std::cmp::Ordering;

mod array;
mod call;
mod num;
mod pointer;
mod record;
mod sum;

#[derive(new)]
pub struct Translator<'c, 'a, 'f> {
    ctx: &'c mut Context<'a>,
    f: Current<'a, 'f>,
}

pub(super) type Predecessors = u16;

#[derive(new)]
struct Current<'a, 'f> {
    func: &'a lir::Function,
    builder: FunctionBuilder<'f>,
    blockmap: Map<lir::Block, (Block, Predecessors)>,
    #[new(default)]
    imports: HashMap<FuncId, ir::FuncRef>,
    #[new(default)]
    vmap: Map<lir::V, VEntry>,
    #[new(value = "lir::Block::entry()")]
    block: lir::Block,
}

/// During the lower from LIR to CLIR, we have three categories of values.
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
    Scalar(Scalar<Value>),
    ZST,

    // F_Stable
    StructStackPointer(MonoTypeKey, Value), // Struct by-value which is implicitly passed as a pointer.
    ArrayStackPointer(MonoType, u64, Value),
    SumPayloadStackPointer { largest: u32, ptr: Value },

    // Unstable
    StructFlat(MonoTypeKey, Map<layout::Field, VEntry>),
    ArrayFlat(MonoType, Vec<VEntry>),
}

impl VEntry {
    fn direct(point: Value) -> Self {
        VEntry::Scalar(Scalar { kind: ScalarKind::Direct, point })
    }

    #[track_caller]
    fn as_direct(&self) -> Value {
        match self {
            VEntry::Scalar(Scalar { kind: ScalarKind::Direct, point }) => *point,
            other => panic!("as_direct called on non-direct ventry: {other:?}"),
        }
    }

    #[track_caller]
    fn as_intable(&self) -> (Value, ScalarKind) {
        match self {
            VEntry::Scalar(Scalar {
                kind: kind @ (ScalarKind::Direct | ScalarKind::Pointer(_)),
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

        let header = &ctx.funcmap[key];

        clfunc.signature = ctx.structs.signature(&header.typing);
        let mut builder = FunctionBuilder::new(&mut clfunc, &mut func_builder_ctx);

        let blockmap = func
            .blocks
            .blocks()
            .map(|block| ctx.declare_block_from_lir(&mut builder, func, block))
            .collect();

        Translator { ctx, f: Current::new(func, builder, blockmap) }.lower_and_finalize_current();

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

    fn block(&mut self) {
        let mut raw_params = self.f.builder.block_params(self.f.blockmap[self.f.block].0);
        let raw_params_ref: &mut &[Value] = &mut raw_params;

        // Map the block parameters to ventries
        let pvalues = self.f.func.blocks.params(self.f.block);
        let ptypes = self.f.func.blocks.param_types(self.f.block);
        for (v, ty) in pvalues.zip(ptypes) {
            let ventry = self.fparam_to_ventry(false, ty, raw_params_ref);
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
            self.f.vmap.push_as(v, entry);
        }

        let flow = self.f.func.blocks.flow_of(self.f.block);
        trace!("lowering flow {flow}");
        self.flow(flow)
    }

    fn entry(&mut self, entry: &lir::Entry, ty: &MonoType) -> VEntry {
        match entry {
            lir::Entry::Copy(value) => self.value_to_entry(*value),
            lir::Entry::BlockParam(v) => self.value_to_entry(lir::Value::V(*v)),
            lir::Entry::Transmute(value) => self.transmute(*value, ty),
            lir::Entry::SizeOf(ty) => self.size_of(ty),
            lir::Entry::AlignOf(ty) => self.align_of(ty),

            lir::Entry::CallStatic(mfunc, params) => {
                let fheader = &self.ctx.funcmap[*mfunc];
                self.call_func(fheader.id, fheader.typing.clone(), params)
            }
            lir::Entry::CallExtern(key, params) => {
                let fheader = &self.ctx.externmap[key];
                self.call_func(fheader.id, fheader.typing.clone(), params)
            }
            lir::Entry::CallValue(ptr, params) => {
                let entry = self.value_to_entry(*ptr);
                self.call_entry(entry, params)
            }

            lir::Entry::RefStaticVal(val) => self.ref_static_val(*val, ty),

            lir::Entry::Construct(values) => match ty {
                MonoType::Monomorphised(mk) => self.construct_record(*mk, values),
                MonoType::Array(len, inner) => {
                    assert_eq!(values.len() as u64, *len);
                    self.construct_array(values, inner)
                }
                _ => panic!("cannot construct: {ty:?}"),
            },
            lir::Entry::Replicate(value, times) => match ty {
                MonoType::Monomorphised(_) => todo!("replicating to construct struct/tuple"),
                MonoType::Array(len, inner) => {
                    assert_eq!(*times, *len);
                    self.replicate_array(*value, inner, *times)
                }
                _ => panic!("cannot replicate into: {ty:?}"),
            },
            lir::Entry::Field { of, field, .. } => {
                let entry = self.value_to_entry(*of);
                self.field_of_entry(entry, *field)
            }
            lir::Entry::Indice { of, indice } => {
                let entry = self.value_to_entry(*of);
                let indice = self.value_to_entry(*indice).as_direct();
                self.indice_of_entry(entry, indice)
            }

            lir::Entry::Variant(var, values) => match ty {
                MonoType::Monomorphised(mk) => self.construct_variant(*mk, *var, values),
                _ => panic!("cannot construct: {ty:?}"),
            },
            lir::Entry::TagFromSum { of } => self.tag_of_sum(*of),
            lir::Entry::CastFromSum { of } => {
                let entry = self.value_to_entry(*of);
                self.cast_from_sum(entry, ty)
            }

            lir::Entry::IntAdd(left, right) => self.ibinary(
                ty,
                [*left, *right],
                InstBuilder::sadd_overflow,
                InstBuilder::uadd_overflow,
                InstBuilder::iadd,
            ),
            lir::Entry::IntSub(left, right) => self.ibinary(
                ty,
                [*left, *right],
                InstBuilder::ssub_overflow,
                InstBuilder::usub_overflow,
                InstBuilder::isub,
            ),
            lir::Entry::IntMul(left, right) => self.ibinary(
                ty,
                [*left, *right],
                InstBuilder::smul_overflow,
                InstBuilder::umul_overflow,
                InstBuilder::imul,
            ),
            lir::Entry::IntDiv(left, right) => self.int_div([*left, *right], as_int(ty)),
            lir::Entry::IntAbs(v) => self.iunary(*v, as_int(ty), |ins, _, v| ins.iabs(v)),

            lir::Entry::IntCmpInclusive(left, cmp, right, bitsize) => {
                self.int_cmpi([*left, *right], *cmp, *bitsize)
            }

            lir::Entry::Reduce(v) => self.iunary(*v, as_int(ty), InstBuilder::ireduce),
            lir::Entry::ExtendSigned(v) => self.iunary(*v, as_int(ty), InstBuilder::sextend),
            lir::Entry::ExtendUnsigned(v) => self.iunary(*v, as_int(ty), InstBuilder::uextend),

            lir::Entry::FloatToInt(v, intsize) => self.float_to_int(*v, *intsize),
            lir::Entry::IntToFloat(v, intsize) => self.int_to_float(*v, *intsize),

            lir::Entry::BitAnd(ints) => self.bit_and(*ints),
            lir::Entry::BitNot(v) => self.bit_not(*v),

            lir::Entry::Alloc => self.alloc(ty),
            lir::Entry::Alloca => self.alloca(ty),
            lir::Entry::Dealloc { ptr } => {
                self.heap_dealloc(*ptr, 0);
                VEntry::ZST
            }
            lir::Entry::WritePtr { ptr, value } => {
                self.write_ptr(*ptr, *value);
                VEntry::ZST
            }
            lir::Entry::Deref(ptr) => self.deref_ptr(*ptr, ty),
        }
    }

    fn value_to_entry(&mut self, value: lir::Value) -> VEntry {
        match value {
            lir::Value::V(v) => self.f.vmap[v].clone(),
            lir::Value::ReadOnly(ronly) => {
                let dataid = self.ctx.rotable[ronly];
                let ptr = self.dataid_as_pointer(dataid);

                let ty = self.ctx.lir.read_only_table[ronly].1.clone();
                VEntry::Scalar(Scalar::pointer(ptr, &ty))
            }
            lir::Value::FuncPtr(mfunc) => {
                let header = self.ctx.funcmap[mfunc].clone();
                let fref = self.declare_func_in_func(header.id);
                let ptr_type = self.ctx.size_t();
                let fptr = self.ins().func_addr(ptr_type, fref);

                VEntry::Scalar(Scalar::fn_pointer(fptr, header.typing))
            }
            lir::Value::ExternFuncPtr(key) => {
                let fheader = self.ctx.externmap[&key].clone();
                let fref = self.declare_func_in_func(fheader.id);
                let ptr_type = self.ctx.size_t();
                let fptr = self.ins().func_addr(ptr_type, fref);
                VEntry::Scalar(Scalar::fn_pointer(fptr, fheader.typing))
            }
            lir::Value::Int(n, bitsize) => {
                let ty = Type::int(bitsize.bits() as u16).unwrap();
                VEntry::direct(self.ins().iconst(ty, n as i64))
            }
            lir::Value::Float(_) => todo!(),
        }
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
                let params = self.params(params);

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

                let then_params = self.params(&on_true.1);
                let else_params = self.params(&on_false.1);

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

    fn return_(&mut self, entry: VEntry) {
        let ret = self.ctx.structs.ftype(&self.f.func.returns);

        match ret {
            FType::ArrayPointer(..) | FType::Struct(PassBy::Pointer, _) => {
                let dst = self
                    .f
                    .builder
                    .func
                    .special_param(ir::ArgumentPurpose::StructReturn)
                    .unwrap();

                self.write_entry_to_ptr(dst, &entry);
                self.ins().return_(&[]);
            }
            FType::Struct(PassBy::Value, mk) => match entry {
                VEntry::StructHeapPointer(key, ptr) | VEntry::StructStackPointer(key, ptr) => {
                    assert_eq!(key, mk);
                    let mut buf = vec![];
                    self.deref_struct_into_raw(&mut |point| buf.push(point), mk, ptr);
                    self.ins().return_(&buf);
                }
                VEntry::StructFlat(mk_, values) if mk == mk_ => {
                    let mut flat = Vec::with_capacity(values.len());
                    self.fields_to_sstable(values.as_slice(), &mut flat);
                    self.ins().return_(&flat);
                }
                _ => unreachable!(),
            },
            FType::StructZST => {
                self.ins().return_(&[]);
            }
            FType::Scalar(tscalar) => match entry {
                VEntry::Scalar(scalar) if scalar.kind == tscalar.kind => {
                    self.ins().return_(&[scalar.point]);
                }
                other => panic!("return abi mismatch: {other:?} for {tscalar:?}"),
            },
        }
    }

    fn fparam_to_ventry(&self, embed: bool, ty: &MonoType, input: &mut &[Value]) -> VEntry {
        match self.ctx.structs.ftype(ty) {
            FType::Scalar(scalar) => {
                let point = next_value(input);
                VEntry::Scalar(Scalar { kind: scalar.kind.clone(), point })
            }
            FType::StructZST => VEntry::ZST,
            FType::Struct(PassBy::Value, mk) => self.fparam_fields_to_ventry(mk, input),
            FType::Struct(PassBy::Pointer, mk) if embed => self.fparam_fields_to_ventry(mk, input),
            FType::Struct(PassBy::Pointer, mk) => {
                let ptr = next_value(input);
                VEntry::StructStackPointer(mk, ptr)
            }
            FType::ArrayPointer(len, inner) if embed => {
                self.fparam_elems_to_ventry(len, inner, input)
            }
            FType::ArrayPointer(len, inner) => {
                let ptr = next_value(input);
                VEntry::ArrayStackPointer(inner, len, ptr)
            }
        }
    }

    fn fparam_fields_to_ventry(&self, mk: MonoTypeKey, input: &mut &[Value]) -> VEntry {
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

    fn fparam_elems_to_ventry(&self, len: u64, inner: MonoType, input: &mut &[Value]) -> VEntry {
        let buf = (0..len)
            .map(|_| self.fparam_to_ventry(true, &inner, input))
            .collect();

        VEntry::ArrayFlat(inner, buf)
    }

    fn abi_field_to_ventry(&self, field: &FieldV, input: &mut &[Value]) -> VEntry {
        match field {
            FieldV::Flat(inner) => self.fparam_to_ventry(true, inner, input),
            &FieldV::AutoBoxed(mk) => {
                let ptr = next_value(input);
                VEntry::StructHeapPointer(mk, ptr)
            }
            FieldV::SumPayloadPointer { sum } => {
                let ptr = next_value(input);
                let largest = self.ctx.structs.sum_payload_alloca_size(*sum);
                VEntry::SumPayloadStackPointer { largest, ptr }
            }
            &FieldV::SumPayloadInline(clty) => {
                let inline = next_value(input);
                VEntry::Scalar(Scalar::new(inline, ScalarKind::SumInline(clty)))
            }
        }
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
        ret: &MonoType,
        slot: Option<(ir::StackSlot, Value)>,
    ) -> VEntry {
        let mut rvalues = self.f.builder.inst_results(call);

        match self.ctx.structs.ftype(ret) {
            FType::Scalar(scalar) => {
                assert_eq!(rvalues.len(), 1);
                VEntry::Scalar(Scalar::new(rvalues[0], scalar.kind))
            }
            FType::StructZST => VEntry::ZST,
            FType::Struct(PassBy::Value, mk) => self.fparam_fields_to_ventry(mk, &mut rvalues),
            FType::Struct(PassBy::Pointer, mk) => {
                let ptr = slot.unwrap().1;
                VEntry::StructStackPointer(mk, ptr)
            }
            FType::ArrayPointer(len, inner) => {
                let ptr = slot.unwrap().1;
                VEntry::ArrayStackPointer(inner, len, ptr)
            }
        }
    }

    fn create_struct_stack_slot(&mut self, size: u32, align: u8) -> ir::StackSlot {
        let slotdata = StackSlotData::new(StackSlotKind::ExplicitSlot, size, align as u8);
        self.f.builder.create_sized_stack_slot(slotdata)
    }

    fn append_rptr_if_needed(
        &mut self,
        ret: &MonoType,
        params: &mut Vec<Value>,
    ) -> Option<(ir::StackSlot, Value)> {
        match self.ctx.structs.ftype(ret) {
            FType::Struct(PassBy::Pointer, mk) => {
                let size_t = self.ctx.size_t();
                let (size, align) = self.ctx.structs.size_and_align_of(&mk.into());
                let slot = self.create_struct_stack_slot(size, align as u8);
                let ptr = self.ins().stack_addr(size_t, slot, 0);
                params.push(ptr);
                Some((slot, ptr))
            }
            FType::ArrayPointer(len, inner) => {
                let size_t = self.ctx.size_t();
                let (size, _, align) = self.ctx.structs.size_and_align_of_array(&inner, len);
                let slot = self.create_struct_stack_slot(size, align as u8);
                let ptr = self.ins().stack_addr(size_t, slot, 0);
                params.push(ptr);
                Some((slot, ptr))
            }
            _ => None,
        }
    }

    #[track_caller]
    fn lower_block_if_last_predecessor(&mut self, block: lir::Block) {
        let (clblock, predecessors) = &mut self.f.blockmap[block];
        *predecessors += 1;

        debug_assert!(*predecessors <= self.f.func.blocks.predecessors(block));

        if *predecessors == self.f.func.blocks.predecessors(block) {
            self.f.builder.seal_block(*clblock);
            self.f.switch_to_block(block);
            self.block();
        }
    }

    fn transmute(&mut self, v: lir::Value, ty: &MonoType) -> VEntry {
        match self.value_to_entry(v) {
            VEntry::ArrayFlat(..)
            | VEntry::ArrayStackPointer(..)
            | VEntry::StructFlat(_, _)
            | VEntry::StructStackPointer(_, _)
            | VEntry::StructHeapPointer(_, _) => {
                unimplemented!("transmuting structs or arrays");
            }
            VEntry::Scalar(scalar) => self.cast_value_from_scalar(ty, scalar.point),
            VEntry::SumPayloadStackPointer { ptr, .. } => self.cast_value_from_scalar(ty, ptr),
            VEntry::ZST => VEntry::ZST,
        }
    }

    fn tag_of_sum(&mut self, v: lir::Value) -> VEntry {
        match self.value_to_entry(v) {
            VEntry::StructFlat(key, scalars) => {
                let i = self.ctx.structs.get(key).field_map[key::Field(0)];
                match scalars[i].clone() {
                    VEntry::Scalar(scalar) => {
                        assert_eq!(scalar.kind, ScalarKind::Direct);
                        VEntry::Scalar(scalar)
                    }
                    other => panic!("unexpected entry kind for tag: {other:?}"),
                }
            }
            VEntry::StructStackPointer(key, ptr) => {
                let (tagsize, _) = self.ctx.structs.records[key].as_sum();
                let tagt = Type::int(tagsize.bits() as u16).unwrap();

                let size_t = self.ctx.size_t().bytes();
                let v = self.deref_scalar(ptr, ByteOffset(size_t), tagt);
                VEntry::direct(v)
            }
            other => unreachable!("{other:?}"),
        }
    }

    fn ref_static_val(&mut self, val: M<key::Val>, ty: &MonoType) -> VEntry {
        let MonoType::Pointer(ty) = ty else {
            panic!("ref_static_val into non-pointer type");
        };

        let dataid = self.ctx.val_to_globals[val];
        let ptr = self.dataid_as_pointer(dataid);

        VEntry::Scalar(Scalar::pointer(ptr, &*ty))
    }

    fn size_of(&mut self, ty: &MonoType) -> VEntry {
        let size = self.ctx.structs.size_of(ty);
        let size_t = self.ctx.size_t();
        let v = self.ins().iconst(size_t, size as i64);
        VEntry::direct(v)
    }

    fn align_of(&mut self, ty: &MonoType) -> VEntry {
        let (_, align) = self.ctx.structs.size_and_align_of(ty);
        let size_t = self.ctx.size_t();
        let v = self.ins().iconst(size_t, align as i64);
        VEntry::direct(v)
    }

    fn cast_value_from_scalar(&mut self, ty: &MonoType, point: Value) -> VEntry {
        match ty {
            MonoType::Int(intsize) => {
                let int = Type::int(intsize.bits() as u16).unwrap();
                VEntry::direct(self.resize_uint(point, int))
            }
            MonoType::Pointer(ty) => VEntry::Scalar(Scalar::pointer(point, &ty)),
            MonoType::Array(len, inner) => {
                VEntry::ArrayStackPointer((**inner).clone(), *len, point)
            }
            MonoType::FnPointer(params, ret) => {
                let typing = self.ctx.structs.records.get_abi_typing(params.iter(), ret);

                VEntry::Scalar(Scalar::fn_pointer(point, typing))
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

    fn dataid_as_pointer(&mut self, dataid: DataId) -> Value {
        let data = self
            .ctx
            .objmodule
            .declare_data_in_func(dataid, &mut self.f.builder.func);

        let size_t = self.ctx.size_t();
        self.ins().symbol_value(size_t, data)
    }

    fn fields_to_sstable(&mut self, fields: &[VEntry], dst: &mut Vec<Value>) {
        for entry in fields {
            match entry {
                // All scalars are assumed to be S_Stable
                VEntry::Scalar(scalar) => dst.push(scalar.point),
                VEntry::ZST => {}

                VEntry::StructFlat(_, inner_fields) => {
                    self.fields_to_sstable(inner_fields.as_slice(), dst);
                }
                VEntry::ArrayFlat(_, elems) => {
                    self.fields_to_sstable(elems.as_slice(), dst);
                }

                // A `Struct` which we've implicitly been passing around as a pointer.
                &VEntry::StructHeapPointer(inner, ptr)
                | &VEntry::StructStackPointer(inner, ptr) => {
                    self.deref_struct_into_raw(&mut |point| dst.push(point), inner, ptr);
                }
                VEntry::ArrayStackPointer(inner, len, ptr) => {
                    self.deref_array_into_raw(&mut |point| dst.push(point), inner, *len, *ptr)
                }

                &VEntry::SumPayloadStackPointer { largest, ptr } => {
                    // TODO: I think this can cause undefined behavior as we do not know the exact
                    // alignment of the underlying struct.
                    //
                    // We probably need to set alignment of *all* varaint param tuple to that of
                    // the *largest*.
                    let nptr = self.memcpy_to_heap(ptr, largest as u64, 8, true);
                    dst.push(nptr);
                }
            }
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
