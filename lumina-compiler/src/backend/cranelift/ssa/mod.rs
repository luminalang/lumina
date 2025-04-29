use super::{
    debuginfo, layout,
    layout::{ByteOffset, Layout, PassBy, Scalar, SpecialPointer, Stability, Structs},
    Context, FuncLayout,
};
use crate::lir::ty_fmt;
use crate::prelude::*;
use cranelift::frontend::FuncInstBuilder;
use cranelift::prelude::*;
use cranelift_codegen::ir;
use cranelift_module::{DataId, FuncId, Module};
use lir::{MonoFunc, MonoType, MonoTypeKey};
use lumina_typesystem::IntSize;
use std::sync::Arc;

mod array;
mod call;
mod layout_mapping;
mod num;
mod pointer;
mod record;
mod sum;

use num::binops_from_kind;

#[derive(new)]
pub struct Translator<'c, 'a, 'f> {
    ctx: &'c mut Context<'a>,
    f: Current<'a, 'f>,
}

pub(super) type Predecessors = u16;

#[derive(new)]
struct Current<'a, 'f> {
    func: &'a lir::Function,
    fkey: MonoFunc,
    id: FuncId,
    builder: FunctionBuilder<'f>,
    blockmap: Map<lir::Block, (Block, Predecessors)>,
    #[new(default)]
    imports: HashMap<FuncId, ir::FuncRef>,
    #[new(default)]
    vmap: Map<lir::V, VLayout>,
    #[new(value = "lir::Block::entry()")]
    block: lir::Block,
    #[new(default)]
    ret_pointer: Option<Value>,
}

type VLayout = Layout<Value>;

impl<'a, 'f> Current<'a, 'f> {
    fn switch_to_block(&mut self, block: lir::Block) {
        let (clblock, _) = self.blockmap[block];

        info!("switching to {} (cl{clblock})", block);

        if self.func.ssa.predecessors(block) == 0 {
            self.builder.seal_block(clblock);
        }

        self.builder.switch_to_block(clblock);
        self.block = block;
    }

    fn type_of_value(&self, value: Value) -> Type {
        self.builder.func.dfg.value_type(value)
    }

    fn append_rptr_param_if_needed(&mut self, layout: &Layout<Type>) {
        layout.out_pointers(&mut |_, size_t| {
            assert_eq!(self.block, lir::Block::entry());
            let clblock = self.blockmap[self.block].0;
            let v = self.builder.append_block_param(clblock, size_t);
            self.ret_pointer = Some(v);
        });
    }
}

impl<'c, 'a, 'f> Translator<'c, 'a, 'f> {
    fn cins(&mut self) -> FuncInstBuilder<'_, 'f> {
        self.f.builder.ins()
    }

    fn ins(&mut self) -> InstHelper<'_, '_, 'f> {
        InstHelper {
            size_t: self.ctx.size_t(),
            structs: &self.ctx.structs,
            alloc: self.ctx.funcmap[self.ctx.lir.alloc],
            isa: self.ctx.isa.clone(),
            objmodule: &mut self.ctx.objmodule,
            func_imports: &mut self.f.imports,
            builder: &mut self.f.builder,
        }
    }

    fn types(&self) -> &lir::Types {
        &self.ctx.lir.types
    }

    pub fn func(
        ctx: &'c mut Context<'a>,
        cctx: &mut codegen::Context,
        fctx: &mut FunctionBuilderContext,
        func: &'a lir::Function,
        key: MonoFunc,
    ) -> debuginfo::FunctionDebugContext {
        let id = ctx.funcmap[key];

        cctx.func.signature = ctx
            .objmodule
            .declarations()
            .get_function_decl(id)
            .signature
            .clone();
        let mut builder = FunctionBuilder::new(&mut cctx.func, fctx);

        let blockmap = func
            .ssa
            .blocks()
            .map(|_| (builder.create_block(), 0))
            .collect();

        let f_dbg_ctx = ctx.def_function(key);

        Translator { ctx, f: Current::new(func, key, id, builder, blockmap) }
            .lower_and_finalize_current();

        info!("lowered {}:\n {}", func.symbol, &cctx.func);

        if let Err(err) = cranelift_codegen::verify_function(&cctx.func, ctx.isa.as_ref()) {
            error!("cranelift_codegen verifier error:\n{err}");
        }

        f_dbg_ctx
    }

    fn lower_and_finalize_current(mut self) {
        self.seal_block_if_last_predecessor(self.f.block);
        self.f.switch_to_block(self.f.block);

        self.f
            .append_rptr_param_if_needed(&self.ctx.flayouts[self.f.id].ret);

        for v in self.f.func.ssa.iterv() {
            let entry = self.f.func.ssa.entry_of(v);
            let ty = self.f.func.ssa.type_of(v);

            if let Some((block, _binfo)) = self.f.func.ssa.as_block_start(v) {
                self.f.switch_to_block(block);
            }

            let vlayout = self.entry(v, entry, ty);
            self.f.vmap.push_as(v, vlayout);
        }

        self.f.builder.finalize();
    }

    fn declare_block_param(&mut self, block: lir::Block, _: u32, ty: &MonoType) -> VLayout {
        assert_eq!(self.f.block, block);

        let (clblock, _) = self.f.blockmap[block];

        let layout = self.ctx.structs.type_to_layout(ty, Stability::F);
        layout.map_layout(
            &mut |clty| self.f.builder.append_block_param(clblock, clty),
            &mut |_, _| panic!("out pointer without FRet stability"),
        )
    }

    fn entry(&mut self, v: lir::V, entry: &lir::Entry, ty: &MonoType) -> VLayout {
        trace!(
            "lowering {v} = {} : {ty:?}",
            ty_fmt(&self.ctx.lir.types, entry).fns(&self.ctx.lir.functions)
        );

        match entry {
            &lir::Entry::BlockParam(block, i) => self.declare_block_param(block, i, ty),
            lir::Entry::Transmute(value) => {
                let v = self.value_to_vlayout(*value);
                self.ins().transmute(v, ty)
            }
            lir::Entry::SizeOf(ty) => self.ins().size_of(ty),
            lir::Entry::AlignOf(ty) => self.ins().align_of(ty),

            lir::Entry::CallStatic(mfunc, params) => self.call_func(*mfunc, params),
            lir::Entry::CallExtern(key, params) => self.call_extern(*key, params),
            lir::Entry::CallValue(ptr, params) => {
                let entry = self.value_to_vlayout(*ptr);
                self.call_vlayout(entry, params)
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
                let entry = self.value_to_vlayout(*of);
                self.ins().field_of_vlayout(entry.clone(), *field)
            }
            lir::Entry::Indice { of, indice } => {
                let entry = self.value_to_vlayout(*of);
                let indice = self.value_to_vlayout(*indice).as_direct();
                self.ins().indice_of_entry(entry, indice)
            }

            lir::Entry::Variant(var, values) => match ty {
                MonoType::Monomorphised(mk) => self.construct_variant(*mk, *var, values),
                _ => panic!("cannot construct: {ty:?}"),
            },
            lir::Entry::TagFromSum { of } => {
                let of = self.value_to_vlayout(*of);
                self.ins().tag_of_sum(of)
            }
            lir::Entry::CastFromSum { of } => {
                let entry = self.value_to_vlayout(*of);
                self.cast_from_sum(entry, ty)
            }

            lir::Entry::BinOp(lir::BinOp::And, ints) => self.bit_and(*ints),
            lir::Entry::BinOp(lir::BinOp::Div, ints) => self.int_div(*ints, as_int(ty)),
            lir::Entry::BinOp(kind, values) => self.ibinary(ty, *values, binops_from_kind(*kind)),
            lir::Entry::IntAbs(v) => self.iunary(*v, as_int(ty), |ins, _, v| ins.iabs(v)),

            lir::Entry::IntCmpInclusive(values, cmp, bitsize) => {
                self.int_cmpi(*values, *cmp, *bitsize)
            }

            lir::Entry::Reduce(v) => self.iunary(*v, as_int(ty), InstBuilder::ireduce),
            lir::Entry::ExtendSigned(v) => self.iunary(*v, as_int(ty), InstBuilder::sextend),
            lir::Entry::ExtendUnsigned(v) => self.iunary(*v, as_int(ty), InstBuilder::uextend),

            lir::Entry::FloatToInt(v, intsize) => self.float_to_int(*v, *intsize),
            lir::Entry::IntToFloat(v, intsize) => self.int_to_float(*v, *intsize),

            lir::Entry::BitNot(v) => self.bit_not(*v),

            lir::Entry::Alloc => {
                let MonoType::Pointer(innert) = ty else {
                    panic!("Alloc to non-pointer");
                };
                let ptr = self.ins().heap_alloc_type(innert);
                VLayout::pointer((**innert).clone(), ptr)
            }
            lir::Entry::Alloca => {
                let ptr = self.ins().stack_alloc_type(ty);
                VLayout::pointer(ty.clone(), ptr)
            }
            lir::Entry::Dealloc { .. } => {
                panic!("dealloc should be removed");
            }
            lir::Entry::WritePtr { ptr, value } => {
                let [ptr, value] = [*ptr, *value].map(|v| self.value_to_vlayout(v));
                let ptr = ptr.as_pointer().1;
                self.ins().write_vlayout_to_ptr(ptr, &value);
                Layout::ZST
            }
            lir::Entry::MemCpy { dst, src, count } => {
                let [dst, src] = [*dst, *src].map(|v| self.value_to_vlayout(v).as_pointer().1);
                let count = self.value_to_vlayout(*count).as_direct();
                let config = self.ctx.isa.frontend_config();
                self.ins().builder.call_memcpy(config, dst, src, count);
                Layout::ZST
            }
            lir::Entry::Deref(ptr) => {
                let ptr = self.value_to_vlayout(*ptr);
                let (ty, ptr) = ptr.as_pointer();
                self.ins().deref_type(ptr, ByteOffset(0), ty)
            }

            lir::Entry::JmpFunc(mfunc, params) => {
                self.tail_call(*mfunc, params);
                Layout::ZST
            }
            lir::Entry::JmpBlock(jump) => {
                let params = self.bparams(jump);

                let clblock = self.f.blockmap[jump.id].0;
                self.cins().jump(clblock, &params);

                self.seal_block_if_last_predecessor(jump.id);
                VLayout::ZST
            }
            lir::Entry::Trap(code) => {
                self.cins().trap(*code);
                VLayout::ZST
            }
            &lir::Entry::Return(v) => {
                let entry = self.value_to_vlayout(v);
                self.return_(false, entry);
                Layout::ZST
            }
            lir::Entry::Select { value, on_true, on_false } => {
                let int = self.value_to_vlayout(*value).as_direct();

                let then_params = self.bparams(on_true);
                let else_params = self.bparams(on_false);

                let [block_then, block_else] =
                    [on_true.id, on_false.id].map(|block| self.f.blockmap[block].0);

                self.cins()
                    .brif(int, block_then, &then_params, block_else, &else_params);

                for block in [on_true.id, on_false.id] {
                    self.seal_block_if_last_predecessor(block);
                }
                Layout::ZST
            }
            lir::Entry::JmpTable(of, blocks) => {
                let indice = self.value_to_vlayout(*of).as_direct();

                let def = self.f.builder.create_block();

                let pool = &mut self.f.builder.func.dfg.value_lists;

                let table: Vec<ir::BlockCall> = blocks
                    .iter()
                    .map(|block| ir::BlockCall::new(self.f.blockmap[*block].0, &[], pool))
                    .collect();

                let defcall = ir::BlockCall::new(def, &[], pool);

                let jump_data = JumpTableData::new(defcall, &table);
                let table = self.f.builder.create_jump_table(jump_data);

                let indice = self.resize_uint(indice, types::I32);

                self.cins().br_table(indice, table);

                self.f.builder.seal_block(def);
                self.f.builder.switch_to_block(def);
                self.cins()
                    .trap(TrapCode::user(lir::TRAP_UNREACHABLE).unwrap());

                for block in blocks {
                    self.seal_block_if_last_predecessor(*block);
                }

                Layout::ZST
            }
        }
    }

    fn value_to_vlayout(&mut self, value: lir::Value) -> VLayout {
        trace!("yielding {value}");

        match value {
            lir::Value::V(v) => self.f.vmap[v].clone(),
            lir::Value::ReadOnly(ronly) => {
                let dataid = self.ctx.rotable[ronly];
                let ptr = self.ins().dataid_as_pointer(dataid);

                let ty = self.ctx.lir.read_only_table_types[ronly].clone();
                Layout::pointer(ty, ptr)
            }
            lir::Value::FuncPtr(mfunc) => {
                let id = self.ctx.funcmap[mfunc];
                self.funcid_to_fptr_layout(id)
            }
            lir::Value::ExternFuncPtr(key) => {
                let fheader = self.ctx.externmap[&key].clone();
                self.funcid_to_fptr_layout(fheader)
            }
            lir::Value::Int(n, bitsize) => {
                let ty = Type::int(bitsize.bits() as u16).unwrap();
                let n = self.cins().iconst(ty, n as i64);
                Layout::direct(n)
            }
            lir::Value::Float(_) => todo!(),
        }
    }

    fn funcid_to_fptr_layout(&mut self, id: FuncId) -> VLayout {
        let fref = self.ins().declare_func_in_func(id);
        let ptr_type = self.ctx.size_t();
        let fptr = self.cins().func_addr(ptr_type, fref);
        let layout = self.ctx.flayouts[id].clone();
        Layout::Scalar(Scalar::FuncPointer(Box::new(layout)), fptr)
    }

    fn return_(&mut self, ignore_outptr: bool, vlayout: VLayout) {
        let rlayout = self.ctx.flayouts[self.f.id].ret.clone();
        let mut buf = Vec::new();
        let out_pointer = match self.f.ret_pointer {
            Some(ptr) if ignore_outptr => OutReturn::Ignored(ptr),
            Some(ptr) => OutReturn::Pointer(ptr),
            None => OutReturn::None,
        };
        let returnable = self
            .ins()
            .make_compatible_plus(out_pointer, &rlayout, vlayout);
        self.return_compatible(returnable, &mut buf);
        self.cins().return_(&buf);
    }

    fn return_compatible(&mut self, vlayout: VLayout, buf: &mut Vec<Value>) {
        match vlayout {
            Layout::SpecialPointer(_, v) | Layout::AutoBoxed(_, v) | Layout::Scalar(_, v) => {
                buf.push(v)
            }
            Layout::ZST => {}
            Layout::ArrayFlat(_, elems) => elems
                .into_iter()
                .for_each(|layout| self.return_compatible(layout, buf)),
            Layout::StructFlat(_, fields) => fields
                .into_iter()
                .for_each(|(_, layout)| self.return_compatible(layout, buf)),
            Layout::OutPointer(_, _) => {} // has already been written
        }
    }

    fn seal_block_if_last_predecessor(&mut self, block: lir::Block) {
        let (clblock, predecessors) = &mut self.f.blockmap[block];
        *predecessors += 1;

        debug_assert!(*predecessors <= self.f.func.ssa.predecessors(block));

        if *predecessors == self.f.func.ssa.predecessors(block) {
            self.f.builder.seal_block(*clblock);
        }
    }

    fn ref_static_val(&mut self, val: M<key::Val>, ty: &MonoType) -> VLayout {
        let MonoType::Pointer(ty) = ty else {
            panic!("ref_static_val into non-pointer type");
        };

        let dataid = self.ctx.val_to_globals[val];
        let ptr = self.ins().dataid_as_pointer(dataid);

        Layout::pointer((**ty).clone(), ptr)
    }
}

#[derive(Clone, Copy)]
pub enum OutReturn {
    Pointer(Value),
    Ignored(Value),
    None,
}

#[derive(new)]
pub(super) struct InstHelper<'f, 's, 'a> {
    builder: &'f mut FunctionBuilder<'a>,
    structs: &'f Structs<'s>,
    size_t: Type,
    alloc: FuncId,
    isa: Arc<dyn isa::TargetIsa>,
    objmodule: &'f mut cranelift_object::ObjectModule,
    func_imports: &'f mut HashMap<FuncId, ir::FuncRef>,
}

impl<'f, 's, 'a> InstHelper<'f, 's, 'a> {
    pub fn create_struct_stack_slot(&mut self, size: u32, align: u8) -> ir::StackSlot {
        let slotdata = StackSlotData::new(StackSlotKind::ExplicitSlot, size, align as u8);
        self.builder.create_sized_stack_slot(slotdata)
    }

    fn transmute(&mut self, v: Layout<Value>, ty: &MonoType) -> VLayout {
        match v {
            Layout::ZST => Layout::ZST,
            Layout::Scalar(_, v) => {
                match ty {
                    MonoType::Pointer(ty) => {
                        let size_t = self.size_t;
                        assert_eq!(self.type_of_value(v), size_t);
                        VLayout::pointer((**ty).clone(), v)
                    }
                    MonoType::Int(intsize) => {
                        assert_eq!(intsize.bytes(), self.type_of_value(v).bytes() as u8);
                        // int and uint have the same representation so we don't need to do anything
                        VLayout::direct(v)
                    }
                    other => unimplemented!("transmuting {other:?} into {ty:?}"),
                }
            }
            other => unimplemented!("transmuting {other:?} into {ty:?}"),
        }
    }

    fn tag_of_sum(&mut self, v: Layout<Value>) -> VLayout {
        let find = |key| self.structs.get(key).field_map[key::Field(0)];
        match v {
            Layout::StructFlat(key, mut entries) => {
                let i = find(key);
                let tag = entries.as_mut_vec().remove(i.into()).as_direct();
                VLayout::direct(tag)
            }
            Layout::SpecialPointer(SpecialPointer::StackStruct(key), ptr) => {
                self.structs.records[key].as_sum();
                let i = find(key);
                let offset = self.structs.offset_of(key, i);
                let layout::StructField::Flat(MonoType::Int(isize)) =
                    self.structs.get(key).fields[i]
                else {
                    unreachable!();
                };
                let clty = Type::int(isize.bits() as u16).unwrap();
                let tag = self
                    .ins()
                    .load(clty, MemFlags::trusted(), ptr, offset.0 as i32);
                VLayout::direct(tag)
            }
            other => unreachable!("{other:?}"),
        }
    }

    fn size_of(&mut self, ty: &MonoType) -> VLayout {
        let size = self.structs.size_of(ty);
        let size_t = self.size_t;
        let v = self.ins().iconst(size_t, size as i64);
        Layout::direct(v)
    }

    fn align_of(&mut self, ty: &MonoType) -> VLayout {
        let (_, align) = self.structs.size_and_align_of(ty);
        let size_t = self.size_t;
        let v = self.ins().iconst(size_t, align as i64);
        Layout::direct(v)
    }

    pub fn dataid_as_pointer(&mut self, dataid: DataId) -> Value {
        let data = self
            .objmodule
            .declare_data_in_func(dataid, &mut self.builder.func);

        let size_t = self.size_t;
        self.ins().symbol_value(size_t, data)
    }

    pub fn ins(&mut self) -> FuncInstBuilder<'_, 'a> {
        self.builder.ins()
    }

    fn memcpy_struct(&mut self, dst: Value, src: Value, size: u64, align: u8) {
        let config = self.isa.frontend_config();
        let flags = MemFlags::trusted();
        self.builder
            .emit_small_memory_copy(config, dst, src, size, align, align, true, flags);
    }

    fn memcpy_to_heap(&mut self, src: Value, size: u64, align: u8, check_null: bool) -> Value {
        if !check_null {
            return self.memcpy_to_heap_unchecked(src, size, align);
        }

        let continuation = self.builder.create_block();

        // Null pointers are valid for some sum variants. So; we can't always look up
        // statically whether null is possible or not.

        let is_null = self.ins().icmp_imm(IntCC::Equal, src, 0);
        let [identity, allocate] = [(), ()].map(|_| self.builder.create_block());
        self.ins().brif(is_null, identity, &[], allocate, &[]);

        for block in [identity, allocate] {
            self.builder.seal_block(block)
        }

        self.builder.switch_to_block(identity);
        self.ins().jump(continuation, &[src]);

        self.builder.switch_to_block(allocate);
        let nptr = self.memcpy_to_heap_unchecked(src, size, align);
        self.ins().jump(continuation, &[nptr]);

        self.builder.seal_block(continuation);
        self.builder.switch_to_block(continuation);
        let out = self.builder.append_block_param(continuation, self.size_t);

        out
    }

    fn memcpy_to_heap_unchecked(&mut self, src: Value, size: u64, align: u8) -> Value {
        let nptr = self.heap_alloc(size as i128);
        self.memcpy_struct(nptr, src, size, align);
        return nptr;
    }

    fn declare_func_in_func(&mut self, id: FuncId) -> ir::FuncRef {
        *self.func_imports.entry(id).or_insert_with(|| {
            self.objmodule
                .declare_func_in_func(id, &mut self.builder.func)
        })
    }

    fn type_of_value(&self, value: Value) -> Type {
        self.builder.func.dfg.value_type(value)
    }

    fn heaplift_sum_payload(&mut self, sptr: Value, tag_size: u8, largest: u32) -> Value {
        let size_t = self.size_t;
        let align = if largest < size_t.bytes() {
            (largest as u8).max(tag_size)
        } else {
            size_t.bytes() as u8
        };
        let nptr = self.memcpy_to_heap(sptr, largest as u64, align, true);
        nptr
    }
}

pub fn next_value(values: &mut &[Value]) -> Value {
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
