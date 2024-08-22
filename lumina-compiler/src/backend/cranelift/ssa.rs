use super::{abi, Context};
use crate::prelude::*;
use cranelift::frontend::FuncInstBuilder;
use cranelift::prelude::*;
use cranelift_codegen::ir;
use cranelift_module::{FuncId, Module};
use lir::{BitOffset, MonoFunc, MonoType, MonoTypeKey};
use lumina_typesystem::IntSize;
use lumina_util::Highlighting;

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

impl<'a, 'f> Current<'a, 'f> {
    fn switch_to_block(&mut self, block: lir::Block) {
        let (clblock, _) = self.blockmap[block];

        info!("switching to {} (cl{clblock})", block);

        self.builder.switch_to_block(clblock);
        self.block = block;
    }

    fn type_of_value(&self, value: Value) -> Type {
        self.builder.func.stencil.dfg.value_type(value)
    }
}

impl<'c, 'a, 'f> Translator<'c, 'a, 'f> {
    fn ins(&mut self) -> FuncInstBuilder<'_, 'f> {
        self.f.builder.ins()
    }

    fn types(&self) -> &lir::Records {
        &self.ctx.lir.types
    }

    pub fn func(ctx: &'c mut Context<'a>, func: &'a lir::Function, key: MonoFunc) -> ir::Function {
        let mut func_builder_ctx = FunctionBuilderContext::new();
        let mut clfunc = ir::Function::new();

        let triple = ctx.isa.triple();
        let abi_block_types = func
            .blocks
            .blocks()
            .map(|block| {
                ctx.lir
                    .types
                    .get_abi_params(triple, func.blocks.param_types(block))
            })
            .collect::<Map<_, _>>();

        let header = &ctx.funcmap[key];

        let params = &abi_block_types[lir::Block::entry()];
        clfunc.signature = abi::signature(isa::CallConv::Fast, &params, &header.ret);
        let mut builder = FunctionBuilder::new(&mut clfunc, &mut func_builder_ctx);

        let blockmap = func
            .blocks
            .blocks()
            .map(|block| {
                let clblock = builder.create_block();

                func.blocks.params(block).for_each(|v| {
                    let t = func.blocks.type_of(v);
                    let entry = ctx.lir.types.abi_param(triple, t);
                    entry.visit(|ty| {
                        builder.append_block_param(clblock, ty);
                    })
                });

                if block == lir::Block::entry() {
                    if let abi::Return::StructOutPtr(_, _) = &header.ret {
                        builder.append_block_param(clblock, Type::triple_pointer_type(triple));
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

        info!("lowered:\n {clfunc}");

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
        // Map the block parameters to entries
        let clblock = self.f.blockmap[self.f.block].0;
        let mut raw_params = self.f.builder.block_params(clblock).iter();
        for (i, v) in self.f.func.blocks.params(self.f.block).enumerate() {
            let entry = self.f.abi_block_types[self.f.block][key::Param(i as u32)]
                .map(&mut |_type| *raw_params.next().unwrap());

            self.f.vmap.push_as(v, entry);
        }

        for v in self.f.func.blocks.entries(self.f.block) {
            let entry = self.f.func.blocks.entry_of(v);
            let ty = self.f.func.blocks.type_of(v);
            trace!("lowering {v} {} {entry} : {ty:?}", '='.symbol());
            let entry = self.entry(entry, ty);
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
        slot: Option<ir::StackSlot>,
    ) -> VEntry {
        let rvalues = self.f.builder.inst_results(call);
        match ret {
            abi::Return::Param(entry) => match entry {
                abi::Param::Direct(_) => {
                    assert_eq!(rvalues.len(), 1);
                    VEntry::Direct(rvalues[0])
                }
                abi::Param::FuncPointer(_, _) => todo!(),
                abi::Param::Struct(mk, fields) => {
                    // Reconstruct the struct by mapping the ABI information with the
                    // results given by Cranelift.
                    let mut rvalues = rvalues.iter().copied();
                    let vfields = fields
                        .values()
                        .map(|field| field.map(&mut |_type| rvalues.next().unwrap()))
                        .collect();
                    VEntry::Struct(*mk, vfields)
                }
                abi::Param::SumPayload { .. } => {
                    unreachable!(
                        "sum payload returned independently of it's attached data pointer"
                    );
                }
                abi::Param::ZST => VEntry::ZST,
            },
            // TODO: right now we flatten but in the future I'd like to be able to copy around
            // the pointer instead and instead memcpy/flatten on return *if needed*
            abi::Return::StructOutPtr(mk, fields) => {
                let slot = slot.unwrap();
                let mut offset = 0;
                let dereferenced_fields = fields
                    .values()
                    .map(|field| {
                        field.map(&mut |ty| {
                            let v = self.ins().stack_load(ty, slot, offset);
                            offset += ty.bytes() as i32;
                            v
                        })
                    })
                    .collect();
                VEntry::Struct(*mk, dereferenced_fields)
            }
        }
    }

    fn append_rptr_if_needed(
        &mut self,
        ret: &abi::Return,
        rvalues: &mut Vec<Value>,
    ) -> Option<ir::StackSlot> {
        match ret {
            abi::Return::Param(_) => None,
            abi::Return::StructOutPtr(kind, _) => {
                let size = self.types().size_of_defined(*kind);
                let slotdata = StackSlotData::new(StackSlotKind::ExplicitSlot, size);
                let slot = self.f.builder.create_sized_stack_slot(slotdata);
                let ptr = self.ins().stack_addr(types::I64, slot, 0);
                rvalues.push(ptr);
                Some(slot)
            }
        }
    }

    fn call_func(&mut self, id: FuncId, ret: abi::Return, params: &[lir::Value]) -> VEntry {
        let mut appl = self.params(params);
        let slot = self.append_rptr_if_needed(&ret, &mut appl);

        let fref = self.declare_func_in_func(id);
        let call = self.ins().call(fref, &appl);

        self.map_call_results_to_ventry(call, &ret, slot)
    }

    fn construct_record(&mut self, key: MonoTypeKey, values: &[lir::Value]) -> VEntry {
        let fields = values.iter().map(|v| self.value_to_field(*v)).collect();
        VEntry::Struct(key, fields)
    }

    fn value_to_field(&mut self, v: lir::Value) -> VField {
        match self.value_to_entry(v) {
            VEntry::Direct(v) => VField::Direct(v),
            VEntry::Struct(_, fields) => VField::Struct(fields),
            VEntry::FuncPointer(typing, ptr) => VField::FuncPointer(typing, ptr),
            VEntry::SumPayload { largest, ptr } => VField::SumPayload { largest, ptr },
            VEntry::ZST => VField::ZST,
        }
    }

    #[track_caller]
    fn lower_block_if_last_predecessor(&mut self, block: lir::Block) {
        let (clblock, predecessors) = &mut self.f.blockmap[block];
        *predecessors += 1;

        if *predecessors == self.f.func.blocks.predecessors(block) {
            if self.f.block == lir::Block::from_u32(13) && block == lir::Block::from_u32(7) {
                panic!("trap");
            };
            self.f.builder.seal_block(*clblock);
            self.f.switch_to_block(block);
            self.block();
        }
    }

    fn entry(&mut self, entry: &lir::Entry, ty: &MonoType) -> VEntry {
        match entry {
            lir::Entry::Copy(value) => self.value_to_entry(*value),
            lir::Entry::Transmute(value) => match self.value_to_entry(*value) {
                VEntry::Direct(v) => VEntry::Direct(v),
                VEntry::FuncPointer(_, v) => match ty {
                    MonoType::FnPointer(_, _) => {
                        todo!("casting function pointers to other function pointers")
                    }
                    MonoType::Int(size) if size.bits() == self.ctx.isa.pointer_bits() => {
                        VEntry::Direct(v)
                    }
                    MonoType::Pointer(_) => todo!("casting fnptr to ptr"),
                    _ => panic!("unsupported cast: {ty:?}"),
                },
                _ => panic!("unsupported transmute: {value} -> {ty:?}"),
            },
            lir::Entry::BlockParam(_) => VEntry::ZST,
            lir::Entry::CallStatic(mfunc, params) => {
                let fheader = &self.ctx.funcmap[*mfunc];
                let (id, ret) = (fheader.id, fheader.ret.clone());
                self.call_func(id, ret, params)
            }
            lir::Entry::CallExtern(key, params) => {
                let fheader = &self.ctx.externmap[key];
                let (id, ret) = (fheader.id, fheader.ret.clone());
                self.call_func(id, ret, params)
            }
            lir::Entry::CallValue(ptr, params) => {
                let ptr = self.value_to_entry(*ptr);
                let mut params = self.params(params);

                match ptr {
                    abi::Entry::FuncPointer(typing, ptr) => {
                        let slot = self.append_rptr_if_needed(&typing.ret, &mut params);
                        let sig = abi::signature(typing.conv, &typing.params, &typing.ret);
                        let sigref = self.f.builder.import_signature(sig);
                        let call = self.ins().call_indirect(sigref, ptr, &params);
                        self.map_call_results_to_ventry(call, &typing.ret, slot)
                    }
                    _ => panic!("not a fnpointer: {ptr:?}\n{}", &self.f.builder.func),
                }
            }
            lir::Entry::Construct(values) => match ty {
                MonoType::Monomorphised(mk) => self.construct_record(*mk, values),
                &MonoType::SumDataCast { largest } => {
                    let ptr = self.heap_alloc(largest as i128);

                    let mut offset = 0;
                    for v in values {
                        self.value_to_entry(*v).visit(|v| {
                            self.ins().store(MemFlags::new(), v, ptr, offset);
                            offset += self.f.type_of_value(v).bytes() as i32;
                        });
                    }

                    VEntry::SumPayload { largest, ptr }
                }
                _ => panic!("cannot construct: {ty:?}"),
            },
            lir::Entry::RefStaticVal(val) => {
                let dataid = self.ctx.val_to_globals[*val];
                let data = self
                    .ctx
                    .objmodule
                    .declare_data_in_func(dataid, &mut self.f.builder.func);

                let ptr = self.ins().symbol_value(types::I64, data);

                VEntry::Direct(ptr)
            }
            lir::Entry::Field { of, key, field } => match self.value_to_entry(*of) {
                VEntry::Struct(k, fields) => {
                    assert_eq!(k, *key);
                    let field_type = self.types().type_of_field(k, *field);
                    self.field_to_value(fields[*field].clone(), field_type)
                }
                _ => panic!("cannot access field of non-struct"),
            },
            lir::Entry::SumField { of, offset } => match of {
                lir::Value::V(v) => match self.f.vmap[*v].clone() {
                    VEntry::SumPayload { ptr, .. } => self.deref_type(ptr, *offset, ty).0,
                    other => panic!("impossible source of sum data payload: {other:#?}"),
                },
                _ => panic!("impossible source of sum data payload"),
            },

            lir::Entry::IntAdd(left, right) => {
                let [left, right] = [*left, *right].map(|v| self.value_to_entry(v).as_direct());
                self.checked_numop(
                    ty,
                    |this| this.ins().sadd_overflow(left, right),
                    |this| this.ins().uadd_overflow(left, right),
                    |this| this.ins().iadd(left, right),
                )
            }
            lir::Entry::IntSub(left, right) => {
                let [left, right] = [*left, *right].map(|v| self.value_to_entry(v).as_direct());
                self.checked_numop(
                    ty,
                    |this| this.ins().ssub_overflow(left, right),
                    |this| this.ins().usub_overflow(left, right),
                    |this| this.ins().isub(left, right),
                )
            }
            lir::Entry::IntMul(left, right) => {
                let [left, right] = [*left, *right].map(|v| self.value_to_entry(v).as_direct());
                self.checked_numop(
                    ty,
                    |this| this.ins().smul_overflow(left, right),
                    |this| this.ins().umul_overflow(left, right),
                    |this| this.ins().imul(left, right),
                )
            }
            lir::Entry::IntDiv(left, right) => {
                let [left, right] = [*left, *right].map(|v| self.value_to_entry(v).as_direct());
                let v = if as_int(ty).signed {
                    self.ins().sdiv(left, right)
                } else {
                    self.ins().udiv(left, right)
                };
                VEntry::Direct(v)
            }
            lir::Entry::IntAbs(v) => {
                let v = self.value_to_entry(*v).as_direct();
                let v = self.ins().iabs(v);
                VEntry::Direct(v)
            }
            lir::Entry::IntCmpInclusive(left, cmp, right, bitsize) => {
                let [left, right] = [*left, *right].map(|v| self.value_to_entry(v).as_direct());
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

                VEntry::Direct(v)
            }
            lir::Entry::Reduce(v) => {
                let v = self.value_to_entry(*v).as_direct();
                let ty = Type::int(as_int(ty).bits() as u16).unwrap();
                let v = self.ins().ireduce(ty, v);
                VEntry::Direct(v)
            }
            lir::Entry::ExtendSigned(v) => {
                let v = self.value_to_entry(*v).as_direct();
                let ty = Type::int(as_int(ty).bits() as u16).unwrap();
                let v = self.ins().sextend(ty, v);
                VEntry::Direct(v)
            }
            lir::Entry::ExtendUnsigned(v) => {
                let v = self.value_to_entry(*v).as_direct();
                let ty = Type::int(as_int(ty).bits() as u16).unwrap();
                let v = self.ins().uextend(ty, v);
                VEntry::Direct(v)
            }
            lir::Entry::FloatToInt(v, intsize) => {
                let v = self.value_to_entry(*v).as_direct();
                let int = Type::int(intsize.bits() as u16).unwrap();
                let v = if intsize.signed {
                    self.ins().fcvt_to_sint_sat(int, v)
                } else {
                    self.ins().fcvt_to_uint_sat(int, v)
                };
                VEntry::Direct(v)
            }
            lir::Entry::IntToFloat(v, intsize) => {
                let v = self.value_to_entry(*v).as_direct();
                let v = if intsize.signed {
                    self.ins().fcvt_from_sint(types::F64, v)
                } else {
                    self.ins().fcvt_from_uint(types::F64, v)
                };
                VEntry::Direct(v)
            }
            lir::Entry::BitAnd(_) => todo!(),
            lir::Entry::BitNot(v) => {
                let v = self.value_to_entry(*v).as_direct();
                let v = self.ins().bnot(v);
                VEntry::Direct(v)
            }
            // TODO: for ZST we should just give a hardcoded `0`
            lir::Entry::Alloc { size } => VEntry::Direct(self.heap_alloc(*size as i128)),
            lir::Entry::Dealloc { ptr } => {
                self.heap_dealloc(*ptr, 0);
                VEntry::ZST
            }
            lir::Entry::WritePtr { ptr, value } => {
                let ptr = self.value_to_entry(*ptr).as_direct();
                let entry = self.value_to_entry(*value);
                self.write_entry_to_ptr(ptr, 0, &entry);
                VEntry::ZST
            }
            lir::Entry::Deref(ptr) => {
                let ptr = self.value_to_entry(*ptr).as_direct();
                self.deref_type(ptr, BitOffset(0), ty).0
            }
        }
    }

    fn field_to_value(&mut self, field: VField, field_type: MonoType) -> VEntry {
        match field {
            VField::Direct(v) => VEntry::Direct(v),
            VField::FuncPointer(typing, ptr) => VEntry::FuncPointer(typing, ptr),
            VField::AutoBoxedRecursion(_, _) => todo!("dereference"),
            VField::Struct(fields) => VEntry::Struct(field_type.as_key(), fields),
            VField::SumPayload { largest, ptr } => VEntry::SumPayload { largest, ptr },
            VField::ZST => VEntry::ZST,
        }
    }

    fn write_entry_to_ptr(&mut self, ptr: Value, offset: i32, entry: &VEntry) -> i32 {
        assert_eq!(self.f.type_of_value(ptr), self.ctx.pointer_type());

        match entry {
            VEntry::ZST => offset,
            VEntry::FuncPointer(_, _) => {
                todo!();
            }
            VEntry::Direct(v) => {
                let size = self.f.type_of_value(*v);
                self.ins().store(MemFlags::new(), *v, ptr, offset);
                offset + size.bytes() as i32
            }
            VEntry::Struct(_, fields) => fields.values().fold(offset, |offset, entry| {
                self.write_field_to_ptr(ptr, offset, entry)
            }),
            VEntry::SumPayload { .. } => {
                panic!("opaque SumPayload written to pointer");
                // We could implement it with memcpy but should we?
            }
        }
    }

    fn write_field_to_ptr(&mut self, ptr: Value, offset: i32, field: &VField) -> i32 {
        match field {
            VField::FuncPointer(_, v) | VField::Direct(v) => {
                let size = self.f.type_of_value(*v);
                self.ins().store(MemFlags::new(), *v, ptr, offset);
                offset + size.bytes() as i32
            }
            VField::AutoBoxedRecursion(_, _) => todo!(),
            VField::Struct(fields) => fields.values().fold(offset, |offset, field| {
                self.write_field_to_ptr(ptr, offset, field)
            }),
            VField::SumPayload { ptr: payload, .. } => {
                let size = self.f.type_of_value(*payload);
                self.ins().store(MemFlags::new(), *payload, ptr, offset);
                offset + size.bytes() as i32
            }
            VField::ZST => offset,
        }
    }

    fn heap_alloc(&mut self, size: i128) -> Value {
        let alloc = self.ctx.lir.alloc;
        let fheader = self.ctx.funcmap[alloc].clone();

        let size = lir::Value::Int(
            size,
            IntSize::new(true, self.ctx.pointer_type().bits() as u8),
        );

        let entry = self.call_func(fheader.id, fheader.ret, &[size]);
        match entry {
            abi::Entry::Direct(ptr) => ptr,
            _ => panic!("alloc has unexpected signature"),
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

        let entry = self.call_func(fheader.id, fheader.ret, &[ptr, size]);
        match entry {
            abi::Entry::ZST => {}
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

                let ptr = self.ins().symbol_value(types::I64, data);

                VEntry::Direct(ptr)
            }
            lir::Value::FuncPtr(mfunc) => {
                let header = &self.ctx.funcmap[mfunc];
                let typing = abi::Typing {
                    conv: self
                        .ctx
                        .objmodule
                        .declarations()
                        .get_function_decl(header.id)
                        .signature
                        .call_conv,
                    params: header.params.clone(),
                    ret: header.ret.clone(),
                };
                let fref = self.declare_func_in_func(header.id);
                let ptr_type = self.ctx.pointer_type();
                let fptr = self.ins().func_addr(ptr_type, fref);

                VEntry::FuncPointer(Box::new(typing), fptr)
            }
            lir::Value::ExternFuncPtr(key) => {
                let fheader = &self.ctx.externmap[&key];
                let fref = self.declare_func_in_func(fheader.id);
                let ptr_type = self.ctx.pointer_type();
                let fptr = self.ins().func_addr(ptr_type, fref);
                VEntry::Direct(fptr)
            }
            lir::Value::Int(n, bitsize) => {
                let ty = Type::int(bitsize.bits() as u16).unwrap();
                VEntry::Direct(self.ins().iconst(ty, n as i64))
            }
            lir::Value::Float(_) => todo!(),
        }
    }

    fn deref_type(&mut self, ptr: Value, offset: BitOffset, ty: &MonoType) -> (VEntry, BitOffset) {
        let triple = self.ctx.isa.triple();
        let abi = self.types().abi_param(triple, ty);
        let mut offset = (offset.0 / 8) as i32;
        let entry = abi.map(&mut |ty| {
            let v = self.ins().load(ty, MemFlags::new(), ptr, offset);
            offset += ty.bytes() as i32;
            v
        });
        (entry, BitOffset(offset as u32 * 8))
    }

    fn flatten_entry(&mut self, entry: &VEntry, dst: &mut Vec<Value>) {
        entry.visit(&mut |v| dst.push(v));
    }

    fn flow(&mut self, flow: &lir::ControlFlow) {
        match flow {
            lir::ControlFlow::JmpFunc(mfunc, params) => {
                let fheader = &self.ctx.funcmap[*mfunc];
                let (id, ret) = (fheader.id, fheader.ret.clone());
                // TODO: tail calls
                let entry = self.call_func(id, ret, params);
                self.return_(entry);
            }
            lir::ControlFlow::JmpBlock(block, params) => {
                let clblock = self.f.blockmap[*block].0;

                let params = self.params(params);
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
        let triple = self.ctx.isa.triple();
        match self.types().get_abi_return(triple, &self.f.func.returns) {
            abi::Return::Param(_) => {
                let mut values = vec![];
                self.flatten_entry(&entry, &mut values);
                self.ins().return_(&values);
            }
            abi::Return::StructOutPtr(_, _) => {
                let ret_ptr = self
                    .f
                    .builder
                    .func
                    .special_param(ir::ArgumentPurpose::StructReturn)
                    .unwrap();

                self.write_entry_to_ptr(ret_ptr, 0, &entry);
                self.ins().return_(&[]);
            }
        }
    }

    fn params(&mut self, src: &[lir::Value]) -> Vec<Value> {
        let mut buf = Vec::with_capacity(src.len());
        src.iter().for_each(|p| self.param(*p, &mut buf));
        buf
    }

    fn param(&mut self, src: lir::Value, dst: &mut Vec<Value>) {
        let entry = self.value_to_entry(src);
        self.flatten_entry(&entry, dst);
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
                    .map(key::RecordField)
                    .map(|field| self.types().type_of_field(*mk, field));

                assert_eq!(bool_, MonoType::bool());
                Some((*mk, as_int(&int).signed))
            }
            MonoType::Int(_) | MonoType::Pointer(_) => None,
            _ => panic!("invalid return signature for num binop: {ty:?}"),
        };

        match with_check {
            Some((mk, true)) => {
                let (n, c) = signed(self);
                let fields = [n, c].into_iter().map(VField::Direct).collect();
                VEntry::Struct(mk, fields)
            }
            Some((mk, false)) => {
                let (n, c) = unsigned(self);
                let fields = [n, c].into_iter().map(VField::Direct).collect();
                VEntry::Struct(mk, fields)
            }
            None => VEntry::Direct(simple(self)),
        }
    }
}

type VEntry = abi::Entry<Value>;
type VField = abi::StructField<Value>;

fn as_int(ty: &MonoType) -> IntSize {
    match ty {
        MonoType::Int(bits) => *bits,
        _ => unreachable!(),
    }
}
