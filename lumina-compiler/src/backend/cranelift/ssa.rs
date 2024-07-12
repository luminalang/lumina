use super::{abi, Context};
use crate::prelude::*;
use cranelift::frontend::FuncInstBuilder;
use cranelift::prelude::*;
use cranelift_codegen::ir;
use cranelift_module::{FuncId, Module};
use lir::{BitOffset, MonoFunc, MonoType, MonoTypeKey};
use lumina_typesystem::Bitsize;
use lumina_util::Highlighting;

pub struct Translator<'c, 'a, 'f> {
    ctx: &'c mut Context<'a>,
    f: Current<'a, 'f>,
}

#[derive(new)]
struct Current<'a, 'f> {
    func: &'a lir::Function,
    builder: FunctionBuilder<'f>,
    blockmap: Map<lir::Block, (Block, bool)>,
    abi_block_types: Map<lir::Block, Map<key::Param, abi::Param>>,
    #[new(default)]
    vmap: Map<lir::V, VEntry>,
    #[new(value = "lir::Block::entry()")]
    block: lir::Block,
}

impl<'a, 'f> Current<'a, 'f> {
    fn switch_to_block(&mut self, block: lir::Block, seal: bool) {
        let (clblock, filled) = self.blockmap[block];
        assert!(!filled);
        self.builder.switch_to_block(clblock);
        self.block = block;
        if seal {
            self.builder.seal_block(clblock);
        }
    }

    fn type_of_value(&self, value: Value) -> Type {
        self.builder.func.stencil.dfg.value_type(value)
    }

    fn skip_to_offset<'p>(&self, mut offset: BitOffset, mut values: &'p [Value]) -> &'p [Value] {
        while offset.0 != 0 {
            offset.0 -= self.builder.func.dfg.value_type(values[0]).bits();
            values = &values[1..];
        }
        values
    }
}

#[derive(Clone, Debug)]
enum ValuePlus {
    Value(Value),
    FlatStruct(MonoTypeKey, Vec<Value>),
    FlatArray(Vec<Value>),
    ArrayPtr(Value),
    StructStackPtr(MonoTypeKey, Value),
    InlineSum([Value; 2]),
    SumStackPtr { tag: Value, payload: Value },
}

impl ValuePlus {
    fn as_value(&self) -> Value {
        match self {
            Self::Value(value) => *value,
            _ => panic!("not a value"),
        }
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
                    .get_abi_params(triple, func.blocks.params(block).values())
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

                func.blocks.params(block).values().for_each(|t| {
                    let v = ctx.lir.types.abi_param(triple, t);
                    v.visit(|ty| {
                        builder.append_block_param(clblock, ty);
                    })
                });

                if block == lir::Block::entry() {
                    if let abi::Return::StructOutPtr(_, _) = &header.ret {
                        builder.append_block_param(clblock, Type::triple_pointer_type(triple));
                    }
                }

                (clblock, false)
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
        self.f.switch_to_block(self.f.block, true);
        self.block();

        // Blocks used as continuation in decision trees aren't sealed while lowering.
        // In the future we should mark which branch is the last one so we can seal it during
        // codegen instead.
        self.f.builder.seal_all_blocks();

        self.f.builder.finalize();
    }

    fn block(&mut self) {
        assert_eq!(
            std::mem::replace(&mut self.f.blockmap[self.f.block].1, true),
            false
        );

        for v in self.f.func.blocks.entries(self.f.block) {
            let entry = self.f.func.blocks.entry_of(v);
            let ty = self.f.func.blocks.type_of(v);
            trace!("lowering {v} {} {entry} : {ty:?}", '='.symbol());
            let entry = self.entry(entry, ty);
            self.f.vmap.push_as(v, entry);
        }

        let flow = self.f.func.blocks.flow_of(self.f.block);
        self.flow(flow)
    }

    fn declare_func_in_func(&mut self, id: FuncId) -> ir::FuncRef {
        self.ctx
            .objmodule
            .declare_func_in_func(id, &mut self.f.builder.func)
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

    fn entry(&mut self, entry: &lir::Entry, ty: &MonoType) -> VEntry {
        // Let's try a `V -> Data` mapping instead
        match entry {
            lir::Entry::Copy(value) => self.value_to_entry(*value),
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
                    _ => panic!("not a fnpointer: {ptr:?}"),
                }
            }
            lir::Entry::Construct(values) => match ty {
                MonoType::Monomorphised(mk) => self.construct_record(*mk, values),
                &MonoType::SumDataCast { largest } => {
                    let ptr = self.heap_alloc(largest as i128);
                    // todo!("... wait a second, why are we constructing this?");

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
                    // VEntry::Direct(ptr) => self.deref_type(ptr, *offset, ty).0,
                    VEntry::SumPayload { ptr, .. } => self.deref_type(ptr, *offset, ty).0,
                    other => panic!("impossible source of sum data payload: {other:#?}"),
                },
                _ => panic!("impossible source of sum data payload"),
            },

            lir::Entry::IntAdd(left, right) => {
                let [left, right] = [*left, *right].map(|v| self.value_to_entry(v).as_direct());
                let v = self.ins().iadd(left, right);
                VEntry::Direct(v)
            }
            lir::Entry::IntSub(left, right) => {
                let [left, right] = [*left, *right].map(|v| self.value_to_entry(v).as_direct());
                let v = self.ins().isub(left, right);
                VEntry::Direct(v)
            }
            lir::Entry::IntMul(left, right) => {
                let [left, right] = [*left, *right].map(|v| self.value_to_entry(v).as_direct());
                let v = self.ins().imul(left, right);
                VEntry::Direct(v)
            }
            lir::Entry::IntDiv(left, right) => {
                let [left, right] = [*left, *right].map(|v| self.value_to_entry(v).as_direct());
                let v = if as_int(ty).0 {
                    self.ins().sdiv(left, right)
                } else {
                    self.ins().udiv(left, right)
                };
                VEntry::Direct(v)
            }
            lir::Entry::IntCmpInclusive(left, cmp, right, bitsize) => {
                let [left, right] = [*left, *right].map(|v| self.value_to_entry(v).as_direct());
                let intty = Type::int(bitsize.0 as u16).unwrap();
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
                let ty = Type::int(as_int(ty).1 .0 as u16).unwrap();
                let v = self.ins().ireduce(ty, v);
                VEntry::Direct(v)
            }
            lir::Entry::ExtendSigned(v) => {
                let v = self.value_to_entry(*v).as_direct();
                let ty = Type::int(as_int(ty).1 .0 as u16).unwrap();
                let v = self.ins().sextend(ty, v);
                VEntry::Direct(v)
            }
            lir::Entry::ExtendUnsigned(v) => {
                let v = self.value_to_entry(*v).as_direct();
                let ty = Type::int(as_int(ty).1 .0 as u16).unwrap();
                let v = self.ins().uextend(ty, v);
                VEntry::Direct(v)
            }
            lir::Entry::BitAnd(_) => todo!(),
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
            VEntry::SumPayload { largest, ptr } => {
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

        let size = lir::Value::Int(size, Bitsize(self.ctx.pointer_type().bits() as u8));

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

        let size = lir::Value::Int(size, Bitsize(triple.pointer_width().unwrap().bits()));

        let entry = self.call_func(fheader.id, fheader.ret, &[ptr, size]);
        match entry {
            abi::Entry::ZST => {}
            _ => panic!("dealloc has unexpected signature"),
        }
    }

    fn value_to_entry(&mut self, value: lir::Value) -> VEntry {
        match value {
            lir::Value::BlockParam(bparam) => {
                let mut raw_params = self
                    .f
                    .builder
                    .block_params(self.f.blockmap[self.f.block].0)
                    .iter();

                // Skip values that come before this block param
                for bparam in 0..bparam.0 {
                    let bparam = lir::BlockParam(bparam);
                    self.f.abi_block_types[self.f.block][key::Param(bparam.0)].visit(|_ty| {
                        raw_params.next();
                    });
                }

                // Then Map the values from the start to the entry
                self.f.abi_block_types[self.f.block][key::Param(bparam.0)]
                    .map(&mut |_type| *raw_params.next().unwrap())
            }
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
                let fptr = self.fptr_to_value(mfunc);
                VEntry::Direct(fptr)
            }
            lir::Value::Int(n, bitsize) => {
                let ty = Type::int(bitsize.0 as u16).unwrap();
                VEntry::Direct(self.ins().iconst(ty, n as i64))
            }
            lir::Value::UInt(n, bitsize) => {
                let ty = Type::int(bitsize.0 as u16).unwrap();
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
            lir::ControlFlow::JmpBlock(block, is_continuation, params) => {
                let (clblock, filled) = self.f.blockmap[*block];
                let params = self.params(params);

                self.ins().jump(clblock, &params);

                if !filled {
                    self.f.switch_to_block(*block, !*is_continuation);
                    self.block();
                }
            }
            lir::ControlFlow::Unreachable => todo!("handle unreachable?"),
            lir::ControlFlow::Return(v) => {
                let entry = self.value_to_entry(*v);
                self.return_(entry);
            }
            lir::ControlFlow::Select { value, on_true, on_false } => {
                let int = self.value_to_entry(*value).as_direct();

                let block_then = self.f.blockmap[on_true.0].0;
                let then_params = self.params(&on_true.1);

                let block_else = self.f.blockmap[on_false.0].0;
                let else_params = self.params(&on_false.1);

                self.ins()
                    .brif(int, block_then, &then_params, block_else, &else_params);

                for block in [on_true.0, on_false.0] {
                    let (_, filled) = self.f.blockmap[block];
                    if !filled {
                        self.f.switch_to_block(block, true);
                        self.block();
                    }
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

                self.f.builder.switch_to_block(def);
                self.f.builder.seal_block(def);
                self.ins().trap(TrapCode::UnreachableCodeReached);

                for block in blocks {
                    let (_, filled) = self.f.blockmap[*block];
                    if !filled {
                        self.f.switch_to_block(*block, true);
                        self.block();
                    }
                }
            }
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

    fn fptr_to_value(&mut self, mfunc: MonoFunc) -> Value {
        let header = &self.ctx.funcmap[mfunc];
        let fref = self.declare_func_in_func(header.id);
        let ptr_type = self.ctx.pointer_type();
        self.ins().func_addr(ptr_type, fref)
    }
}

type VEntry = abi::Entry<Value>;
type VField = abi::StructField<Value>;

fn as_int(ty: &MonoType) -> (bool, Bitsize) {
    match ty {
        MonoType::Int(bits) => (true, *bits),
        MonoType::UInt(bits) => (false, *bits),
        _ => unreachable!(),
    }
}
