use super::*;

impl<'c, 'a, 'f> Translator<'c, 'a, 'f> {
    pub(super) fn alloc(&mut self, ty: &MonoType) -> VEntry {
        let MonoType::Pointer(ty) = ty else {
            panic!("attempted to allocate for non-pointer");
        };
        let size = self.ctx.structs.size_of(ty);
        let ptr = self.heap_alloc(size as i128);
        VEntry::Scalar(Scalar::pointer(ptr, &*ty))
    }

    pub(super) fn write_ptr(&mut self, ptr: lir::Value, value: lir::Value) {
        let v = self.value_to_entry(value);
        match self.value_to_entry(ptr) {
            VEntry::Scalar(Scalar { kind: ScalarKind::Pointer(_), point }) => {
                self.write_entry_to_ptr(point, &v)
            }
            entry => panic!("attempted write to non-ptr: {entry:?}"),
        }
    }

    pub(super) fn deref_ptr(&mut self, ptr: lir::Value, ty: &MonoType) -> VEntry {
        let (ptr, kind) = self.value_to_entry(ptr).as_intable();

        assert!(
            matches!(kind, ScalarKind::Pointer(_)),
            "should we allow builtin deref on int without cast?"
        );

        self.deref_type(ptr, ByteOffset(0), ty)
    }

    pub(super) fn deref_type(&mut self, ptr: Value, offset: ByteOffset, ty: &MonoType) -> VEntry {
        match self.deref_scalar_or_getm(ptr, offset, ty) {
            Either::Left(scalar) => VEntry::Scalar(scalar),
            Either::Right(mk) => {
                // Create a pointer to the inner struct
                let innerp = if offset == ByteOffset(0) {
                    ptr
                } else {
                    self.ins().iadd_imm(ptr, offset.0 as i64)
                };

                // Let's try just not dereferencing it here and let it happen lazily
                VEntry::StructStackPointer(mk, innerp)
            }
        }
    }

    pub(super) fn deref_scalar_or_getm(
        &mut self,
        ptr: Value,
        offset: ByteOffset,
        ty: &MonoType,
    ) -> Either<Scalar<Value>, MonoTypeKey> {
        self.ctx.structs.scalar_or_struct(ty).map_left(|scalar| {
            let point = self.deref_scalar(ptr, offset, scalar.point);
            Scalar::new(point, scalar.kind)
        })
    }

    pub(super) fn deref_scalar(&mut self, ptr: Value, offset: ByteOffset, ty: Type) -> Value {
        self.ins()
            .load(ty, MemFlags::trusted(), ptr, offset.0 as i32)
    }

    pub(super) fn deref_struct_into_raw(
        &mut self,
        for_each: &mut dyn FnMut(Scalar<Value>),
        key: MonoTypeKey,
        ptr: Value,
    ) {
        for field in self.ctx.structs.get(key).fields.keys() {
            let size_t = self.ctx.size_t();
            let offset = self.ctx.structs.offset_of(key, field);

            match self.ctx.structs.get(key).fields[field].clone() {
                FieldV::Flat(fty) => match self.deref_scalar_or_getm(ptr, offset, &fty) {
                    Either::Left(scalar) => for_each(scalar),
                    Either::Right(field_mk) => {
                        // Create a pointer to the inner struct
                        let innerp = self.ins().iadd_imm(ptr, offset.0 as i64);

                        // Then dereference fields from that inner struct
                        self.deref_struct_into_raw(for_each, field_mk, innerp);
                    }
                },

                // Since we're reading this from a pointer, we know that the sum pointer is S_Stable.
                FieldV::SumPayloadPointer { sum } => {
                    let largest = self.ctx.structs.sum_payload_alloca_size(sum);
                    let point = self.deref_scalar(ptr, offset, size_t);
                    let scalar = Scalar { point, kind: ScalarKind::HeapSumPointer { largest } };
                    for_each(scalar);
                }
                FieldV::SumPayloadInline(clty) => {
                    let point = self.deref_scalar(ptr, offset, clty);
                    let scalar = Scalar { point, kind: ScalarKind::SumInline(clty) };
                    for_each(scalar);
                }

                FieldV::AutoBoxed(to) => {
                    let point = self.deref_scalar(ptr, offset, size_t);
                    let scalar = Scalar::pointer(point, &to.into());
                    for_each(scalar);
                }
            }
        }
    }

    // S_Stable write of entry to pointer
    pub(super) fn write_entry_to_ptr(&mut self, dst: Value, entry: &VEntry) {
        match entry {
            &VEntry::StructStackPointer(key, src) | &VEntry::StructHeapPointer(key, src) => {
                let (size, align) = self.ctx.structs.size_and_align_of(&key.into());
                self.memcpy_struct(dst, src, size as u64, align as u8);
            }
            &VEntry::SumPayloadStackPointer { largest, ptr } => {
                // TODO: I think this can cause undefined behavior as we do not know the exact
                // alignment of the underlying struct.
                //
                // We probably need to set alignment of *all* varaint param tuple to that of
                // the *largest*.
                let nptr = self.memcpy_to_heap(ptr, largest as u64, 8, true);
                self.ins().store(MemFlags::trusted(), nptr, dst, 0);
            }
            VEntry::StructFlat(key, flat) => self.write_fields_to_structptr(*key, &flat, dst),
            VEntry::Scalar(scalar) => {
                self.ins().store(MemFlags::trusted(), scalar.point, dst, 0);
            }

            VEntry::ZST => {}
        }
    }

    pub(super) fn write_fields_to_structptr(
        &mut self,
        key: MonoTypeKey,
        fields: &Map<layout::Field, VEntry>,
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

    pub(super) fn memcpy_struct(&mut self, dst: Value, src: Value, size: u64, align: u8) {
        let config = self.ctx.isa.frontend_config();
        let flags = MemFlags::trusted();
        self.f
            .builder
            .emit_small_memory_copy(config, dst, src, size, align, align, true, flags);
    }

    pub(super) fn memcpy_to_heap(
        &mut self,
        src: Value,
        size: u64,
        align: u8,
        check_null: bool,
    ) -> Value {
        if !check_null {
            return self.memcpy_to_heap_unchecked(src, size, align);
        }

        let continuation = self.f.builder.create_block();
        let out = self
            .f
            .builder
            .append_block_param(continuation, self.ctx.size_t());

        let is_null = self.ins().icmp_imm(IntCC::Equal, src, 0);
        let [identity, allocate] = [(), ()].map(|_| self.f.builder.create_block());
        self.ins().brif(is_null, identity, &[], allocate, &[]);

        for block in [identity, allocate] {
            self.f.builder.seal_block(block)
        }

        self.f.builder.switch_to_block(identity);
        self.ins().jump(continuation, &[src]);

        self.f.builder.switch_to_block(allocate);
        let nptr = self.memcpy_to_heap_unchecked(src, size, align);
        self.ins().jump(continuation, &[nptr]);

        self.f.builder.seal_block(continuation);
        self.f.builder.switch_to_block(continuation);

        out
    }

    fn memcpy_to_heap_unchecked(&mut self, src: Value, size: u64, align: u8) -> Value {
        let nptr = self.heap_alloc(size as i128);
        self.memcpy_struct(nptr, src, size, align);
        return nptr;
    }

    pub(super) fn heap_alloc(&mut self, size: i128) -> Value {
        if size == 0 {
            let size_t = self.ctx.size_t();
            return self.ins().iconst(size_t, 0);
        }

        let alloc = self.ctx.lir.alloc;
        let fheader = self.ctx.funcmap[alloc].clone();

        let size = lir::Value::Int(size, IntSize::new(true, self.ctx.size_t().bits() as u8));

        let entry = self.call_func(fheader.id, fheader.typing, &[size]);

        match entry {
            VEntry::Scalar(Scalar { kind: ScalarKind::Pointer(_), point }) => point,
            _ => panic!("alloc has unexpected signature: {entry:?}"),
        }
    }

    pub(super) fn heap_dealloc(&mut self, ptr: lir::Value, size: i128) {
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
}
