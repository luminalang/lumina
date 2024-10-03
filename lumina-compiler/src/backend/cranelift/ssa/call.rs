use super::*;

impl<'c, 'a, 'f> Translator<'c, 'a, 'f> {
    pub(super) fn call_func(
        &mut self,
        id: FuncId,
        typing: layout::Typing,
        params: &[lir::Value],
    ) -> VEntry {
        let mut appl = self.params(params);
        let slot = self.append_rptr_if_needed(&typing.ret, &mut appl);

        let fref = self.declare_func_in_func(id);
        let call = self.ins().call(fref, &appl);

        self.map_call_results_to_ventry(call, &typing.ret, slot)
    }

    pub(super) fn call_func_pointer(
        &mut self,
        fptr: Value,
        typing: &layout::Typing,
        params: &[lir::Value],
    ) -> VEntry {
        let mut appl = self.params(params);
        let slot = self.append_rptr_if_needed(&typing.ret, &mut appl);

        let sig = self.ctx.structs.signature(&typing);
        let sigref = self.f.builder.import_signature(sig);
        let call = self.ins().call_indirect(sigref, fptr, &appl);

        self.map_call_results_to_ventry(call, &typing.ret, slot)
    }

    pub(super) fn call_entry(&mut self, entry: VEntry, params: &[lir::Value]) -> VEntry {
        match entry {
            VEntry::Scalar(Scalar { kind: ScalarKind::FuncPointer(typing), point }) => {
                self.call_func_pointer(point, &typing, params)
            }
            _ => panic!("call to non-function"),
        }
    }

    pub(super) fn params(&mut self, src: &[lir::Value]) -> Vec<Value> {
        let mut buf = Vec::with_capacity(src.len());
        src.iter().enumerate().for_each(|(i, p)| {
            let entry = self.value_to_entry(*p);
            trace!("{i} as {entry:?}");
            self.entry_to_fstable(false, &entry, &mut buf)
        });
        buf
    }

    fn struct_to_fstable(
        &mut self,
        key: MonoTypeKey,
        fields: &Map<layout::Field, VEntry>,
        dst: &mut Vec<Value>,
    ) {
        match self.ctx.structs.pass_mode(key) {
            PassBy::Pointer => {
                let (size, align) = self.ctx.structs.size_and_align_of(&key.into());
                let (slot, _) = self.create_struct_stack_slot(size, align as u8);
                let size_t = self.ctx.size_t();
                let ptr = self.ins().stack_addr(size_t, slot, 0);
                self.write_fields_to_structptr(key, fields, ptr);
                dst.push(ptr);
            }
            PassBy::Value => self.fields_to_fstable(fields, dst),
        }
    }

    fn fields_to_fstable(&mut self, fields: &Map<layout::Field, VEntry>, dst: &mut Vec<Value>) {
        fields
            .values()
            .for_each(|entry| self.entry_to_fstable(true, entry, dst))
    }

    fn entry_to_fstable(&mut self, embed: bool, entry: &VEntry, dst: &mut Vec<Value>) {
        match entry {
            // All scalars are assumed to be S_Stable
            VEntry::Scalar(scalar) => dst.push(scalar.point),
            VEntry::ZST => {}

            VEntry::StructFlat(mk, inner_fields) => {
                if embed {
                    self.fields_to_fstable(inner_fields, dst)
                } else {
                    // Difference is that this might append an implicit struct pointer to `dst` instead
                    self.struct_to_fstable(*mk, inner_fields, dst)
                }
            }

            // A `Struct` which we've implicitly been passing around as a pointer.
            &VEntry::StructHeapPointer(inner, ptr) | &VEntry::StructStackPointer(inner, ptr) => {
                match self.ctx.structs.pass_mode(inner) {
                    PassBy::Pointer => dst.push(ptr),
                    PassBy::Value => {
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
