use super::*;

impl<'c, 'a, 'f> Translator<'c, 'a, 'f> {
    pub(super) fn construct_record(&mut self, key: MonoTypeKey, values: &[lir::Value]) -> VEntry {
        debug_assert_eq!(values.len(), self.ctx.structs.get(key).fields.len());

        if values.is_empty() {
            return VEntry::ZST;
        }

        match self.ctx.structs.pass_mode(key) {
            PassBy::Pointer => {
                let ptr = self.construct_record_on_stack(None, key, values);
                VEntry::StructStackPointer(key, ptr)
            }
            PassBy::Value => {
                let fields = self.construct_record_in_regs(key, values);
                VEntry::StructFlat(key, fields)
            }
        }
    }

    pub(super) fn construct_record_on_stack(
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
        let size_t = self.ctx.size_t();

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

    pub(super) fn construct_record_in_regs(
        &mut self,
        key: MonoTypeKey,
        values: &[lir::Value],
    ) -> Map<layout::Field, VEntry> {
        let mut fields = Map::<layout::Field, _>::with_capacity(values.len());

        for fi in self.ctx.structs.get(key).fields.keys() {
            let struct_ = self.ctx.structs.get(key);
            let fieldkey = struct_.field_map.find(|i| *i == fi).unwrap();

            let matching = values[fieldkey.0 as usize];
            let entry = self.value_to_entry(matching);

            fields.push(entry);
        }

        fields
    }

    pub(super) fn field_of_entry(&mut self, entry: VEntry, field: key::Field) -> VEntry {
        match entry {
            VEntry::StructStackPointer(mk, ptr) | VEntry::StructHeapPointer(mk, ptr) => {
                let rfield = self.ctx.structs.get_real_field(mk, field);
                let offset = self.ctx.structs.offset_of(mk, rfield);

                let fty = &self.ctx.structs.records[mk].as_record()[field].clone();
                self.deref_type(ptr, offset, fty)
            }
            VEntry::StructFlat(mk, fields) => {
                let rfield = self.ctx.structs.get_real_field(mk, field);
                fields[rfield].clone()
            }
            other => panic!("field of non-struct: {other:?}"),
        }
    }
}
