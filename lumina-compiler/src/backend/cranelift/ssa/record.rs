use super::*;
use layout::SpecialPointer;

impl<'c, 'a, 'f> Translator<'c, 'a, 'f> {
    pub(super) fn construct_record(&mut self, key: MonoTypeKey, values: &[lir::Value]) -> VLayout {
        debug_assert_eq!(values.len(), self.ctx.structs.get(key).fields.len());

        if values.is_empty() {
            return Layout::ZST;
        }

        match self.ctx.structs.pass_mode(key) {
            PassBy::Pointer => {
                let ptr = self.construct_record_on_stack(None, key, values);
                Layout::SpecialPointer(SpecialPointer::StackStruct(key), ptr)
            }
            PassBy::Value => {
                let fields = self.construct_record_in_regs(key, values);
                Layout::StructFlat(key, fields)
            }
            PassBy::Transparent(_) => {
                assert_eq!(values.len(), 1);
                let field = self.value_to_vlayout(values[0]);
                Layout::StructFlat(key, [field].into())
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

        let slot = self.ins().create_struct_stack_slot(size, align as u8);
        let size_t = self.ctx.size_t();

        for (field, value) in values.iter().enumerate() {
            let field = key::Field(field as u32);
            let vlayout = self.value_to_vlayout(*value);

            let abi_field = self.ctx.structs.get_real_field(key, field);
            let offset = self.ctx.structs.offset_of(key, abi_field);

            let slot_addr = self.cins().stack_addr(size_t, slot, offset.0 as i32);

            self.ins().write_vlayout_to_ptr(slot_addr, &vlayout);
        }

        self.cins().stack_addr(size_t, slot, 0)
    }

    pub(super) fn construct_record_in_regs(
        &mut self,
        key: MonoTypeKey,
        values: &[lir::Value],
    ) -> Map<layout::Field, VLayout> {
        let mut fields = Map::<layout::Field, _>::with_capacity(values.len());

        for fi in self.ctx.structs.get(key).fields.keys() {
            let struct_ = self.ctx.structs.get(key);
            let fieldkey = struct_.field_map.find(|i| *i == fi).unwrap();

            let matching = values[fieldkey.0 as usize];
            let vlayout = self.value_to_vlayout(matching);

            fields.push(vlayout);
        }

        fields
    }
}

impl<'f, 's, 'a> InstHelper<'f, 's, 'a> {
    pub(super) fn field_of_vlayout(&mut self, vlayout: VLayout, field: key::Field) -> VLayout {
        match vlayout {
            Layout::SpecialPointer(
                SpecialPointer::StackStruct(mk) | SpecialPointer::HeapStruct(mk),
                ptr,
            ) => self.field_of_structptr(mk, ptr, field),

            Layout::StructFlat(mk, fields) => {
                let rfield = self.structs.get_real_field(mk, field);
                fields[rfield].clone()
            }

            Layout::AutoBoxed(ty, ptr) => self.field_of_structptr(ty.as_key(), ptr, field),
            // We can't assert here so I really hope we're never accidentally constructing ZST incorrectly
            Layout::ZST => VLayout::ZST,
            other => panic!("field of non-struct: {other:?}"),
        }
    }

    fn field_of_structptr(&mut self, mk: MonoTypeKey, ptr: Value, field: key::Field) -> VLayout {
        let rfield = self.structs.get_real_field(mk, field);
        let offset = self.structs.offset_of(mk, rfield);

        let fty = &self.structs.records[mk].as_record()[field].clone();
        self.deref_type(ptr, offset, fty)
    }
}
