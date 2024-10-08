use super::*;

impl<'c, 'a, 'f> Translator<'c, 'a, 'f> {
    pub(super) fn indice_of_entry(&mut self, entry: VEntry, indice: Value) -> VEntry {
        match entry {
            VEntry::ArrayStackPointer(inner, len, ptr) => {
                let (_, elem_size, _) = self.ctx.structs.size_and_align_of_array(&inner, len);
                let indice_offset = self.ins().imul_imm(indice, elem_size as i64);
                let nptr = self.ins().iadd(ptr, indice_offset);
                self.deref_type(nptr, ByteOffset(0), &inner)
            }
            VEntry::ArrayFlat(_, _) => {
                todo!("we need to stackalloc it");
            }
            VEntry::StructFlat(_, _)
            | VEntry::StructHeapPointer(..)
            | VEntry::StructStackPointer(..) => {
                unimplemented!("access struct/tuple fields by indice")
            }
            _ => panic!("cannot access indice of: {entry:?}"),
        }
    }

    pub(super) fn construct_array(&mut self, values: &[lir::Value], inner: &MonoType) -> VEntry {
        if values.is_empty() {
            return VEntry::ZST;
        }

        match self.ctx.structs.arr_pass_mode(values.len() as u64, inner) {
            PassBy::Pointer => {
                let ptr = self.construct_array_on_stack(inner, values);
                VEntry::ArrayStackPointer(inner.clone(), values.len() as u64, ptr)
            }
            PassBy::Value => {
                let values = self.construct_array_in_regs(values);
                VEntry::ArrayFlat(inner.clone(), values)
            }
        }
    }

    pub(super) fn construct_array_on_stack(
        &mut self,
        inner: &MonoType,
        values: &[lir::Value],
    ) -> Value {
        let (slot, elem_size) = self.alloc_array(inner, values.len());
        let size_t = self.ctx.size_t();

        let mut offset = 0;

        for value in values.iter() {
            let entry = self.value_to_entry(*value);
            let slot_addr = self.ins().stack_addr(size_t, slot, offset);

            self.write_entry_to_ptr(slot_addr, &entry);
            offset += elem_size as i32;
        }

        self.ins().stack_addr(size_t, slot, 0)
    }

    fn alloc_array(&mut self, inner: &MonoType, len: usize) -> (ir::StackSlot, u32) {
        let (size, elem_size, align) = self.ctx.structs.size_and_align_of_array(inner, len as u64);
        (self.create_struct_stack_slot(size, align as u8), elem_size)
    }

    pub(super) fn construct_array_in_regs(&mut self, values: &[lir::Value]) -> Vec<VEntry> {
        values.iter().map(|v| self.value_to_entry(*v)).collect()
    }

    pub(super) fn replicate_array(
        &mut self,
        value: lir::Value,
        inner: &MonoType,
        times: u64,
    ) -> VEntry {
        let values = vec![value; times as usize];
        self.construct_array(&values, inner)
    }
}
