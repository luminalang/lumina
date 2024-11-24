use super::*;
use layout::SpecialPointer;

impl<'a, 's, 'f> InstHelper<'a, 's, 'f> {
    pub(super) fn indice_of_entry(&mut self, entry: VLayout, indice: Value) -> VLayout {
        match entry {
            Layout::SpecialPointer(kind, ptr) => match kind {
                SpecialPointer::StackArray(inner, len) => {
                    let (_, elem_size, _) = self.structs.size_and_align_of_array(&inner, len);
                    let indice_offset = self.ins().imul_imm(indice, elem_size as i64);
                    let nptr = self.ins().iadd(ptr, indice_offset);
                    self.deref_type(nptr, ByteOffset(0), &inner)
                }
                _ => panic!("attempted indice of non-array"),
            },
            _ => panic!("cannot access indice of: {entry:?}"),
        }
    }

    fn alloc_array(&mut self, inner: &MonoType, len: usize) -> (ir::StackSlot, u32) {
        let (size, elem_size, align) = self.structs.size_and_align_of_array(inner, len as u64);
        (self.create_struct_stack_slot(size, align as u8), elem_size)
    }
}

impl<'c, 'a, 'f> Translator<'c, 'a, 'f> {
    pub(super) fn construct_array(&mut self, values: &[lir::Value], inner: &MonoType) -> VLayout {
        if values.is_empty() {
            return Layout::ZST;
        }

        match self.ctx.structs.arr_pass_mode(values.len() as u64, inner) {
            PassBy::Pointer => {
                let ptr = self.construct_array_on_stack(inner, values);
                let kind = SpecialPointer::StackArray(inner.clone(), values.len() as u64);
                Layout::SpecialPointer(kind, ptr)
            }
            PassBy::Value => {
                let values = self.construct_array_in_regs(values);
                Layout::ArrayFlat(inner.clone(), values)
            }
            PassBy::Transparent(_) => unreachable!(),
        }
    }

    pub(super) fn construct_array_on_stack(
        &mut self,
        inner: &MonoType,
        values: &[lir::Value],
    ) -> Value {
        let (slot, elem_size) = self.ins().alloc_array(inner, values.len());
        let size_t = self.ctx.size_t();

        let mut offset = 0;

        for value in values.iter() {
            let entry = self.value_to_vlayout(*value);
            let slot_addr = self.cins().stack_addr(size_t, slot, offset);

            self.ins().write_vlayout_to_ptr(slot_addr, &entry);
            offset += elem_size as i32;
        }

        self.cins().stack_addr(size_t, slot, 0)
    }

    pub(super) fn construct_array_in_regs(&mut self, values: &[lir::Value]) -> Vec<VLayout> {
        values.iter().map(|v| self.value_to_vlayout(*v)).collect()
    }

    pub(super) fn replicate_array(
        &mut self,
        value: lir::Value,
        inner: &MonoType,
        times: u64,
    ) -> VLayout {
        let values = vec![value; times as usize];
        self.construct_array(&values, inner)
    }
}
