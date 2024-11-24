use super::*;
use layout::SpecialPointer;

impl<'a, 's, 'f> InstHelper<'a, 's, 'f> {
    pub(super) fn heap_alloc_type(&mut self, inner: &MonoType) -> Value {
        let size = self.structs.size_of(inner);
        self.heap_alloc(size as i128)
    }

    pub(super) fn heap_alloc(&mut self, size: i128) -> Value {
        let size_t = self.size_t;

        if size == 0 {
            return self.ins().iconst(size_t, 0);
        }

        let id = self.alloc;

        let size = self.ins().iconst(size_t, size as i64);

        let fref = self.declare_func_in_func(id);
        let call = self.ins().call(fref, &[size]);
        let [ptr] = self.builder.inst_results(call) else {
            panic!("alloc has unexpected signature");
        };

        assert_eq!(self.type_of_value(*ptr), size_t);

        *ptr
    }

    pub(super) fn stack_alloc_type(&mut self, ty: &MonoType) -> Value {
        let (size, align) = self.structs.size_and_align_of(ty);
        self.stack_alloc(size, align as u8)
    }

    pub(super) fn stack_alloc(&mut self, size: u32, align: u8) -> Value {
        let slot = self.create_struct_stack_slot(size, align as u8);
        let size_t = self.size_t;
        self.ins().stack_addr(size_t, slot, 0)
    }

    pub(super) fn deref_type(&mut self, ptr: Value, offset: ByteOffset, ty: &MonoType) -> VLayout {
        let layout = self.structs.type_to_layout(ty, Stability::S);
        self.deref(ptr, offset, layout)
    }

    /// Reads `layout` at `offset` from `ptr`
    pub(super) fn deref(
        &mut self,
        ptr: Value,
        offset: ByteOffset,
        layout: Layout<Type>,
    ) -> VLayout {
        match layout {
            Layout::AutoBoxed(_, _) => panic!("???"),
            Layout::SpecialPointer(kind, _) => match kind {
                // We forward both Heap and Stack structs as a Stack struct since then we don't
                // risk the offsetted heap pointer outliving the original heap pointer.
                SpecialPointer::HeapStruct(mk) | SpecialPointer::StackStruct(mk) => {
                    let innerp = self.ptr_offset(ptr, offset);
                    let kind = SpecialPointer::StackStruct(mk);
                    Layout::SpecialPointer(kind, innerp)
                }
                SpecialPointer::StackArray(_, _) => todo!(),
                _ => panic!("direct read from opaque sum data"),
            },
            Layout::ArrayFlat(_, _) => {
                todo!("properly find the corresponding `fields` instead");
                // let innerp = self.ptr_offset(ptr, offset);
                // Layout::ArrayStackPointer(inner, fields.len() as u64, innerp)
            }

            Layout::StructFlat(mk, fields) => {
                fields.values().for_each(Layout::assert_no_stack_leak);
                let innerp = self.ptr_offset(ptr, offset);
                Layout::SpecialPointer(SpecialPointer::StackStruct(mk), innerp)
            }
            Layout::ZST => Layout::ZST,
            Layout::OutPointer(..) => panic!("cant read from OutPointer"),
            Layout::Scalar(kind, clty) => {
                let v = self
                    .ins()
                    .load(clty, MemFlags::trusted(), ptr, offset.0 as i32);
                Layout::Scalar(kind.clone(), v)
            }
        }
    }

    pub(super) fn ptr_offset(&mut self, ptr: Value, offset: ByteOffset) -> Value {
        if offset == ByteOffset(0) {
            ptr
        } else {
            self.ins().iadd_imm(ptr, offset.0 as i64)
        }
    }

    // S_Stable write of layout to pointer
    pub fn write_vlayout_to_ptr(&mut self, dst: Value, vlayout: &VLayout) {
        match vlayout {
            Layout::SpecialPointer(kind, ptr) => match kind {
                &SpecialPointer::StackSumPayload { sum } => {
                    let largest = self.structs.sum_payload_alloca_size(sum);
                    let (tag_size, _, _) = self.structs.records[sum].as_sum();
                    let nptr = self.heaplift_sum_payload(*ptr, tag_size.bytes(), largest);
                    self.ins().store(MemFlags::trusted(), nptr, dst, 0);
                }
                SpecialPointer::HeapSumPayload { .. } => {
                    self.ins().store(MemFlags::trusted(), *ptr, dst, 0);
                }
                SpecialPointer::HeapStruct(key) | SpecialPointer::StackStruct(key) => {
                    let (size, align) = self.structs.size_and_align_of_mk(*key);
                    self.memcpy_struct(dst, *ptr, size as u64, align as u8);
                }
                SpecialPointer::StackArray(inner, len) => {
                    let (size, _, align) = self.structs.size_and_align_of_array(inner, *len);
                    self.memcpy_struct(dst, *ptr, size as u64, align as u8)
                }
            },

            Layout::StructFlat(key, flat) => self.write_fields_to_structptr(*key, &flat, dst),
            Layout::ArrayFlat(inner, flat) => self.write_elems_to_arrayptr(inner, flat, dst),

            Layout::ZST => {}
            Layout::Scalar(_, v) => {
                self.ins().store(MemFlags::trusted(), *v, dst, 0);
            }

            Layout::AutoBoxed(_, _) => todo!("memcpy underlying type of autoboxed value"),
            Layout::OutPointer(..) => panic!("attempted to write out pointer"),
        }
    }

    pub(super) fn write_elems_to_arrayptr(
        &mut self,
        inner: &MonoType,
        flat: &[VLayout],
        ptr: Value,
    ) {
        let (_, elem_size, align) = self
            .structs
            .size_and_align_of_array(inner, flat.len() as u64);

        let mut offset = 0;

        for flayout in flat {
            let ptr = self.ptr_offset(ptr, ByteOffset(offset));
            self.write_vlayout_to_ptr(ptr, flayout);
            let padding = (align - offset % align) % align;
            offset += elem_size + padding;
        }
    }

    pub(super) fn write_fields_to_structptr(
        &mut self,
        key: MonoTypeKey,
        fields: &Map<layout::Field, VLayout>,
        ptr: Value,
    ) {
        for (field, flayout) in fields.iter() {
            let offset = self.structs.offset_of(key, field);
            let ptr = self.ptr_offset(ptr, offset);
            self.write_vlayout_to_ptr(ptr, flayout);
        }
    }
}
