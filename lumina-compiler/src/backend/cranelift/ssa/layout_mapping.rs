use super::*;
use layout::SpecialPointer;

impl<'f, 's, 'a> InstHelper<'f, 's, 'a> {
    pub fn layout_from_raw_values(
        &mut self,
        out_pointer: Option<Value>,
        layout: &Layout<Type>,
        values: &mut &[Value],
    ) -> VLayout {
        layout.map_layout(&mut |_| next_value(values), &mut |kind, _| {
            // Convert the out pointer to a normal special pointer
            let ptr = out_pointer.unwrap();
            Layout::SpecialPointer(kind, ptr)
        })
    }

    pub fn layout_into_raw_values<T: Clone>(&mut self, layout: &Layout<T>, buf: &mut Vec<T>) {
        self.scalars(layout, &mut |_, v| buf.push(v.clone()));
    }

    /// Visit all scalars (including special pointers) in this layout
    pub fn scalars<T>(&mut self, layout: &Layout<T>, f: &mut dyn FnMut(&mut Self, &T)) {
        match layout {
            Layout::AutoBoxed(_, v) | Layout::Scalar(_, v) => {
                f(self, v);
            }
            Layout::ZST => {}
            Layout::ArrayFlat(_, fields) => {
                fields.iter().for_each(|layout| self.scalars(layout, f))
            }
            Layout::StructFlat(_, fields) => {
                fields.values().for_each(|layout| self.scalars(layout, f))
            }
            Layout::SpecialPointer(_, v) => f(self, v),
            Layout::OutPointer(_, _) => {
                panic!("this seems dangerous. ");
            }
        }
    }

    pub fn make_compatible(&mut self, want: &Layout<Type>, have: Layout<Value>) -> Layout<Value> {
        self.make_compatible_allow_outptr(None, want, have)
    }

    pub fn make_compatible_allow_outptr(
        &mut self,
        outptr: Option<Value>,
        want: &Layout<Type>,
        have: Layout<Value>,
    ) -> Layout<Value> {
        macro_rules! expect {
            ($have:pat, $err:literal => $then:expr) => {
                match have {
                    $have => $then,
                    _ => panic!("want `{}`, have {:?}", $err, have),
                }
            };
        }

        match want {
            Layout::Scalar(kind, _) => match have {
                Layout::Scalar(k, v) if k == *kind => Layout::Scalar(k.clone(), v),
                _ => panic!("want `Scalar`, have {:?}", have),
            },
            Layout::ZST => expect!(Layout::ZST, "ZST" => Layout::ZST),
            Layout::SpecialPointer(kind, _) => match kind {
                SpecialPointer::StackSumPayload { .. } => match have {
                    Layout::SpecialPointer(kind, ptr) => {
                        match kind {
                            SpecialPointer::StackSumPayload { .. }
                            | SpecialPointer::HeapSumPayload { .. } => {}
                            _ => unreachable!(),
                        }
                        Layout::SpecialPointer(kind.clone(), ptr)
                    }
                    // Layout::Scalar(Scalar::SumPayloadInline)
                    _ => panic!("{want:?} {have:?}"),
                },
                SpecialPointer::HeapSumPayload { .. } => match have {
                    Layout::SpecialPointer(SpecialPointer::HeapSumPayload { sum }, ptr) => {
                        Layout::SpecialPointer(SpecialPointer::HeapSumPayload { sum }, ptr)
                    }
                    Layout::SpecialPointer(SpecialPointer::StackSumPayload { sum }, ptr) => {
                        let largest = self.structs.sum_payload_alloca_size(sum);
                        let size_t = self.size_t;
                        // TODO: figure out how we're meant to get the alignment here
                        let nptr =
                            self.memcpy_to_heap(ptr, largest as u64, size_t.bytes() as u8, true);
                        Layout::SpecialPointer(SpecialPointer::HeapSumPayload { sum }, nptr)
                    }
                    _ => panic!("want `SumPayload`, have {have:?}"),
                },
                SpecialPointer::StackStruct(mk) => {
                    let ptr = self.layout_to_struct_stack_pointer(*mk, &have);
                    Layout::SpecialPointer(SpecialPointer::StackStruct(*mk), ptr)
                }
                SpecialPointer::HeapStruct(mk) => {
                    let ptr = self.layout_to_struct_heap_pointer(*mk, &have);
                    Layout::SpecialPointer(SpecialPointer::HeapStruct(*mk), ptr)
                }
                SpecialPointer::StackArray(_, _) => todo!("array repr"),
            },
            Layout::AutoBoxed(ty, _) => {
                let ptr = self.layout_to_autoboxed(&ty, &have);
                Layout::AutoBoxed(ty.clone(), ptr)
                // buf.push(ptr);
            }
            Layout::ArrayFlat(inner, fields) => {
                expect!(Layout::ArrayFlat(ginner, gfields), "ArrayFlat" => {
                    assert_eq!((inner, fields.len()), (&ginner, gfields.len()));
                    let fields = fields.iter().zip(gfields).map(|(e, g)| self.make_compatible_allow_outptr(outptr, e, g)).collect();
                    Layout::ArrayFlat(ginner, fields)
                })
            }
            Layout::StructFlat(mk, fields) => match have {
                Layout::StructFlat(gmk, gfields) => {
                    assert_eq!((mk, fields.len()), (&gmk, gfields.len()));
                    let fields = fields
                        .values()
                        .zip(gfields)
                        .map(|(e, (_, g))| self.make_compatible_allow_outptr(outptr, e, g))
                        .collect();
                    Layout::StructFlat(gmk, fields)
                }
                Layout::SpecialPointer(
                    SpecialPointer::StackStruct(gmk) | SpecialPointer::HeapStruct(gmk),
                    ptr,
                ) => {
                    assert_eq!(gmk, *mk);
                    let fields =
                        self.get_fields_from_structptr(outptr, ptr, ByteOffset(0), *mk, fields);
                    VLayout::StructFlat(*mk, fields)
                }
                _ => panic!("want `Struct`, have `{have:?}`"),
            },
            Layout::OutPointer(kind, _) => match outptr {
                None => panic!("attempted to pass an out pointer as value"),
                Some(dst) => {
                    match have {
                        // TODO: This needs to be this way for things to work.
                        // But; `get_field_from_structptr` actually does things differently.
                        // And upon porting that code to here, everything segfaults.
                        // I'm not sure why. Because; it doesn't make sense to treat both payload
                        // pointers and implicit struct pointers the exact same way no?
                        //
                        // Perhaps there's an inconsistency but it happens to work because the
                        // first implementation only works for structs while the second
                        // implementation only works for sums.
                        Layout::SpecialPointer(_, src) => {
                            let (size, align) = self.structs.size_and_align_of_ptr_dst(kind);
                            self.memcpy_struct(dst, src, size as u64, align as u8);
                            Layout::OutPointer(kind.clone(), dst)
                            // self.write_special_pointer_to_out_pointer(outptr, src, ByteOffset(0), kind)
                        }
                        Layout::StructFlat(_, _) => todo!("TODO: write fields to outptr"),
                        Layout::ArrayFlat(_, _) => todo!(),
                        _ => unreachable!(),
                    }
                }
            },
        }
    }

    fn write_special_pointer_to_out_pointer(
        &mut self,
        outptr: Option<Value>,
        src: Value,
        offset: ByteOffset,
        kind: &SpecialPointer,
    ) -> VLayout {
        match outptr {
            None => panic!("attempted to pass an out pointer as value"),
            Some(dst) => {
                let (size, align) = self.structs.size_and_align_of_ptr_dst(kind);
                match kind {
                    // we have a sum payload represented as a pointer.
                    // memcpy the data stored behind that payload pointer into dst.
                    SpecialPointer::StackSumPayload { .. }
                    | SpecialPointer::HeapSumPayload { .. } => {
                        // copy the sum payload pointer
                        let flags = MemFlags::trusted();
                        let size_t = self.size_t;
                        let innerp = self.ins().load(size_t, flags, src, offset.0 as i32);
                        self.memcpy_struct(dst, innerp, size as u64, align as u8);
                        Layout::OutPointer(kind.clone(), dst)
                    }
                    _ => {
                        // We have a by-value struct represented as a pointer.
                        // Memcpy it.
                        let innerp = self.ptr_offset(src, offset);
                        self.memcpy_struct(dst, innerp, size as u64, align as u8);
                        Layout::OutPointer(kind.clone(), dst)
                    }
                }
            }
        }
    }

    fn get_fields_from_structptr(
        &mut self,
        outptr: Option<Value>,
        ptr: Value,
        offset: ByteOffset,
        mk: MonoTypeKey,
        fields: &Map<layout::Field, Layout<Type>>,
    ) -> Map<layout::Field, VLayout> {
        fields
            .iter()
            .map(|(field, flayout)| {
                let mut foffset = self.structs.offset_of(mk, field);
                foffset.0 += offset.0;
                self.get_field_from_structptr(outptr, flayout, ptr, foffset)
            })
            .collect()
    }

    // NOTE: This is *not* allowed to return special pointers because it's used to directly collect
    // flatstructs.
    pub(super) fn get_field_from_structptr(
        &mut self,
        outptr: Option<Value>,
        flayout: &Layout<Type>,
        ptr: Value,
        offset: ByteOffset,
    ) -> VLayout {
        match flayout {
            Layout::Scalar(kind, clty) => {
                let v = self
                    .ins()
                    .load(*clty, MemFlags::trusted(), ptr, offset.0 as i32);
                VLayout::Scalar(kind.clone(), v)
            }
            Layout::AutoBoxed(ty, clty) => {
                let v = self
                    .ins()
                    .load(*clty, MemFlags::trusted(), ptr, offset.0 as i32);
                VLayout::AutoBoxed(ty.clone(), v)
            }
            Layout::ZST => VLayout::ZST,
            Layout::ArrayFlat(_, _) => todo!(),
            Layout::StructFlat(mk, fields) => {
                let fields = self.get_fields_from_structptr(outptr, ptr, offset, *mk, fields);
                Layout::StructFlat(*mk, fields)
            }
            Layout::SpecialPointer(kind, clty) => {
                let v = self
                    .ins()
                    .load(*clty, MemFlags::trusted(), ptr, offset.0 as i32);
                VLayout::SpecialPointer(kind.clone(), v)
            }
            Layout::OutPointer(kind, _) => {
                self.write_special_pointer_to_out_pointer(outptr, ptr, offset, kind)
            }
        }
    }

    fn layout_to_struct_stack_pointer(&mut self, mk: MonoTypeKey, got: &VLayout) -> Value {
        match got {
            Layout::AutoBoxed(ty, ptr) => {
                assert_eq!(ty.as_key(), mk);
                *ptr
            }
            Layout::StructFlat(key, fields) => {
                assert_eq!(*key, mk);
                let dst = self.stack_alloc_type(&MonoType::Monomorphised(mk));
                self.write_fields_to_structptr(mk, fields, dst);
                dst
            }
            &Layout::SpecialPointer(
                SpecialPointer::StackStruct(key) | SpecialPointer::HeapStruct(key),
                ptr,
            ) => {
                assert_eq!(mk, key);
                ptr
            }
            Layout::ZST => {
                assert!(self.structs.is_zst(mk));
                let size_t = self.size_t;
                self.ins().iconst(size_t, 0)
            }
            _ => panic!("not a struct: {got:?}"),
        }
    }

    fn layout_to_struct_heap_pointer(&mut self, mk: MonoTypeKey, got: &VLayout) -> Value {
        match got {
            Layout::AutoBoxed(ty, ptr) => {
                assert_eq!(ty.as_key(), mk);
                *ptr
            }
            Layout::StructFlat(key, fields) => {
                assert_eq!(*key, mk);
                let dst = self.heap_alloc_type(&MonoType::Monomorphised(mk));
                self.write_fields_to_structptr(mk, fields, dst);
                dst
            }
            Layout::SpecialPointer(SpecialPointer::HeapStruct(key), ptr) => {
                assert_eq!(mk, *key);
                *ptr
            }
            Layout::SpecialPointer(SpecialPointer::StackStruct(key), ptr) => {
                assert_eq!(mk, *key);
                let (size, align) = self.structs.size_and_align_of_mk(mk);
                let dst = self.heap_alloc_type(&MonoType::Monomorphised(mk));
                self.memcpy_struct(dst, *ptr, size as u64, align as u8);
                dst
            }
            _ => panic!("not a struct: {got:?}"),
        }
    }

    fn layout_to_autoboxed(&mut self, ty: &MonoType, got: &VLayout) -> Value {
        match ty {
            MonoType::Monomorphised(mk) => self.layout_to_struct_heap_pointer(*mk, got),
            _ => unimplemented!("autoboxing of non-structs"),
        }
    }
}
