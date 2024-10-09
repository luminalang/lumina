use super::*;

impl<'c, 'a, 'f> Translator<'c, 'a, 'f> {
    pub(super) fn construct_variant(
        &mut self,
        key: MonoTypeKey,
        var: key::Variant,
        values: &[lir::Value],
    ) -> VEntry {
        let lir::MonoTypeData::Sum { tag, variants, .. } = &self.ctx.structs.records[key] else {
            panic!("attempted to construct variant of non-sum");
        };

        let param_tuple = variants[var];

        let tagt = Type::int(tag.bits() as u16).unwrap();
        let tag = self.ins().iconst(tagt, var.0 as i64);
        let tagfield = VEntry::direct(tag);

        let sum_struct = self.ctx.structs.get(key);

        match sum_struct.field_map.as_slice() {
            [_tagi] => VEntry::StructFlat(key, [tagfield].into()),
            [_tagi, payloadi] => {
                let payload = match &sum_struct.fields[*payloadi] {
                    &FieldV::SumPayloadPointer { sum } => {
                        let largest = self.ctx.structs.sum_payload_alloca_size(sum);
                        let ptr =
                            self.construct_record_on_stack(Some(largest), param_tuple, values);
                        VEntry::SumPayloadStackPointer { largest, ptr }
                    }
                    &FieldV::SumPayloadInline(clty) => {
                        let inline = self.compress_into_inline_payload(clty, param_tuple, values);
                        VEntry::Scalar(Scalar::new(inline, ScalarKind::SumInline(clty)))
                    }
                    other => unreachable!("{other:?}"),
                };
                VEntry::StructFlat(key, [payload, tagfield].into())
            }
            other => unreachable!("bad field count for sum type: {other:?}"),
        }
    }

    // Creates a payload compressed scalar from a list of values
    fn compress_into_inline_payload(
        &mut self,
        clty: Type,
        param_tuple: MonoTypeKey,
        values: &[lir::Value],
    ) -> Value {
        let fields = self.construct_record_in_regs(param_tuple, values);
        match fields.as_slice() {
            [] | [VEntry::ZST] => self.ins().iconst(clty, 0),
            [VEntry::Scalar(scalar)] => {
                match scalar.kind {
                    ScalarKind::Direct => {
                        let has = self.f.type_of_value(scalar.point);
                        if has == types::F64 {
                            todo!("we need to cast as well");
                        } else {
                            assert!(has.is_int());
                            self.resize_uint(scalar.point, clty)
                        }
                    }
                    ScalarKind::SumInline(_) => unreachable!(),

                    // Pointers
                    _ => scalar.point,
                }
            }
            [_, _] => todo!("compress multiple small variants into one sum payload"),
            _ => panic!("cannot compress variants with 3 or more fields into inline payload"),
        }
    }

    pub(super) fn cast_from_sum(&mut self, sum: VEntry, ty: &MonoType) -> VEntry {
        match sum {
            // Sum types without a payload
            VEntry::StructFlat(key, params) if params.len() == 1 => {
                assert_eq!(self.ctx.structs.get(key).fields.len(), 1);
                assert_eq!(self.ctx.structs.size_of(ty), 0);
                VEntry::ZST
            }
            VEntry::StructFlat(key, params) => {
                let payi = self.ctx.structs.get_real_field(key, key::Field(1));
                let payload = &params[payi];

                match payload {
                    // Simply cast the associated type
                    //
                    // we don't use `Scalar::pointer` because it's of type `Struct` and not `*Struct` in LIR
                    &VEntry::SumPayloadStackPointer { ptr, .. } => {
                        VEntry::StructStackPointer(ty.as_key(), ptr)
                    }
                    &VEntry::Scalar(Scalar {
                        point: ptr,
                        kind: ScalarKind::HeapSumPointer { .. },
                    }) => VEntry::StructHeapPointer(ty.as_key(), ptr),

                    VEntry::Scalar(Scalar { point, kind: ScalarKind::SumInline(clty) }) => {
                        let fields = self.decompress_inline_payload(*clty, *point, ty.as_key());
                        VEntry::StructFlat(ty.as_key(), fields)
                    }
                    _ => panic!("casting non-opaque sum payload. Should that be allowed?"),
                }
            }
            VEntry::StructStackPointer(key, ptr) => {
                let struct_ = self.ctx.structs.get(key);

                let payi = self.ctx.structs.get_real_field(key, key::Field(1));
                let poffset = self.ctx.structs.offset_of(key, payi);

                match &struct_.fields[payi] {
                    FieldV::SumPayloadPointer { .. } => {
                        let size_t = self.ctx.size_t();
                        let payload_pointer = self.deref_scalar(ptr, poffset, size_t);
                        // Since it was stored as a field, it should already be S_Stable
                        VEntry::StructHeapPointer(ty.as_key(), payload_pointer)
                    }
                    &FieldV::SumPayloadInline(clty) => {
                        let inline = self.deref_scalar(ptr, poffset, clty);
                        let fields = self.decompress_inline_payload(clty, inline, ty.as_key());
                        VEntry::StructFlat(ty.as_key(), fields)
                    }
                    _ => unreachable!(),
                }
            }
            other => unreachable!("{other:?}"),
        }
    }

    pub(super) fn decompress_inline_payload(
        &mut self,
        _from: Type,
        inline: Value,
        param_tuple: MonoTypeKey,
    ) -> Map<layout::Field, VEntry> {
        let struct_ = self.ctx.structs.get(param_tuple).clone();
        match struct_.fields.as_slice() {
            [] => Map::new(),
            [x] => match x {
                FieldV::Flat(ty) => [self.cast_value_from_scalar(ty, inline)].into(),
                FieldV::AutoBoxed(inner) => [VEntry::Scalar(Scalar::new(
                    inline,
                    ScalarKind::AutoBoxed((*inner).into()),
                ))]
                .into(),
                _ => unreachable!(),
            },
            [_, _] => todo!(),
            _ => panic!("cannot compress variants with 3 or more fields into inline payload"),
        }
    }
}
