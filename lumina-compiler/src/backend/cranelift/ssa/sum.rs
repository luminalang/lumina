use super::*;
use layout::SpecialPointer;

impl<'c, 'a, 'f> Translator<'c, 'a, 'f> {
    pub(super) fn construct_variant(
        &mut self,
        key: MonoTypeKey,
        var: key::Variant,
        values: &[lir::Value],
    ) -> VLayout {
        let lir::MonoTypeData::Sum { tag, variants, .. } = &self.ctx.structs.records[key] else {
            panic!("attempted to construct variant of non-sum");
        };

        let param_tuple = variants[var];

        let tagt = Type::int(tag.bits() as u16).unwrap();
        let tag = self.cins().iconst(tagt, var.0 as i64);
        let tagfield = Layout::direct(tag);

        let sum_struct = self.ctx.structs.get(key);

        let layout = match sum_struct.field_map.as_slice() {
            [_tagi] => Layout::StructFlat(key, [tagfield].into()),
            [_tagi, payloadi] => {
                let payload = match &sum_struct.fields[*payloadi] {
                    &layout::StructField::SumPayloadPointer { sum } => {
                        let largest = self.ctx.structs.sum_payload_alloca_size(sum);
                        let ptr =
                            self.construct_record_on_stack(Some(largest), param_tuple, values);
                        Layout::SpecialPointer(layout::SpecialPointer::StackSumPayload { sum }, ptr)
                    }

                    &layout::StructField::SumPayloadInline(clty) => {
                        let inline = self.compress_into_inline_payload(clty, param_tuple, values);
                        Layout::Scalar(Scalar::SumPayloadInline, inline)
                    }
                    other => unreachable!("{other:?}"),
                };
                Layout::StructFlat(key, [payload, tagfield].into())
            }
            other => unreachable!("bad field count for sum type: {other:?}"),
        };

        layout
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
            [] | [Layout::ZST] => self.cins().iconst(clty, 0),
            [Layout::Scalar(_, v)] => {
                let has = self.f.type_of_value(*v);
                if has == types::F64 {
                    todo!("we need to cast as well?");
                } else {
                    assert!(has.is_int());
                    self.resize_uint(*v, clty)
                }
            }
            [_, _] => todo!("compress multiple small variants into one sum payload"),
            _ => panic!("cannot compress variants with 3 or more fields into inline payload"),
        }
    }

    pub(super) fn cast_from_sum(&mut self, sum: VLayout, ty: &MonoType) -> VLayout {
        match sum {
            // Sum types without a payload
            Layout::StructFlat(key, params) if params.len() == 1 => {
                assert_eq!(self.ctx.structs.get(key).fields.len(), 1);
                assert_eq!(self.ctx.structs.size_of(ty), 0);
                Layout::ZST
            }
            Layout::StructFlat(key, params) => {
                let payi = self.ctx.structs.get_real_field(key, key::Field(1));
                let payload = &params[payi];

                match payload {
                    // Simply cast the associated type
                    //
                    // we don't use `Scalar::pointer` because it's of type `Struct` and not `*Struct` in LIR
                    Layout::SpecialPointer(kind, ptr) => {
                        let kind = match kind {
                            SpecialPointer::StackSumPayload { .. } => {
                                SpecialPointer::StackStruct(ty.as_key())
                            }
                            SpecialPointer::HeapSumPayload { .. } => {
                                SpecialPointer::HeapStruct(ty.as_key())
                            }
                            _ => panic!("casting non-opaque sum payload"),
                        };
                        Layout::SpecialPointer(kind, *ptr)
                    }
                    &Layout::Scalar(Scalar::SumPayloadInline, point) => {
                        let clty = self.f.type_of_value(point);
                        let fields = self.decompress_inline_payload(clty, point, ty.as_key());
                        Layout::StructFlat(ty.as_key(), fields)
                    }
                    _ => panic!("casting non-opaque sum payload"),
                }
            }
            Layout::SpecialPointer(SpecialPointer::HeapStruct(key), ptr) => {
                self.load_payload_from_ptr_to_sum(key, ptr, true, ty)
            }
            Layout::SpecialPointer(SpecialPointer::StackStruct(key), ptr) => {
                self.load_payload_from_ptr_to_sum(key, ptr, false, ty)
            }
            other => unreachable!("{other:?}"),
        }
    }

    fn load_payload_from_ptr_to_sum(
        &mut self,
        key: MonoTypeKey,
        ptr: Value,
        heap: bool,
        requested: &MonoType,
    ) -> VLayout {
        match self.ctx.structs.get(key).fields.len() {
            1 => Layout::ZST,
            2 => {
                let struct_ = self.ctx.structs.get(key);

                let payi = self.ctx.structs.get_real_field(key, key::Field(1));
                let poffset = self.ctx.structs.offset_of(key, payi);
                let flags = MemFlags::trusted();

                match &struct_.fields[payi] {
                    layout::StructField::SumPayloadPointer { .. } => {
                        let size_t = self.ctx.size_t();
                        let payload_pointer =
                            self.cins().load(size_t, flags, ptr, poffset.0 as i32);

                        let kind = if heap {
                            SpecialPointer::HeapStruct
                        } else {
                            SpecialPointer::StackStruct
                        }(requested.as_key());

                        Layout::SpecialPointer(kind, payload_pointer)
                    }
                    &layout::StructField::SumPayloadInline(clty) => {
                        let inline = self.cins().load(clty, flags, ptr, poffset.0 as i32);
                        let fields =
                            self.decompress_inline_payload(clty, inline, requested.as_key());
                        Layout::StructFlat(requested.as_key(), fields)
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!("invalid sum type"),
        }
    }

    pub(super) fn decompress_inline_payload(
        &mut self,
        _from: Type,
        inline: Value,
        param_tuple: MonoTypeKey,
    ) -> Map<layout::Field, VLayout> {
        let struct_ = self.ctx.structs.get(param_tuple).clone();
        match struct_.fields.as_slice() {
            [] => Map::new(),
            [x] => match x {
                layout::StructField::Flat(ty) => {
                    // TODO: perform a proper fold which would support multi-scalar decompression
                    let layout = self.ctx.structs.type_to_layout(ty, Stability::S);
                    let field = layout.map_layout(
                        &mut |clty| self.resize_uint(inline, clty),
                        &mut |_, _| unreachable!(),
                    );
                    [field].into()
                }
                _ => unreachable!(),
            },

            _ => panic!("cannot compress variants with 2 or more fields into inline payload"),
        }
    }
}
