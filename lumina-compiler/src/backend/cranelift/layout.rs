use crate::lir;
use crate::lir::{MonoType, MonoTypeKey};
use crate::prelude::*;
use ast::attr::Repr;
use cranelift::codegen::ir::ArgumentPurpose;
use cranelift::prelude::*;
use cranelift_codegen::isa::CallConv;
use lumina_collections::{map_key_impl, KeysIter};
use lumina_typesystem::ConstValue;
use std::cell::RefCell;

pub struct Structs<'a> {
    structs: Map<MonoTypeKey, Struct>,
    pub records: &'a lir::Types,

    autobox_stack: RefCell<Vec<M<key::TypeKind>>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
/// Similar to key::Field except after the ABI has possibly re-ordered fields
pub struct Field(u32);
map_key_impl!(Field(u32), "afield");

// The goal is to make this `struct` completely agnostic to ABI.
#[derive(Clone, Debug, new)]
pub struct Struct {
    pub align: u32,

    // CL lower is allowed to reorder fields for optimal alignment.
    //
    // Therefore; we need to map the original fields to their new indice.
    pub field_map: Map<key::Field, Field>,
    pub fields: Map<Field, StructField>,
}

#[derive(Clone, Debug)]
pub enum StructField {
    Flat(MonoType),
    AutoBoxed(MonoType),

    SumPayloadPointer { sum: MonoTypeKey },
    SumPayloadInline(Type),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PassBy {
    Transparent(MonoType),
    Pointer,
    Value,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum Stability {
    F,
    S,
    FRet,
}

/// During the lower from LIR to CLIR, we have three categories of values.
///
/// * S_Stable
/// S_Stable values are those which are in the memory layout that's expected when stored inside
/// another struct, either behind a pointer or embedded.
///
/// * F_Stable
/// F_Stable values are those which are in the memory layout that's expected by other functions when
/// handed as a parameter. They may contain parameters pointing to the callés stack, thus may need
/// to be memcpy'd into a heap allocation if they are to escape.
///
/// * Unstable
/// Unstable values have much looser requirements and thus much greater variation in their layout
/// which is checked dynamically during the lower of a function. Unstable values are never allowed
/// to escape the function they're created in.
///
/// All S_Stable values can be treated as F_Stable.
/// All S_Stable and F_Stable values can be treated as Unstable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Layout<T> {
    Scalar(Scalar, T),
    AutoBoxed(MonoType, T),

    // Inlined Aggregates
    ZST,
    ArrayFlat(MonoType, Vec<Layout<T>>),
    StructFlat(MonoTypeKey, Map<Field, Layout<T>>),

    // Implicitly added special representations
    SpecialPointer(SpecialPointer, T),
    OutPointer(SpecialPointer, T),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Scalar {
    Pointer(MonoType),
    FuncPointer(Box<FuncLayout>),
    Direct,
    SumPayloadInline,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SpecialPointer {
    StackSumPayload { sum: MonoTypeKey },
    HeapSumPayload { sum: MonoTypeKey },

    // Struct/Arrays implicitly passed as a pointer
    StackStruct(MonoTypeKey),
    HeapStruct(MonoTypeKey),
    StackArray(MonoType, u64),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FuncLayout {
    pub conv: CallConv,
    pub params: Map<key::Param, Layout<Type>>,
    pub ret: Layout<Type>,
}

impl Layout<Type> {
    // NOTE: ignores out pointers
    pub fn promote_all_stack_to_heap(&self) -> Self {
        match self {
            Layout::OutPointer(..)
            | Layout::Scalar(_, _)
            | Layout::AutoBoxed(_, _)
            | Layout::ZST => self.clone(),
            Layout::ArrayFlat(inner, elems) => Layout::ArrayFlat(
                inner.clone(),
                elems
                    .iter()
                    .map(Layout::promote_all_stack_to_heap)
                    .collect(),
            ),
            Layout::StructFlat(key, fields) => Layout::StructFlat(
                *key,
                fields
                    .values()
                    .map(Layout::promote_all_stack_to_heap)
                    .collect(),
            ),
            Layout::SpecialPointer(kind, ptr) => {
                let kind = match kind {
                    SpecialPointer::HeapSumPayload { .. } | SpecialPointer::HeapStruct(_) => {
                        kind.clone()
                    }

                    &SpecialPointer::StackSumPayload { sum } => {
                        SpecialPointer::HeapSumPayload { sum }
                    }
                    SpecialPointer::StackStruct(key) => SpecialPointer::HeapStruct(*key),
                    SpecialPointer::StackArray(..) => unimplemented!("auto-boxed arrays"),
                };
                Layout::SpecialPointer(kind, *ptr)
            }
        }
    }
}

impl<T: Copy> Layout<T> {
    pub fn direct(v: T) -> Self {
        Layout::Scalar(Scalar::Direct, v)
    }
    pub fn pointer(ty: MonoType, v: T) -> Self {
        Layout::Scalar(Scalar::Pointer(ty), v)
    }

    pub fn as_direct(&self) -> T {
        match self {
            Layout::Scalar(Scalar::Direct, v) => *v,
            _ => panic!("as_direct called on non-direct layout"),
        }
    }

    pub fn as_scalar(&self) -> T {
        match self {
            Layout::Scalar(_, v) => *v,
            _ => panic!("as_intable called on non-intable layout"),
        }
    }

    pub fn as_pointer(&self) -> (&MonoType, T) {
        match self {
            Layout::Scalar(Scalar::Pointer(ty), ptr) => (ty, *ptr),
            _ => panic!("as_pointer called on non-pointer layout"),
        }
    }

    pub fn assert_no_stack_leak(&self) {
        if self.has_stack_pointers() {
            panic!("stack leak");
        }
    }

    // NOTE: ignores out pointers
    pub fn has_stack_pointers(&self) -> bool {
        match self {
            Layout::Scalar(_, _) | Layout::AutoBoxed(_, _) | Layout::ZST => false,
            Layout::ArrayFlat(_, elems) => elems.iter().any(Layout::has_stack_pointers),
            Layout::StructFlat(_, fields) => fields.values().any(Layout::has_stack_pointers),
            Layout::SpecialPointer(kind, _) => match kind {
                SpecialPointer::HeapSumPayload { .. } | SpecialPointer::HeapStruct(_) => false,
                _ => true,
            },
            Layout::OutPointer(_, _) => false,
        }
    }

    pub fn map_layout<U, F, OF>(&self, f: &mut F, out_pointer: &mut OF) -> Layout<U>
    where
        F: FnMut(T) -> U,
        OF: FnMut(SpecialPointer, T) -> Layout<U>,
    {
        match self {
            Layout::Scalar(kind, t) => Layout::Scalar(kind.clone(), f(*t)),
            Layout::SpecialPointer(special, ptr) => {
                Layout::SpecialPointer(special.clone(), f(*ptr))
            }
            Layout::ArrayFlat(ty, elems) => {
                Layout::ArrayFlat(ty.clone(), Self::map_layouts(elems, f, out_pointer))
            }
            Layout::ZST => Layout::ZST,
            Layout::StructFlat(mk, fields) => {
                Layout::StructFlat(*mk, Self::map_layouts(fields.as_slice(), f, out_pointer))
            }

            Layout::AutoBoxed(ty, ptr) => Layout::AutoBoxed(ty.clone(), f(*ptr)),

            Layout::OutPointer(kind, size_t) => out_pointer(kind.clone(), size_t.clone()),
        }
    }

    pub fn map_layouts<U, F, OF, C>(layouts: &[Self], f: &mut F, out_pointer: &mut OF) -> C
    where
        F: FnMut(T) -> U,
        OF: FnMut(SpecialPointer, T) -> Layout<U>,
        C: FromIterator<Layout<U>>,
    {
        layouts
            .iter()
            .map(|layout| layout.map_layout(f, out_pointer))
            .collect()
    }

    pub fn out_pointers<F: FnMut(SpecialPointer, T)>(&self, f: &mut F) {
        // TODO: optimize with imut visitor
        self.map_layout(&mut |ty| ty, &mut |kind, ptr| {
            f(kind, ptr);
            Layout::ZST
        });
    }
}

impl Struct {
    fn is_lowered(&self) -> bool {
        self.align != u32::MAX
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ByteOffset(pub u32);

impl<'a> Structs<'a> {
    pub fn new(records: &'a lir::Types) -> Self {
        let mut this = Self {
            structs: records
                .keys()
                .map(|_| Struct { align: u32::MAX, field_map: Map::new(), fields: Map::new() })
                .collect(),
            records,
            autobox_stack: RefCell::new(vec![]),
        };

        for mk in records.keys() {
            let _ = this.get_or_make(mk);
        }

        this
    }

    fn should_autobox_field(&self, key: MonoTypeKey, ty: &MonoType) -> bool {
        match &self.records[key] {
            lir::MonoTypeData::Record { key: rkey, .. } => {
                let Some(original) = rkey else {
                    return false;
                };

                let kind = original.map(key::TypeKind::Record);
                self.autobox_stack.borrow_mut().push(kind);

                let autobox = self.autobox_check(ty);

                let kind = original.map(key::Record::into);
                assert_eq!(self.autobox_stack.borrow_mut().pop(), Some(kind));

                autobox
            }
            _ => false,
        }
    }

    fn autobox_check(&self, ty: &MonoType) -> bool {
        match ty {
            MonoType::Monomorphised(key) => match &self.records[*key] {
                lir::MonoTypeData::Record { key: rkey, fields, .. } => {
                    if let Some(original) = rkey {
                        let mut stack = self.autobox_stack.borrow_mut();
                        let kind = original.map(key::TypeKind::Record);

                        // early-return if one of the inner structs are recursive so we don't halt
                        //
                        // but if it's the struct we're checking autobox for that's recursive; then
                        // return that we're meant to autobox.
                        if stack.iter().any(|k| *k == kind) {
                            return stack[0] == kind;
                        }

                        stack.push(kind);
                    }

                    let autobox = fields.values().any(|fty| self.autobox_check(fty));

                    if let Some(original) = rkey {
                        let kind = original.map(key::Record::into);
                        assert_eq!(self.autobox_stack.borrow_mut().pop(), Some(kind));
                    }

                    autobox
                }
                _ => false,
            },
            _ => false,
        }
    }

    pub fn get(&self, key: MonoTypeKey) -> &Struct {
        let struct_ = &self.structs[key];
        assert!(
            struct_.is_lowered(),
            "`get` used for struct before it's been converted to ABI Struct"
        );
        struct_
    }

    // NOTE: This borrows `self` mutably even though most of the time it doesn't need to.
    //
    // Using `make()` followed by `get()` is generally a better idea.
    fn get_or_make(&mut self, key: MonoTypeKey) -> &Struct {
        self.make(key);
        self.get(key)
    }

    pub fn arr_pass_mode(&self, len: u64, inner: &MonoType) -> PassBy {
        if len > 2 {
            return PassBy::Pointer;
        }

        match self.c_class_of(inner) {
            SystemVClass::Integer => PassBy::Value,
            SystemVClass::Memory => PassBy::Pointer,
        }
    }

    // Whether this struct should be passed as inlined scalars or is implicitly put behind a pointer.
    pub fn pass_mode(&self, key: MonoTypeKey) -> PassBy {
        match &self.records[key] {
            lir::MonoTypeData::Record { fields, .. } if fields.len() == 1 => {
                PassBy::Transparent(fields[key::Field(0)].clone())
            }
            lir::MonoTypeData::Record { repr, .. } => {
                let fields = &self.structs[key].fields;
                let (size, _) = self.size_and_align_of_mk(key);

                match self.c_class_of_aggregate_layout_struct(
                    Some(key),
                    size,
                    *repr,
                    fields.as_slice(),
                ) {
                    SystemVClass::Integer => PassBy::Value,
                    SystemVClass::Memory => PassBy::Pointer,
                }
            }
            _ => PassBy::Value,
        }
    }

    // Gets the largest scalar count and largest size among the sum variants
    fn largest_sum_variant(&self, variants: &Map<key::Variant, MonoTypeKey>) -> (usize, u32) {
        let mut scalars = 0;
        let mut size = 0;

        for param_tuple in variants.values() {
            let (s, ss) = self.variant_inline_size_of_struct(*param_tuple);
            scalars = scalars.max(s);
            size = size.max(ss);
        }

        (scalars, size)
    }

    fn variant_inline_size_of_struct(&self, key: MonoTypeKey) -> (usize, u32) {
        let ptr = self.records.pointer_bits / 8;

        match &self.records[key] {
            lir::MonoTypeData::Record { key: _, fields, .. } => {
                let mut scalars = 0;
                let mut size = 0;

                for v in fields.values() {
                    if self.should_autobox_field(key, v) {
                        scalars += 1;
                        size += self.records.pointer_bits / 8;
                    } else {
                        let (s, ss) = self.variant_inline_size(v);
                        scalars += s;
                        size += ss;
                    }
                }

                (scalars, size)
            }
            lir::MonoTypeData::Sum { tag, .. } => (2, tag.bytes() as u32 + ptr),
            lir::MonoTypeData::DynTraitObject { .. } => (2, ptr * 2),
            lir::MonoTypeData::Placeholder => unreachable!(),
        }
    }

    fn variant_inline_size(&self, ty: &MonoType) -> (usize, u32) {
        let ptr = || self.records.pointer_bits / 8;

        match ty {
            MonoType::Int(intsize) => (1, intsize.bytes() as u32),
            MonoType::Pointer(_) | MonoType::FnPointer(_, _) => (1, ptr()),
            MonoType::Float => (1, 8),
            MonoType::Const(const_) => match const_ {
                lumina_typesystem::ConstValue::Usize(_) => (1, self.records.pointer_bits / 8),
                lumina_typesystem::ConstValue::Bool(_) => (1, 1),
                lumina_typesystem::ConstValue::Char(_) => (1, 4),
            },
            MonoType::Array(len, inner) => {
                let (c, s) = self.variant_inline_size(inner);
                (c * *len as usize, s * *len as u32)
            }
            MonoType::Monomorphised(inner) => self.variant_inline_size_of_struct(*inner),
            MonoType::Unreachable => unreachable!(),
        }
    }

    // TODO: we're calling this operation *a lot* while it's fairly expensive we should probably
    // memoizise it.
    pub fn sum_payload_alloca_size(&self, sum: MonoTypeKey) -> u32 {
        let (_, _, variants) = self.records[sum].as_sum();
        variants
            .values()
            .map(|&param_tuple| self.size_of(&param_tuple.into()))
            .max()
            .unwrap()
    }

    fn make(&mut self, key: MonoTypeKey) {
        if self.structs[key].is_lowered() {
            trace!("{key}: reusing existing");
            return;
        }

        trace!(
            "{key}: lowering with type id {:?}",
            self.records[key].original()
        );

        let size_t = Type::int(self.records.pointer_bits as u16).unwrap();

        match &self.records[key] {
            lir::MonoTypeData::Sum { tag, variants, .. } => {
                trace!(
                    "{key}: lowering variant types {}",
                    variants.values().format(" & ")
                );

                // let tagfield = Layout::Direct(Type::int(tag.bits() as u16).unwrap());
                let tagfield = StructField::Flat(MonoType::Int(*tag));
                let ptr = size_t;

                let (scalarcount, largest) = self.largest_sum_variant(variants);

                trace!("for sum {key}: scalarcount={scalarcount}");

                self.structs[key] = if scalarcount == 0 {
                    Struct::new(tag.bytes() as u32, [Field(0)].into(), [tagfield].into())
                } else if scalarcount == 1 {
                    assert!(largest <= ptr.bytes());
                    let payload =
                        StructField::SumPayloadInline(Type::int(largest as u16 * 8).unwrap());
                    let fields = [payload, tagfield].into();
                    Struct::new(ptr.bytes(), [1, 0].map(Field).into(), fields)
                } else if scalarcount == 2 && largest <= ptr.bytes() {
                    let size = Type::int(largest as u16 * 8).unwrap_or(ptr);
                    let fields = [StructField::SumPayloadInline(size), tagfield].into();
                    Struct::new(ptr.bytes(), [1, 0].map(Field).into(), fields)
                } else {
                    let fields = [StructField::SumPayloadPointer { sum: key }, tagfield].into();
                    Struct::new(ptr.bytes(), [1, 0].map(Field).into(), fields)
                };
            }
            lir::MonoTypeData::Record { repr, key: _, fields } => {
                if fields.is_empty() {
                    trace!("{key}: ZST");
                    self.structs[key].align = 0;
                    return;
                }

                match repr {
                    ast::attr::Repr::Lumina => {
                        let _align = self.calculate_align_of_struct(key);
                        let mut fieldorder: Vec<key::Field> = fields.keys().collect();

                        // Sort fields for better alignment
                        fieldorder.sort_by(|&a, &b| {
                            self.calculate_align_of(&fields[b])
                                .cmp(&self.calculate_align_of(&fields[a]))
                        });

                        self.structs[key].field_map = fields
                            .keys()
                            .map(|field| {
                                Field(fieldorder.iter().position(|k| *k == field).unwrap() as u32)
                            })
                            .collect();

                        assert_eq!(fields.len(), fieldorder.len());

                        self.lower_struct_fields(key, fieldorder.into_iter());

                        assert!(!self.structs[key].fields.is_empty(), "{key}");
                    }
                    ast::attr::Repr::C => {
                        let _align = self.calculate_align_of_struct(key);
                        self.structs[key].field_map = fields.keys().map(|k| Field(k.0)).collect();

                        let fieldorder = fields.keys();
                        self.lower_struct_fields(key, fieldorder);
                    }
                    ast::attr::Repr::Packed => todo!(),
                    ast::attr::Repr::Align(_) => todo!(),
                    ast::attr::Repr::Enum(_) => unreachable!(),
                }
            }
            lir::MonoTypeData::DynTraitObject { vtable, .. } => {
                let align = self.records.pointer_bits / 8;

                let data_field = StructField::Flat(MonoType::u8_pointer());

                self.structs[key] = Struct {
                    align,
                    field_map: [0, 1].map(Field).into(),
                    fields: [data_field, StructField::Flat(vtable.clone())].into(),
                };
            }
            lir::MonoTypeData::Placeholder => unreachable!(),
        }
    }

    fn lower_struct_fields<I>(&mut self, key: MonoTypeKey, fields: I)
    where
        I: Iterator<Item = key::Field> + Clone,
    {
        trace!("{key}: lowering fields");

        for field in fields {
            let fty = &self.records[key].as_record()[field];

            if self.should_autobox_field(key, fty) {
                trace!("{key}:{fty:?}: recursive autobox");
                let autoboxed = StructField::AutoBoxed(fty.clone());
                self.structs[key].fields.push(autoboxed);
            } else {
                let flat = StructField::Flat(fty.clone());
                self.structs[key].fields.push(flat);
            }
        }
    }

    pub fn get_real_field(&self, key: MonoTypeKey, field: key::Field) -> Field {
        self.get(key).field_map[field]
    }

    pub fn offset_of(&self, key: MonoTypeKey, field: Field) -> ByteOffset {
        let struct_ = &self.structs[key];

        let mut offset = 0;
        for i in KeysIter::up_to(field) {
            let field = &struct_.fields[i];
            let (fsize, pad) = self.field_size_and_pad(offset, field);
            offset += fsize + pad;
        }

        let (_, align) = self.size_and_align_of_field(&struct_.fields[field]);
        let end_padding = (align - offset % align) % align;

        ByteOffset(end_padding + offset)
    }

    fn calculate_align_of(&mut self, ty: &MonoType) -> u32 {
        match ty {
            MonoType::Monomorphised(key) => self.get_or_make(*key).align,
            MonoType::Array(_, inner) => self.calculate_align_of(inner),
            _ => {
                let (size, align) = self.size_and_align_of(ty);
                assert_eq!(size, align);
                align
            }
        }
    }

    fn calculate_align_of_struct(&mut self, for_: MonoTypeKey) -> u32 {
        if self.structs[for_].is_lowered() {
            let align = self.structs[for_].align;
            trace!("{for_}: fetched alignment {align}");
            return align;
        }

        trace!("{for_}: calculating alignment");

        let mut align = 0;

        for (_, ty) in self.records[for_].as_record() {
            if self.should_autobox_field(for_, ty) {
                align = align.max(self.records.pointer_bits / 8);
            } else {
                let field_alignment = self.calculate_align_of(ty);
                align = align.max(field_alignment);
            }
        }

        trace!("{for_}: alignment calculated to {align}");

        assert!(align <= self.records.pointer_bits / 8);
        self.structs[for_].align = align;

        align
    }

    pub fn field_size_and_pad(&self, offset: u32, field: &StructField) -> (u32, u32) {
        let (size, align) = self.size_and_align_of_field(field);
        if align == 0 {
            assert_eq!(size, 0);
            (0, 0)
        } else {
            let padding = (align - offset % align) % align;
            (padding, size)
        }
    }

    pub fn size_and_align_of_mk(&self, mk: MonoTypeKey) -> (u32, u32) {
        let align = self.structs[mk].align;
        if align == 0 {
            return (0, 0);
        }

        let mut offset = 0;

        for field in self.structs[mk].fields.values() {
            let (fsize, pad) = self.field_size_and_pad(offset, field);
            offset += fsize + pad;
        }

        let end_padding = (align - offset % align) % align;
        let size = offset + end_padding;

        (size, align)
    }

    pub fn size_and_align_of_ptr_dst(&self, special: &SpecialPointer) -> (u32, u32) {
        match special {
            &SpecialPointer::HeapSumPayload { sum, .. }
            | &SpecialPointer::StackSumPayload { sum, .. } => {
                let size = self.sum_payload_alloca_size(sum);
                // TODO: this align seems weird. Not sure whether it's valid
                let align = (self.records.pointer_bits / 8).min(size);
                (size, align)
            }
            &SpecialPointer::HeapStruct(mk) | &SpecialPointer::StackStruct(mk) => {
                self.size_and_align_of_mk(mk)
            }
            SpecialPointer::StackArray(inner, n) => {
                let (size, _, align) = self.size_and_align_of_array(inner, *n);
                (size, align)
            }
        }
    }

    pub fn size_and_align_of(&self, ty: &MonoType) -> (u32, u32) {
        match ty {
            MonoType::Monomorphised(mk) => self.size_and_align_of_mk(*mk),
            MonoType::Const(const_) => {
                let size = self.const_to_ty(const_).bytes();
                (size, size)
            }
            MonoType::Array(len, inner) => {
                let (size, _, align) = self.size_and_align_of_array(inner, *len);
                (size, align)
            }
            MonoType::Int(intsize) => (intsize.bytes() as u32, intsize.bytes() as u32),
            MonoType::Float | MonoType::FnPointer(_, _) | MonoType::Pointer(_) => {
                let size = self.records.pointer_bits / 8;
                (size, size)
            }
            MonoType::Unreachable => (0, 0),
        }
    }

    fn const_to_ty(&self, const_: &ConstValue) -> Type {
        match const_ {
            lumina_typesystem::ConstValue::Usize(_) => {
                Type::int(self.records.pointer_bits as u16).unwrap()
            }
            lumina_typesystem::ConstValue::Bool(_) => types::I8,
            lumina_typesystem::ConstValue::Char(_) => types::I32,
        }
    }

    pub fn size_and_align_of_array(&self, inner: &MonoType, times: u64) -> (u32, u32, u32) {
        let (elem_size, align) = self.size_and_align_of(inner);

        if times == 0 {
            return (0, elem_size, 0);
        }

        let padding = (align - elem_size % align) % align;

        let total_size = (elem_size + padding) * times as u32;
        (total_size, elem_size, align)
    }

    pub fn size_of(&self, ty: &MonoType) -> u32 {
        self.size_and_align_of(ty).0
    }

    fn struct_has_unaligned_fields(&self, key: Option<MonoTypeKey>) -> bool {
        match key {
            Some(key) => {
                let struct_ = self.get(key);
                struct_.align == 0 && !struct_.fields.is_empty()
            }
            None => false,
        }
    }

    fn is_non_trivial(&self) -> bool {
        false
    }

    pub fn size_and_align_of_field(&self, f: &StructField) -> (u32, u32) {
        match f {
            StructField::Flat(ty) => self.size_and_align_of(ty),
            StructField::SumPayloadInline(clty) => (clty.bytes(), clty.bytes()),
            StructField::AutoBoxed(_) | StructField::SumPayloadPointer { .. } => {
                let ptr = self.records.pointer_bits / 8;
                (ptr, ptr)
            }
        }
    }

    // ref: System V Application Binary Interface Version 1.0
    //      page 24
    fn c_class_of_aggregate_layout_struct(
        &self,
        key: Option<MonoTypeKey>,
        struct_size: u32,
        repr: Repr,
        fields: &[StructField],
    ) -> SystemVClass {
        let eightbyte = self.records.pointer_bits / 8;

        if struct_size > (eightbyte * 8) || self.struct_has_unaligned_fields(key) {
            // 1. always pass large structs through the stack
            SystemVClass::Memory
        } else if self.is_non_trivial() {
            // 2. use Memory for non-trivial objects
            unreachable!("not possible to define non-trivial C structs in Lumina");
        } else {
            // 4. use the class of the fields. Prioritise Memory over Integer.
            let class = fields
                .iter()
                .fold(None, |class, field| {
                    // let field_class = self.c_class_of_aggregate_of_layout(layout);
                    let field_class = match field {
                        StructField::Flat(ty) => self.c_class_of(&ty),
                        _ => SystemVClass::Integer,
                    };

                    match class {
                        None => Some(field_class),
                        Some(SystemVClass::Memory) => Some(SystemVClass::Memory),
                        Some(SystemVClass::Integer) => match field_class {
                            SystemVClass::Memory => Some(SystemVClass::Memory),
                            SystemVClass::Integer => Some(SystemVClass::Integer),
                        },
                    }
                })
                .unwrap_or(SystemVClass::Integer);

            let max_struct_regs = if repr == Repr::Lumina { 4 } else { 2 };

            // 5c. if the size exceeds two eightbytes and there are no floats or
            // vectors (which we don't support C repr of) then also use the stack.
            if struct_size > (eightbyte * max_struct_regs) {
                SystemVClass::Memory
            } else {
                class
            }
        }
    }

    fn c_class_of_array(&self, len: u64, inner: &MonoType) -> SystemVClass {
        let eightbyte = self.records.pointer_bits / 8;
        let (size, _, _) = self.size_and_align_of_array(inner, len);

        if size > (eightbyte * 8) {
            SystemVClass::Memory
        } else {
            let class = self.c_class_of(inner);

            if size > (eightbyte * 2) {
                SystemVClass::Memory
            } else {
                class
            }
        }
    }

    fn c_class_of(&self, ty: &MonoType) -> SystemVClass {
        match ty {
            MonoType::FnPointer(_, _) | MonoType::Pointer(_) | MonoType::Int(_) => {
                SystemVClass::Integer
            }
            MonoType::Monomorphised(mk) => {
                let fields = &self.structs[*mk].fields;
                let (size, _) = self.size_and_align_of_mk(*mk);
                let repr = match &self.records[*mk] {
                    lir::MonoTypeData::Record { repr, .. } => *repr,
                    _ => Repr::Lumina,
                };
                self.c_class_of_aggregate_layout_struct(Some(*mk), size, repr, fields.as_slice())
            }
            MonoType::Array(len, inner) => self.c_class_of_array(*len, inner),
            _ => panic!("unsupported type in repr C struct: {ty:?}"),
        }
    }
}

enum SystemVClass {
    Integer,
    Memory,
    // Sse,
    // Sseup,
    // NoClass,
}

impl<'a> Structs<'a> {
    pub fn flayout<'t, 'f, P>(
        &self,
        conv: CallConv,
        params: P,
        ret: &MonoType,
    ) -> (FuncLayout, Signature)
    where
        P: IntoIterator<Item = &'t MonoType>,
    {
        let params = params
            .into_iter()
            .map(|ty| self.type_to_layout(ty, Stability::F))
            .collect::<Map<_, _>>();

        let ret = self.type_to_layout(&ret, Stability::FRet);

        let flayout = FuncLayout { conv, params, ret };
        let sig = self.signature(&flayout);

        (flayout, sig)
    }

    pub fn signature(&self, layout: &FuncLayout) -> Signature {
        let mut sig = Signature::new(layout.conv);

        // Since the large struct return is added as the *first* parameter; we check the return first.
        self.sig_return(&mut sig, &layout.ret);

        for layout in layout.params.values() {
            self.sig_param(&mut sig, layout);
        }

        sig
    }

    fn sig_return(&self, sig: &mut Signature, layout: &Layout<Type>) {
        match layout {
            Layout::OutPointer(kind, size_t) => {
                let purpose = match kind {
                    // Cranelift dissallows mixing StructReturn and register returns.
                    // Since that's exactly what we want to do for tagged unions,
                    // I hope there isn't a good reason for that...
                    SpecialPointer::HeapSumPayload { .. }
                    | SpecialPointer::StackSumPayload { .. } => ArgumentPurpose::Normal,
                    _ => ArgumentPurpose::StructReturn,
                };
                sig.params.push(AbiParam::special(*size_t, purpose));
            }
            Layout::AutoBoxed(_, size_t) => {
                sig.returns.push(AbiParam::new(*size_t));
            }
            Layout::ZST => {}
            Layout::ArrayFlat(_, elems) => {
                for v in elems {
                    self.sig_return(sig, v);
                }
            }
            Layout::StructFlat(_, fields) => {
                for v in fields.values() {
                    self.sig_return(sig, v);
                }
            }
            Layout::Scalar(_, clty) | Layout::SpecialPointer(_, clty) => {
                sig.returns.push(AbiParam::new(*clty));
            }
        }
    }

    fn sig_param(&self, sig: &mut Signature, layout: &Layout<Type>) {
        match layout {
            Layout::OutPointer(_, _) => {
                panic!("out pointer as parameter for signature");
            }
            Layout::AutoBoxed(_, size_t) => {
                sig.params.push(AbiParam::new(*size_t));
            }
            Layout::ZST => {}
            Layout::ArrayFlat(_, elems) => {
                for v in elems {
                    self.sig_param(sig, v);
                }
            }
            Layout::StructFlat(_, fields) => {
                for v in fields.values() {
                    self.sig_param(sig, v);
                }
            }
            Layout::Scalar(_, clty) | Layout::SpecialPointer(_, clty) => {
                sig.params.push(AbiParam::new(*clty));
            }
        }
    }

    fn try_sum_to_layout(&self, mk: MonoTypeKey, stab: Stability) -> Option<Layout<Type>> {
        if let lir::MonoTypeData::Sum { .. } = &self.records[mk] {
            let size_t = Type::int(self.records.pointer_bits as u16).unwrap();

            match self.structs[mk].fields.len() {
                1 => None,
                2 if matches!(stab, Stability::S) => None,
                2 => {
                    assert_eq!(self.structs[mk].field_map.as_slice(), &[Field(1), Field(0)]);
                    let tag =
                        self.field_to_layout(&self.structs[mk].fields[Field(1)], Stability::S);
                    let payload = match &self.structs[mk].fields[Field(0)] {
                        StructField::SumPayloadPointer { .. }
                            if matches!(stab, Stability::FRet) =>
                        {
                            let kind = SpecialPointer::StackSumPayload { sum: mk };
                            Layout::OutPointer(kind, size_t)
                        }
                        StructField::SumPayloadPointer { .. } => {
                            let kind = SpecialPointer::StackSumPayload { sum: mk };
                            Layout::SpecialPointer(kind, size_t)
                        }
                        StructField::SumPayloadInline(clty) => {
                            Layout::Scalar(Scalar::SumPayloadInline, *clty)
                        }
                        _ => unreachable!(),
                    };
                    Some(Layout::StructFlat(mk, [payload, tag].into()))
                }
                _ => unreachable!(),
            }
        } else {
            None
        }
    }

    pub(super) fn type_to_layout<'t, 'f>(&self, ty: &MonoType, stab: Stability) -> Layout<Type> {
        let size_t = Type::int(self.records.pointer_bits as u16).unwrap();

        match ty {
            MonoType::Int(size) => Layout::direct(Type::int(size.bits() as u16).unwrap()),
            MonoType::Pointer(inner) => Layout::pointer((**inner).clone(), size_t),
            MonoType::Const(ConstValue::Bool(_)) => Layout::direct(types::I8),
            MonoType::Const(ConstValue::Char(_)) => Layout::direct(types::I8), // TODO: unicode char
            MonoType::Const(ConstValue::Usize(_)) => Layout::direct(size_t),
            MonoType::FnPointer(params, ret) => {
                let (flayout, _sig) = self.flayout(CallConv::Tail, params, &ret);
                Layout::Scalar(Scalar::FuncPointer(Box::new(flayout)), size_t)
            }
            MonoType::Float => Layout::direct(types::F64),
            MonoType::Array(n, inner) => {
                if *n == 0 || self.size_of(inner) == 0 {
                    return Layout::ZST;
                }

                match self.arr_pass_mode(*n, inner) {
                    PassBy::Pointer if matches!(stab, Stability::F) => Layout::SpecialPointer(
                        SpecialPointer::StackArray(*inner.clone(), *n),
                        size_t,
                    ),
                    PassBy::Pointer if matches!(stab, Stability::FRet) => {
                        Layout::OutPointer(SpecialPointer::StackArray(*inner.clone(), *n), size_t)
                    }
                    _ => {
                        let layout = self.type_to_layout(&inner, Stability::S);
                        Layout::ArrayFlat((**inner).clone(), vec![layout; *n as usize])
                    }
                }
            }
            &MonoType::Monomorphised(mk) => {
                if self.is_zst(mk) {
                    return Layout::ZST;
                }

                // Edge-case to not heap-allocate sum payload even though it's technically a field
                if let Some(layout) = self.try_sum_to_layout(mk, stab) {
                    return layout;
                }

                match self.pass_mode(mk) {
                    PassBy::Transparent(ty) => {
                        Layout::StructFlat(mk, [self.type_to_layout(&ty, stab)].into())
                    }
                    PassBy::Pointer if matches!(stab, Stability::F) => {
                        Layout::SpecialPointer(SpecialPointer::StackStruct(mk), size_t)
                    }
                    PassBy::Pointer if matches!(stab, Stability::FRet) => {
                        Layout::OutPointer(SpecialPointer::StackStruct(mk), size_t)
                    }
                    _ => {
                        let fields = self.structs[mk]
                            .fields
                            .values()
                            .map(|f| self.field_to_layout(f, Stability::S))
                            .collect();

                        Layout::StructFlat(mk, fields)
                    }
                }
            }
            MonoType::Unreachable => todo!("unreachable type"),
        }
    }

    fn field_to_layout(&self, field: &StructField, stab: Stability) -> Layout<Type> {
        let size_t = Type::int(self.records.pointer_bits as u16).unwrap();

        match field {
            StructField::Flat(ty) => self.type_to_layout(ty, stab),
            StructField::AutoBoxed(inner) => Layout::AutoBoxed(inner.clone(), size_t),
            &StructField::SumPayloadPointer { sum } => match stab {
                Stability::FRet => {
                    let kind = SpecialPointer::StackSumPayload { sum };
                    Layout::OutPointer(kind, size_t)
                }
                Stability::S => {
                    let kind = SpecialPointer::HeapSumPayload { sum };
                    Layout::SpecialPointer(kind, size_t)
                }
                Stability::F => {
                    let kind = SpecialPointer::StackSumPayload { sum };
                    Layout::SpecialPointer(kind, size_t)
                }
            },
            &StructField::SumPayloadInline(clty) => Layout::Scalar(Scalar::SumPayloadInline, clty),
        }
    }

    pub fn is_zst(&self, mk: MonoTypeKey) -> bool {
        self.size_of(&mk.into()) == 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lumina_typesystem::IntSize;

    #[test]
    fn repr_c_padding() {
        let mut records = lir::MonomorphisedTypes::new(
            M(key::Module(0), key::Trait::from(0)),
            64,
            ast::attr::Repr::C,
        );

        let int = |bits| MonoType::Int(IntSize::new(false, bits));

        let small = records.get_or_make_tuple(vec![int(16), int(32)]);
        let large = records.get_or_make_tuple(vec![small.into(), int(16), int(64)]);

        let mut structs = Structs::new(&records.types);

        let small_struct = structs.get_or_make(small).clone();
        assert_eq!(small_struct.align, 32 / 8);
        assert_eq!(structs.size_of(&small.into()), (32 / 8) * 2);

        let u16_ = structs.get_real_field(large, key::Field(1));
        assert_eq!(structs.offset_of(large, u16_), ByteOffset(4 * 2));

        let large_struct = structs.get_or_make(large);
        assert_eq!(large_struct.align, 64 / 8);

        assert_eq!(
            structs.size_of(&large.into()),
            structs.size_of(&small.into()) + ((64 / 8) * 2)
        );
    }

    #[test]
    fn padding_at_end() {
        lumina_util::test_logger();

        let m = key::Module(0);

        let mut records =
            lir::MonomorphisedTypes::new(M(m, key::Trait::from(0)), 64, ast::attr::Repr::Lumina);

        let int = |bits| MonoType::Int(IntSize::new(false, bits));

        let record = records.get_or_make_tuple(vec![int(64), int(16)]);

        let mut structs = Structs::new(&records.types);
        let struct_ = structs.get_or_make(record);
        assert_eq!(struct_.align, 8);

        assert_eq!(structs.size_of(&record.into()), 16);
    }

    #[test]
    fn recursive_types() {
        lumina_util::test_logger();

        let m = key::Module(0);

        let mut records =
            lir::MonomorphisedTypes::new(M(m, key::Trait::from(0)), 64, ast::attr::Repr::Lumina);

        let point = MonoTypeKey(1);
        let tuple = MonoTypeKey(2);
        let pointr = M(m, key::Record(0));

        // type Point { x: (Self, Self), y: (Self, Self) }
        assert_eq!(
            point,
            records.get_or_make_record(
                pointr,
                vec![tuple.into()],
                [tuple.into(), tuple.into()].into(),
            )
        );

        assert_eq!(
            tuple,
            records.get_or_make_tuple(vec![point.into(), point.into()])
        );

        let mut structs = Structs::new(&records.types);

        let point_struct = structs.get_or_make(point);
        assert_eq!(point_struct.align, 8);
        assert_eq!(structs.size_of(&point.into()), 8 * 2);

        let tuple_struct = structs.get_or_make(tuple);
        assert_eq!(tuple_struct.align, 8);
        assert_eq!(structs.size_of(&tuple.into()), 8 * 4);
    }
}
