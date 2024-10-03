use crate::lir;
use crate::lir::{MonoType, MonoTypeKey};
use crate::prelude::*;
use cranelift::codegen::ir::ArgumentPurpose;
use cranelift::prelude::*;
use either::Either;
use lumina_collections::{map_key_impl, KeysIter};
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
    pub fields: Map<Field, FieldV>,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum PassBy {
    Pointer,
    Value,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FieldV {
    Flat(MonoType),
    AutoBoxed(MonoTypeKey),

    SumPayloadPointer { sum: MonoTypeKey },
    SumPayloadInline(Type),
}

pub enum FType<T> {
    StructZST,
    Struct(PassBy, MonoTypeKey),
    Scalar(Scalar<T>),
}

impl Struct {
    fn is_lowered(&self) -> bool {
        self.align != u32::MAX
    }
}

impl lir::Types {
    pub(super) fn get_abi_typing<'t, I>(&self, params: I, ret: &MonoType) -> Typing
    where
        I: IntoIterator<Item = &'t MonoType>,
    {
        // TODO: Internally this can be represented as `Key(MonoFunc) | ([MonoType], Type)` instead
        // which would be much more efficient.
        Typing {
            params: params.into_iter().cloned().collect(),
            ret: ret.clone(),
            conv: isa::CallConv::Fast,
        }
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

    fn scalars_of_ty<F: FnMut(Scalar<Type>)>(&self, ty: &MonoType, f: &mut F) {
        let ptr = || Type::int(self.records.pointer_bits as u16).unwrap();

        match ty {
            MonoType::Int(intsize) => f(Scalar::direct(Type::int(intsize.bits() as u16).unwrap())),
            MonoType::Pointer(inner) => f(Scalar::pointer(ptr(), &*inner)),
            MonoType::FnPointer(params, ret) => {
                let typing = self.records.get_abi_typing(params, ret);
                f(Scalar::fn_pointer(ptr(), typing))
            }
            MonoType::Float => f(Scalar::direct(types::F64)),
            MonoType::Unreachable => unreachable!(),
            MonoType::Monomorphised(key) => self.iter_s_stable_fields(*key, f),
        }
    }

    /// Visitor over S_Stable representation of fields within a Struct
    pub fn iter_s_stable_fields<F: FnMut(Scalar<Type>)>(&self, key: MonoTypeKey, f: &mut F) {
        for field in self.structs[key].fields.values() {
            let ptr = || Type::int(self.records.pointer_bits as u16).unwrap();

            match field {
                FieldV::Flat(ty) => self.scalars_of_ty(ty, f),
                &FieldV::SumPayloadInline(size) => {
                    f(Scalar::new(size, ScalarKind::SumInline(size)))
                }
                &FieldV::SumPayloadPointer { sum } => {
                    // Since this is a field of a struct, we're assuming that the inner pointer
                    // is S_Stable. However; make sure we're not accidentally breaking this
                    // assumption somewhere.
                    let largest = self.sum_payload_alloca_size(sum);
                    f(Scalar::new(ptr(), ScalarKind::HeapSumPointer { largest }))
                }
                FieldV::AutoBoxed(ty) => {
                    f(Scalar { point: ptr(), kind: ScalarKind::Pointer((*ty).into()) })
                }
            }
        }
    }

    // NOTE: This borrows `self` mutably even though most of the time it doesn't need to.
    //
    // Using `make()` followed by `get()` is generally a better idea.
    fn get_or_make(&mut self, key: MonoTypeKey) -> &Struct {
        self.make(key);
        self.get(key)
    }

    // Whether this struct should be passed as inlined scalars or is implicitly put behind a pointer.
    pub fn pass_mode(&self, key: MonoTypeKey) -> PassBy {
        match &self.records[key] {
            lir::MonoTypeData::Record { key: _, .. } => match self.c_class_of_aggregate(key) {
                SystemVClass::Integer => PassBy::Value,
                SystemVClass::Memory => PassBy::Pointer,
            },
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
            MonoType::Monomorphised(inner) => self.variant_inline_size_of_struct(*inner),
            MonoType::Unreachable => unreachable!(),
        }
    }

    // TODO: we're calling this operation *a lot* while it's fairly expensive we should probably
    // memoizise it.
    pub fn sum_payload_alloca_size(&self, sum: MonoTypeKey) -> u32 {
        let (_, variants) = self.records[sum].as_sum();
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
            "{key}: lowering without type id {:?}",
            self.records[key].original()
        );

        match &self.records[key] {
            lir::MonoTypeData::Sum { tag, key: _, variants } => {
                trace!(
                    "{key}: lowering variant types {}",
                    variants.values().format(" & ")
                );

                let tagfield = FieldV::Flat(MonoType::Int(*tag));
                let ptr = Type::int(self.records.pointer_bits as u16).unwrap();

                let (scalarcount, largest) = self.largest_sum_variant(variants);

                trace!("for sum {key}: scalarcount={scalarcount} largest={largest}");

                // TODO: once iconcat/isplit scalar compression is fully implemented we want to
                // check scalarcount <= 2 instead
                let allowed_to_compress = || scalarcount == 1 && largest <= 8;

                self.structs[key] = if largest == 0 {
                    assert_eq!(scalarcount, 0);
                    Struct::new(tag.bytes() as u32, [Field(0)].into(), [tagfield].into())
                } else if allowed_to_compress() {
                    // Assuming all sum types are tag+arch makes things easier for now.
                    // They'd likely be padded to that regardless so it doesn't seem all
                    // that wasteful.
                    let fields = [FieldV::SumPayloadInline(ptr), tagfield].into();
                    Struct::new(ptr.bytes(), [1, 0].map(Field).into(), fields)
                } else {
                    let fields = [FieldV::SumPayloadPointer { sum: key }, tagfield].into();
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
                self.structs[key] = Struct {
                    align,
                    field_map: [0, 1].map(Field).into(),
                    fields: [
                        FieldV::Flat(MonoType::u8_pointer()),
                        FieldV::Flat(vtable.clone()),
                    ]
                    .into(),
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
                let autoboxed = FieldV::AutoBoxed(key);
                self.structs[key].fields.push(autoboxed);
            } else {
                let flat = FieldV::Flat(fty.clone());
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
            let (size, align) = self.size_and_align_of_field(field);
            let padding = (align - offset % align) % align;
            offset += padding + size;
        }

        let (_, align) = self.size_and_align_of_field(&struct_.fields[field]);
        let padding = (align - offset % align) % align;

        ByteOffset(offset + padding)
    }

    fn calculate_align_of(&mut self, ty: &MonoType) -> u32 {
        match ty {
            MonoType::Monomorphised(key) => self.get_or_make(*key).align,
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

    pub fn size_and_align_of(&self, ty: &MonoType) -> (u32, u32) {
        let (size, align) = match ty {
            MonoType::Monomorphised(mk) => {
                let align = self.structs[*mk].align;
                if align == 0 {
                    assert!(self.structs[*mk].fields.is_empty());
                    return (0, 0);
                }

                let mut offset = 0;

                for field in self.structs[*mk].fields.values() {
                    let (size, align) = self.size_and_align_of_field(field);
                    let padding = (align - offset % align) % align;
                    offset += padding + size;
                }

                let end_padding = (align - offset % align) % align;
                let size = offset + end_padding;

                // trace!("size={size} align={align} for {mk}");

                (size, align)
            }
            MonoType::Int(intsize) => (intsize.bytes() as u32, intsize.bytes() as u32),
            MonoType::Float | MonoType::FnPointer(_, _) | MonoType::Pointer(_) => {
                let size = self.records.pointer_bits / 8;
                (size, size)
            }
            MonoType::Unreachable => (0, 0),
        };

        (size, align)
    }

    pub fn size_and_align_of_field(&self, f: &FieldV) -> (u32, u32) {
        match f {
            FieldV::Flat(ty) => self.size_and_align_of(ty),
            FieldV::SumPayloadInline(clty) => (clty.bytes(), clty.bytes()),
            FieldV::AutoBoxed(_) | FieldV::SumPayloadPointer { .. } => {
                let ptr = self.records.pointer_bits / 8;
                (ptr, ptr)
            }
        }
    }

    pub fn size_of(&self, ty: &MonoType) -> u32 {
        self.size_and_align_of(ty).0
    }

    fn struct_has_unaligned_fields(&self, key: MonoTypeKey) -> bool {
        let struct_ = self.get(key);
        struct_.align == 0
    }

    fn is_non_trivial(&self) -> bool {
        false
    }

    // ref: System V Application Binary Interface Version 1.0
    //      page 24
    fn c_class_of_aggregate(&self, key: MonoTypeKey) -> SystemVClass {
        let eightbyte = self.records.pointer_bits / 8;
        let struct_size = self.size_of(&key.into());

        if struct_size > (eightbyte * 8) || self.struct_has_unaligned_fields(key) {
            // 1. always pass large structs through the stack
            SystemVClass::Memory
        } else if self.is_non_trivial() {
            // 2. use Memory for non-trivial objects
            unreachable!("not possible to define non-trivial C structs in Lumina");
        } else {
            let mut class = None;

            // 4. use the class of the fields. Prioritise Memory over Integer.
            for i in self.structs[key].fields.keys() {
                let field_class = match self.structs[key].fields[i].clone() {
                    FieldV::Flat(ty) => self.c_class_of(&ty),
                    _ => SystemVClass::Integer,
                };

                match class {
                    None => class = Some(field_class),
                    Some(SystemVClass::Memory) => {}
                    Some(SystemVClass::Integer) => match field_class {
                        SystemVClass::Memory => class = Some(SystemVClass::Memory),
                        SystemVClass::Integer => {}
                    },
                }
            }

            // 5c. if the size exceeds two eightbytes and there are no floats or
            // vectors (which we don't support C repr of) then also use the stack.
            if struct_size > (eightbyte * 2) {
                SystemVClass::Memory
            } else {
                class.expect("empty repr C structs are not valid")
            }
        }
    }

    fn c_class_of(&self, ty: &MonoType) -> SystemVClass {
        match ty {
            MonoType::FnPointer(_, _) | MonoType::Pointer(_) | MonoType::Int(_) => {
                SystemVClass::Integer
            }
            MonoType::Monomorphised(mk) => self.c_class_of_aggregate(*mk),
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Typing {
    pub conv: isa::CallConv,
    pub params: Map<key::Param, MonoType>,
    pub ret: MonoType,
}

/// A singular S_Stable hardware value with attached metadata
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scalar<T> {
    pub point: T,
    pub kind: ScalarKind,
}

impl<T> Scalar<T> {
    pub fn new(point: T, kind: ScalarKind) -> Self {
        Self { point, kind }
    }

    pub fn direct(point: T) -> Self {
        Self { point, kind: ScalarKind::Direct }
    }

    pub fn pointer(point: T, inner: &MonoType) -> Self {
        Self { point, kind: ScalarKind::Pointer(inner.clone()) }
    }

    pub fn fn_pointer(point: T, typing: Typing) -> Self {
        Self { point, kind: ScalarKind::FuncPointer(Box::new(typing)) }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ScalarKind {
    Direct,

    // TODO: We want to re-use signatures, currently we create a new signature each time this
    // function pointer is invoked.
    FuncPointer(Box<Typing>),

    // Since we stack-allocate large payloads by default; we cannot treat the payload differently
    // on a variant-basis since whether we need to heap-allocate or not for escape analysis
    // wouldn't be statically known.
    // All variants are either treated as having inline or pointer, no mixture.
    SumInline(Type),
    HeapSumPointer { largest: u32 },

    Pointer(MonoType),
    AutoBoxed(MonoType),
}

impl<'a> Structs<'a> {
    pub fn scalar_or_struct(&self, ty: &MonoType) -> Either<Scalar<Type>, MonoTypeKey> {
        let size_t = Type::int(self.records.pointer_bits as u16).unwrap();

        match ty {
            MonoType::Int(intsize) => {
                let ty = Type::int(intsize.bits() as u16).unwrap();
                Either::Left(Scalar::direct(ty))
            }
            MonoType::Pointer(inner) => Either::Left(Scalar::pointer(size_t, &**inner)),
            MonoType::FnPointer(params, ret) => {
                let typing = self.records.get_abi_typing(params, ret);
                Either::Left(Scalar::fn_pointer(size_t, typing))
            }
            MonoType::Float => Either::Left(Scalar::direct(types::F64)),
            MonoType::Unreachable => unreachable!(),
            MonoType::Monomorphised(mk) => Either::Right(*mk),
        }
    }

    pub fn signature(&mut self, typing: &Typing) -> Signature {
        let mut sig = Signature::new(typing.conv);

        let size_t = Type::int(self.records.pointer_bits as u16).unwrap();

        let normal = AbiParam::new;

        for ty in typing.params.values() {
            // self.ty_to_abi_param(false, ty, &mut sig);
            match self.ftype(ty) {
                FType::StructZST => {}
                FType::Struct(PassBy::Value, mk) => self
                    .iter_s_stable_fields(mk, &mut |scalar| sig.params.push(normal(scalar.point))),
                FType::Struct(PassBy::Pointer, mk) => {
                    let purpose = match sig.call_conv {
                        isa::CallConv::AppleAarch64
                        | isa::CallConv::SystemV
                        | isa::CallConv::WindowsFastcall => {
                            let size = self.size_of(&mk.into());
                            ArgumentPurpose::StructArgument(size)
                        }
                        _ => ArgumentPurpose::Normal,
                    };

                    sig.params.push(AbiParam::special(size_t, purpose));
                }
                FType::Scalar(scalar) => sig.params.push(normal(scalar.point)),
            }
        }

        match self.ftype(&typing.ret) {
            FType::StructZST => {}
            FType::Struct(PassBy::Value, mk) => {
                self.iter_s_stable_fields(mk, &mut |scalar| sig.returns.push(normal(scalar.point)))
            }
            FType::Struct(PassBy::Pointer, _) => {
                sig.params
                    .push(AbiParam::special(size_t, ArgumentPurpose::StructReturn));
            }
            FType::Scalar(scalar) => sig.returns.push(normal(scalar.point)),
        }

        sig
    }

    pub fn ftype(&self, ty: &MonoType) -> FType<Type> {
        match self.scalar_or_struct(ty) {
            Either::Left(scalar) => FType::Scalar(scalar),
            Either::Right(mk) if self.is_zst(mk) => FType::StructZST,
            Either::Right(mk) => FType::Struct(self.pass_mode(mk), mk),
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
