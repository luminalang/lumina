use crate::lir;
use crate::lir::{MonoType, MonoTypeKey};
use crate::prelude::*;
use cranelift::prelude::*;
use cranelift_codegen::ir;
use target_lexicon::Triple;

impl lir::Records {
    pub(super) fn get_abi_typing<'t, I>(&self, triple: &Triple, params: I, ret: &MonoType) -> Typing
    where
        I: IntoIterator<Item = &'t MonoType>,
    {
        Typing {
            params: self.get_abi_params(triple, params),
            ret: self.get_abi_return(triple, ret),
            conv: isa::CallConv::Fast,
        }
    }

    pub(super) fn get_abi_params<'t>(
        &self,
        triple: &Triple,
        params: impl IntoIterator<Item = &'t MonoType>,
    ) -> Map<key::Param, Param> {
        params
            .into_iter()
            .map(|ty| self.abi_param(triple, ty))
            .collect()
    }

    pub(super) fn abi_param(&self, triple: &Triple, ty: &MonoType) -> Param {
        match ty {
            MonoType::Int(bitsize) => Param::Direct(Type::int(bitsize.bits() as u16).unwrap()),
            MonoType::Pointer(_) => Param::Direct(Type::triple_pointer_type(triple)),
            MonoType::FnPointer(params, ret) => {
                let typing = self.get_abi_typing(triple, params, ret);
                Param::FuncPointer(Box::new(typing), Type::triple_pointer_type(triple))
            }
            MonoType::Float => Param::Direct(types::F64),
            MonoType::Unreachable => Param::ZST,
            MonoType::Monomorphised(mk) => {
                let fields = self.abi_record_fields(triple, *mk);
                if fields.is_empty() {
                    Param::ZST
                } else {
                    Param::Struct(*mk, fields)
                }
            }
            // We store all data payloads as pointers for now
            //
            // Makes the destructoring casts a lot easier. Although; inline optimizations is
            // definitely a good idea.
            MonoType::SumDataCast { .. } => Param::Direct(Type::triple_pointer_type(triple)),
        }
    }

    pub(super) fn get_abi_return(&self, triple: &Triple, ty: &MonoType) -> Return {
        let fits_in_ret =
            |size, len| size <= Type::triple_pointer_type(triple).bits() * 3 && len <= 3;

        match self.abi_param(triple, ty) {
            Param::Struct(mk, fields) if !fits_in_ret(self[mk].size, self[mk].fields.len()) => {
                Return::StructOutPtr(mk, fields)
            }
            param => Return::Param(param),
        }
    }

    fn abi_record_fields(
        &self,
        triple: &Triple,
        mk: MonoTypeKey,
    ) -> Map<key::RecordField, StructField<Type>> {
        self[mk]
            .fields
            .iter()
            .map(|(fieldkey, ty)| {
                if self[mk].autoboxed.contains(&fieldkey) {
                    let key = ty.as_key();
                    StructField::AutoBoxedRecursion(key, Type::triple_pointer_type(triple))
                } else {
                    match ty {
                        MonoType::Int(bitsize) => {
                            StructField::Direct(Type::int(bitsize.bits() as u16).unwrap())
                        }
                        MonoType::FnPointer(params, ret) => {
                            let typing = self.get_abi_typing(triple, params, ret);
                            StructField::FuncPointer(
                                Box::new(typing),
                                Type::triple_pointer_type(triple),
                            )
                        }
                        MonoType::Pointer(_) => {
                            StructField::Direct(Type::triple_pointer_type(triple))
                        }
                        MonoType::Float => StructField::Direct(types::F64),
                        MonoType::Unreachable => StructField::ZST,

                        // Flatten & inline regardless of size
                        MonoType::Monomorphised(mk) => StructField::Struct(
                            self.abi_record_fields(triple, *mk)
                                .into_iter()
                                .map(|(_, ty)| ty)
                                .collect(),
                        ),
                        MonoType::SumDataCast { largest } => StructField::SumPayload {
                            largest: *largest,
                            ptr: Type::triple_pointer_type(triple),
                        },
                    }
                }
            })
            .collect()
    }
}

#[derive(Clone, Debug)]
pub enum StructField<T> {
    Direct(T),
    AutoBoxedRecursion(MonoTypeKey, T),
    FuncPointer(Box<Typing>, T),
    Struct(Map<key::RecordField, StructField<T>>),
    SumPayload { largest: u32, ptr: T },
    ZST,
}

#[derive(Clone, Debug)]
pub struct Typing {
    pub conv: isa::CallConv,
    pub params: Map<key::Param, Param>,
    pub ret: Return,
}

// TODO: using SmallVec could do a lot of good here
#[derive(Clone, Debug)]
pub enum Entry<T> {
    Direct(T),
    FuncPointer(Box<Typing>, T),
    Struct(MonoTypeKey, Map<key::RecordField, StructField<T>>),
    SumPayload { largest: u32, ptr: T },
    ZST,
}

pub type Param = Entry<Type>;

#[derive(Clone, Debug)]
pub enum Return {
    Param(Entry<Type>),
    StructOutPtr(MonoTypeKey, Map<key::RecordField, StructField<Type>>),
}

impl<T: Clone + Copy> Entry<T> {
    pub fn visit(&self, mut on: impl FnMut(T)) {
        match self {
            Entry::ZST => {}
            Entry::Direct(ty) => on(*ty),
            Entry::Struct(_, fields) => fields.values().for_each(|field| field.visit(&mut on)),
            Entry::FuncPointer(_, ptr) => on(*ptr),
            Entry::SumPayload { ptr, .. } => on(*ptr),
            // Param::FlatArray { size, times } => match Type::int(*size as u16) {
            //     Some(elem) => (0..*times).for_each(|_| on(elem)),
            //     None => unimplemented!("uneven arrays by-value"),
            // },
            // Param::ArrayPtr { .. } => on(Type::triple_pointer_type(triple)),
            // Param::ZST => {}
        }
    }

    pub fn map<U>(&self, on: &mut dyn FnMut(T) -> U) -> Entry<U> {
        match self {
            Entry::Direct(v) => Entry::Direct(on(*v)),
            Entry::FuncPointer(typing, ptr) => Entry::FuncPointer(typing.clone(), on(*ptr)),
            Entry::Struct(mk, fields) => Entry::Struct(
                *mk,
                fields.values().map(|field| field.map(&mut *on)).collect(),
            ),
            &Entry::SumPayload { largest, ptr } => Entry::SumPayload { largest, ptr: on(ptr) },
            Entry::ZST => Entry::ZST,
        }
    }

    #[track_caller]
    pub fn as_direct(self) -> T {
        match self {
            Entry::Direct(v) => v,
            _ => panic!("not a direct value"),
        }
    }
}

impl<T: Clone + Copy> StructField<T> {
    pub fn visit(&self, on: &mut impl FnMut(T)) {
        match self {
            StructField::ZST => {}
            StructField::FuncPointer(_, ptr) => on(*ptr),
            StructField::AutoBoxedRecursion(_, ty) | StructField::Direct(ty) => on(*ty),
            StructField::Struct(fields) => fields.values().for_each(|field| field.visit(on)),
            // StructField::Array { size, times } => match Type::int(*size as u16) {
            //     Some(elem) => (0..*times).for_each(|_| on(elem)),
            //     None => unimplemented!("uneven arrays by-value"),
            // },
            StructField::SumPayload { ptr, .. } => on(*ptr),
        }
    }

    pub fn map<U>(&self, on: &mut dyn FnMut(T) -> U) -> StructField<U> {
        match self {
            StructField::Direct(v) => StructField::Direct(on(*v)),
            StructField::FuncPointer(typing, ptr) => {
                StructField::FuncPointer(typing.clone(), on(*ptr))
            }
            StructField::AutoBoxedRecursion(mk, ty) => {
                StructField::AutoBoxedRecursion(*mk, on(*ty))
            }
            StructField::Struct(fields) => {
                StructField::Struct(fields.values().map(|field| field.map(&mut *on)).collect())
            }
            &StructField::SumPayload { largest, ptr } => {
                StructField::SumPayload { largest, ptr: on(ptr) }
            }
            StructField::ZST => todo!(),
        }
    }
}

pub fn signature(
    conv: isa::CallConv,
    abiparams: &Map<key::Param, Param>,
    abiret: &Return,
) -> Signature {
    let mut sig = Signature::new(conv);

    for p in abiparams.values() {
        p.visit(|p| sig.params.push(AbiParam::new(p)));
    }

    match abiret {
        Return::Param(p) => p.visit(|p| sig.returns.push(AbiParam::new(p))),
        Return::StructOutPtr(..) => {
            sig.params.push(AbiParam::special(
                types::I64,
                ir::ArgumentPurpose::StructReturn,
            ));
        }
    }

    sig
}
