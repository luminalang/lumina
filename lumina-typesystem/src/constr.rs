//! A bunch of convenience constructors for types
use super::{Bitsize, Container, FuncKind, Generic, GenericKind, IType, Prim, Type};
use crate::M;
use lumina_key as key;

impl Type {
    pub fn self_to_u8_ptr(&self) -> Type {
        match self {
            Type::Container(con) => con.map(Self::self_to_u8_ptr).into(),
            Type::Prim(prim) => Type::Prim(*prim),
            Type::Generic(generic) => Type::Generic(*generic),
            Type::List(kind, params) | Type::Defined(kind, params) => {
                Type::Defined(*kind, params.iter().map(Self::self_to_u8_ptr).collect())
            }
            Type::Self_ => Type::u8_ptr(),
        }
    }

    pub fn i<'s>(&self) -> IType {
        match self {
            Type::List(key, params) | Type::Defined(key, params) => {
                let params = params.iter().map(Type::i).collect();
                IType::Defined(*key, params)
            }
            Type::Container(container) => container.map(Type::i).into(),
            Type::Prim(prim) => IType::Prim(*prim),
            Type::Generic(generic) => IType::Generic(*generic),
            Type::Self_ => IType::Self_,
        }
    }
}

macro_rules! impl_helpers {
    ($ty:ty) => {
        impl $ty {
            pub fn boxed(self) -> Box<Self> {
                Box::new(self)
            }

            pub fn poison() -> Self {
                Self::Prim(Prim::Poison)
            }

            pub fn i64() -> Self {
                Self::Prim(Prim::Int(true, Bitsize(64)))
            }

            pub fn tuple() -> Self {
                Self::Container(Container::Tuple(vec![]))
            }

            pub fn defined<K: Into<key::TypeKind>>(key: M<K>, params: Vec<Self>) -> Self {
                Self::Defined(key.map(K::into), params)
            }

            pub fn fnptr(params: Vec<Self>, ret: Self) -> Self {
                Self::Container(Container::Func(FuncKind::FnPointer, params, Box::new(ret)))
            }
            pub fn closure(params: Vec<Self>, ret: Self) -> Self {
                Self::Container(Container::Func(FuncKind::Closure, params, Box::new(ret)))
            }

            pub fn u8_ptr() -> Self {
                Self::Container(Container::Pointer(Box::new(Self::u8())))
            }

            pub fn u8() -> Self {
                Self::Prim(Prim::Int(false, Bitsize(8)))
            }

            pub fn ptr(self) -> Self {
                Self::Container(Container::Pointer(Box::new(self)))
            }

            pub fn for_forall<K: Into<key::TypeKind>>(
                key: M<K>,
                forall: impl IntoIterator<Item = key::Generic>,
                gkind: GenericKind,
            ) -> Self {
                let key = key.map(K::into);
                let params = forall
                    .into_iter()
                    .map(|g| Generic::new(g, gkind).into())
                    .collect();
                Self::Defined(key, params)
            }
        }
    };
}

impl_helpers!(IType);
impl_helpers!(Type);
