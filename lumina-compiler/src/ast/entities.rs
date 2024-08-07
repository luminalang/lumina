use super::{FuncAttr, TypeAttr};
use crate::prelude::*;
use lumina_parser::r#impl::Association;
use lumina_parser::{func, ty, Type};

#[derive(Default)]
pub struct Entities<'s> {
    pub fheaders: ModMap<key::Func, func::Header<'s>>,
    pub fbodies: ModMap<key::Func, FuncBody<'s>>,
    pub fattributes: ModMap<key::Func, FuncAttr<'s>>,
    pub vals: ModMap<key::Val, M<key::Func>>,

    pub field_types: ModMap<key::Record, Map<key::RecordField, Tr<Type<'s>>>>,
    pub field_names: ModMap<key::Record, Map<key::RecordField, Tr<&'s str>>>,
    pub records: ModMap<key::Record, RecordHeader<'s>>,

    pub variant_types: ModMap<key::Sum, Map<key::SumVariant, Vec<Tr<Type<'s>>>>>,
    pub variant_names: ModMap<key::Sum, Map<key::SumVariant, Tr<&'s str>>>,
    pub sums: ModMap<key::Sum, SumHeader<'s>>,

    pub methods: ModMap<key::Trait, Map<key::Method, key::Func>>,
    pub traits: ModMap<key::Trait, TraitHeader<'s>>,
    pub associated_types: ModMap<key::Trait, Map<key::AssociatedType, Association<'s>>>,

    pub impls: ModMap<key::Impl, ImplDef<'s>>,
    pub imethods: ModMap<key::Impl, Map<key::Method, key::Func>>,

    pub langitems: Map<key::Module, Vec<(Tr<&'s str>, Type<'s>)>>,
}

pub enum FuncBody<'s> {
    Extern { link_name: String },
    Val(func::Body<'s>, key::Val),
    Func(func::Body<'s>),
    TraitMethod(Option<func::Body<'s>>, M<key::Trait>),
    ImplMethod(func::Body<'s>, M<key::Impl>),
}

impl<'s> Entities<'s> {
    pub fn add_langitem(&mut self, module: key::Module, name: Tr<&'s str>, ty: Type<'s>) {
        self.langitems[module].push((name, ty))
    }
    pub fn get_langitems(&self, module: key::Module) -> &[(Tr<&'s str>, Type<'s>)] {
        self.langitems[module].as_slice()
    }
}

type SumHeader<'s> = TyHeader<'s>;
type RecordHeader<'s> = TyHeader<'s>;
type TraitHeader<'s> = TyHeader<'s>;

#[derive(new)]
pub struct TyHeader<'s> {
    pub attributes: TypeAttr<'s>,
    pub header: ty::Header<'s>,
}

pub struct ImplDef<'s> {
    pub header: lumina_parser::r#impl::Header<'s>,
    pub associations: Map<key::AssociatedType, Association<'s>>,
}

impl<'s> Entities<'s> {
    pub fn add_module(&mut self, module: key::Module) {
        assert_eq!(module, self.fheaders.add_module(5));
        assert_eq!(module, self.fbodies.add_module(5));
        assert_eq!(module, self.fattributes.add_module(5));
        assert_eq!(module, self.vals.add_module(0));
        assert_eq!(module, self.field_types.add_module(5));
        assert_eq!(module, self.field_names.add_module(5));
        assert_eq!(module, self.records.add_module(2));
        assert_eq!(module, self.variant_types.add_module(2));
        assert_eq!(module, self.variant_names.add_module(2));
        assert_eq!(module, self.sums.add_module(2));
        assert_eq!(module, self.methods.add_module(0));
        assert_eq!(module, self.traits.add_module(0));
        assert_eq!(module, self.associated_types.add_module(0));
        assert_eq!(module, self.impls.add_module(0));
        assert_eq!(module, self.imethods.add_module(0));
        self.langitems.push_as(module, vec![]);
    }

    pub fn header_of_ty(&self, kind: M<key::TypeKind>) -> &TyHeader<'s> {
        match kind.value {
            key::TypeKind::Record(record) => &self.records[kind.module.m(record)],
            key::TypeKind::Sum(sum) => &self.sums[kind.module.m(sum)],
            key::TypeKind::Trait(trait_) => &self.traits[kind.module.m(trait_)],
        }
    }
}
