use super::{FuncAttr, TypeAttr};
use crate::prelude::*;
use lumina_parser::r#impl::Association;
use lumina_parser::{func, ty, Type};

#[derive(Default)]
pub struct Entities<'s> {
    pub fheaders: MMap<key::Func, func::Header<'s>>,
    pub fbodies: MMap<key::Func, FuncBody<'s>>,
    pub fattributes: MMap<key::Func, FuncAttr<'s>>,
    pub vals: MMap<key::Val, M<key::Func>>,

    pub field_types: MMap<key::Record, Map<key::Field, Tr<Type<'s>>>>,
    pub field_names: MMap<key::Record, Map<key::Field, Tr<&'s str>>>,
    pub records: MMap<key::Record, RecordHeader<'s>>,

    pub variant_types: MMap<key::Sum, Map<key::Variant, Vec<Tr<Type<'s>>>>>,
    pub variant_names: MMap<key::Sum, Map<key::Variant, Tr<&'s str>>>,
    pub sums: MMap<key::Sum, SumHeader<'s>>,

    pub methods: MMap<key::Trait, Map<key::Method, key::Func>>,
    pub traits: MMap<key::Trait, TraitHeader<'s>>,
    pub associated_types: MMap<key::Trait, Map<key::AssociatedType, Association<'s>>>,

    pub impls: MMap<key::Impl, ImplDef<'s>>,
    pub imethods: MMap<key::Impl, Map<key::Method, key::Func>>,

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

    pub fn header_of_ty(&self, M(module, kind): M<key::TypeKind>) -> &TyHeader<'s> {
        match kind {
            key::TypeKind::Record(record) => &self.records[record.inside(module)],
            key::TypeKind::Sum(sum) => &self.sums[sum.inside(module)],
            key::TypeKind::Trait(trait_) => &self.traits[trait_.inside(module)],
        }
    }
}
