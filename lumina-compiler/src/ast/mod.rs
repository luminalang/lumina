use crate::{key, project::Files};
use lumina_key::{PrimaryMap as Map, SecondaryMap};
use lumina_parser as parser;
use lumina_util::{Span, Tr};
use std::path::Path;
use tracing::info;

mod attr;
pub use attr::{
    AliasAttr, FuncAttr, ImplAttr, ProjectAttr, SharedAttr, TypeAttr, UseAttr, ValAttr,
};
mod collect;
use collect::Collector;

/// Contains raw vectorized item data for a full translation unit.
#[derive(Debug)]
pub struct AST<'s> {
    // Item-generic data, contains keys for item-specific
    pub items: Map<key::Item, Item<'s>>,

    // Item-specific data
    pub functions: Map<key::Func, ItemData<FuncAttr, parser::func::Declaration<'s>>>,
    pub types: Map<key::Type, ItemData<TypeAttr, TypeDeclaration<'s>>>,
    pub impls: Map<key::Impl, ItemData<ImplAttr, ImplDeclaration<'s>>>,
    pub vals: Map<key::Val, key::Func>,
    pub uses: Map<key::Use, ItemData<UseAttr, parser::r#use::Declaration<'s>>>,
    pub aliases: Map<key::Alias, ItemData<AliasAttr, parser::alias::Declaration<'s>>>,

    // Project-level attributes
    pub attributes: Vec<(key::File, ProjectAttr<'s>)>,

    pub method_member_mapping: SecondaryMap<key::Func, Option<key::Impl>>,
}

#[derive(Debug)]
pub struct TypeDeclaration<'s> {
    pub header: parser::ty::Header<'s>,

    pub body: TypeBody<'s>,
}

#[derive(Debug)]
pub enum TypeBody<'s> {
    Record(parser::ty::RecordBody<'s>),
    Sum(parser::ty::SumBody<'s>),
    Trait {
        associations: Map<key::AssociatedType, parser::r#impl::Association<'s>>,
        methods: Map<key::Method, key::Func>,
    },
    Alias(Tr<parser::ty::Type<'s>>),
}

// When constructing the AST we insert trait and impl member functions as ordinary functions and
// map them to their parent impl here.
#[derive(Debug)]
pub struct ImplDeclaration<'s> {
    pub header: parser::r#impl::Header<'s>,

    pub methods: Map<key::Method, key::Func>,
    pub associations: Map<key::AssociatedType, parser::r#impl::Association<'s>>,
}

/// Metadata for an item
#[derive(Debug)]
pub struct ItemData<Attr, Declaration> {
    pub item: key::Item,
    pub decl: Declaration,
    pub attr: Attr,
}

impl<A, D> std::ops::Deref for ItemData<A, D> {
    type Target = D;

    fn deref(&self) -> &Self::Target {
        &self.decl
    }
}

impl<Attr: Default + Clone, Declaration> ItemData<Attr, Declaration> {
    pub fn new(item: key::Item, decl: Declaration, attr: Attr) -> Self {
        Self { item, decl, attr }
    }

    // TODO: Add default optimization since 90% of items will be default
    pub fn get_attr(&self) -> Attr {
        self.attr.clone()
    }
}

/// Generic information about an item
#[derive(Debug, Clone)]
pub struct Item<'s> {
    pub file: key::File,
    pub attr: SharedAttr<'s>,
    pub kind: ItemKind,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Func(key::Func),
    ImplMethod(key::Impl, key::Method, key::Func),
    TraitMethod(key::Type, key::Method, key::Func),
    Type(key::Type),
    Impl(key::Impl),
    Val(key::Val),
    Use(key::Use),
    Alias(key::Alias),
}

impl<'s> AST<'s> {
    pub fn new() -> Self {
        Self {
            items: Map::new(),
            functions: Map::with_capacity(5),
            types: Map::with_capacity(2),
            impls: Map::with_capacity(0),
            uses: Map::with_capacity(0),
            vals: Map::with_capacity(0),
            aliases: Map::with_capacity(0),
            attributes: Vec::new(),
            method_member_mapping: SecondaryMap::new(),
        }
    }

    pub fn include_project(
        &mut self,
        root_name: &str,
        src_dir: &Path,
    ) -> (Vec<(parser::Error, key::File)>, Files) {
        info!("collecting AST for {}", src_dir.display());
        let mut collector = Collector::new(self);
        collector.include_project(root_name, src_dir);
        collector.finish()
    }
}
