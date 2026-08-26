use crate::Target;
use crate::{ast, ast::AST, errors, key};
use derive_more::From;
use itertools::Itertools;
use lumina_key::{Map, entity_impl};
use lumina_parser::{self as parser, r#use::Exposing};
use lumina_typesystem as ts;
use lumina_util::{Highlighting, Span};
use owo_colors::OwoColorize;
use serde::Serialize;
use std::collections::{HashMap, HashSet, hash_map::Entry};
use std::fmt;
use tracing::error;
use tracing::trace;

use super::Files;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Name(u32);
entity_impl!(Name, "name");

// Attaches metadata to a key depending on whether it originates from this translation unit or an External.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct WithOrigin<T> {
    pub key: T,
    pub origin: Origin,
    pub public: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum Origin {
    Intra,
    Inter(key::External),
}

impl<T> WithOrigin<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithOrigin<U> {
        WithOrigin { key: f(self.key), origin: self.origin, public: self.public }
    }

    pub fn local(public: bool, key: T) -> Self {
        Self { key, origin: Origin::Intra, public }
    }

    pub fn public(key: T) -> Self {
        Self { key, origin: Origin::Intra, public: true }
    }

    pub fn private(key: T) -> Self {
        Self { key, origin: Origin::Intra, public: false }
    }

    pub fn external(in_: key::External, public: bool, key: T) -> Self {
        Self { key, origin: Origin::Inter(in_), public }
    }
}

type TypeParameterCount = usize;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Symbols {
    functions: HashMap<String, WithOrigin<key::Func>>,
    vals: HashMap<String, WithOrigin<key::Val>>,
    types: HashMap<String, WithOrigin<(key::Type, TypeParameterCount)>>,

    // May be accessed with `.fieldname`
    //
    // bool is field visibility, which is distinct from type visibility.
    fields: HashMap<String, WithOrigin<(key::Type, (bool, key::Field))>>,

    // Methods that are available directly in a files namespace rather than through its trait.
    methods: HashMap<String, WithOrigin<(key::Type, key::Method)>>,

    // Are meant to be accessed through the type namespace instead but it's good to detect when
    // users make this mistake.
    variants: HashMap<String, WithOrigin<(key::Type, key::Variant)>>,

    glob_imports: Vec<Module>,

    poisoned: HashSet<String>,

    // Use'd and filesystem children of this module
    submodules: HashMap<String, Module>,
}

#[derive(Debug)]
pub struct Unit {
    dependencies: HashMap<String, key::External>,
    type_members: Map<key::Type, TypeKindSymbols>,
    dotcall_lookup: HashMap<ts::KnownTypeRoot<key::Type>, HashMap<String, (Origin, key::Func)>>,
    pub arena: Map<key::File, Symbols>,
}

impl Unit {
    pub fn new(files: usize) -> Self {
        Self {
            dependencies: HashMap::new(),
            type_members: Map::new(),
            dotcall_lookup: HashMap::new(),
            arena: std::iter::repeat_with(Symbols::default)
                .take(files)
                .collect(),
        }
    }

    pub fn files(&self) -> impl Iterator<Item = key::File> + use<> {
        self.arena.keys()
    }

    pub fn glob_imports(&self, file: key::File) -> impl Iterator<Item = Module> + '_ {
        self.arena[file].glob_imports.iter().copied()
    }

    /// Resolve an identifier within this unit. May return Resolve::Redirect if the resolve was
    /// unable to finish due to it needing to continue in another unit.
    pub fn find<'a, 's>(
        &self,
        in_: key::File,
        priority: Namespace,
        path: &'a [&'s str],
    ) -> Resolve<'a, 's> {
        if let [segment, xs @ ..] = path {
            if let Some(ext) = self.dependencies.get(*segment) {
                trace!(
                    "found an external dependency, redirecting to `{ext}` for `{}`",
                    xs.iter().format(":")
                );

                return Resolve::Redirect(*ext, key::File::ROOT, xs);
            }
        }

        match self.resolve(in_, priority, path) {
            Resolve::NotFound { in_: err_in, at } if err_in == in_ && path.len() == 1 => {
                if let Some(method) = self.arena[in_].methods.get(path[0]) {
                    let oitem = method.map(|(type_, method)| Item::Method(type_, method));
                    Resolve::Item(oitem)
                } else {
                    Resolve::NotFound { in_, at }
                }
            }
            resolve => resolve,
        }
    }

    // Similar to `find` but does not check the first segment of the path for builtin/dependency namespaces or methods.
    fn resolve<'a, 's>(
        &self,
        in_: key::File,
        priority: Namespace,
        path: &'a [&'s str],
    ) -> Resolve<'a, 's> {
        let symbols = &self.arena[in_];

        match path {
            [name] => {
                let order = match priority {
                    Namespace::Functions => {
                        [Namespace::Functions, Namespace::Types, Namespace::Fields].as_slice()
                    }
                    Namespace::Types => {
                        [Namespace::Types, Namespace::Functions, Namespace::Fields].as_slice()
                    }
                    Namespace::Modules => [
                        Namespace::Modules,
                        Namespace::Types,
                        Namespace::Functions,
                        Namespace::Fields,
                    ]
                    .as_slice(),
                    Namespace::Fields => {
                        [Namespace::Fields, Namespace::Functions, Namespace::Types].as_slice()
                    }
                };

                for &p in order {
                    match p {
                        Namespace::Functions => {
                            if let Some(func) = symbols.functions.get(*name) {
                                return Resolve::Item(func.map(Item::Func));
                            }

                            if let Some(val) = symbols.vals.get(*name) {
                                return Resolve::Item(val.map(Item::Val));
                            }

                            if let Some(link) = symbols.variants.get(*name) {
                                let link = link.map(|(type_key, variant_key)| {
                                    Item::Variant(type_key, variant_key)
                                });
                                return Resolve::Item(link);
                            }
                        }
                        Namespace::Types => {
                            if let Some(type_) = symbols.types.get(*name) {
                                return Resolve::Item(type_.map(Item::Type));
                            }
                        }
                        Namespace::Modules => {
                            if let Some(m) = symbols.submodules.get(*name) {
                                return match m {
                                    Module::Intra(file) => Resolve::Module(*file),
                                    Module::Inter(external, file) => {
                                        Resolve::Redirect(*external, *file, &[])
                                    }
                                };
                            }
                        }
                        Namespace::Fields => {
                            // TODO: Check visibility of fields
                            if let Some(ty) = symbols.fields.get(*name) {
                                let item = ty.map(|(ty, (_, name))| Item::Field(ty, name));
                                return Resolve::Item(item);
                            }
                        }
                    }

                    // If the prioritised namespace failed, then check the poison set before
                    // progressing to the alternate namespaces.
                    if p == priority && symbols.poisoned.contains(*name) {
                        return Resolve::Poison;
                    }
                }

                if let Some(link) = symbols.fields.get(*name) {
                    let link = link.map(|(type_key, (_, field))| Item::Field(type_key, field));
                    return Resolve::Item(link);
                }

                Resolve::NotFound { in_, at: *name }
            }
            [segment, xs @ ..] => {
                if let Some(ty) = symbols.types.get(*segment) {
                    if let TypeKindSymbols::Trait { methods } = &self.type_members[ty.key.0] {
                        if let [name] = xs {
                            if let Some(method) = methods
                                .iter()
                                .find_map(|(method, mname)| (mname == name).then_some(method))
                            {
                                let item = ty.map(|(trait_, _)| Item::Method(trait_, method));
                                return Resolve::Item(item);
                            }
                        }
                    }

                    if let TypeKindSymbols::Sum { variants } = &self.type_members[ty.key.0] {
                        if let [name] = xs {
                            if let Some(variant) = variants
                                .iter()
                                .find_map(|(variant, vname)| (vname == name).then_some(variant))
                            {
                                let item = ty.map(|(type_, _)| Item::Variant(type_, variant));
                                return Resolve::Item(item);
                            }
                        }
                    }
                }

                if let Some(m) = symbols.submodules.get(*segment) {
                    trace!(
                        "found a submodule, continuing from {m:?} for `{}`",
                        xs.iter().format(":")
                    );

                    return match m {
                        Module::Intra(file) => self.resolve(*file, priority, xs),
                        Module::Inter(external, file) => Resolve::Redirect(*external, *file, xs),
                    };
                }

                if symbols.poisoned.contains(*segment) {
                    return Resolve::Poison;
                }

                Resolve::NotFound { in_, at: *segment }
            }
            [] => panic!("empty path given to `find`"),
            // [] => Resolve::Module(in_),
        }
    }

    pub fn insert_dotcall_lookup(
        &mut self,
        ty: ts::KnownTypeRoot<key::Type>,
        name: impl Into<String>,
        func: (Origin, key::Func),
    ) {
        self.dotcall_lookup
            .entry(ty)
            .or_insert_with(HashMap::new)
            .insert(name.into(), func);
    }

    pub fn dotcall_lookup(
        &self,
        ty: ts::KnownTypeRoot<key::Type>,
        name: &str,
    ) -> Option<(Origin, key::Func)> {
        self.dotcall_lookup
            .get(&ty)
            .and_then(|funcs| funcs.get(name))
            .copied()
    }

    pub fn def_project_self_namespace(&mut self, project_name: &str) {
        for file in self.files() {
            let m = Module::Intra(key::File::ROOT);
            self.def_submodule(file, project_name, m);
        }
    }

    pub fn def_poison(&mut self, file: key::File, name: impl Into<String>) {
        self.arena[file].poisoned.insert(name.into());
    }

    pub fn def_glob_import(&mut self, file: key::File, m: Module) {
        self.arena[file].glob_imports.push(m);
    }

    pub fn def_item(
        &mut self,
        file: key::File,
        name: impl ToString,
        item: WithOrigin<Item>,
    ) -> DefResult<Item> {
        match item.key {
            Item::Func(func) => self
                .def_func(file, name, item.map(|_| func))
                .map_err(|err| err.map(Item::Func)),
            Item::Val(val) => self
                .def_val(file, name, item.map(|_| val))
                .map_err(|err| err.map(Item::Val)),
            Item::Type(type_) => {
                self.arena[file]
                    .types
                    .insert(name.to_string(), item.map(|_| type_));

                Ok(())
            }
            Item::Variant(type_, variant) => {
                self.arena[file]
                    .variants
                    .insert(name.to_string(), item.map(|_| (type_, variant)));

                Ok(())
            }
            Item::Field(type_, field) => {
                self.arena[file]
                    .fields
                    .insert(name.to_string(), item.map(|_| (type_, (false, field))));

                Ok(())
            }
            Item::Method(type_, method) => {
                self.arena[file]
                    .methods
                    .insert(name.to_string(), item.map(|_| (type_, method)));

                Ok(())
            }
        }
    }

    pub fn def_func(
        &mut self,
        file: key::File,
        name: impl ToString,
        item: WithOrigin<key::Func>,
    ) -> DefResult<key::Func> {
        let name = name.to_string();
        let symbols = &mut self.arena[file];

        match symbols.functions.entry(name) {
            Entry::Vacant(entry) => {
                entry.insert(item);
                Ok(())
            }
            Entry::Occupied(occupied) => {
                symbols.poisoned.insert(occupied.key().clone());
                Err(AlreadyExists(occupied.get().clone()))
            }
        }
    }

    pub fn def_val(
        &mut self,
        file: key::File,
        name: impl ToString,
        item: WithOrigin<key::Val>,
    ) -> DefResult<key::Val> {
        let name = name.to_string();
        let symbols = &mut self.arena[file];

        match symbols.vals.entry(name) {
            Entry::Vacant(entry) => {
                entry.insert(item);
                Ok(())
            }
            Entry::Occupied(occupied) => {
                symbols.poisoned.insert(occupied.key().clone());
                Err(AlreadyExists(occupied.get().clone()))
            }
        }
    }

    pub fn def_type_kind(
        &mut self,
        file: key::File,
        name: impl Into<String>,
        item: WithOrigin<(key::Type, usize)>,
        kind: impl FnOnce(&mut Symbols) -> TypeKindSymbols,
    ) -> DefResult<key::Type> {
        let name = name.into();
        let symbols = &mut self.arena[file];

        match symbols.types.entry(name) {
            Entry::Vacant(entry) => {
                entry.insert(item);
                let kind = kind(symbols);
                assert_eq!(item.key.0, self.type_members.push(kind));
                Ok(())
            }
            Entry::Occupied(occupied) => {
                error!("type already exists: {}", occupied.key());
                symbols.poisoned.insert(occupied.key().clone());
                Err(AlreadyExists(occupied.get().map(|(type_, _)| type_)))
            }
        }
    }

    // TODO: As this method depends on Files and AST, it should not be a method or be declared in
    // this module.
    pub fn declare_ast_items(&mut self, target: Target, files: &Files, ast: &AST) {
        for item in ast.items.values() {
            let file = item.file;

            if !item.attr.platforms.is_empty()
                && item
                    .attr
                    .platforms
                    .iter()
                    .all(|plat| !target.include_for(plat))
            {
                continue;
            }

            files.switch_errors_file(file);

            match item.kind {
                ast::ItemKind::Func(key) => {
                    let func = &ast.functions[key];
                    let public = ast.items[func.item].attr.public;

                    if let Err(AlreadyExists(..)) =
                        self.def_func(file, &func.decl.header.name, WithOrigin::local(public, key))
                    {
                        err_already_exists(func.decl.header.name.span);
                    }
                }
                ast::ItemKind::Val(key) => {
                    let func = &ast.vals[key];
                    let initfunc = &ast.functions[*func];
                    let public = ast.items[initfunc.item].attr.public;

                    if let Err(AlreadyExists(..)) = self.def_val(
                        file,
                        &initfunc.decl.header.name,
                        WithOrigin::local(public, key),
                    ) {
                        err_already_exists(initfunc.decl.header.name.span);
                    }
                }
                ast::ItemKind::Type(key) => {
                    let type_ = &ast.types[key];
                    let public = ast.items[type_.item].attr.public;
                    let span = type_.header.span;
                    let name = type_.header.name;
                    let plen = type_.header.type_params.len();

                    match &type_.body {
                        ast::TypeBody::Record(record_body) => {
                            let fields: Map<key::Field, (bool, String)> = record_body
                                .fields
                                .values()
                                .map(|field| (true, field.1.to_string()))
                                .collect();

                            let item = WithOrigin::local(public, (key, plen));
                            let result = self.def_type_kind(file, name, item, |symbols| {
                                for (field, (fpublic, name)) in fields.iter() {
                                    symbols.fields.insert(
                                        name.clone(),
                                        WithOrigin::local(*fpublic, (key, (*fpublic, field))),
                                    );
                                }

                                TypeKindSymbols::Record { fields: fields.into() }
                            });

                            if let Err(AlreadyExists(..)) = result {
                                err_already_exists(span);
                            }
                        }
                        ast::TypeBody::Sum(sum_body) => {
                            let variants: Map<key::Variant, String> = sum_body
                                .variants
                                .values()
                                .map(|variant| variant.1.to_string())
                                .collect();

                            let item = WithOrigin::local(public, (key, plen));
                            let result = self.def_type_kind(file, name, item, |symbols| {
                                for (variant, name) in variants.iter() {
                                    symbols.variants.insert(
                                        name.clone(),
                                        WithOrigin::local(true, (key, variant)),
                                    );
                                }

                                TypeKindSymbols::Sum { variants: variants.into() }
                            });

                            if let Err(AlreadyExists(..)) = result {
                                err_already_exists(span);
                            }
                        }
                        ast::TypeBody::Trait { associations, methods } => {
                            let methods: Map<key::Method, String> = methods
                                .values()
                                .map(|&func_key| ast.functions[func_key].header.name.to_string())
                                .collect();

                            let item = WithOrigin::local(public, (key, plen));
                            let result = self.def_type_kind(file, name, item, |_| {
                                TypeKindSymbols::Trait { methods: methods.into() }
                            });

                            if let Err(AlreadyExists(..)) = result {
                                err_already_exists(span);
                            }
                        }
                        ast::TypeBody::Alias(_) => {
                            let item = WithOrigin::local(public, (key, plen));
                            let result =
                                self.def_type_kind(file, name, item, |_| TypeKindSymbols::Alias);

                            if let Err(AlreadyExists(..)) = result {
                                err_already_exists(span);
                            }
                        }
                    }
                }

                ast::ItemKind::Impl(_impl_) => {}

                ast::ItemKind::TraitMethod(_, _, fkey) => {
                    let func = &ast.functions[fkey];

                    // We let local functions shadow local methods as a method can still be called
                    // through its trait.
                    //
                    // Although we set visibility to public because accessing it through the module
                    // namespace and not the trait namespace is only for the local module.
                    //
                    // TODO: Currently it's available in the module namespace for the unit, not
                    // just module. Should we fix that?
                    let _ =
                        self.def_func(file, &func.decl.header.name, WithOrigin::local(false, fkey));

                    // The method is already declared in symbols through the insertion of the type.
                }

                // Impl methods are only accessible directly by annotating a trait method call.
                ast::ItemKind::ImplMethod(..) => {}

                // These require forward-declarations to be properly handled.
                //
                // So they have their own iteration afterwards instead.
                ast::ItemKind::Use(_) | ast::ItemKind::Alias(_) => {}
            }
        }
    }

    /// Declare a dependency that is accessible from any file in this unit
    pub fn def_unit_dependency(&mut self, name: impl Into<String>, ext: key::External) {
        self.dependencies.insert(name.into(), ext);
    }

    pub fn def_submodule(&mut self, in_: key::File, name: impl Into<String>, m: Module) {
        self.arena[in_].submodules.insert(name.into(), m);
    }

    pub fn get_dependency(&self, name: &str) -> Option<key::External> {
        self.dependencies.get(name).copied()
    }

    pub fn declare_filesystem_child(&mut self, parent: key::File, name: &str, child: key::File) {
        trace!("assigning `{name}` as child `{child}` in `{parent}`");

        self.arena[parent]
            .submodules
            .insert(name.into(), Module::Intra(child));
    }

    pub fn type_members(&self, key: key::Type) -> &TypeKindSymbols {
        &self.type_members[key]
    }

    fn poison_all_exposing(&mut self, file: key::File, exposed: &Exposing<'_>) {
        match exposed {
            Exposing::None | Exposing::All(_) => {}
            Exposing::Set(exposed) => {
                for e in exposed {
                    self.arena[file].poisoned.insert(e.name.to_string());
                    self.poison_all_members(file, &e.members)
                }
            }
        }
    }

    fn poison_all_members(&mut self, file: key::File, m: &parser::r#use::Members<'_>) {
        match m {
            parser::r#use::Members::All(_) | parser::r#use::Members::None => {}
            parser::r#use::Members::Members(members) => {
                for m in members {
                    self.arena[file].poisoned.insert(m.to_string());
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Resolve<'a, 's> {
    Redirect(key::External, key::File, &'a [&'s str]),
    Item(WithOrigin<Item>),
    Module(key::File),
    NotFound { in_: key::File, at: &'s str },
    Poison,
}

#[derive(Clone, Copy, PartialEq, Eq, From, Hash)]
pub enum Module {
    Intra(key::File),
    Inter(key::External, key::File),
}

impl Module {
    pub fn file(&self) -> key::File {
        match self {
            Module::Inter(_, file) | Module::Intra(file) => *file,
        }
    }

    pub fn origin(&self) -> Origin {
        match self {
            Module::Intra(_) => Origin::Intra,
            Module::Inter(external, _) => Origin::Inter(*external),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Namespace {
    Functions,
    Types,
    Modules,
    Fields,
}

type DefResult<K> = Result<(), AlreadyExists<K>>;

#[derive(Clone, Debug)]
pub struct AlreadyExists<K>(pub WithOrigin<K>);

impl<K> AlreadyExists<K> {
    fn map<U>(self, f: impl FnOnce(K) -> U) -> AlreadyExists<U> {
        AlreadyExists(self.0.map(f))
    }
}

pub fn err_already_exists(span: Span) {
    errors::err("duplicate item name").line(span, "").emit();
}

#[derive(From, Debug, Clone, PartialEq, Eq, Copy)]
pub enum Item {
    Func(key::Func),
    Type((key::Type, TypeParameterCount)),
    Val(key::Val),
    Variant(key::Type, key::Variant),
    Field(key::Type, key::Field),
    Method(key::Type, key::Method),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKindSymbols {
    Sum {
        variants: Map<key::Variant, String>,
    },
    Record {
        fields: Map<key::Field, (bool, String)>,
    },
    Trait {
        methods: Map<key::Method, String>,
    },
    Alias,
}

impl<T: fmt::Debug> fmt::Debug for WithOrigin<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.origin {
            Origin::Intra => write!(f, "{:?}", self.key),
            Origin::Inter(external) => {
                write!(f, "{:?} {} {:#?}", self.key, "in".purple(), external)
            }
        }
    }
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Module::Intra(file) => write!(f, "Intra({file})"),
            Module::Inter(external, file) => write!(f, "Inter({external}, {file})"),
        }
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Module::Intra(file) => file.fmt(f),
            Module::Inter(external, file) => write!(f, "{external}·{file}"),
        }
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "symbols::Unit {{\n")?;

        for (name, ext) in &self.dependencies {
            writeln!(f, "  {} {ext} {} {name}", "use".keyword(), "as".keyword())?;
        }
        if !self.dependencies.is_empty() {
            writeln!(f)?;
        }

        let mut has_members = false;
        let arrow = "=>".symbol();
        for (ty, type_syms) in &self.type_members {
            match type_syms {
                TypeKindSymbols::Sum { variants } if !variants.is_empty() => {
                    has_members = true;
                    write!(f, "\n  {} {ty}:", "type".keyword())?;
                    write!(
                        f,
                        "\n    {}",
                        variants.iter().format_with("\n  ", |(var, name), f| {
                            f(&format_args!("{name} {arrow} {ty}:{var}"))
                        })
                    )?;
                }
                TypeKindSymbols::Record { fields } if !fields.is_empty() => {
                    has_members = true;
                    write!(f, "\n  {} {ty}:", "type".keyword())?;
                    write!(
                        f,
                        "\n    {}",
                        fields
                            .iter()
                            .format_with("\n    ", |(field, (pub_, name)), f| {
                                f(&format_args!(
                                    "{name} {arrow} {}{ty}.{field}",
                                    if *pub_ { "pub " } else { "" }.keyword()
                                ))
                            })
                    )?;
                }
                TypeKindSymbols::Trait { methods } if !methods.is_empty() => {
                    has_members = true;
                    write!(f, "\n  {} {ty}:", "type".keyword())?;
                    write!(
                        f,
                        "\n    {}",
                        methods.iter().format_with("\n    ", |(m, name), f| {
                            f(&format_args!("{name} {arrow} {ty}:{m}"))
                        })
                    )?;
                }
                TypeKindSymbols::Alias => {
                    has_members = true;
                    write!(f, "\n    alias")?;
                }
                _ => {}
            }
        }
        if has_members {
            writeln!(f)?;
        }

        for (root, funcs) in self.dotcall_lookup.iter() {
            writeln!(f, "  Dotcalls for {root:?}:")?;
            for (name, (origin, func)) in funcs {
                writeln!(f, "    {} {name} => {origin}:{func}", "fn".keyword())?;
            }
        }
        if !self.dotcall_lookup.is_empty() {
            writeln!(f)?;
        }

        for (file, sym) in &self.arena {
            writeln!(
                f,
                "  {file}:\n    {}",
                format!("{sym}").lines().format("\n    ")
            )?;
        }

        write!(f, "}}")
    }
}

impl fmt::Display for Origin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Origin::Intra => Ok(()),
            Origin::Inter(external) => write!(f, "{external}·"),
        }
    }
}

impl<T: fmt::Display> fmt::Display for WithOrigin<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.origin.fmt(f)?;
        self.key.fmt(f)
    }
}

impl fmt::Display for Symbols {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (fname, key) in &self.functions {
            writeln!(f, "{} {fname} -> {key}", "fn".keyword())?;
        }

        for (vname, key) in &self.vals {
            writeln!(f, "{} {vname} -> {key}", "val".keyword())?;
        }

        for (tname, key) in &self.types {
            let key = key.map(|(type_, _plen)| type_);
            writeln!(f, "{} {tname} -> {key}", "type".keyword())?;
        }

        for (fname, key) in &self.fields {
            writeln!(
                f,
                "{} {fname} -> {}{}.{}",
                "field".keyword(),
                key.origin,
                key.key.0,
                key.key.1.1
            )?;
        }

        for (vname, key) in &self.variants {
            writeln!(
                f,
                "{} {vname} -> {}{}:{}",
                "variant".keyword(),
                key.origin,
                key.key.0,
                key.key.1
            )?;
        }

        for module in &self.glob_imports {
            writeln!(f, "{} {module}", "glob_use".keyword())?;
        }

        for name in &self.poisoned {
            writeln!(f, "{} {name}", "poison".keyword())?;
        }

        for (name, module) in &self.submodules {
            writeln!(f, "{} {name} {module}", "use".keyword())?;
        }

        Ok(())
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    pub const ROOT: key::File = key::File::ROOT;
    pub const RECORDS: key::File = key::File(1);
    pub const SUMS: key::File = key::File(2);
    pub const FUNCS: key::File = key::File(3);
    pub const TRAITS: key::File = key::File(4);

    pub const MAIN: key::Func = key::Func(0);
    pub const PRINT: key::Func = key::Func(1);
    pub const ADD: key::Func = key::Func(2);
    pub const SHOW_FUNC: key::Func = key::Func(3);

    pub const POINT: key::Type = key::Type(0);
    pub const PERSON: key::Type = key::Type(1);
    pub const MAYBE: key::Type = key::Type(2);
    pub const RESULT: key::Type = key::Type(3);
    pub const COLOR: key::Type = key::Type(4);
    pub const LIST: key::Type = key::Type(5);
    pub const FUNCTOR: key::Type = key::Type(6);
    pub const SHOW: key::Type = key::Type(7);
    pub const CLONE: key::Type = key::Type(8);
    pub const DEFAULT: key::Type = key::Type(9);

    impl Unit {
        fn def_record(&mut self, name: &str, item: WithOrigin<key::Type>, fields: Vec<&str>) {
            self.def_type_kind(RECORDS, name, item.map(|type_| (type_, 0)), |symbols| {
                for (i, field) in fields.iter().enumerate() {
                    let fkey = key::Field::from_u32(i as u32);
                    let field_item = WithOrigin::local(true, (item.key, (true, fkey)));
                    symbols.fields.insert(field.to_string(), field_item);
                }

                let fields = fields
                    .into_iter()
                    .map(|name| (true, name.to_string()))
                    .collect();

                TypeKindSymbols::Record { fields }
            })
            .unwrap();
        }

        fn def_sum(&mut self, name: &str, item: WithOrigin<key::Type>, variants: Vec<&str>) {
            self.def_type_kind(SUMS, name, item.map(|type_| (type_, 0)), |symbols| {
                for (i, field) in variants.iter().enumerate() {
                    let variantkey = key::Variant::from_u32(i as u32);
                    let field_item = WithOrigin::local(true, (item.key, variantkey));
                    symbols.variants.insert(field.to_string(), field_item);
                }

                let variants = variants.into_iter().map(|name| name.to_string()).collect();

                TypeKindSymbols::Sum { variants }
            })
            .unwrap();
        }

        fn def_trait(&mut self, name: &str, item: WithOrigin<key::Type>, methods: Vec<&str>) {
            self.def_type_kind(TRAITS, name, item.map(|type_| (type_, 0)), |_| {
                let methods = methods.into_iter().map(|name| name.to_string()).collect();
                TypeKindSymbols::Trait { methods }
            })
            .unwrap();
        }
    }

    impl Unit {
        pub fn test() -> Unit {
            fn public<T>(v: T) -> WithOrigin<T> {
                WithOrigin::public(v)
            }

            let mut sym = Unit::new([ROOT, RECORDS, SUMS, FUNCS, TRAITS].len());

            sym.declare_filesystem_child(ROOT, "records", RECORDS);
            sym.declare_filesystem_child(ROOT, "sums", SUMS);
            sym.declare_filesystem_child(ROOT, "funcs", FUNCS);
            sym.declare_filesystem_child(ROOT, "traits", TRAITS);

            sym.def_project_self_namespace("test");

            sym.def_record("Point", public(POINT), vec!["x", "y"]);
            sym.def_record("Person", public(PERSON), vec!["name", "age", "id"]);

            // Sum types
            sym.def_sum("Maybe", public(MAYBE), vec!["Just", "Nothing"]);
            sym.def_sum("Result", public(RESULT), vec!["Ok", "Err"]);
            sym.def_sum("Color", public(COLOR), vec!["Red", "Green", "Blue"]);
            sym.def_sum("List", public(LIST), vec!["Cons", "Nil"]);

            // Functions
            sym.def_func(FUNCS, "main", public(MAIN)).unwrap();
            sym.def_func(FUNCS, "print", public(PRINT)).unwrap();
            sym.def_func(FUNCS, "add", public(ADD)).unwrap();
            sym.def_func(FUNCS, "Show", public(SHOW_FUNC)).unwrap();

            // Traits
            sym.def_trait("Functor", public(FUNCTOR), vec!["fmap"]);
            sym.def_trait("Show", public(SHOW), vec!["show"]);
            sym.def_trait("Clone", public(CLONE), vec!["clone"]);
            sym.def_trait("Default", public(DEFAULT), vec!["default"]);

            sym
        }
    }

    fn item<'a, 's>(v: Item) -> Resolve<'a, 's> {
        Resolve::Item(WithOrigin::public(v))
    }

    #[test]
    fn intra_unit_find() {
        let sym = Unit::test();
        let file = key::File::ROOT;

        assert_eq!(
            sym.find(file, Namespace::Functions, &["funcs", "Show"]),
            item(Item::Func(SHOW_FUNC))
        );

        assert_eq!(
            sym.find(file, Namespace::Types, &["traits", "Show"]),
            item(Item::Type((SHOW, 0)))
        );

        assert_eq!(
            sym.find(file, Namespace::Functions, &["funcs", "aaaaaaaaaaaa"]),
            Resolve::NotFound { at: "aaaaaaaaaaaa", in_: FUNCS },
        );
    }

    #[test]
    fn project_self_namespace() {
        let sym = Unit::test();
        let file = key::File::ROOT;

        assert_eq!(
            sym.find(file, Namespace::Functions, &["test", "funcs", "Show"]),
            item(Item::Func(SHOW_FUNC))
        );
    }
}
