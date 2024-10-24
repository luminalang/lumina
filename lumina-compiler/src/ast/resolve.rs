use super::Sources;
use crate::impl_map_arrow_fmt;
use itertools::Itertools;
use lumina_key as key;
use lumina_key::{Map, M};
use lumina_parser::Type;
use lumina_util::Tr;
use lumina_util::{Highlighting, Span};
use std::collections::HashMap;
use std::fmt;
use tracing::trace;

#[derive(Clone, Copy, Debug)]
pub struct Mod<K> {
    pub visibility: Visibility,
    pub module: key::Module,
    pub key: K,
}

impl<K> Mod<K> {
    pub fn map<O>(self, f: impl FnOnce(K) -> O) -> Mod<O> {
        Mod {
            key: f(self.key),
            module: self.module,
            visibility: self.visibility,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Visibility {
    Project(key::Module),
    Public,
}

impl Visibility {
    pub fn from_public_flag(module: key::Module, public: bool) -> Self {
        if public {
            Visibility::Public
        } else {
            Visibility::Project(module)
        }
    }
}

pub struct Lookups<'s> {
    modules: Map<key::Module, Namespaces<'s>>,
    libs: HashMap<&'static str, HashMap<String, key::Module>>,
}

impl<'s> Lookups<'s> {
    pub fn new() -> Lookups<'s> {
        let modules = Map::new();

        let mut libs = HashMap::new();
        libs.insert("std", HashMap::new());
        libs.insert("ext", HashMap::new());

        Lookups { libs, modules }
    }

    pub fn find_lib(&self, root: &str, name: &str) -> Option<key::Module> {
        self.libs.get(root).and_then(|lib| lib.get(name)).copied()
    }

    pub fn to_field_lookup(&self) -> Map<key::Module, HashMap<&'s str, Vec<M<key::Record>>>> {
        self.modules
            .values()
            .map(|namespace| {
                namespace
                    .accessors
                    .iter()
                    .map(|(name, pos)| {
                        (
                            *name,
                            pos.iter().map(|r| r.key.0.inside(r.module)).collect(),
                        )
                    })
                    .collect()
            })
            .collect()
    }

    pub fn new_root_module(&mut self, parent: Option<key::Module>) -> key::Module {
        let mut namespaces = Namespaces::default();
        namespaces.kind = ModuleKind::Root {
            parent,
            stdlib: parent.map(|m| self.is_stdlib(m)).unwrap_or(false),
        };
        self.modules.push(namespaces)
    }

    pub fn is_entry_module(&mut self, m: key::Module) -> bool {
        match self.modules[m].kind {
            ModuleKind::Root { parent, .. } => parent.is_none(),
            _ => false,
        }
    }

    pub fn is_stdlib(&self, module: key::Module) -> bool {
        match self.modules[module].kind {
            ModuleKind::Root { stdlib: true, .. } => true,
            ModuleKind::Root { parent: Some(module), .. } => self.is_stdlib(module),
            ModuleKind::Root { .. } => false,
            ModuleKind::Member { root } => self.is_stdlib(root),
        }
    }

    pub fn new_member_module(&mut self, root: key::Module) -> key::Module {
        let mut namespaces = Namespaces::default();
        namespaces.kind = ModuleKind::Member { root };
        self.modules.push(namespaces)
    }

    pub fn new_lib(&mut self, in_: &'static str, lib: String) -> key::Module {
        let mut namespaces = Namespaces::default();
        namespaces.kind = ModuleKind::Root { parent: None, stdlib: in_ == "std" };
        let module = self.modules.push(namespaces);
        self.libs.get_mut(in_).unwrap().insert(lib, module);
        module
    }

    pub fn declare<T: EntityT>(
        &mut self,
        module: key::Module,
        visibility: Visibility,
        name: &'s str,
        dstmodule: key::Module,
        entity: T,
    ) -> Option<Mod<T>> {
        let m = Mod { visibility, module: dstmodule, key: entity };
        T::insert(m, name, &mut self.modules[module])
    }

    pub fn declare_alias(
        &mut self,
        module: key::Module,
        visibility: Visibility,
        name: &'s str,
        dst: Tr<Type<'s>>,
    ) {
        let dst = Mod { module, visibility, key: dst };
        self.modules[module].aliases.insert(name, dst);
    }

    pub fn declare_accessor(
        &mut self,
        module: key::Module,
        visibility: Visibility,
        name: &'s str,
        type_: M<key::Record>,
        field: key::Field,
    ) {
        let m = Mod { visibility, module: type_.0, key: (type_.1, field) };
        self.modules[module]
            .accessors
            .entry(name)
            .or_default()
            .push(m);
    }

    pub fn declare_module_link(
        &mut self,
        module: key::Module,
        visibility: Visibility,
        name: String,
        dst: key::Module,
    ) {
        let m = Mod { module, visibility, key: dst };
        let children = &mut self.modules[module].child_modules;

        // If we're shadowing the same entity with itself then make sure we use the highest
        // visibility of either version.
        if let Some(previous) = children.get_mut(&name) {
            if previous.key == dst && previous.module == module {
                if matches!(visibility, Visibility::Public)
                    || matches!(previous.visibility, Visibility::Public)
                {
                    previous.visibility = Visibility::Public;
                    return;
                }
            }
        }

        children.insert(name, m);
    }

    pub fn alias_as_func<'t, 'a>(
        &self,
        from: key::Module,
        alias: &'t Type<'a>,
    ) -> Option<(&'t [&'a str], Mod<NFunc>)> {
        if let Type::Defined(path, params) = alias {
            if params.is_empty() {
                if let Ok(f) = self.resolve_func(from, path.path.as_slice()) {
                    if let Entity::Func(nfunc) = f.key {
                        return Some((path.path.as_slice(), f.map(|_| nfunc)));
                    }
                }
            }
        }

        None
    }

    /// Resolve an entity and prioritise the function namespace
    pub fn resolve_func<'a>(
        &self,
        from: key::Module,
        path: &[&'a str],
    ) -> Result<Mod<Entity<'a, 's>>, ImportError<'a>> {
        self.resolve(from, Namespace::Functions, path, false)
    }
    /// Resolve an entity and prioritise the type namespace
    pub fn resolve_type<'a>(
        &self,
        from: key::Module,
        path: &[&'a str],
    ) -> Result<Mod<Entity<'a, 's>>, ImportError<'a>> {
        self.resolve(from, Namespace::Types, path, false)
    }
    /// Resolve an entity and prioritise the module/imports namespace
    pub fn resolve_module<'a>(
        &self,
        from: key::Module,
        path: &[&'a str],
    ) -> Result<Mod<Entity<'a, 's>>, ImportError<'a>> {
        self.resolve(from, Namespace::Modules, path, false)
    }
    pub fn resolve_import(&self, from: key::Module, name: &str) -> Option<Mod<key::Module>> {
        self.modules[from]
            .child_modules
            .get(name)
            .copied()
            .or_else(|| match self.modules[from].kind {
                ModuleKind::Member { root } => self.resolve_import(root, name),
                _ => None,
            })
    }

    pub fn resolve_langitem<'a>(
        &self,
        from: key::Module,
        names: &[&'a str],
    ) -> Result<Mod<Entity<'a, 's>>, ImportError<'a>> {
        let root = self.get_root_module(from);
        self.resolve(root, Namespace::Functions, names, true)
    }

    fn resolve<'a>(
        &self,
        from: key::Module,
        namespace: Namespace,
        mut path: &[&'a str],
        mut ignore_vis: bool,
    ) -> Result<Mod<Entity<'a, 's>>, ImportError<'a>> {
        trace!(
            "attempting to resolve {} from {from} in namespace {namespace:?}",
            path.iter().format(":")
        );

        // If we access prelude via absolute path, we want to ignore namespace rules
        // This is mainly so that standard library modules can directly call private langitems
        if Some(["std", "prelude"].as_slice()) == path.get(..2) {
            ignore_vis = true;
        }

        let start_at = match self.libs.get(path[0]) {
            Some(libs) => {
                let lname = path.get(1).copied().unwrap_or("");
                path = &path[2..];

                match libs.get(lname) {
                    None => return Err(ImportError::LibNotInstalled(lname)),
                    Some(module) => *module,
                }
            }
            None if path[0] == "project" => {
                path = &path[1..];
                self.get_root_module(from)
            }
            None => from,
        };

        self.resolve_in(from, namespace, start_at, path, ignore_vis)
            .or_else(|err| match err {
                ImportError::BadAccess(..) | ImportError::LibNotInstalled(..) => Err(err),
                err => self
                    .resolve_in(from, namespace, key::PRELUDE, path, ignore_vis)
                    .map_err(|_| err),
            })
    }

    pub fn resolve_entity_in<'a>(
        &self,
        origin: key::Module,
        module: key::Module,
        name: &'a str,
        igv: bool,
    ) -> Result<Mod<Entity<'a, 's>>, ImportError<'a>> {
        self.resolve_in(origin, Namespace::Types, module, &[name], igv)
    }

    fn poison_or_not_found<'a>(&self, module: key::Module, name: &'a str) -> ImportError<'a> {
        self.modules[module]
            .try_poison_namespace(name)
            .map(|_| ImportError::Poison)
            .unwrap_or(ImportError::NotFound(module, name))
    }

    fn resolve_in<'a>(
        &self,
        origin: key::Module,
        namespace: Namespace,
        module: key::Module,
        path: &[&'a str],
        ignore_vis: bool,
    ) -> Result<Mod<Entity<'a, 's>>, ImportError<'a>> {
        let entity = match path {
            [] => Ok(Mod {
                key: Entity::Module(module),
                module,
                visibility: Visibility::Public,
            }),
            [x] => match namespace {
                Namespace::Modules => self
                    .resolve_import(module, *x)
                    .map(|m| m.map(Entity::Module))
                    .or_else(|| self.modules[module].try_namespace(namespace, *x))
                    .ok_or_else(|| self.poison_or_not_found(module, *x)),
                _ => self.modules[module]
                    .try_namespace(namespace, *x)
                    .ok_or_else(|| self.poison_or_not_found(module, *x)),
            },
            [x, xs @ ..] => {
                match self.resolve_import(module, x) {
                    Some(m) => {
                        if !self.is_valid_reachability(origin, m.visibility) && !ignore_vis {
                            return Err(ImportError::BadAccess(m.visibility, "module", x));
                        }

                        self.resolve_in(origin, namespace, m.key, xs, ignore_vis)
                    }

                    // no module of this name found, but it could still be a type/trait
                    None if xs.len() == 1 => {
                        match self.modules[module].try_namespace(Namespace::Types, *x) {
                            None if self.modules[module].try_poison_namespace(*x).is_some() => {
                                Err(ImportError::Poison)
                            }
                            None => Err(ImportError::ModNotFound(module, *x)),
                            Some(entity) => match entity.key {
                                Entity::Type(type_) => {
                                    Ok(entity.map(|_| Entity::Member(type_, xs[0])))
                                }
                                _ => Err(ImportError::ModNotFound(module, *x)),
                            },
                        }
                    }

                    None => Err(ImportError::ModNotFound(module, *x)),
                }
            }
        }?;

        if !self.is_valid_reachability(origin, entity.visibility) && !ignore_vis {
            return Err(ImportError::BadAccess(
                entity.visibility,
                entity.key.describe(),
                path.last().unwrap(),
            ));
        }

        Ok(entity)
    }

    pub fn resolve_accessor(
        &self,
        module: key::Module,
        name: &'s str,
    ) -> &[Mod<(key::Record, key::Field)>] {
        self.modules[module]
            .accessors
            .get(name)
            .map(Vec::as_ref)
            .unwrap_or(&[])
    }

    pub fn is_valid_reachability<'a>(&self, current: key::Module, visibility: Visibility) -> bool {
        match visibility {
            Visibility::Public => true,
            Visibility::Project(m) if self.are_members_of_same_project(&[current, m]) => true,
            Visibility::Project(m) if self.is_stdlib(current) && self.is_stdlib(m) => true,
            _ => false,
        }
    }

    fn are_members_of_same_project(&self, modules: &[key::Module]) -> bool {
        let mut ms = modules.iter().copied().map(|of| self.get_root_module(of));
        let m = ms.next().unwrap();
        ms.all(|m_| m_ == m)
    }

    fn get_root_module(&self, of: key::Module) -> key::Module {
        match self.modules[of].kind {
            ModuleKind::Root { parent, .. } => match parent {
                Some(p) => self.get_root_module(p),
                None => of,
            },
            ModuleKind::Member { root } => root,
        }
    }

    pub fn get_parent(&self, of: key::Module) -> Option<key::Module> {
        match self.modules[of].kind {
            ModuleKind::Root { parent, .. } => match parent {
                Some(p) => Some(p),
                None => None,
            },
            ModuleKind::Member { root } => Some(root),
        }
    }

    /// Standard libraries do not have to be declared as dependencies but are still lazily included
    /// when imported. This checks whether the given path is an un-included standard library
    pub fn lib_should_be_included<'a, 'b>(
        &self,
        path: &'a [&'b str],
    ) -> Option<(&'b str, &'a [&'b str])> {
        (path[0] == "std")
            .then(|| {
                self.libs
                    .get("std")
                    .expect("compiler is running without standard library")
                    .get(path[1])
                    .is_none()
                    .then_some((path[1], &path[2..]))
            })
            .flatten()
    }
}

#[derive(Debug, Clone, Copy)]
enum Namespace {
    Functions,
    Types,
    Modules,
}

#[derive(Debug)]
pub enum Entity<'a, 's> {
    Module(key::Module),
    Func(NFunc),
    Type(key::TypeKind),
    Member(key::TypeKind, &'a str),
    /// Even though it's parsed as a type; it could be a function. You need to check
    Alias(Tr<Type<'s>>),
}

impl<'a, 's> Entity<'a, 's> {
    pub fn describe(&self) -> &'static str {
        match self {
            Entity::Alias(ty) => match &ty.value {
                Type::Defined(_, params) if params.is_empty() => "alias",
                _ => "type",
            },
            Entity::Module(_) => "module",
            Entity::Func(_) => "function",
            Entity::Type(_) => "type",
            Entity::Member(..) => "member",
        }
    }
}

#[derive(Debug)]
pub enum ImportError<'s> {
    BadAccess(Visibility, &'static str, &'s str),
    LibNotInstalled(&'s str),
    NotFound(key::Module, &'s str),
    ModNotFound(key::Module, &'s str),
    Poison,
}

pub trait EntityT: Sized {
    fn insert<'s>(
        m: Mod<Self>,
        name: &'s str,
        namespaces: &mut Namespaces<'s>,
    ) -> Option<Mod<Self>>;
}

macro_rules! impl_entityt {
    ($t:ty, $field:ident) => {
        impl EntityT for $t {
            fn insert<'s>(
                m: Mod<Self>,
                name: &'s str,
                namespaces: &mut Namespaces<'s>,
            ) -> Option<Mod<Self>> {
                namespaces.$field.insert(name, m)
            }
        }
    };
}

impl_entityt!(Span, poisoned);
impl_entityt!(NFunc, funcs);
impl_entityt!(key::TypeKind, types);

#[derive(Default, Debug)]
pub struct Namespaces<'s> {
    aliases: HashMap<&'s str, Mod<Tr<Type<'s>>>>,
    funcs: HashMap<&'s str, Mod<NFunc>>,
    types: HashMap<&'s str, Mod<key::TypeKind>>,

    child_modules: HashMap<String, Mod<key::Module>>,

    kind: ModuleKind,

    accessors: HashMap<&'s str, Vec<Mod<(key::Record, key::Field)>>>,

    poisoned: HashMap<&'s str, Mod<Span>>,
}

#[derive(Debug)]
pub enum ModuleKind {
    Root {
        parent: Option<key::Module>,
        stdlib: bool,
    },
    Member {
        root: key::Module,
    },
}

impl Default for ModuleKind {
    fn default() -> Self {
        ModuleKind::Root { parent: None, stdlib: false }
    }
}

impl<'s> Namespaces<'s> {
    fn try_namespace<'a>(
        &self,
        namespace: Namespace,
        name: &'a str,
    ) -> Option<Mod<Entity<'a, 's>>> {
        match namespace {
            Namespace::Functions => self
                .try_function_namespace(name)
                .or_else(|| self.try_alias_namespace(name))
                .or_else(|| self.try_type_namespace(name))
                .or_else(|| self.try_child_imports(name)),
            Namespace::Types => self
                .try_type_namespace(name)
                .or_else(|| self.try_alias_namespace(name))
                .or_else(|| self.try_function_namespace(name))
                .or_else(|| self.try_child_imports(name)),
            Namespace::Modules => self
                .try_child_imports(name)
                .or_else(|| self.try_alias_namespace(name))
                .or_else(|| self.try_function_namespace(name))
                .or_else(|| self.try_type_namespace(name)),
        }
    }

    fn try_function_namespace<'a>(&self, name: &'a str) -> Option<Mod<Entity<'a, 's>>> {
        self.funcs.get(name).copied().map(|m| m.map(Entity::Func))
    }

    fn try_alias_namespace<'a>(&self, name: &'a str) -> Option<Mod<Entity<'a, 's>>> {
        self.aliases
            .get(name)
            .cloned()
            .map(|m| m.map(Entity::Alias))
    }

    fn try_child_imports<'a>(&self, name: &'a str) -> Option<Mod<Entity<'a, 's>>> {
        self.child_modules
            .get(name)
            .copied()
            .map(|m| m.map(Entity::Module))
    }

    fn try_type_namespace<'a>(&self, name: &'a str) -> Option<Mod<Entity<'a, 's>>> {
        self.types.get(name).copied().map(|m| m.map(Entity::Type))
    }

    fn try_poison_namespace<'a>(&self, name: &'a str) -> Option<Mod<Span>> {
        self.poisoned.get(name).copied()
    }
}

/// Pointer to something in the function namespace
#[derive(Clone, Copy, Debug)]
pub enum NFunc {
    Key(key::Func),
    Method(key::Trait, key::Method),
    SumVar(key::Sum, key::Variant),
    Val(key::Val),
}

impl Sources {
    pub fn emit_lookup_err(&self, span: Span, module: key::Module, kind: &str, err: ImportError) {
        match err {
            ImportError::Poison => {}
            ImportError::LibNotInstalled(str) => self
                .error("library not found")
                .m(module)
                .eline(span, format!("no library named {str} is installed"))
                .emit(),
            ImportError::NotFound(_, name) => self
                .error("identifier not found")
                .m(module)
                .eline(span, format!("no {kind} named {name}"))
                .emit(),
            ImportError::ModNotFound(m, name) => self
                .error("module not found")
                .m(module)
                .eline(
                    span,
                    format!("`{}` has no module named `{name}`", self.name_of_module(m)),
                )
                .emit(),
            ImportError::BadAccess(_vis, k, name) if k == "module" => self
                .error("module not found")
                .m(module)
                .eline(
                    span,
                    format!("there is a module named {name} but it's not public"),
                )
                .emit(),
            ImportError::BadAccess(_vis, k, name) => self
                .error("identifier not found")
                .m(module)
                .eline(span, "")
                .text(format!("there is a {k} named {name} but it's not public"))
                .emit(),
        }
    }
}

impl_map_arrow_fmt!(<'s> std::fmt::Debug; for Lookups<'s>;  ("modules", modules, |(k, v)| format!("{k} → {v:#?}")));

impl fmt::Display for NFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NFunc::Key(key) => key.fmt(f),
            NFunc::Method(trait_, method) => write!(f, "{trait_}{}{method}", ':'.symbol()),
            NFunc::SumVar(sum, var) => write!(f, "{sum}{}{var}", ':'.symbol()),
            NFunc::Val(ro) => ro.fmt(f),
        }
    }
}

impl<K: fmt::Display> fmt::Display for Mod<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({}) {}", "pub".keyword(), self.visibility, self.key)
    }
}

impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Visibility::Project(m) => write!(f, "project_of({m})"),
            Visibility::Public => "public".fmt(f),
        }
    }
}
