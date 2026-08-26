use crate::hir::TypeKey;
use crate::prelude::*;
use crate::project::symbols::{self, Namespace, Resolve};
use lumina_parser as parser;
use owo_colors::OwoColorize;
use project::symbols::AlreadyExists;
use project::{Files, HeaderFile, ProjectNode, config};
use std::fmt;
use std::sync::Arc;

mod errors;

pub mod env;
mod project;
pub use project::ProjectPath;
pub use project::symbols::Symbols;

pub mod ast;
pub mod hir;

pub mod backend;
mod prelude;
mod target;
pub use target::Target;

pub mod key;

mod context;
pub use context::Context;

pub mod export;

pub fn compile(
    epanic: bool,
    target: Target,
    project: ProjectPath,
    env: env::Environment,
) -> Context<TranslationUnit> {
    let (dep_tree, errors) = project::Collector::root(project, env.lumina_directory.as_path());

    errors::set_epanic(epanic);

    for err in errors {
        match err {
            project::ProjectError::Config(error, path_buf) => {
                unsafe {
                    let src = std::fs::read_to_string(&path_buf).unwrap();
                    errors::switch_file(key::File(0), &src, path_buf.as_path());
                }

                match error {
                    config::Error::ParseError(error) => emit_ast_error(error),
                    config::Error::Invalid(span) => {
                        errors::err("project error")
                            .line(span, "unknown configuration item")
                            .emit();
                    }
                    config::Error::UnknownOperator(op) => {
                        errors::err("project error")
                            .line(op.span, format!("unknown operator `{op}`"))
                            .emit();
                    }
                    config::Error::ExpectedButGot(span, msg) => {
                        errors::err("project error")
                            .line(span, format!("expected {msg}"))
                            .emit();
                    }
                };
            }
            err => {
                let err = lumina_util::Error::err("project error").with_text(format!("{err:?}"));
                eprintln!("{err}");
            }
        }
    }

    info!("{dep_tree:#?}");

    dep_tree.traverse_in_parallel(Compilation { env: Arc::new(env), target })
}

#[derive(Clone)]
struct Compilation {
    target: Target,
    env: Arc<env::Environment>,
}

pub struct TranslationUnit {
    header: project::HeaderFile,

    langitems: LangItems,
}

// Lang items explicitly set by a project attribute in this project.
#[derive(Default)]
struct LangItems {
    default_listable: Option<key::Type>,
    default_string: Option<key::Type>,
}

impl fmt::Debug for TranslationUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.header)
    }
}

impl project::traverse::PassOverProjectTree for Compilation {
    type TUnit = TranslationUnit;

    fn for_project(self, ctx: Context<TranslationUnit>, node: &ProjectNode) {
        println!(" {} {}", "Compiling".green(), &node.config.name);

        let mut ast = ast::AST::new();

        // NOTE: We assume that the compiled project is the first one to reserve a key.
        let root_name = if node.key == key::Project::from_u32(0) {
            "main.lm"
        } else {
            "lib.lm"
        };

        let (errors, files) = ast.include_project(root_name, node.src_dir.as_path());

        for (err, file) in errors {
            files.switch_errors_file(file);
            emit_ast_error(err);
        }

        ctx.initialize_unit(node.key, files.count());

        // Symbols: Add the submodules implicitly declared by the filesystem tree
        ctx.in_project_mut(node.key, |unit| {
            sym_add_filesystem_childs(&mut unit.header.symbols, &files);
        });

        // Symbols: Add the submodules explicitly declared in the `config.lm` file
        sym_declare_dependencies(ctx.clone(), node.key);

        // Symbols: Add the items declared in this module.
        ctx.in_project_mut(node.key, |unit| {
            unit.header
                .symbols
                .declare_ast_items(self.target, &files, &ast);
        });

        // Symbols: Declare aliases in an alias table to be filled in later
        sym_declare_aliases(ctx.clone(), &files, node.key, &ast);

        // Symbols: Resolve the `use` items to declare additional items and submodules
        sym_resolve_and_declare_imports(ctx.clone(), &files, node.key, &ast);

        // HIR: Lower the translation nit to HIR
        hir::lower_project_to_hir(ctx.clone(), node, &files, &ast);

        ctx.in_project(node.key, |unit| {
            assert_eq!(ast.types.len(), unit.header.type_signatures.len());
            assert_eq!(ast.types.len(), unit.header.typedefs.len());

            trace!(
                "Finished lowering translation unit {}\n{}",
                &node.config.name, &unit.header
            );
        });
    }
}

impl Context<TranslationUnit> {
    fn resolve_module(&self, from: key::Project, m: symbols::Module) -> (key::Project, key::File) {
        match m {
            symbols::Module::Intra(file) => (from, file),
            symbols::Module::Inter(external, file) => {
                let ext_module_key = self.get_node(from).ext_as_unstable(external);
                (ext_module_key, file)
            }
        }
    }

    fn resolve_origin(&self, from: key::Project, origin: symbols::Origin) -> key::Project {
        match origin {
            symbols::Origin::Intra => from,
            symbols::Origin::Inter(external) => self.get_node(from).ext_as_unstable(external),
        }
    }

    fn name_of_type(&self, from: key::Project, tkey: hir::TypeKey) -> String {
        let project = self.resolve_origin(from, tkey.origin);

        self.in_project(project, |unit| {
            unit.header.type_signatures[tkey.key].name.clone()
        })
    }

    fn origin_to_module(&self, file: key::File, origin: symbols::Origin) -> symbols::Module {
        match origin {
            symbols::Origin::Intra { .. } => symbols::Module::Intra(file),
            symbols::Origin::Inter(external) => symbols::Module::Inter(external, file),
        }
    }

    /// `symbols::Unit::find` except we traverse across multiple translation units when external
    /// modules are encountered.
    fn find<'s>(
        &self,
        project: key::Project,
        ext: Option<key::External>,
        file: key::File,
        ns: Namespace,
        path: &[&'s str],
        check_globs: bool,
    ) -> InterResolved<'s> {
        if let ["builtin", from_builtin @ ..] = path {
            return InterResolved::Builtin(from_builtin.to_vec());
        }

        let mut traverse = FindTraverse::new(self.clone(), project, ns, check_globs);

        match ext {
            Some(ext) => traverse.seek_in_external(project, ext, file, path),
            None => traverse.seek(file, path),
        }
    }

    fn collect_listables(&mut self, for_: key::Project) -> (Vec<TypeKey>, Option<TypeKey>) {
        let mut listables = Vec::with_capacity(4);

        let resolve = self.find(
            for_,
            None,
            key::File(0),
            Namespace::Types,
            &["std", "langitem", "Listable"],
            true,
        );

        let InterResolved::Item(origin, symbols::Item::Type((trait_, _))) = resolve else {
            return (listables, None);
        };

        let of = TypeKey { origin, key: trait_ };

        self.for_implementations_of_trait(for_, of, |ty| match ty {
            hir::Type::Defined(ident, _) => listables.push(ident),
            hir::Type::List(_) => {
                panic!("list sugar used for Listable trait implementation is not allowed")
            }
            _ => {}
        });

        let default = self
            .in_origin(for_, origin, |unit| unit.langitems.default_listable)
            .map(|key| TypeKey { origin: origin, key });

        (listables, default)
    }

    // Collects all implementations of a trait in a project and its direct dependencies
    fn for_implementations_of_trait(
        &mut self,
        for_: key::Project,
        of: TypeKey,
        mut f: impl FnMut(hir::Type),
    ) {
        let trait_unstable_project = self.get_node(for_).origin_as_unstable(of.origin);

        self.in_project(for_, |unit| {
            // for implementations in this unit
            for impldef in unit.header.implementations.values() {
                if impldef.trait_ == of {
                    f(impldef.self_.clone());
                }
            }

            // for implementations in direct dependencies
            for (ext, dep) in unit.header.externals.iter() {
                if dep.indirect || symbols::Origin::Inter(ext) == of.origin {
                    continue;
                }

                let dep_project = self.get_node(for_).ext_as_unstable(ext);

                self.in_project(dep_project, |unit| {
                    for impldef in unit.header.implementations.values() {
                        let impl_trait_unstable = self
                            .get_node(for_)
                            .origin_as_unstable(impldef.trait_.origin);

                        if impldef.trait_.key == of.key
                            && impl_trait_unstable == trait_unstable_project
                        {
                            f(impldef.self_.clone());
                        }
                    }
                });
            }
        })
    }
}

#[derive(Debug)]
pub enum InterResolved<'s> {
    Item(symbols::Origin, symbols::Item),
    Module(symbols::Module),
    Builtin(Vec<&'s str>),
    NotFound {
        in_: key::File,
        at: &'s str,
        exists: bool,
    },
    Poison,
}

pub trait ResolvedItemKind: TryFrom<symbols::Item> {}

fn sym_add_filesystem_childs(sym: &mut symbols::Unit, files: &Files) {
    files.for_children(|parent, children| {
        for &child in children {
            let name = files.path(child).file_stem().unwrap().to_string_lossy();
            sym.declare_filesystem_child(parent, &name, child);
        }
    });
}

fn sym_declare_dependencies(ctx: Context<TranslationUnit>, from: key::Project) {
    let node = ctx.get_node(from);

    // Add the self-referential namespace to all files
    ctx.in_project_mut(from, |unit| {
        unit.header
            .symbols
            .def_project_self_namespace(&node.config.name);
    });

    // Add name for each explicitly declared dependency
    for ext in node.externals() {
        if let Some(dep_name) = node.ext_name(ext) {
            ctx.in_project_mut(from, |unit| {
                unit.header.symbols.def_unit_dependency(dep_name, ext);
            });
        }
    }

    // Set up `prelude` namespace and glob import
    match ctx.in_project(from, |unit| unit.header.symbols.get_dependency("std")) {
        Some(external) => {
            let project = ctx.external(from, external);
            find_and_def_prelude(&ctx, [from, project], |file| {
                symbols::Module::Inter(external, file)
            })
        }
        None => {
            trace!("edge-casing `std` for standard library project {from}");

            assert_eq!(
                ctx.project_nodes.name(from),
                "std",
                "missing standard library"
            );

            find_and_def_prelude(&ctx, [from, from], symbols::Module::Intra);
        }
    };
}

fn find_and_def_prelude<K>(ctx: &Context<TranslationUnit>, [from, in_]: [key::Project; 2], k: K)
where
    K: FnOnce(key::File) -> symbols::Module,
{
    let module = ctx.in_project(in_, |unit| {
        if let Resolve::Module(prelude_file) =
            unit.header
                .symbols
                .find(key::File::ROOT, Namespace::Modules, &["prelude"])
        {
            k(prelude_file)
        } else {
            panic!("Standard library module does not have a prelude file/folder")
        }
    });

    ctx.in_project_mut(from, |unit| {
        // Make the `prelude` namespace accessible from all files
        //
        // Glob import the `prelude` namespace in all files
        for file in unit.header.symbols.files() {
            unit.header.symbols.def_submodule(file, "prelude", module);
            unit.header.symbols.def_glob_import(file, module);
        }
    });
}

fn emit_err_module_not_found(span: Span, files: &Files, in_: key::File, at: &str, exists: bool) {
    let in_path = files.path(in_).display();
    let err = errors::err("module not found")
        .line(span, format!("no module named `{at}` in `{in_path}`"));

    if exists {
        err.text("hint: the item exists but is not public")
    } else {
        err
    }
    .emit();
}

fn emit_err_item_not_found(span: Span, files: &Files, in_: key::File, at: &str, exists: bool) {
    let in_path = files.path(in_).display();
    let err =
        errors::err("item not found").line(span, format!("no item named `{at}` in `{in_path}`"));

    if exists {
        err.text("hint: the item exists but is not public")
    } else {
        err
    }
    .emit();
}

fn emit_does_not_have_members(span: Span, name: &str) {
    errors::err("invalid use item")
        .line(span, format!("`{name}` does not have any members"))
        .emit();
}

fn emit_does_not_have_member(span: Span, member: &str) {
    errors::err("invalid use item")
        .line(span, format!("type does not have the member {member}"))
        .emit();
}

fn sym_resolve_and_declare_imports<'s>(
    ctx: Context<TranslationUnit>,
    files: &Files,
    from: key::Project,
    ast: &ast::AST<'s>,
) {
    for r#use in ast.uses.values() {
        let file = ast.items[r#use.item].file;
        let public = r#use.public;

        files.switch_errors_file(file);

        // Get the `as name` or default to last segment of the path
        let assign_to = r#use.assign_to.unwrap_or_else(|| {
            let span = r#use.path.span;
            r#use.path.as_slice().last().copied().unwrap().tr(span)
        });

        // Resolve the module to `use` from
        let segments = r#use.path.as_slice();
        let module = match ctx.find(from, None, file, Namespace::Modules, segments, true) {
            InterResolved::Item(_, _) => todo!(),
            InterResolved::Builtin(_) => {
                errors::err("invalid use item")
                    .line(assign_to.span, "can not `r#use` a builtin")
                    .emit();

                continue;
            }
            InterResolved::NotFound { in_, at, exists } => {
                emit_err_module_not_found(r#use.path.span, files, in_, at, exists);

                // TODO: Poison all exposed

                continue;
            }
            InterResolved::Poison => todo!(),
            InterResolved::Module(module) => module,
        };

        // Add the module as a submodule
        //
        // TODO: we need to be able to mark this as not public
        ctx.in_project_mut(from, |unit| {
            unit.header.symbols.def_submodule(file, *assign_to, module);
        });

        // The `[]` clause of the `use` item
        match &r#use.exposing {
            parser::r#use::Exposing::None => {}

            // We're glob-importing all public items from a module.
            //
            // To be somewhat inefficient, these will be resolved lazily later.
            parser::r#use::Exposing::All(_) => {
                ctx.in_project_mut(from, |unit| {
                    unit.header.symbols.def_glob_import(file, module);
                });
            }

            // We're importing an explicit set of items
            parser::r#use::Exposing::Set(exposed) => {
                for exposed in exposed {
                    let (ext, f) = match module {
                        symbols::Module::Intra(file) => (None, file),
                        symbols::Module::Inter(external, file) => (Some(external), file),
                    };

                    match ctx.find(from, ext, f, Namespace::Functions, &[exposed.name], true) {
                        InterResolved::Builtin(_) => errors::err("invalid use item")
                            .line(assign_to.span, "can not `r#use` a builtin")
                            .emit(),
                        InterResolved::Item(origin, item) => {
                            if let Err(AlreadyExists(..)) = ctx.in_project_mut(from, |unit| {
                                let oitem = symbols::WithOrigin { key: item, origin, public };
                                unit.header.symbols.def_item(file, exposed.name, oitem)
                            }) {
                                symbols::err_already_exists(exposed.span);
                            }

                            match &exposed.members {
                                parser::r#use::Members::All(span) => match item {
                                    symbols::Item::Type((type_, _)) => sym_expose_all_members(
                                        ctx.clone(),
                                        (from, file),
                                        module.origin(),
                                        type_,
                                        public,
                                    ),
                                    _ => emit_does_not_have_members(*span, exposed.name),
                                },
                                parser::r#use::Members::Members(member_set) => match item {
                                    symbols::Item::Type((type_, _)) => {
                                        for member in member_set {
                                            sym_expose_member(
                                                ctx.clone(),
                                                (from, file),
                                                module.origin(),
                                                type_,
                                                *member,
                                                public,
                                            );
                                        }
                                    }
                                    _ => emit_does_not_have_members(
                                        Span::from_elems(member_set, |member| member.span),
                                        exposed.name,
                                    ),
                                },
                                parser::r#use::Members::None => {}
                            }
                        }
                        InterResolved::Module(m) => {
                            ctx.in_project_mut(from, |unit| {
                                unit.header.symbols.def_submodule(file, exposed.name, m);
                            });
                        }
                        InterResolved::NotFound { in_, at, exists } => {
                            emit_err_item_not_found(exposed.span, files, in_, at, exists);

                            ctx.in_project_mut(from, |unit| {
                                unit.header.symbols.def_poison(file, exposed.name);
                            })
                        }
                        InterResolved::Poison => ctx.in_project_mut(from, |unit| {
                            unit.header.symbols.def_poison(file, exposed.name);
                        }),
                    }
                }
            }
        }
    }
}

fn sym_expose_member(
    ctx: Context<TranslationUnit>,
    (from, file): (key::Project, key::File),
    origin: symbols::Origin,
    type_: key::Type,
    member: Tr<&str>,
    public: bool,
) {
    let mut field_public_error = false;

    let item = ctx.in_origin(from, origin, |unit| {
        match unit.header.symbols.type_members(type_) {
            symbols::TypeKindSymbols::Sum { variants } => {
                variants.iter().find_map(|(variant, vname)| {
                    (vname == *member).then(|| symbols::Item::Variant(type_, variant))
                })
            }
            symbols::TypeKindSymbols::Record { fields } => {
                for (field, (is_pub, fname)) in fields {
                    if fname == *member {
                        match origin {
                            symbols::Origin::Intra if public && !*is_pub => {
                                field_public_error = true;
                                return None;
                            }
                            _ => return Some(symbols::Item::Field(type_, field)),
                        }
                    }
                }

                None
            }
            symbols::TypeKindSymbols::Trait { methods } => {
                methods.iter().find_map(|(method, mname)| {
                    (mname == *member).then(|| symbols::Item::Method(type_, method))
                })
            }
            symbols::TypeKindSymbols::Alias => unimplemented!("ReExport member of type alias"),
        }
    });

    if field_public_error {
        panic!("ET: field is not public and cannot be re-exported");
        // return;
    }

    let Some(item) = item else {
        emit_does_not_have_member(member.span, *member);
        return;
    };

    ctx.in_project_mut(from, |unit| {
        let oitem = symbols::WithOrigin { key: item, origin, public };
        unit.header.symbols.def_item(file, *member, oitem).ok()
    });
}

fn sym_expose_all_members(
    ctx: Context<TranslationUnit>,
    (from, file): (key::Project, key::File),
    origin: symbols::Origin,
    type_: key::Type,
    public: bool,
) {
    let members = ctx.in_origin(from, origin, |unit| {
        unit.header.symbols.type_members(type_).clone()
    });

    ctx.in_project_mut(from, |unit| match members {
        symbols::TypeKindSymbols::Sum { variants } => {
            for (variant, vname) in variants {
                let key = symbols::Item::Variant(type_, variant);
                let oitem = symbols::WithOrigin { origin, key, public };

                unit.header.symbols.def_item(file, vname, oitem).ok();
            }
        }
        symbols::TypeKindSymbols::Record { fields } => {
            for (field, (is_pub, fname)) in fields {
                if public && !is_pub {
                    continue;
                }

                let oitem =
                    symbols::WithOrigin { origin, key: symbols::Item::Field(type_, field), public };

                unit.header.symbols.def_item(file, fname, oitem).ok();
            }
        }
        symbols::TypeKindSymbols::Trait { methods } => {
            for (method, mname) in methods {
                let key = symbols::Item::Method(type_, method);
                let oitem = symbols::WithOrigin { origin, key, public };

                unit.header.symbols.def_item(file, mname, oitem).ok();
            }
        }
        symbols::TypeKindSymbols::Alias => unimplemented!("ReExport member of type alias"),
    });
}

fn sym_declare_aliases<'s>(
    ctx: Context<TranslationUnit>,
    files: &Files,
    from: key::Project,
    ast: &ast::AST<'s>,
) {
    // We want to be able to make aliases public
    //
    // But; to do that they need to be before the pass that declares imports
    //
    // However then we can't import aliases
    //
    // Hm... I think this can still work fine. We can declare aliases in Symbols to point to an
    // `Alias` and then lower those recursively like we do with functions. That way we can also
    // detect cycles since those aren't allowed in aliases.
    //
    // Although; that will need to be done in HIR
    //
    // We definitely shouldn't include aliases in Header though.
    //
    // So; we could make it a purely Symbols concept?
    //
    // Or have an alias table separately?
    //
    // Hm. One problem though is that types need to exist before aliases but aliases need to exist
    // before types......
    //
    // That would mean that we need to recursively lower types as well, if we don't want to add
    // Alias indirection in the HIR.
    //
    // Hm. actually, we wanted aliases to be able to be full statictypes and not just names as
    // well. So; we can't actually get away from them being an Header concept.
    //
    // All type signatures can be lowered ahead of time.
    // Actually, they're already declared in `AST` at this point regardless.
    // So; I suppose we don't need the hir types to be lowered at all.
    //
    // We can just link up the alias'es recursively.
    // But since they can hold StaticType that does need to be done in HIR meaning that we still
    // need an alias table at least temporarily because StaticType occurs after Import.
    //
    // And aliases need to be done recursively still.
    // SO: the HIR lower of types will need an alias lookup
    // But that means that types will need an alias variant...
    //
    // Hm. No; I'm pretty sure we can get away with only having an alias lookup for alias lower.
    //
    // OK: I think we should limit type aliases to be for types at least. That should make things a
    // little easier.
    //
    // OK: Them mapping to entire StaticType and not just an ID means that we do need to add them
    // to header.
    //
    // Let's re-collect from SecondaryMap to Map still
    //
    // NOTE: we need to be very careful with how we later destruct the Symbols::Item to edge-case
    // the Alias consistently.

    // `sym_resolve_and_declare_imports`

    for alias in ast.aliases.values() {
        todo!();
        // TODO: But; we will still need to declare them in Symbols for imports to work.
    }
}

pub type AliasTable = Map<key::Alias, Option<hir::Type>>;

#[derive(new)]
struct FindTraverse {
    ctx: Context<TranslationUnit>,
    origin: key::Project,
    ns: Namespace,
    check_globs: bool,
}

impl FindTraverse {
    fn seek_in_external<'s>(
        &mut self,
        project: key::Project,
        in_external: key::External,
        in_external_file: key::File,
        xs: &[&'s str],
    ) -> InterResolved<'s> {
        let in_ext_unstable = self.ctx.get_node(project).ext_as_unstable(in_external);

        if xs.is_empty() {
            InterResolved::Module(symbols::Module::Inter(in_external, in_external_file))
        } else {
            match self.ctx.in_project(in_ext_unstable, |unit| {
                unit.header.symbols.find(in_external_file, self.ns, xs)
            }) {
                Resolve::Redirect(external, file, xs) => {
                    let indirect_ext = self.ctx.get_or_add_indirect_dependency(
                        self.origin,
                        project,
                        [external, in_external],
                    );

                    self.seek_in_external(in_ext_unstable, indirect_ext, file, xs)
                }
                Resolve::Item(item) => {
                    let origin = match item.origin {
                        symbols::Origin::Intra => {
                            if item.public || project == self.origin {
                                symbols::Origin::Inter(in_external)
                            } else {
                                return InterResolved::NotFound {
                                    in_: in_external_file,
                                    at: xs.last().unwrap(),
                                    exists: true,
                                };
                            }
                        }
                        // symbols::Origin::Intra { .. } => symbols::Module::Intra(file),
                        symbols::Origin::Inter(external) => {
                            let indirect_ext = self.ctx.get_or_add_indirect_dependency(
                                self.origin,
                                project,
                                [external, in_external],
                            );

                            symbols::Origin::Inter(indirect_ext)
                        }
                    };

                    InterResolved::Item(origin, item.key)
                }
                Resolve::Module(file) => {
                    let m = symbols::Module::Inter(in_external, file);
                    InterResolved::Module(m)
                }
                Resolve::NotFound { in_, at } => InterResolved::NotFound { in_, at, exists: false },
                Resolve::Poison => InterResolved::Poison,
            }
        }
    }

    fn seek<'s>(&mut self, file: key::File, path: &[&'s str]) -> InterResolved<'s> {
        let resolve = self.ctx.clone().in_project(self.origin, |unit| {
            unit.header.symbols.find(file, self.ns, path)
        });

        match resolve {
            Resolve::Redirect(external, file, xs) => {
                self.seek_in_external(self.origin, external, file, xs)
            }
            Resolve::Item(item) => InterResolved::Item(item.origin, item.key),
            Resolve::Module(file) => {
                let module = symbols::Module::Intra(file);
                InterResolved::Module(module)
            }
            Resolve::Poison => InterResolved::Poison,
            Resolve::NotFound { in_, at } => {
                if self.check_globs {
                    self.check_globs = false;

                    let found_glob_item = self.ctx.clone().in_project(self.origin, |unit| {
                        for module in unit.header.symbols.glob_imports(file) {
                            let glob_resolve = match module {
                                symbols::Module::Intra(glob_imported_file) => {
                                    self.seek(glob_imported_file, path)
                                }
                                symbols::Module::Inter(external, file) => {
                                    self.seek_in_external(self.origin, external, file, path)
                                }
                            };

                            if !matches!(glob_resolve, InterResolved::NotFound { .. }) {
                                return Some(glob_resolve);
                            }
                        }

                        None
                    });

                    if let Some(resolve) = found_glob_item {
                        return resolve;
                    }
                }

                InterResolved::NotFound { in_, at, exists: false }
            }
        }
    }
}

fn emit_ast_error(err: parser::Error) {
    let base = errors::err("syntax error");

    match err {
        parser::Error::ExpectedButGot(span, exp, got) => {
            base.line(span, format!("expected {exp} but got {got}"))
        }
        parser::Error::ExpectedTokenButGot(span, exp, got) => base.line(
            span,
            format!("expected {exp} but got this {}", got.describe()),
        ),
        parser::Error::MissingSquareForExtractor(span) => {
            base.line(span, "missing square bracket for extractor pattern")
        }
        parser::Error::FnNeedsParenthesis(span) => base.line(span, "parenthesis are needed here"),
        parser::Error::InvalidAttributes(span, _) => base.line(span, "invalid attribute"),
        parser::Error::ToplevelWhere(span) => base
            .line(span, "unexpected `where`")
            .text("hint: did you mean to use `when`?")
            .text("hint: are you missing indentation?"),
        parser::Error::BadIndentation(span) => base.line(span, "invalid indentation for this item"),
        parser::Error::BadDefault(span, _) => {
            base.line(span, "default keyword is only valid for `impl` blocks")
        }
        parser::Error::BadIndentForMatch(span, diff_conflict) => {
            base.line(span, "invalid indentation for match expression")
        }
        parser::Error::BadHeaderForWhere(span, _) => base.line(
            span,
            "only function items are allowed to be defined in `where` block",
        ),
        parser::Error::Unmatched(span, open) => base.line(span, format!("unmatched {open}")),
        parser::Error::InvalidTraitMember(span) => base.line(
            span,
            format!("only functions and associated types may be defined as members of a trait"),
        ),
        parser::Error::InvalidNestedMatch { new, .. } => base.line(new, "unexpected `match`"),
        parser::Error::ConflictingBars(indent_conflict) => todo!(),
        parser::Error::NestedWhere { previous, kw } => base
            .line(kw, "unexpected `where`")
            .text("where blocks can not be nested")
            .line(previous, "previous where block starts here"),
    }
    .emit()
}
