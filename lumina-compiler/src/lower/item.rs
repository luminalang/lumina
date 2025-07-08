use super::LoweredProject;
use cranelift::codegen::entity::SecondaryMap;
use lumina_parser as parser;
use lumina_util::{Identifier, Spanned, Tr};
use object::pe::ImageLoadConfigCodeIntegrity;
use std::{
    collections::HashMap,
    ffi::{OsStr, OsString},
    fs::DirEntry,
    io::Read,
    path::Path,
    sync::RwLockReadGuard,
};

use crate::{config, deptree::ProjectNode, files::Files};

#[derive(Default)]
pub struct ParsedProject<'s> {
    items: HashMap<Identifier<'s>, Item<'s>>,

    glob_imports: HashMap<Identifier<'s>, ()>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct AbsoluteIdent(String);

// type GlobImport = Identifier | (config::Dependency, Identifier)

pub struct ProjectBuilder<'a, 's> {
    node: &'a ProjectNode,
    project: ParsedProject<'s>,
    ctx: rvsdg::TranslationUnitContext,
    omega: rvsdg::id::Region,

    files: Files,

    // The items exposed by this translation unit
    items: HashMap<AbsoluteIdent, Item<'s>>,

    // External dependencies of this translation unit
    // externals: SecondaryMap<rvsdg::id::Argument, External<'s>>,
    externals: ExtDeps<'s>,

    // Used for lifting declared items to toplevel scope with absolute paths
    //
    // TODO: if we move the OsString to whatever we use for `Sources` we can use `'s` instead.
    //
    // actually, we sort of *have* to do that for this to even work.
    //
    // Although; if we're not even traversing modules anymore and just canoncalizing everything
    // then it might as well just be string?
    //
    // Hm. It feels hacky to not use the proper idents.
    // They might still be useful for other stuff.
    path: Vec<String>,

    // TODO: I don't think we'll actually need this?
    //
    // Nah nvm we definitely do. Because; otherwise we can't type check properly.
    // Since inference can fail on foreign functions that have an explicit signature.

    // What would `use` actually do?
    //
    // We currently lower functions to nodes right away. So; they'll have external
    // dependencies.
    //
    // I suppose we want to just store these for now. And then; we'll have a second
    // pass over the nodes that'll try to connect things.
    //
    // Ye that'd work. We can't *create* the inputs now due to glob imports.
    //
    // Oh ye and this is where we can desugar paths since all modules will become
    // a single unit.
    //
    // Oh no the plan is to take it a step further since we have the deps. So; we
    // can resolve directly.
    //
    // Although those would still be inputs. We'd theoretically not even need the
    // `deps` here and we can hook that up after.
    //
    // So; we can reduce the state and then add that to the second path.
    // Theoretically we could even parelalize reading the file for this, and just
    // add them on a queue. Because they're technically independent. So we'd have
    // one processing and one file reading thread.
    //
    // But that's overthinking it for now. This component will effectively become
    // standalone without needint state. Which is lovely.
    deps: HashMap<&'a config::Dependency, RwLockReadGuard<'a, Option<LoweredProject>>>,
}

enum Item<'s> {
    Declaration(rvsdg::id::AnyNode),
    External(rvsdg::id::Argument),
    UseAlias(Identifier<'s>),
    UseGlob(config::Dependency, Identifier<'s>),
    Poison,
}

impl<'a, 's> ProjectBuilder<'a, 's> {
    pub fn new(
        node: &'a ProjectNode,
        deps: HashMap<&'a config::Dependency, RwLockReadGuard<'a, Option<LoweredProject>>>,
    ) -> Self {
        let ctx = rvsdg::TranslationUnitContext::new();
        let omega = ctx.region;
        Self {
            node,
            files: Files::new(),
            project: ParsedProject::default(),
            externals: ExtDeps::new(omega),
            omega,
            deps,
            path: vec![],
            ctx,
            items: HashMap::new(),
        }
    }

    pub fn finish(self) -> rvsdg::TranslationUnitContext {
        self.ctx
    }

    fn canonicalize_item(&self, name: &str) -> AbsoluteIdent {
        AbsoluteIdent(
            self.path
                .iter()
                .map(|string| string.as_str())
                .chain(std::iter::once(name))
                .collect(),
        )
    }

    pub fn include_project(&mut self, node: &ProjectNode) -> () {
        let src_dir = std::fs::read_dir(&node.src_dir).unwrap();

        for entry in src_dir {
            let entry = entry.unwrap();
            let path = entry.path();
            if let Err(err) = self.include_entry(&path) {
                let err = lumina_util::Error::error("general file error").with_text(format!(
                    "Could not read {}: {}",
                    entry.path().display(),
                    err
                ));

                eprintln!("{err}");
            }
        }
    }

    fn include_entry(&mut self, path: &Path) -> Result<(), std::io::Error> {
        let Some(file_name) = path.file_name() else {
            return Ok(());
        };

        if path.extension() == Some(&OsStr::new("lm")) && path.is_file() {
            if file_name != OsStr::new("lib") && file_name != OsStr::new("main") {
                let utf8 = file_name.to_str().expect("filename is not valid UTF-8");
                self.path.push(utf8.to_string());
            }

            let file_key = self.files.open(path)?;

            unsafe {
                let src = self.files.get_unsafe(file_key);
                self.include_file(&src);
            }

            return Ok(());
        }

        if path.is_dir() {
            todo!();
        }

        Ok(())
    }

    fn include_file(&mut self, src: &'s str) {
        // We probably don't need to store the items anywhere intermediately.
        //
        // We can just create the nodes and then connect them later.
        //
        // Also; I think we're gonna start with only incrementally storing non-monomorphised RVSDG.
        let mut parser = parser::Parser::new(src);

        while let Some((span, item)) = parser.item() {
            match item {
                parser::Declaration::ModuleAttribute(span, trs) => todo!(),
                parser::Declaration::Function(declaration) => self.include_func(declaration),
                parser::Declaration::Alias(declaration) => todo!(),
                parser::Declaration::Type(declaration) => todo!(),
                parser::Declaration::Impl(declaration) => todo!(),
                parser::Declaration::Use(declaration) => self.include_use(declaration),
                parser::Declaration::Val(declaration) => todo!(),
                parser::Declaration::Failure => todo!(),
            }
        }
    }

    fn include_func(&mut self, declaration: parser::func::Declaration<'s>) {
        let mut flower = super::func::FuncNodeLower::new(&declaration.header, &mut self.ctx);

        flower.patterns(&declaration.header.params);

        if let Some(body) = declaration.body.as_ref() {
            flower.body(body);
        }
    }

    fn include_use(&mut self, declaration: parser::r#use::Declaration<'s>) {
        let path = declaration.path.as_slice();

        let name = declaration
            .assign_to
            .unwrap_or_else(|| (*path.last().unwrap()).tr(declaration.path.span));

        let abs = self.canonicalize_item(*name);

        match declaration.exposing {
            parser::r#use::Exposing::All(span) => {
                self.add_ext_as_item(abs, External::Glob { in_: declaration.path });
            }
            parser::r#use::Exposing::None => {
                self.add_ext_as_item(abs, External::Pending(declaration.path));
            }
            parser::r#use::Exposing::Set(exposed) => {
                for e in exposed {
                    match e.value.members {
                        parser::r#use::Members::All(span) => todo!(),
                        parser::r#use::Members::None => todo!(),
                        parser::r#use::Members::Members(members) => {
                            members.into_iter().for_each(|m| {
                                let abs = self.canonicalize_item(*m);
                                let ext = External::Member { in_: declaration.path.clone() };
                                self.add_ext_as_item(abs, ext);
                            });
                        }
                    }
                }
            }
        };
    }

    // Add an external dependency for the translation unit and expose it as an item
    fn add_ext_as_item(&mut self, abs: AbsoluteIdent, ext: External<'s>) -> rvsdg::Argument {
        let arg = self.externals.add(&mut self.ctx, ext);
        let item = Item::External(arg.id);
        self.items.insert(abs, item);
        arg
    }
}

/// External dependencies for the translation unit.
struct ExtDeps<'s> {
    omega: rvsdg::id::Region,
    map: SecondaryMap<rvsdg::id::Argument, External<'s>>,
}

#[derive(Clone)]
enum External<'s> {
    Pending(Tr<Identifier<'s>>),
    Member { in_: Tr<Identifier<'s>> },
    Glob { in_: Tr<Identifier<'s>> },
    Poison,
    None,
}

impl<'s> ExtDeps<'s> {
    fn new(omega: rvsdg::id::Region) -> Self {
        Self { omega, map: SecondaryMap::with_default(External::None) }
    }

    pub fn add(
        &mut self,
        ctx: &mut rvsdg::TranslationUnitContext,
        ext: External<'s>,
    ) -> rvsdg::Argument {
        let arg = ctx.in_region(self.omega, |ctx| ctx.add_argument());
        self.map[arg.id] = ext;
        arg
    }
}
