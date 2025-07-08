use config::ProjectConfig;
use lumina_util::Identifier;
use prelude::*;
use std::{
    ffi::OsStr,
    io::Read,
    path::{Path, PathBuf},
};

pub mod env;

mod lower;

mod files;
use files::Files;

pub mod backend;
mod prelude;

mod target;
pub use target::Target;

pub mod config;

pub mod key;

mod deptree;

pub fn testing(target: Target, project: PathBuf, env: env::Environment) {
    let (dep_tree, errors) = deptree::Collector::root(env.ext_directory.clone(), &project);

    for err in errors {
        let err = lumina_util::Error::error("project error").with_text(format!("{err:?}"));
        eprintln!("{err}");
    }

    info!("{dep_tree:#?}");

    let root = dep_tree.root;
    lower::project(dep_tree, root);
}

pub struct Projects<'src> {
    projects: PrimaryMap<key::Project, Project<'src>>,
}

pub struct Project<'src> {
    rvsdg: rvsdg::TranslationUnitContext,
    external_dependencies: SecondaryMap<rvsdg::id::Input, ExternalDependency>,
    _a: &'src (),
    // files: PrimaryMap<key::File, File<'src>>,
}

#[derive(Clone)]
enum ExternalDependency {
    Function,
    // ...
}

// pub struct File<'src> {
//     items: HashMap<&'src str, Item<'src>>,
//     file_glob_imports: HashMap<&'src str, key::File>,
//
//     // Mapping of field -> record
//     fields: HashMap<&'src str, &'src str>,
//
//     // Mapping of variant -> sum
//     variants: HashMap<&'src str, &'src str>,
// }
//
// pub struct Item<'src> {
//     attr: Attributes,
//     name: &'src str,
//     kind: ItemKind<'src>,
// }
//
// struct Attributes {}
//
// // TODO: But since re-exports still exist, we can't store Module under ItemKind.
//
// pub enum ItemKind<'src> {
//     Module(key::File),
//     Function(&'src ()),
//     Record,
//     Sum,
//     Trait,
// }
//
// impl<'src> Item<'src> {}
