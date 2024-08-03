//! Handles project structure, source file collection and parsing.

use crate::Target;
use derive_more::From;
use lumina_key as key;
use std::path::PathBuf;
use tracing::info_span;

mod sources;
pub use sources::{ErrorBuilder, Sources};

mod resolve;
pub use resolve::{Entity, ImportError, Lookups, Mod, NFunc, Visibility};

mod entities;
pub use entities::{Entities, FuncBody};

mod collect;
use collect::Collector;
pub use collect::Error as CollectError;

mod config;
pub use config::{Error as ConfigError, ProjectConfig};

pub mod attr;
pub use attr::{FuncAttr, ImplAttr, ModuleAttr, SharedAttr, TypeAttr};

pub struct AST<'s> {
    pub entities: Entities<'s>,
    pub lookups: Lookups<'s>,
    pub sources: Sources,

    pub config: ProjectConfig,
}

#[derive(From)]
pub enum Error {
    ProjectNotDir(PathBuf),
    LuminaNotDir(PathBuf),
    SrcDir(std::io::Error),
    Config(std::io::Error),
    ConfigError(String, PathBuf, config::Error),
    #[from]
    Collect(collect::Error),
}

pub fn parse<'s>(
    project: PathBuf,
    lumina: PathBuf,
    epanic: bool,
    target: Target,
) -> Result<AST<'s>, Error> {
    if !project.is_dir() {
        return Err(Error::ProjectNotDir(project));
    }

    if !lumina.is_dir() {
        return Err(Error::LuminaNotDir(lumina));
    }

    let mut config = {
        info_span!("project config");
        let configpath = project.join("config.lm");
        let str = std::fs::read_to_string(&configpath).map_err(Error::Config)?;
        ProjectConfig::parse(&str).map_err(|cerr| Error::ConfigError(str, configpath, cerr))?
    };

    config.epanic |= epanic;

    parse_with_config(project, lumina, config, target)
}

pub fn parse_with_config<'s>(
    project: PathBuf,
    lumina: PathBuf,
    config: ProjectConfig,
    target: Target,
) -> Result<AST<'s>, Error> {
    unsafe {
        let ispan = info_span!("collector");
        let _ispan = ispan.enter();

        let std_lib_directory = lumina.join("std");
        let mut collector = Collector::new(std_lib_directory.clone(), target);
        collector.sources.set_panicy(config.epanic);

        // include the prelude directory
        assert_eq!(
            key::PRELUDE,
            collector.lookups.new_lib("std", "prelude".into())
        );
        collector.entities.add_module(key::PRELUDE);

        let prelude_path = lumina_util::Identifier::parse(&config.prelude)
            .expect("invalid prelude path")
            .to_directory(&lumina, &project, &lumina);
        collector.include_dir(key::PRELUDE, prelude_path)?;

        // include the project source directory recursively
        collector.lookups.project = collector.lookups.new_root_module(None);
        collector.entities.add_module(collector.lookups.project);
        collector.include_dir(collector.lookups.project, project.join("src"))?;

        // include all external dependencies listed in config
        for dep in config.dependencies.iter() {
            let mut path = lumina.clone();
            path.push("ext");
            path.push(&dep.name);

            let module = collector.lookups.new_lib("ext", dep.name.clone());
            collector.entities.add_module(module);
            collector.include_dir(module, path)?;
        }

        collector.link_up_imports_and_exposed();

        Ok(AST {
            entities: collector.entities,
            lookups: collector.lookups,
            sources: collector.sources,
            config,
        })
    }
}
