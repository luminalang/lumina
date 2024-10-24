//! Handles project structure, source file collection and parsing.

use crate::{debuginfo::BinDebugInfo, Target};
use derive_more::From;
use lumina_key as key;
use std::path::{Path, PathBuf};
use tracing::{info_span, warn};

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

    pub main_module: key::Module,

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
) -> Result<(AST<'s>, BinDebugInfo), Error> {
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
) -> Result<(AST<'s>, BinDebugInfo), Error> {
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

        collector
            .debug
            .add_dir(key::PRELUDE, "lib.lm", &PathBuf::from("std/prelude"), None);
        collector.include_dir("lib.lm", key::PRELUDE, prelude_path)?;

        // include the project source directory recursively
        let main_module = collector.lookups.new_root_module(None);
        collector.dir = PathBuf::from(&config.name);
        collector.dir.push("src");
        collector
            .debug
            .add_dir(main_module, "main.lm", &collector.dir, None);
        collector.entities.add_module(main_module);
        collector.include_dir("main.lm", main_module, project.join("src"))?;

        // include all external dependencies listed in config
        for dep in config.dependencies.iter() {
            include_ext_library(&mut collector, &lumina, dep)?;
        }

        collector.link_up_imports_and_exposed();

        Ok((
            AST {
                entities: collector.entities,
                lookups: collector.lookups,
                sources: collector.sources,

                main_module,

                config,
            },
            collector.debug,
        ))
    }
}

fn include_ext_library<'s>(
    collector: &mut Collector<'s>,
    lumina: &Path,
    dep: &config::Dependency,
) -> Result<key::Module, Error> {
    if let Some(module) = collector.lookups.find_lib("ext", &dep.name) {
        warn!("ignoring versions for whether ext library should be included");
        return Ok(module);
    }

    let module = collector.lookups.new_lib("ext", dep.name.clone());

    let mut path = lumina.to_path_buf();
    path.push("ext");
    path.push(&dep.name);

    let config = {
        info_span!("ext:{} config", dep.name);
        let configpath = path.join("config.lm");
        let str = std::fs::read_to_string(&configpath).map_err(Error::Config)?;
        ProjectConfig::parse(&str).map_err(|cerr| Error::ConfigError(str, configpath, cerr))?
    };

    if config.prelude != "" && config.prelude != "std:prelude" {
        panic!("specifying a prelude in an `ext` library is not supported");
    }
    collector.dir = PathBuf::from(config.name.as_str());
    collector.dir.push("src");
    collector
        .debug
        .add_dir(module, "lib.lm", &collector.dir, None);

    path.push("src");
    collector.entities.add_module(module);
    collector.include_dir("lib.lm", module, path)?;

    // include all external dependencies listed in config
    for dep in config.dependencies.iter() {
        include_ext_library(collector, &lumina, dep)?;
    }

    Ok(module)
}
