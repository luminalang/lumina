pub mod config;
mod filesystem;
mod header;
pub mod traverse;

use crate::key;
use std::path::PathBuf;
pub mod symbols;

pub use config::ProjectConfig;
pub use filesystem::{Collector, Files, ProjectArena, ProjectError, ProjectPath};
pub use header::{FuncDef, FuncSig, HeaderFile, ImplDef, TypeDef, TypeSig, ValDef};
use lumina_key::Map;
use std::sync::RwLock;

#[derive(Debug)]
pub struct ProjectNode {
    pub key: key::Project,
    pub config: ProjectConfig,
    pub src_dir: PathBuf,
    pub dependents: Vec<key::Project>,
    dependencies: RwLock<Map<key::External, ProjectDependency>>,
}

#[derive(Debug)]
pub struct ProjectDependency {
    pub unstable_key: key::Project,
    pub name: Option<String>,
}

impl ProjectNode {
    pub fn resolve_dependency(&self, name: &str) -> Option<key::External> {
        self.dependencies
            .read()
            .unwrap()
            .iter()
            .find_map(|(ext, dep)| (dep.name.as_deref() == Some(name)).then_some(ext))
    }

    pub fn add_dependency(&self, name: Option<String>, dst: key::Project) -> key::External {
        let mut deps = self.dependencies.write().unwrap();

        for (ext, pdep) in deps.iter_mut() {
            if dst == pdep.unstable_key {
                if let Some(name) = name {
                    match pdep.name.as_deref() {
                        None => pdep.name = Some(name),
                        // We might need to support this. If so; `name` can be changed into a `Vec<String>` where `None` being empty
                        Some(existing_name) if existing_name != name => panic!("Identical dependency assigned to two different names: {existing_name} /= {name} for {}:{dst}", self.key),
                        Some(_) => {},
                    }
                }

                return ext;
            }
        }

        deps.push(ProjectDependency { unstable_key: dst, name })
    }

    pub fn ext_as_unstable(&self, ext: key::External) -> key::Project {
        self.dependencies.read().unwrap()[ext].unstable_key
    }

    pub fn origin_as_unstable(&self, origin: symbols::Origin) -> key::Project {
        match origin {
            symbols::Origin::Intra => self.key,
            symbols::Origin::Inter(external) => self.ext_as_unstable(external),
        }
    }

    pub fn find_dependency_by_project(&self, project: key::Project) -> Option<key::External> {
        self.dependencies
            .read()
            .unwrap()
            .iter()
            .find_map(|(ext, dep)| (dep.unstable_key == project).then_some(ext))
    }

    pub fn ext_name(&self, ext: key::External) -> Option<String> {
        self.dependencies.read().unwrap()[ext].name.clone()
    }

    pub fn externals(&self) -> impl Iterator<Item = key::External> {
        self.dependencies.read().unwrap().keys()
    }
}
