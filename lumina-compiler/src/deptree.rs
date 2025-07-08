use crate::config;
use crate::config::ProjectConfig;
use crate::prelude::*;
use std::fs::ReadDir;
use std::io::Read;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct ProjectNode {
    pub config: ProjectConfig,
    pub src_dir: PathBuf,
    pub dependencies: HashMap<config::Dependency, key::Project>,
    pub dependents: Vec<key::Project>,
}

#[derive(Debug)]
pub struct ProjectTree {
    pub root: key::Project,
    pub projects: PrimaryMap<key::Project, Poison<ProjectNode>>,
    pub lookup: HashMap<config::Dependency, key::Project>,
}

/// Traverses dependencies and constructs a dependency tree
///
/// Detects cyclic dependencies
///
/// Recovers upon errors.
pub struct Collector {
    tree: ProjectTree,
    // root: Option<key::Project>,
    // projects: PrimaryMap<key::Project, Option<ProjectNode>>,
    // lookup: HashMap<config::Dependency, key::Project>,
    ext_dir: PathBuf,
    project_stack: Vec<String>,
    errors: Vec<ProjectError>,
}

impl Collector {
    pub fn root(ext_dir: PathBuf, project_dir: &Path) -> (ProjectTree, Vec<ProjectError>) {
        let mut collector = Self {
            tree: ProjectTree {
                projects: PrimaryMap::new(),
                lookup: HashMap::new(),
                root: key::Project(0),
            },
            project_stack: Vec::with_capacity(5),
            errors: vec![],
            ext_dir,
        };
        let root = collector.collect(project_dir);
        assert_eq!(root, key::Project(0));

        collector.tree.root = root;

        (collector.tree, collector.errors)
    }

    pub fn collect(&mut self, project_dir: &Path) -> key::Project {
        info!("collecting {project_dir:?}");

        let key = self.tree.projects.push(None);

        let Some(mut project) = or_poison(&mut self.errors, ProjectNode::open(project_dir)) else {
            return key;
        };

        for dep in &project.config.dependencies {
            let name = dep.name.clone();

            if self.project_stack.contains(&dep.name) {
                self.errors
                    .push(ProjectError::Cycle(self.project_stack.clone(), name));

                continue;
            }

            let dependency_key = self.tree.lookup.get(dep).copied().unwrap_or_else(|| {
                let path = self.dep_path(&name);
                self.project_stack.push(name.clone());
                let dependency = self.collect(&path);
                self.tree.lookup.insert(dep.clone(), dependency);
                assert_eq!(name, self.project_stack.pop().unwrap());
                dependency
            });

            // TODO: is this okay to do?
            if let Some(dep_node) = &mut self.tree.projects[dependency_key] {
                info!(
                    "adding {} as dependent for {}",
                    project.config.name, dep_node.config.name
                );
                dep_node.dependents.push(key);
            }

            project.dependencies.insert(dep.clone(), dependency_key);
        }

        self.tree.projects[key] = Some(project);

        key
    }

    fn dep_path(&self, name: &str) -> PathBuf {
        self.ext_dir.join(name)
    }
}

fn or_poison<T, E>(errors: &mut Vec<E>, res: Result<T, E>) -> Poison<T> {
    match res {
        Ok(v) => Some(v),
        Err(err) => {
            errors.push(err);
            None
        }
    }
}

impl ProjectNode {
    pub fn open(project_dir: &Path) -> Result<ProjectNode, ProjectError> {
        let _dir = std::fs::read_dir(project_dir)
            .map_err(|ioerr| ProjectError::IO(ioerr, project_dir.to_path_buf()))?;

        let src_dir = {
            let path = project_dir.join("src");
            std::fs::read_dir(&path).map_err(|ioerr| ProjectError::IO(ioerr, path.clone()))?;
            path
        };

        let config = {
            let path = project_dir.join("config.lm");
            let mut f = std::fs::File::open(&path)
                .map_err(|ioerr| ProjectError::IO(ioerr, path.clone()))?;

            let mut buf = String::with_capacity(100);
            f.read_to_string(&mut buf)
                .map_err(|ioerr| ProjectError::IO(ioerr, path.clone()))?;

            ProjectConfig::parse(&buf).map_err(|err| ProjectError::Config(err, path))?
        };

        let dependents = vec![];

        Ok(ProjectNode { config, dependencies: HashMap::new(), src_dir, dependents })
    }
}

#[derive(Debug)]
pub enum ProjectError {
    IO(std::io::Error, PathBuf),
    Config(config::Error, PathBuf),
    Cycle(Vec<String>, String),
}
