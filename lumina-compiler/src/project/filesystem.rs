use super::config;
use super::config::ProjectConfig;
use super::ProjectNode;
use crate::prelude::*;
use derive_new::new;
use lumina_key::{PrimaryMap, SecondaryMap};
use std::ffi::OsStr;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::atomic::AtomicUsize;

/// The path to a project is hashed for lookups.
///
/// We do this instead of name+version in case a project specifies a `path` manually in the config
/// which may differ from the defaulted path from luminapath.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProjectPath(pub PathBuf);

impl ProjectPath {
    pub fn empty() -> Self {
        Self(PathBuf::new())
    }

    pub fn from_lumina_path(lumina_path: &Path, name: &str) -> Self {
        if name == "std" {
            ProjectPath(lumina_path.join("std"))
        } else {
            ProjectPath(lumina_path.join("ext").join(name))
        }
    }
}

#[derive(Debug)]
pub struct ProjectArena {
    pub root: key::Project,
    projects: PrimaryMap<key::Project, Poison<ProjectNode>>,
    lookup: HashMap<ProjectPath, key::Project>,
}

impl ProjectArena {
    pub fn projects(&self) -> impl Iterator<Item = key::Project> + 'static {
        self.projects.keys()
    }

    pub fn count_dependencies(&self) -> Map<key::Project, AtomicUsize> {
        self.projects
            .values()
            .map(|project| {
                project
                    .as_ref()
                    .map(|dep| dep.dependencies.read().unwrap().values().count())
                    .unwrap_or(0)
            })
            .map(AtomicUsize::new)
            .collect()
    }

    pub fn name(&self, project: key::Project) -> &str {
        self.projects[project]
            .as_ref()
            .map(|p| p.config.name.as_str())
            .unwrap_or("<poison>")
    }

    pub fn get(&self, project: key::Project) -> Poison<&ProjectNode> {
        self.projects[project].as_ref()
    }
}

/// Traverses dependencies and constructs a project dependency graph
///
/// Detects cyclic dependencies
///
/// Recovers upon errors.
pub struct Collector {
    projects: ProjectArena,
    lumina_path: PathBuf,
    project_stack: Vec<ProjectPath>,
    errors: Vec<ProjectError>,
}

impl Collector {
    pub fn root(project: ProjectPath, lumina_path: &Path) -> (ProjectArena, Vec<ProjectError>) {
        let mut collector = Self {
            projects: ProjectArena {
                projects: PrimaryMap::new(),
                lookup: HashMap::new(),
                root: key::Project(0),
            },
            project_stack: Vec::with_capacity(5),
            errors: vec![],
            lumina_path: lumina_path.to_path_buf(),
        };
        let root = collector.collect(false, project);
        assert_eq!(root, key::Project(0));

        collector.projects.root = root;

        (collector.projects, collector.errors)
    }

    pub fn collect(&mut self, is_std: bool, project_path: ProjectPath) -> key::Project {
        info!("collecting {project_path:?}");

        let key = self.projects.projects.push(None);

        let Some(mut pnode) = or_poison(
            &mut self.errors,
            ProjectNode::init_from_directory(key, &project_path.0, &self.lumina_path),
        ) else {
            return key;
        };

        // self.collect_or_default_special_lib(is_std, &mut pnode);
        let mut has_explicit_std = false;

        for dep in &pnode.config.dependencies {
            if dep.name == "std" {
                has_explicit_std = true;
            }

            let name = dep.name.clone();

            if self.project_stack.contains(&dep.path) {
                self.errors
                    .push(ProjectError::Cycle(self.project_stack.clone(), name));

                todo!("we still want to push a poison dependency");
                // pnode.dependencies.push();

                continue;
            }

            assert_ne!(
                dep.path,
                ProjectPath::empty(),
                "path of dependency {} is not set",
                &dep.name
            );

            let dependency_key = self.get_or_collect(dep.name == "std", &dep.path);

            self.add_dependent(dependency_key, &pnode);

            pnode.add_dependency(Some(dep.name.clone()), dependency_key);
        }

        // Unless a different standard library is specified, implicitly add the standard library as
        // a dependency.
        if !has_explicit_std && !is_std {
            let path = ProjectPath::from_lumina_path(&self.lumina_path, "std");
            let project = self.get_or_collect(true, &path);
            self.add_dependent(project, &pnode);
            pnode.add_dependency(Some("std".to_string()), project);
        }

        self.projects.projects[key] = Some(pnode);

        key
    }

    fn add_dependent(&mut self, dep: key::Project, pnode: &ProjectNode) {
        if let Some(dep_node) = &mut self.projects.projects[dep] {
            info!(
                "adding {} as dependent for {}",
                pnode.config.name, dep_node.config.name
            );
            dep_node.dependents.push(pnode.key);
        }
    }

    // fn collect_or_default_special_lib(&mut self, is_std: bool, pnode: &mut ProjectNode) {
    //     if is_std {
    //         return;
    //     }

    //     let std = pnode
    //         .config
    //         .dependencies
    //         .iter()
    //         .find_map(|dep| (dep.name == "std").then(|| dep.path.clone()))
    //         .unwrap_or_else(|| ProjectPath::from_lumina_path(&self.lumina_path, "std"));

    //     let std_project_key = self.get_or_collect(true, &std);
    //     self.add_dependent(std_project_key, pnode);
    // }

    // Get the dependency key if the dependency has been collected, generate the dependency
    // key by collecting the dependency if it hasn't.
    fn get_or_collect(&mut self, is_std: bool, path: &ProjectPath) -> key::Project {
        self.projects.lookup.get(path).copied().unwrap_or_else(|| {
            // let path = self.dep_path(&name);

            self.project_stack.push(path.clone());
            let project_key = self.collect(is_std, path.clone());

            self.projects.lookup.insert(path.clone(), project_key);
            assert_eq!(*path, self.project_stack.pop().unwrap());

            project_key
        })
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
    pub fn init_from_directory(
        key: key::Project,
        project_dir: &Path,
        // TODO: We want to split out lumina_path into multiple dirs instead and have them default
        // from lumina_path.
        lumina_path: &Path,
    ) -> Result<ProjectNode, ProjectError> {
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

            let mut config = ProjectConfig::parse(&buf, lumina_path, project_dir)
                .map_err(|err| ProjectError::Config(err, path))?;

            // For the dependencies where the path is not explicitly set, default the path to be
            // relative from luminapath.
            for dep in config.dependencies.iter_mut() {
                if dep.path == ProjectPath::empty() {
                    dep.path = ProjectPath::from_lumina_path(lumina_path, &dep.name);
                }
            }

            config
        };

        let dependents = vec![];
        let dependencies = std::sync::RwLock::default();

        Ok(ProjectNode { config, dependencies, src_dir, dependents, key })
    }
}

#[derive(Debug)]
pub enum ProjectError {
    IO(std::io::Error, PathBuf),
    Config(config::Error, PathBuf),
    Cycle(Vec<ProjectPath>, String),
}

/// Arena for open source code
#[derive(Default, Debug, new)]
pub struct Files {
    files: PrimaryMap<key::File, Box<str>>,
    paths: PrimaryMap<key::File, PathBuf>,

    children: SecondaryMap<key::File, Vec<key::File>>,
}

impl Files {
    pub fn count(&self) -> usize {
        self.files.len()
    }

    pub fn open(
        &mut self,
        in_: Option<key::File>,
        path: impl Into<PathBuf>,
    ) -> Result<key::File, FileError> {
        let path = path.into();
        let mut file = std::fs::File::open(&path)?;
        let mut buf = String::with_capacity(40);
        file.read_to_string(&mut buf)?;

        let str = buf.into_boxed_str();
        let key = self.files.push(str);

        assert_eq!(key, self.push_path(path));

        if let Some(parent) = in_ {
            self.children[parent].push(key);
        }

        Ok(key)
    }

    pub fn switch_errors_file(&self, file: key::File) -> Option<key::File> {
        unsafe {
            let src = self.source(file);
            let path = self.path(file);
            crate::errors::switch_file(file, src, path)
        }
    }

    fn push_path(&mut self, mut path: PathBuf) -> key::File {
        if ["lib", "main"]
            .iter()
            .any(|n| path.file_stem() == Some(&OsStr::new(n)))
        {
            path.pop();
        }

        self.paths.push(path)
    }

    pub fn add_empty(&mut self, in_: Option<key::File>, path: impl Into<PathBuf>) -> key::File {
        let path = path.into();
        let src = String::new().into_boxed_str();

        let key = self.files.push(src);
        assert_eq!(key, self.push_path(path));

        if let Some(parent) = in_ {
            self.children[parent].push(key);
        }

        key
    }

    pub fn for_children(&self, mut f: impl FnMut(key::File, &[key::File])) {
        for file in self.files.keys() {
            if let Some(children) = self.children.get(file) {
                f(file, children);
            }
        }
    }

    pub fn source(&self, file: key::File) -> &str {
        &self.files[file]
    }

    pub fn path(&self, file: key::File) -> &Path {
        &self.paths[file]
    }

    pub fn name(&self, file: key::File) -> impl std::fmt::Display + '_ {
        self.path(file).file_stem().unwrap().display()
    }

    pub unsafe fn get_unsafe<'s>(&self, file: key::File) -> &'s str {
        unsafe { &(*(self.files[file].as_ref() as *const str)) }
    }
}

pub type FileError = std::io::Error;
