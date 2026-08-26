use super::{HeaderFile, LangItems, TranslationUnit, config, errors, hir::TypeKey, key, symbols};
use crate::{
    prelude::*,
    project::{ProjectArena, ProjectNode, ProjectPath},
};
use lumina_key::Map;
use std::sync::{Arc, RwLock, RwLockReadGuard};

/// Context is the compilers global state shared across workers
pub struct Context<Unit> {
    pub project_units: Arc<Map<key::Project, RwLock<Option<Unit>>>>,
    pub project_nodes: Arc<ProjectArena>,
}

impl<Unit> Clone for Context<Unit> {
    fn clone(&self) -> Self {
        Self {
            project_units: self.project_units.clone(),
            project_nodes: self.project_nodes.clone(),
        }
    }
}

impl Context<TranslationUnit> {
    // TODO: move other `name_of_` methods to here
    pub fn name_of_origin(&self, from: key::Project, origin: symbols::Origin) -> String {
        self.in_origin(from, origin, |unit| unit.header.name.clone())
    }

    pub fn initialize_unit(&self, key: key::Project, files: usize) {
        let mut unit = self.project_units[key].write().unwrap();
        assert!(unit.is_none());

        let name = &self.get_node(key).config.name;

        *unit = Some(TranslationUnit {
            header: HeaderFile::new(name, files),
            langitems: LangItems::default(),
        });
    }

    // If we're already traversing through an external, then add
    // the externals external as an implicit indirect dependency to
    // retrieve an external that's valid for this origin instead of letting the
    // externals external cross the externals unit boundry.
    pub fn get_or_add_indirect_dependency(
        &self,
        origin: key::Project,
        in_: key::Project,
        [of, for_]: [key::External; 2],
    ) -> key::External {
        info!(
            "Retrieving indirect dependency for {in_}:{of}:{for_} with origin {}",
            origin
        );

        let of_unstable = self.get_node(in_).ext_as_unstable(of);
        let project = self.get_node(of_unstable).ext_as_unstable(for_);

        let ext = self.get_node(origin).add_dependency(None, project);

        self.in_project_mut(origin, |unit| {
            let config = &self.get_node(project).config;
            let externals = &mut unit.header.externals;

            // If the dependency didn't already exist on the project node, at it to the unit
            if externals.next_key() == ext {
                externals.push(config::Dependency {
                    name: config.name.clone(),
                    // We might need to clone the path in the config file of `in_` however that path
                    // can be relative.
                    path: ProjectPath::empty(),
                    version: config.version.clone(),
                    indirect: true,
                });
            }
        });

        ext
    }

    pub fn inst_type_key(
        &self,
        project: key::Project,
        in_: symbols::Origin,
        tkey: TypeKey,
    ) -> TypeKey {
        let origin = self.map_origin(project, in_, tkey.origin);

        TypeKey { origin, key: tkey.key }
    }

    pub fn map_origin(
        &self,
        // The project we're currently in
        project: key::Project,
        // Which module the types we're mapping exist in
        in_: symbols::Origin,
        // The origin we're mapping
        origin: symbols::Origin,
    ) -> symbols::Origin {
        match origin {
            symbols::Origin::Intra => in_,
            symbols::Origin::Inter(external) => match in_ {
                symbols::Origin::Intra => symbols::Origin::Inter(external),
                symbols::Origin::Inter(in_external) => {
                    let ext = self.get_or_add_indirect_dependency(
                        project,
                        project, // TODO: Is this right? I think so...
                        [external, in_external],
                    );

                    symbols::Origin::Inter(ext)
                }
            },
        }
    }

    pub fn default_list_type(&self, span: Span, project: key::Project) -> Option<TypeKey> {
        let Some(stdlib) = self.in_project(project, |unit| unit.header.stdlib()) else {
            errors::err("type not found")
                .line(span, "no standard library available to provide `Listable`")
                .emit();

            return None;
        };

        self.in_origin(project, stdlib, |unit| unit.langitems.default_listable)
            .map(|key| TypeKey { key, origin: stdlib })
    }
}

impl<Unit> Context<Unit> {
    pub fn get_node(&self, key: key::Project) -> &ProjectNode {
        self.project_nodes.get(key).unwrap()
    }

    pub fn get_unit(&self, key: key::Project) -> RwLockReadGuard<'_, Option<Unit>> {
        self.project_units[key].read().unwrap()
    }

    pub fn get_origin(
        &self,
        from: key::Project,
        m: symbols::Origin,
    ) -> RwLockReadGuard<'_, Option<Unit>> {
        match m {
            symbols::Origin::Intra { .. } => self.get_unit(from),
            symbols::Origin::Inter(external) => {
                let project = self.external(from, external);
                self.get_unit(project)
            }
        }
    }

    pub fn external(&self, from: key::Project, ext: key::External) -> key::Project {
        match self.project_nodes.get(from) {
            Some(node) => node.ext_as_unstable(ext),
            None => panic!("{from} has not been initialized"),
        }
    }

    pub fn in_project<T>(&self, key: key::Project, f: impl FnOnce(&Unit) -> T) -> T {
        let guard = self.project_units[key].read().unwrap();

        match guard.as_ref() {
            None => panic!("{key} has not been initialized"),
            Some(unit) => f(unit),
        }
    }

    pub fn in_external<T>(
        &self,
        from: key::Project,
        ext: key::External,
        f: impl FnOnce(&Unit) -> T,
    ) -> T {
        let project = self.external(from, ext);
        self.in_project(project, f)
    }

    pub fn in_module<T, F>(&self, from: key::Project, m: symbols::Module, f: F) -> T
    where
        F: FnOnce(&Unit) -> T,
    {
        let origin = m.origin();
        self.in_origin(from, origin, f)
    }

    pub fn in_origin<T, F>(&self, from: key::Project, origin: symbols::Origin, f: F) -> T
    where
        F: FnOnce(&Unit) -> T,
    {
        match origin {
            symbols::Origin::Intra => self.in_project(from, f),
            symbols::Origin::Inter(external) => self.in_external(from, external, f),
        }
    }

    pub fn in_project_mut<T>(&self, key: key::Project, f: impl FnOnce(&mut Unit) -> T) -> T {
        let mut guard = self.project_units[key].write().unwrap();

        match guard.as_mut() {
            None => panic!("{key} has not been initialized"),
            Some(unit) => f(unit),
        }
    }

    pub fn unstable_stdlib(&self, from: key::Project) -> Option<key::Project> {
        self.get_node(from)
            .resolve_dependency("std")
            .map(|ext| self.get_node(from).ext_as_unstable(ext))
    }
}
