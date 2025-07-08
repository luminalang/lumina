use crate::prelude::*;
use crate::{
    config,
    config::ProjectConfig,
    deptree::{ProjectNode, ProjectTree},
};
use crossbeam::queue::SegQueue;
use lumina_util::{Highlighting, Identifier};
use rand::Rng;
use std::sync::atomic::AtomicUsize;
use std::sync::RwLockReadGuard;
use std::sync::{Arc, RwLock};
use std::{collections::HashMap, fs::ReadDir};

mod func;
mod item;
mod nodes;

// NOTE: Since we know there's no cycles; we *can* cache even type checks.

// struct LoweredProjects {
//     projects: SecondaryMap<key::Project, Arc<RwLock<Option<LoweredProject>>>>,
// }

#[derive(Debug)]
struct LoweredProject {
    rvsdg: rvsdg::TranslationUnitContext,
}

// struct LoweredProjectNode {
//     rvsdg: rvsdg::TranslationUnitContext,
//     dependencies: HashMap<String, LoweredProjectNode>,
// }

type ProjectBuildMap = PrimaryMap<key::Project, Arc<RwLock<Option<LoweredProject>>>>;

pub fn project(tree: ProjectTree, _root: key::Project) {
    let tree = Arc::new(tree);

    let projects: Arc<ProjectBuildMap> = Arc::new(
        tree.projects
            .keys()
            .map(|_| Arc::new(RwLock::new(Option::None)))
            .collect(),
    );

    let work_queue = Arc::new(SegQueue::new());

    let dependency_counts = Arc::new(
        tree.projects
            .iter()
            .filter_map(|(key, project)| {
                let dep_count = project
                    .as_ref()
                    .map(|dep| dep.dependencies.values().count())
                    .unwrap_or(0);

                if dep_count == 0 {
                    work_queue.push(key);
                    info!(
                        "adding {key} ({}) to queue",
                        project
                            .as_ref()
                            .map(|n| n.config.name.as_str())
                            .unwrap_or("<poison>")
                    );
                    None
                } else {
                    Some(AtomicUsize::new(dep_count))
                }
            })
            .collect::<PrimaryMap<key::Project, AtomicUsize>>(),
    );

    // let workers = num_cpus::get();
    let workers = 1;

    let handles = (0..workers)
        .map(|_| {
            let projects = projects.clone();
            let work_queue = work_queue.clone();
            let tree = tree.clone();
            let dependency_counts = dependency_counts.clone();
            std::thread::spawn(|| worker(dependency_counts, tree, projects, work_queue))
        })
        .collect::<Vec<_>>();

    for handle in handles {
        handle.join().unwrap();
    }

    let mut xml = rvsdg::new_xml();
    for (project, unit) in projects.iter() {
        let name = tree.projects[project].as_ref().unwrap().config.name.clone();
        xml = unit
            .read()
            .unwrap()
            .as_ref()
            .unwrap()
            .rvsdg
            .add_to_xml(name, xml);
    }
    xml.end_element();
    let xml = xml.end_document();
    rvsdg::open_viewer(xml);
}

fn worker(
    dependency_counts: Arc<PrimaryMap<key::Project, AtomicUsize>>,
    tree: Arc<ProjectTree>,
    projects: Arc<ProjectBuildMap>,
    work_queue: Arc<SegQueue<key::Project>>,
) {
    loop {
        let Some(key) = work_queue.pop() else {
            info!("no more pending work");
            break;
        };

        let Some(node) = &tree.projects[key] else {
            info!("{key} is poisoned, skipping...");
            continue;
        };

        let deps = node
            .dependencies
            .iter()
            .map(|(dep, _)| {
                let depkey = tree.lookup.get(dep).expect("dependent not lowered");
                let lowered_dep = projects[*depkey].read().unwrap();
                (dep, lowered_dep)
            })
            .collect();

        let project = compile(node, deps);
        info!("compiled {key} ({})", &node.config.name);
        let mut project_guard = projects[key].write().unwrap();

        assert!(
            project_guard.is_none(),
            "same project was lowered twice {}",
            &node.config.name
        );
        *project_guard = Some(project);

        for dependent in node.dependents.iter().copied() {
            // Subtract the pending dependencies count.
            //
            // If the pending dependencies count is now 0, add the project to the queue.
            if dependency_counts[dependent].fetch_sub(1, std::sync::atomic::Ordering::SeqCst) == 1 {
                info!("{dependent} has no further dependencies, adding to queue...");
                work_queue.push(dependent);
            }
        }
    }
}

fn compile(
    node: &ProjectNode,
    deps: HashMap<&config::Dependency, RwLockReadGuard<'_, Option<LoweredProject>>>,
) -> LoweredProject {
    println!(" {} {}", "Building".keyword(), &node.config.name);

    // Re-exports can be RVSDG'able. You just map to the right input multiple times.
    //
    // I think we're gonna remove the module system in favor of just having an absolute to rvsdg
    // omega region result mapping.
    //
    // But wait; that's not how they work. you're not meant to connect to an omega region.
    //
    // Those are meant to become individual translation units...
    //
    // Wait no; we gotta keep monomorphization in mind...

    // I think we shouldn't take this shortcut and instead right as well do an Item = Rvsdg | Cranelift
    // incrementality thing. That way we're not setting ourselves up for failures in the future.

    let mut builder = item::ProjectBuilder::new(node, deps);
    builder.include_project(node);
    let rvsdg = builder.finish();

    println!(" {} {}", "Completed".keyword(), &node.config.name);

    LoweredProject { rvsdg }
}
