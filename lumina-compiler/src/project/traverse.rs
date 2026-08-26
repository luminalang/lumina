use super::*;
use crate::prelude::*;
use crate::Context;
use crossbeam::queue::SegQueue;
use std::sync::{atomic::AtomicUsize, atomic::Ordering, Arc, RwLock};

/// See `ProjectArena::traverse_in_parallel`
pub trait PassOverProjectTree: Clone + Send + Sync + 'static {
    type TUnit: 'static + Send + Sync;

    fn for_project(self, ctx: Context<Self::TUnit>, node: &ProjectNode);
}

impl ProjectArena {
    /// Traverse Lumina projects in the filesystem by checking dependencies in config.lm
    /// and call `C::for_project` for each project in parralel.
    ///
    /// Projects will be compiled in the order of least dependencies. Once a project is being compiled
    /// it's garenteed that all of its dependencies have also already been compiled.
    pub fn traverse_in_parallel<C: PassOverProjectTree>(self, compiler: C) -> Context<C::TUnit> {
        let tree = Arc::new(self);

        let projects: Arc<Map<key::Project, _>> =
            Arc::new(tree.projects().map(|_| RwLock::new(Option::None)).collect());

        let work_queue = Arc::new(SegQueue::new());

        let dependency_counts = Arc::new(tree.count_dependencies());

        // Start with the projects that have 0 dependencies
        for (project, dep_count) in dependency_counts.iter() {
            if dep_count.load(Ordering::Relaxed) == 0 {
                info!(
                    "Adding {project} ({}) to queue as a starting module",
                    tree.name(project)
                );
                work_queue.push(project);
            }
        }

        #[cfg(not(debug_assertions))]
        let workers = num_cpus::get();

        #[cfg(debug_assertions)]
        let workers = 1;

        let handles = (0..workers)
            .map(|i| {
                let projects = projects.clone();
                let work_queue = work_queue.clone();
                let tree = tree.clone();
                let dependency_counts = dependency_counts.clone();
                let compiler = compiler.clone();

                std::thread::Builder::new()
                    .name(format!("worker {i}"))
                    .spawn(move || tree.worker(dependency_counts, projects, work_queue, compiler))
                    .unwrap()
            })
            .collect::<Vec<_>>();

        for handle in handles {
            // TODO: cleanly handle internal compiler panics
            handle.join().ok();
        }

        Context { project_units: projects, project_nodes: tree }
        // Arc::into_inner(projects)
        //     .expect("compilation still in progress even after joining worker handles")
        //     .into_iter()
        //     .map(|(key, project)| {
        //         project
        //             .into_inner()
        //             .inspect_err(|err| {
        //                 warn!("defaulting unit {key} due to poisoned mutex: {err:?}")
        //             })
        //             .unwrap_or_default()
        //     })
        //     .collect()
    }

    fn worker<C: PassOverProjectTree>(
        self: Arc<ProjectArena>,
        dependency_counts: Arc<Map<key::Project, AtomicUsize>>,
        projects: Arc<Map<key::Project, RwLock<Option<C::TUnit>>>>,
        work_queue: Arc<SegQueue<key::Project>>,
        compiler: C,
    ) {
        loop {
            let ctx = Context { project_units: projects.clone(), project_nodes: self.clone() };

            let Some(key) = work_queue.pop() else {
                if dependency_counts
                    .values()
                    .map(|deps| deps.load(std::sync::atomic::Ordering::SeqCst))
                    .filter(|count| *count > 0)
                    .count()
                    > 1
                {
                    // Wait for more work being made available in the future
                    std::thread::sleep(std::time::Duration::from_millis(100));
                    continue;
                }

                info!("no more pending work");

                break;
            };

            let Some(node) = ctx.project_nodes.get(key) else {
                info!("{key} is poisoned, skipping...");
                continue;
            };

            compiler.clone().for_project(ctx.clone(), node);

            assert!(
                projects[key].read().unwrap().is_some(),
                "compile function ran without adding unit to context"
            );

            for dependent in node.dependents.iter().copied() {
                // Subtract the pending dependencies count.
                //
                // If the pending dependencies count is now 0, add the project to the queue.
                if dependency_counts[dependent].fetch_sub(1, std::sync::atomic::Ordering::SeqCst)
                    == 1
                {
                    info!("{dependent} has no further dependencies, adding to queue...");
                    work_queue.push(dependent);
                }
            }
        }
    }
}
