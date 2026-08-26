use crate::cli::DocsFlags;
use lumina_compiler::ProjectPath;
use lumina_compiler::{env::Environment, Target};
use std::ffi::OsStr;
use std::process::ExitCode;

pub fn run(env: Environment, settings: DocsFlags) -> ExitCode {
    let mut project_path = env.current_directory.clone();

    if let Some(path) = settings.project {
        if path.is_absolute() {
            project_path = path;
        } else {
            project_path.push(path);
        }
    }

    let target = settings
        .target
        .map(|name| Target::try_from(name.as_str()).unwrap())
        .unwrap_or_else(Target::native);

    let projects = lumina_compiler::compile(false, target, ProjectPath(project_path), env);

    if settings.output.is_dir() {
        todo!("exporting to static web directory");
    }

    if settings.output.extension() != Some(&OsStr::new("json")) {
        eprintln!(
            "Unsupported export format: {:#?}",
            settings.output.extension()
        );
    }

    match std::fs::File::create(&settings.output) {
        Err(err) => {
            eprintln!(
                "Could not export to file {}: {err}",
                settings.output.display()
            );
        }
        Ok(file) => {
            if let Err(err) = lumina_compiler::export::json(projects, file) {
                eprintln!(
                    "Could not export to file {}: {err}",
                    settings.output.display()
                );
            }
        }
    }

    ExitCode::SUCCESS
}
