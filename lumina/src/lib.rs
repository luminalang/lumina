// Exports for integration tests

mod build;
pub use build::{build_project, run_built_binary};
pub mod cli;
pub mod formatter;
use lumina_util::test_logger;
use std::path::PathBuf;

pub fn run(path: &str) -> std::process::Output {
    test_logger();

    let manifest = env!("CARGO_MANIFEST_DIR");

    let environment = crate::cli::Environment {
        current_directory: PathBuf::from(format!("{manifest}/../{path}")),
        lumina_directory: PathBuf::from(format!("{manifest}/../luminapath")),
    };

    let buildflags = crate::cli::BuildFlags {
        target: None,
        epanic: true,
        output: None,
        super_debug: false,
        project: Some(environment.current_directory.clone()),
    };

    match build_project(environment, true, buildflags) {
        Ok(binary) => {
            let output = std::process::Command::new(binary)
                .output()
                .expect("failed to run produced binary");

            if !output.stderr.is_empty() {
                println!("{}", String::from_utf8_lossy(&output.stderr));
            }

            output
        }
        Err(code) => panic!("running project {path} failed with status code {code:#?}"),
    }
}
