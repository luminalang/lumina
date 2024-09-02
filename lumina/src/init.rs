use super::cli;

use itertools::Itertools;
use std::fs;
use std::io;
use std::io::Write;
use std::process::ExitCode;

pub fn create_new_lumina_project(settings: cli::InitFlags) -> ExitCode {
    fn err_and_failure(and: impl FnOnce(lumina_util::Error) -> lumina_util::Error) -> ExitCode {
        let err = lumina_util::Error::error("lumina project error");
        eprintln!("{}", and(err));
        ExitCode::FAILURE
    }

    let err_already_exists = |e: lumina_util::Error| {
        e.with_text("a lumina project already exists in the destination directory")
            .with_text("use `--force` to overwrite")
    };

    match fs::create_dir(&settings.path) {
        Ok(()) => {}
        Err(err) if err.kind() == io::ErrorKind::AlreadyExists => {}
        Err(err) => return err_and_failure(|e| e.with_text(err.to_string())),
    };

    let config_path = settings.path.join("config.lm");
    if config_path.exists() {
        if settings.force {
            let warning = lumina_util::Error::warning("overwriting existing `config.lm`");
            println!("{warning}");
        } else {
            return err_and_failure(err_already_exists);
        }
    }

    let src_path = settings.path.join("src");
    let main_path = src_path.join("main.lm");

    if src_path.exists() {
        if src_path.is_dir() {
            if main_path.exists() {
                if settings.force {
                    let warning = lumina_util::Error::warning("overwriting existing `main.lm`");
                    println!("{warning}");
                } else {
                    return err_and_failure(err_already_exists);
                }
            } else {
            }
        } else {
            return err_and_failure(|e| {
                e.with_text("`src` is not a directory. Refusing to overwrite")
            });
        }
    } else {
        if let Err(err) = fs::create_dir(src_path) {
            return err_and_failure(|e| e.with_text(err.to_string()));
        }
    };

    let mut config_file = match fs::File::create(&config_path) {
        Ok(file) => file,
        Err(err) => return err_and_failure(|e| e.with_text(err.to_string())),
    };

    let mut main_file = match fs::File::create(&main_path) {
        Ok(file) => file,
        Err(err) => return err_and_failure(|e| e.with_text(err.to_string())),
    };

    if let Err(err) = main_file.write(&DEFAULT_MAIN_SRC) {
        return err_and_failure(|e| e.with_text(err.to_string()));
    }

    if let Err(err) = config_file.write(&config_from_settings(&settings)) {
        return err_and_failure(|e| e.with_text(err.to_string()));
    }

    println!(
        " created\n  {}\n  {}",
        config_path.display(),
        main_path.display()
    );

    ExitCode::SUCCESS
}

fn config_from_settings(settings: &cli::InitFlags) -> Vec<u8> {
    let name = settings.name.clone().unwrap_or_else(|| {
        settings
            .path
            .canonicalize()
            .unwrap()
            .file_name()
            .unwrap()
            .to_string_lossy()
            .to_string()
    });

    let version = &settings.version;
    let authors = settings
        .authors
        .iter()
        .format_with(", ", |author, f| f(&format_args!("\"{author}\"")));

    format!(
        "val name = \"{name}\"
val version = \"{version}\"
val authors = [{authors}]

val dependencies = []
"
    )
    .into_bytes()
}

const DEFAULT_MAIN_SRC: &[u8] = b"use std:io

fn main =
  io:println \"Hello World!\"
";
