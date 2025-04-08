use super::cli;
use lumina_tokentree;

use lumina_util::Highlighting;
use std::ffi::OsStr;
use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

pub fn run(_: cli::Environment, settings: cli::FormatFlags) -> ExitCode {
    match settings.file.as_deref() {
        None => {
            let project = settings.project.as_deref().unwrap();
            if let Err(err) = run_project(project, settings.overwrite) {
                file_io_error(err);
                return ExitCode::FAILURE;
            }
        }
        Some(filepath) => {
            let formatted = run_file(filepath);
            if settings.overwrite {
                let mut f = File::create(&filepath).unwrap();
                f.write_all(formatted.as_bytes()).unwrap();
            } else {
                print!("{formatted}");
            }
        }
    }

    ExitCode::SUCCESS
}

pub fn run_file(path: &Path) -> String {
    let src = std::fs::read_to_string(path).unwrap();

    run_src(&src)
}

pub fn run_src(src: &str) -> String {
    let mut parser = lumina_tokentree::Parser::new(&src);

    let mut formatter = lumina_tokentree::Formatter::new(&src);
    let mut buf = String::new();

    loop {
        let entity = parser.next(false);
        match entity.kind {
            lumina_tokentree::Entity::EOF => {
                formatter.toplevel(&mut buf, entity.as_ref());
                if !buf.ends_with('\n') {
                    buf.push('\n');
                }
                break buf;
            }
            _ => formatter.toplevel(&mut buf, entity.as_ref()),
        }
    }
}

pub fn run_project(path: &Path, overwrite: bool) -> Result<(), std::io::Error> {
    for_each_file_with_ext(path, OsStr::new("lm"), &mut |path, mut file| {
        let mut src = String::new();
        if let Err(error) = file.read_to_string(&mut src) {
            file_io_error(error);
        }
        let formatted = run_src(&src);

        if overwrite {
            file.write_all(formatted.as_bytes()).unwrap();
        } else {
            print!(" {}\n{formatted}", path.display().keyword());
        }
    })
}

fn for_each_file_with_ext<F>(path: &Path, ext: &OsStr, f: &mut F) -> std::io::Result<()>
where
    F: FnMut(PathBuf, File),
{
    let dir = std::fs::read_dir(path)?;

    for file in dir {
        match file {
            Err(error) => file_io_error(error),
            Ok(entry) => match entry.file_type() {
                Ok(kind) if kind.is_file() => {
                    let path = entry.path();
                    if path.extension() == Some(ext) {
                        match File::open(&path) {
                            Err(error) => file_io_error(error),
                            Ok(file) => f(path, file),
                        }
                    }
                }
                Ok(_) => {
                    let path = entry.path();
                    if let Err(error) = for_each_file_with_ext(&path, ext, f) {
                        file_io_error(error);
                    }
                }
                Err(error) => file_io_error(error),
            },
        }
    }
    Ok(())
}

fn file_io_error(err: std::io::Error) {
    let error = lumina_util::Error::error("could not format file").with_text(err.to_string());
    eprintln!("{error}");
}
