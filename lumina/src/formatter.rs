use super::cli;
use lumina_tokentree;

use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::ExitCode;

pub fn run(_: cli::Environment, settings: cli::FormatFlags) -> ExitCode {
    match settings.file {
        None => unimplemented!("formatting entire project tree"),
        Some(filepath) => {
            let formatted = run_file(&filepath);
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
        match entity.value {
            lumina_tokentree::Entity::EOF => {
                if !buf.ends_with('\n') {
                    buf.push('\n');
                }
                break buf;
            }
            _ if buf.is_empty() => formatter.toplevel(&mut buf, entity.as_ref()),
            _ => formatter.toplevel(&mut buf, entity.as_ref()),
        }
    }
}
