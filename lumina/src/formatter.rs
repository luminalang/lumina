use super::cli;
use lumina_tokentree;

use std::path::Path;
use std::process::ExitCode;

pub fn run(_: cli::Environment, settings: cli::FormatFlags) -> ExitCode {
    match settings.file {
        None => unimplemented!("formatting entire project tree"),
        Some(filepath) => {
            let formatted = run_file(&filepath);
            print!("{formatted}");
        }
    }

    ExitCode::SUCCESS
}

pub fn run_file(path: &Path) -> String {
    let src = std::fs::read_to_string(path).unwrap();
    // HACK: The auto-updating of outdated syntax can create syntax that's unformatted.
    // As a temporary workaround, we just format twice.
    run_src(&run_src(&src))
}

pub fn run_src(src: &str) -> String {
    let mut parser = lumina_tokentree::Parser::new(&src);

    let mut formatter = lumina_tokentree::Formatter::new(&src);

    let mut buf = String::with_capacity(src.len());

    for entity in parser.everything() {
        // println!("{}", &entity);
        let (spacing, out) = formatter.toplevel(entity.as_ref());
        for _ in 0..spacing {
            buf += "\n";
        }
        buf += &out;
    }

    if !buf.ends_with('\n') {
        buf += &"\n";
    }

    buf
}
