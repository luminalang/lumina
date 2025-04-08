//! Snapshot testing to test if formatter changes still look good

use insta;
use lumina_util::test_logger;
use std::path::PathBuf;

fn run(path: &str, file: &str, checked: bool) {
    test_logger();

    if checked {
        let output = lumina::run(path);

        if !output.status.success() {
            panic!("non-success error code: {}", output.status);
        }
    }

    let name = path;

    let manifest = env!("CARGO_MANIFEST_DIR");
    let path = PathBuf::from(format!("{manifest}/../{path}/{file}"));

    let formatted = lumina::formatter::run_file(&path);

    insta::assert_snapshot!(format!("format_{name}"), formatted);
}

#[test]
fn format_hello_world() {
    run("examples/hello-world", "src/main.lm", false);
}

#[test]
fn format_lists() {
    run("examples/lists", "src/main.lm", false);
}

#[test]
fn format_maybe_implementation() {
    run("examples/maybe-implementation", "src/main.lm", false);
}

#[test]
fn format_operators() {
    run("examples/operators", "src/main.lm", false);
}

#[test]
fn format_records() {
    run("examples/records", "src/main.lm", false);
}

#[test]
fn format_tuples() {
    run("examples/tuples", "src/main.lm", false);
}

#[test]
fn format_using_ext_library() {
    run("examples/using-ext-library", "src/main.lm", false);
}

#[test]
fn format_modules() {
    run("examples/modules", "src/main.lm", false);
}

#[cfg(unix)]
#[test]
fn format_ffi() {
    run("examples/ffi", "src/main.lm", false);
}

#[test]
fn format_raw_fn_pointers() {
    run("examples/raw-function-pointers", "src/main.lm", false);
}

#[test]
fn format_fizz_buzz() {
    run("examples/fizz-buzz", "src/main.lm", false);
}
