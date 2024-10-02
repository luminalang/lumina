use lumina::build_project;
use lumina_util::test_logger;
use std::path::PathBuf;

fn run(path: &str, expected: &str) {
    test_logger();

    let manifest = env!("CARGO_MANIFEST_DIR");

    let environment = lumina::cli::Environment {
        current_directory: PathBuf::from(format!("{manifest}/../{path}")),
        lumina_directory: PathBuf::from(format!("{manifest}/../luminapath")),
    };

    let buildflags = lumina::cli::BuildFlags {
        target: None,
        epanic: true,
        output: None,
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

            if output.stdout != expected.as_bytes() {
                panic!(
                    "unexpected output from example:\n  expected:\n{expected}\n  got:\n{}",
                    String::from_utf8_lossy(&output.stdout)
                );
            }

            if !output.status.success() {
                panic!("non-success error code: {}", output.status);
            }
        }
        Err(code) => panic!("running project {path} failed with status code {code:#?}"),
    }
}

#[test]
fn example_hello_world() {
    run("examples/hello-world", "Hello World!\n");
}

#[test]
fn example_lists() {
    run("examples/lists", "90\n");
}

#[test]
fn example_maybe_implementation() {
    run("examples/maybe-implementation", "Just 7\n");
}

#[test]
fn example_operators() {
    run("examples/operators", "18\n");
}

#[test]
fn example_records() {
    run("examples/records", "(false, true)\n");
}

#[test]
fn example_tuples() {
    run("examples/tuples", "Jonas 25\n");
}

#[test]
fn example_using_ext_library() {
    run(
        "examples/using-ext-library",
        "Hello from $LUMINAPATH/ext/example_lib/src/main.lm\n",
    );
}

#[test]
fn example_modules() {
    run("examples/modules", "HelloWorld\nHelloWorld\n");
}

#[cfg(unix)]
#[test]
fn example_ffi() {
    run("examples/ffi", "");
}

#[test]
fn example_raw_fn_pointers() {
    run("examples/raw-function-pointers", "5\n");
}
