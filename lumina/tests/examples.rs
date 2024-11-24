fn run(path: &str, expected: &str) {
    let output = lumina::run(path);

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

#[test]
fn example_fizz_buzz() {
    run(
        "examples/fizz-buzz",
        "1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\n",
    );
}
