use std::path::PathBuf;

fn run(path: &str) {
    let output = lumina::run(path);

    let manifest = env!("CARGO_MANIFEST_DIR");
    let expected_exit_code_path = PathBuf::from(format!("{manifest}/../{path}/expected"));

    let expected_exit_code_file = std::fs::read_to_string(expected_exit_code_path).unwrap();

    let expected = expected_exit_code_file
        .strip_suffix("\n")
        .unwrap()
        .parse::<i32>()
        .unwrap();

    assert_eq!(output.status.code(), Some(expected));
}

#[test]
fn tests_mem_autoboxed_struct() {
    run("tests/mem-autoboxed-struct");
}

#[test]
fn tests_mem_recursive_sum() {
    run("tests/mem-recursive-sum");
}

#[test]
fn tests_mem_large_struct() {
    run("tests/mem-large-struct");
}

#[test]
fn tests_mem_small_struct() {
    run("tests/mem-small-struct");
}

#[test]
fn tests_mem_large_sum() {
    run("tests/mem-large-sum");
}

#[test]
fn tests_mem_small_sum() {
    run("tests/mem-small-sum");
}

#[test]
fn tests_mem_nested_combination() {
    run("tests/mem-nested-combination");
}

#[test]
fn tests_mem_sum_in_struct() {
    run("tests/mem-sum-in-struct");
}
