const TEST_SOURCECODE: &[u8] = b"
type Coordinate
    x int
    y int

fn main
   double << add 20 << 20 + 20

";

mod parser;
use parser::Parser;
use std::path::Path;

pub mod datatypes;

fn main() {

    let mut parser = Parser::new();

    read_prelude_source();

    println!("{}\n", String::from_utf8(TEST_SOURCECODE.to_vec()).unwrap());

    // Construct a raw token representation of the code
    let functions = match parser.tokenize("entrypoint", TEST_SOURCECODE) {
        Ok(functions) => functions,
        Err(e) => panic!("{:?}", e),
    };
    println!("{:#?}", parser);
    println!();
    for func in &functions {
        println!("{:?}", func);
    }

    // Verify syntax and infer types
    match parser.type_check(functions) {
        Ok(_) => {}
        Err(e) => panic!("{:?}", e),
    };

    /*
    // Perform optimizations, remove all metadata and generate raw unsafe IR
    let optimized_ir = match parser.optimize {
        Ok(optimized_ir) => optimized_ir,
        Err(e) => panic!("{:?}", e),
    };

    let func = parser.get_function(MAIN_MODULE_ID, "main");
    evaler.run(func);
    */
}

fn read_prelude_source() {
    let env_leafpath = std::env::var("LEAFPATH").expect("LEAFPATH variable not set.");
    // get path object from LEAFPATH environment variable string
    let leafpath : &Path = Path::new(&env_leafpath);

    // get current working directory from environment variable
    let env_current_dir = std::env::current_dir().expect("Current directory couldn't be read.");
    let current_dir : &Path = env_current_dir.as_path();

    tokenize_prelude(leafpath.join("prelude").as_path());
    tokenize_prelude(current_dir.join("prelude").as_path());
}

fn tokenize_prelude(path : &Path) {
    for entry in std::fs::read_dir(path).expect("Couldn't read directory") {
        let file = entry.expect("Couldn't read file.");
        println!("{:?}", file.path());
    }
}
