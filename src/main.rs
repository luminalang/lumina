const TEST_SOURCECODE: &[u8] = b"
type Coordinate
    x int
    y int 

fn main
    add 5 (2 + << 1 + 2)

fn add x y (int int -> int)
    x + y
";

mod parser;
use parser::Parser;

fn main() {
    let mut parser = Parser::new();

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
    let grouped = match parser.type_check(functions) {
        Ok(grouped) => grouped,
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

#[macro_export]
macro_rules! debug {
    ($x:expr) => {
        use colored::*;
        println!("{}: {:?}", " ! ".red(), $x);
    };
}
