const TEST_SOURCECODE: &[u8] = b"
type Coordinate
    x int
    y int 

fn main
   double << add 20 << 20 + 20

";

mod parser;
use parser::Parser;

pub mod datatypes;

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
