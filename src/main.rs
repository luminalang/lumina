const TEST_SOURCECODE: &[u8] = b"
||fn main
fn add x y (int int -> int) 
    4+ << to_int{t|first=1, second=[1,add 2 2]}
";

mod parser;
use parser::Parser;

fn main() {
    let mut parser = Parser::new();

    println!("{}\n", String::from_utf8(TEST_SOURCECODE.to_vec()).unwrap());

    // Construct a raw token representation of the code
    let _tokens = parser.tokenize(TEST_SOURCECODE);

    /*
    // Verify syntax and typing of token representation and group values based on `<<` `()`
    let grouped = match parser.group_and_verify(tokens) {
        Ok(grouped) => grouped,
        Err(e) => panic!("{:?}", e),
    };

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
