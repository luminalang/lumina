const TEST_SOURCECODE: &[u8] = b"

";

mod parser;
use parser::{Parser, MAIN_MODULE_ID};

fn main() {
    let parser = Parser::new();

    // Construct a raw token representation of the code
    let tokens = match parser.tokenize(TEST_SOURCECODE) {
        Ok(tokens) => tokens,
        Err(e) => panic!("{:?}", e),
    };

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
}
