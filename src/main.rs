use std::fs::File;
use std::io::Read;

pub fn entrypoint() -> FileSource {
    FileSource::Project(vec!["main".to_owned()])
}

mod parser;
use parser::Parser;
mod env;
use env::Environment;
use parser::FileSource;

pub mod datatypes;

fn main() {
    let environment = match Environment::discover() {
        Ok(env) => env,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };
    let mut parser = Parser::new(&environment);

    let mut source_code = Vec::with_capacity(20);
    File::open(&environment.entrypoint)
        .unwrap()
        .read_to_end(&mut source_code)
        .unwrap();
    let file_path = entrypoint();

    // Construct a raw token representation of the code
    let fid = match parser.tokenize(file_path.clone(), &source_code) {
        Ok(functions) => functions,
        Err(e) => panic!("{:?}", e),
    };
    println!("{:#?}\n", parser);

    // Verify syntax and infer types
    match parser.type_check(fid, "main") {
        Err(e) => panic!("{:?}", e),
        Ok(_main_return) => {}
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
