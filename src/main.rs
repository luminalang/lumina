#![feature(const_vec_new)]
#![feature(test)]
pub mod env;
pub mod error;
pub mod evaler;
pub mod identifier;
pub mod parser;

use evaler::module;
use evaler::runner::Runner;

use crate::env::cli::debug;
use std::path::PathBuf;

fn main() {
    //let t = std::time::SystemTime::now();
    unsafe {
        env::ENV = env::Environment::initialize();
    }

    let parser = parser::Parser::new();
    match parser.run(PathBuf::from(env::entrypoint())) {
        Err(e) => eprintln!("{}", e),
        Ok(main_id) => {
            let v = Runner::run(&mut module::get(1).unwrap().get_function(main_id), &[]);
            if debug::is_dev() {
                println!("\nmain returns {:?}", v);
            }
        }
    };

    //let took = t.elapsed();
    //println!("{:?}", took.unwrap());
}
