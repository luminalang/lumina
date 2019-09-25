use std::collections::HashMap;

pub const MAIN_MODULE_ID: usize = 1;

mod tokenizer;
pub use tokenizer::{Token, Tokenizer};

pub struct Parser {
    module_ids: HashMap<String, usize>,
    modules: Vec<ParseModule>,
}

struct ParseModule {
    functions: HashMap<String, usize>,
    types: HashMap<String, usize>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            module_ids: HashMap::new(),
            modules: Vec::new(),
        }
    }

    pub fn get_function(&self, fid: usize, name: &str) -> Option<usize> {
        self.modules.get(fid)?.functions.get(name).copied()
    }

    pub fn tokenize(&mut self, source_code: &[u8]) -> Vec<Vec<Token>> {
        let mut tokenizer = Tokenizer::from(source_code);

        let mut body_buf = Vec::new();
        loop {
            let token = match tokenizer.next() {
                Some(t) => t,
                None => return body_buf,
            };
            println!("{:?}", token);
            /*
            match token.inner {
                Token::Header(h) => match h {
                    Header::Function => {
                        let func_buf = Vec::with_capacity(5);
                        loop {
                            let token = tokenizer.next();
                            match token.inner {
                                Token::EOF => break,
                                Token::Header(h) => {
                                    tokenizer.regress(h);
                                    break;
                                }
                                _ => {}
                            }
                            func_buf.push(token);
                        }
                        body_buf.push(func_buf);
                    }
                    Header::Type => {
                        unimplemented!("Custom Types");
                    }
                },
                Token::EOF => break,
                _ => panic!("ERROR_TODO: Unexpected {:?}, wanted header", token),
            }
            */
        }
    }
}
