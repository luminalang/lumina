pub mod error;
pub mod function;
pub mod index;
pub mod list;
pub mod prelude;
pub mod tokenizer;
pub mod util;

use crate::env;
use crate::env::cli::debug;
use crate::error::{Error, Leaf};
use crate::evaler::module;
use crate::evaler::module::Module;
use error::ParseError;
use function::{Context, FunctionBuilder};
use index::Index;
use std::fs::File;
use std::io::ErrorKind;
use std::io::Read;
use std::path::{Path, PathBuf};
use tokenizer::{
    token::{Key, Token},
    Tokenizer,
};

#[derive(Default)]
pub struct Parser {
    index: Index,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            index: Index::new(),
        }
    }

    pub fn run<'a>(mut self, entrypoint: PathBuf) -> Result<(&'a Module, usize), Box<dyn Error>> {
        prelude::include_all(&mut self.index);
        if let Err(e) = self.parse_file(entrypoint.clone()) {
            return Err(Box::new(e));
        };

        // return the entry-point program
        // prelude will always be 0 so entrypoint becomes 1
        match module::get(1) {
            Some(m) => {
                let file = self.index.get_file(&entrypoint);
                let main = file
                    .borrow_mut()
                    .try_get_func("main")
                    .expect("Main not found");
                Ok((m, main))
            }
            None => panic!("No modules were created"),
        }
    }

    pub fn parse_file(&mut self, file: PathBuf) -> Result<(), ParseError> {
        //self.tagger.add_file(file.clone());
        let (fid, _tagger) = self.index.get_file_or_create(&file);

        // This is only the skeleton of the module we allocate.
        // We will now start appending functions and types to it as we go
        let mut module = Module::with_fileid(fid);
        let reserved_module_id = unsafe { module::reserve_module_space() };

        // Initialize the tokenizer
        let mut tokenizer = Tokenizer::new(read_to_end(&file)?);
        let mut functions = Vec::new();

        loop {
            let token = tokenizer.next_token(false);
            // Lets see which sub-parser to switch to
            match token {
                Token::Key(Key::Use) => {
                    if let Token::Import(src) = {
                        let t = tokenizer.next_token(false);
                        import_from(t).map_err(|e| {
                            e.with_linen(tokenizer.linecount)
                                .find_line(&tokenizer.source())
                        })?
                    } {
                        let mod_path = locate_module(src.clone());
                        self.index
                            .get_file(&file)
                            .borrow_mut()
                            .import(src.file_name().unwrap(), &mod_path);
                        self.parse_file(mod_path)?;
                    }
                }
                Token::Key(Key::HeaderFn) => {
                    let mut context = Context::new(&file, &mut tokenizer, &mut self.index);
                    let func = match FunctionBuilder::new(&file).with_header(&mut context) {
                        Err(e) => {
                            return Err(ParseError::new(file.clone(), e)
                                .with_linen(tokenizer.linecount)
                                .find_line(tokenizer.source()))
                        }
                        Ok(f) => f,
                    }
                    .build(&mut context);
                    let func = match func {
                        Err(e) => {
                            return Err(ParseError::new(file.clone(), e)
                                .with_linen(tokenizer.linecount)
                                .find_line(tokenizer.source()))
                        }
                        Ok(f) => f,
                    };

                    functions.push(func);
                }
                Token::Key(Key::HeaderType) => {}
                Token::EOF => break,
                _ => {
                    return Err(ParseError::new(file.clone(), Leaf::ExHeader(token))
                        .with_linen(tokenizer.linecount)
                        .find_line(tokenizer.source()));
                }
            };
        }
        let is_devmode = debug::is_dev();
        if is_devmode {
            eprintln!("INDEX \n{:?}", &self.index.get_file(&file).borrow());
        }

        // Time for pass-2. I want to Optimize away identifiers
        // Function and type names are already indexed.
        for func in functions.drain(0..) {
            let optimized_func = self
                .index
                .optimize_func(func)
                .map_err(|e| e.find_line(&tokenizer.source()))?;
            module.functions.push(optimized_func);
        }

        if is_devmode {
            eprintln!("MODULE {:?}\n{:?}", file, module);
        }

        unsafe {
            module::set(reserved_module_id, module);
        }
        Ok(())
    }
}

fn read_to_end(file: &Path) -> Result<Vec<u8>, ParseError> {
    let mut buf = Vec::with_capacity(30);
    match File::open(file) {
        Err(e) => Err(ParseError::new(
            file.to_owned(),
            match e.kind() {
                ErrorKind::NotFound => Leaf::FileNotFound(file.to_owned()),
                ErrorKind::PermissionDenied => Leaf::FilePermissionDenied(file.to_owned()),
                _ => Leaf::FileUnknownError(file.to_owned()),
            },
        )),
        Ok(mut f) => {
            f.read_to_end(&mut buf).unwrap();
            Ok(buf)
        }
    }
}

pub fn is_valid_identifier(w: &str) -> Result<(), Leaf> {
    let banned = "0123456789(){}[]\\':,.#\"/*-+";

    for c in w.chars() {
        if banned.contains(c) {
            return Err(Leaf::IllegalCharacter(w.to_owned()));
        }
    }
    Ok(())
}

fn import_from(token: Token) -> Result<Token, ParseError> {
    match token {
        Token::Word(w) => {
            let mut path = PathBuf::new();

            let mut spl = w.split(':');
            let first = spl.next().unwrap();
            match spl.next() {
                None => Ok(Token::Import(path.join(first))),
                Some(second) => {
                    path.push(first);
                    path.push(second);
                    for s in spl {
                        path.push(s);
                    }

                    Ok(Token::Import(path))
                }
            }
        }

        _ => panic!("ERROR_TODO: Import not compatible token {:?}", token),
    }
}

fn locate_module(mut generic_path: PathBuf) -> PathBuf {
    generic_path.set_extension("lf");
    if generic_path.exists() {
        generic_path
    } else {
        env::modpath().join(&generic_path)
    }
}
