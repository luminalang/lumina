pub mod error;
pub mod function;
pub mod index;
pub mod list;
pub mod module;
pub mod prelude;
pub mod tokenizer;
pub mod util;

use crate::env;
use crate::env::cli::debug;
use crate::error::{Error, Leaf};
use crate::evaler::module::RUNTIME;
use error::ParseError;
use function::{checker::Checker, Context, FunctionBuilder};
use index::Index;
pub use module::ParseModule;
use std::fs::File;
use std::io::ErrorKind;
use std::io::Read;
use std::path::{Path, PathBuf};
use termion::color::*;
use tokenizer::{
    token::{Key, Token},
    Tokenizer,
};

#[derive(Default)]
pub struct Parser {
    index: Index,
    pub modules: Vec<Option<ParseModule>>, // Option<> because we reserve module ID's for recursive parsing
}

impl Parser {
    pub fn new() -> Self {
        Self {
            index: Index::new(),
            modules: Vec::new(),
        }
    }

    fn reserve_module_space(&mut self) -> usize {
        let id = self.modules.len();
        self.modules.push(None);
        id
    }

    pub fn run(mut self, entrypoint: PathBuf) -> Result<usize, Box<dyn Error>> {
        let prelude_mod = prelude::include_all(&mut self.index);
        if env::cli::debug::is_dev() {
            eprintln!(
                "\n{}PRELUDE MODULE{} \n{:?}",
                Fg(Magenta),
                Fg(Reset),
                prelude_mod
            );
        }
        self.modules.push(Some(prelude_mod));

        if let Err(e) = self.parse_file(entrypoint.clone()) {
            return Err(Box::new(e));
        };
        let (fid, m) = self.index.try_get_file(&entrypoint).unwrap();
        let main_id = m.borrow().get_func("main");
        let modules = self
            .modules
            .drain(0..)
            .map(|a| a.unwrap())
            .collect::<Vec<ParseModule>>();

        if let Err(e) = Checker::new(&self.index, &modules)
            .dive_func(fid, main_id)
            .validate_func(&[])
            .map_err(|e| e.with_linen(modules[fid].functions[main_id].tokens[0].position))
        {
            let (error_source_fid, _) = self.index.try_get_file(&e.file).unwrap();
            return Err(Box::new(
                e.find_line(&modules[error_source_fid].tokenizer.source()),
            ));
        }

        // GLOBAL MUTATION
        unsafe { RUNTIME = modules.into() }

        Ok(main_id)
    }

    pub fn parse_file(&mut self, file: PathBuf) -> Result<(), ParseError> {
        let (fid, _tagger) = self.index.get_file_or_create(&file);

        // Initialize the tokenizer
        let tokenizer = Tokenizer::new(read_to_end(&file)?);
        let mut functions = Vec::new();

        // This is only the skeleton of the module we allocate.
        // We will now start appending functions and types to it as we go
        let mut module = ParseModule::with_fid(fid, tokenizer);
        let reserved_index = self.reserve_module_space();
        assert_eq!(fid, reserved_index);

        loop {
            let token = module.tokenizer.next_token(false);
            // Lets see which sub-parser to switch to
            match token {
                Token::K(Key::Use) => {
                    if let Token::Import(src) = {
                        let t = module.tokenizer.next_token(false);
                        import_from(t).map_err(|e| {
                            e.with_linen(module.tokenizer.linecount)
                                .find_line(&module.tokenizer.source())
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
                Token::K(Key::HeaderFn) => {
                    let mut context = Context::new(&file, &mut module.tokenizer, &mut self.index);
                    let func = match FunctionBuilder::new(file.clone()).with_header(&mut context) {
                        Err(e) => {
                            return Err(ParseError::new(file.clone(), e)
                                .with_linen(module.tokenizer.linecount)
                                .find_line(module.tokenizer.source()))
                        }
                        Ok(f) => f,
                    }
                    .build(&mut context);
                    let func = match func {
                        Err(e) => {
                            return Err(ParseError::new(file.clone(), e)
                                .with_linen(module.tokenizer.linecount)
                                .find_line(module.tokenizer.source()))
                        }
                        Ok(f) => f,
                    };

                    functions.push(func);
                }
                Token::K(Key::HeaderType) => {}
                Token::EOF => break,
                _ => {
                    return Err(ParseError::new(file.clone(), Leaf::ExHeader(token))
                        .with_linen(module.tokenizer.linecount)
                        .find_line(module.tokenizer.source()));
                }
            };
        }
        let is_devmode = debug::is_dev();
        /*
        if is_devmode {
            eprintln!(
                "\n{}INDEX{} \n{:?}",
                Fg(Magenta),
                Fg(Reset),
                &self.index.get_file(&file).borrow()
            );
        }
        */

        // Time for pass-2. I want to Optimize away identifiers
        // Function and type names are already indexed.
        for func in functions.drain(0..) {
            let optimized_func = self
                .index
                .translate_func(func)
                .map_err(|e| e.find_line(&module.tokenizer.source()))?;
            module.functions.push(optimized_func);
        }

        if is_devmode {
            eprintln!(
                "{}MODULE{} {:?}\n{:?}",
                Fg(Magenta),
                Fg(Reset),
                file,
                module
            );
        }

        self.modules[reserved_index] = Some(module);
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
