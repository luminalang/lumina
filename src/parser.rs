use crate::env::Environment;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::rc::Rc;
use termion::{color, color::Fg};

mod tokenizer;
pub use tokenizer::{is_valid_identifier, Header, Inlined, Key, RawToken, Token, Tokenizer};
mod function;
mod leafmod;
pub use function::FunctionBuilder;

pub use leafmod::FileSource;
mod r#type;
pub use r#type::Type;
mod checker;
pub mod flags;
use checker::TypeChecker;
pub mod body;
use body::BodySource;
mod operator;
pub use operator::OperatorBuilder;
mod error;
pub use error::*;

const PRELUDE_FID: usize = 0;

pub struct Parser {
    pub module_ids: HashMap<FileSource, usize>,
    pub modules: Vec<ParseModule>,
    environment: Rc<Environment>,
}

// #[derive(Default)]
pub struct ParseModule {
    //                     identifer       parameters
    pub functions: HashMap<String, HashMap<Vec<Type>, (FunctionBuilder, usize)>>,
    // With a nested hashmap here we can no longer rely on it's .len method to generate new
    // funcid's
    function_count: usize,
    operator_count: usize,
    pub operators: HashMap<String, HashMap<[Type; 2], (OperatorBuilder, usize)>>,

    pub types: HashMap<String, usize>,
    pub type_fields: Vec<Vec<(String, Type)>>,
    pub imports: HashMap<String, usize>,

    pub module_path: FileSource,
}

impl ParseModule {
    pub fn new(module_path: FileSource) -> Self {
        Self {
            functions: HashMap::new(),
            function_count: 0,
            operator_count: 0,
            operators: HashMap::new(),
            types: HashMap::new(),
            type_fields: Vec::new(),
            imports: HashMap::new(),
            module_path,
        }
    }

    fn next_funcid(&mut self) -> usize {
        let id = self.function_count;
        self.function_count += 1;
        id
    }
    fn next_operid(&mut self) -> usize {
        let id = self.operator_count;
        self.function_count += 1;
        id
    }
}

impl Parser {
    pub fn new(environment: Rc<Environment>) -> Self {
        Self {
            module_ids: HashMap::new(),
            modules: Vec::new(),
            environment,
        }
    }

    fn _get_type_id(&self, fid: usize, name: &str) -> Option<usize> {
        self.modules.get(fid)?.types.get(name).copied()
    }

    fn new_module(&mut self, source: FileSource) -> usize {
        match self.module_ids.get(&source) {
            Some(fid) => *fid,
            None => {
                let fid = self.module_ids.len();
                assert_eq!(fid, self.modules.len());
                self.module_ids.insert(source.clone(), fid);
                self.modules.push(ParseModule::new(source));
                fid
            }
        }
    }
    fn new_function(&mut self, fid: usize, funcb: FunctionBuilder) -> usize {
        let module = &mut self.modules[fid];
        let funcid = module.next_funcid();
        match module.functions.get_mut(&funcb.name) {
            Some(existing) => {
                existing.insert(funcb.parameter_types.clone(), (funcb, funcid));
                funcid
            }
            None => {
                let mut hashmap = HashMap::with_capacity(1);
                let name = funcb.name.clone();
                hashmap.insert(funcb.parameter_types.clone(), (funcb, funcid));
                module.functions.insert(name, hashmap);
                funcid
            }
        };
        funcid
    }
    fn new_operator(&mut self, fid: usize, opb: OperatorBuilder) -> usize {
        let module = &mut self.modules[fid];
        let opid = module.next_operid();
        match module.operators.get_mut(&opb.name.identifier) {
            Some(existing) => {
                existing.insert(opb.parameter_types.clone(), (opb, opid));
                opid
            }
            None => {
                let mut hashmap = HashMap::with_capacity(1);
                let name = opb.name.identifier.clone();
                hashmap.insert(opb.parameter_types.clone(), (opb, opid));
                module.operators.insert(name, hashmap);
                opid
            }
        }
    }
    fn new_type(&mut self, fid: usize, name: String, fields: Vec<(String, Type)>) -> usize {
        let module = &mut self.modules[fid];
        let typeid = module.types.len();
        module.types.insert(name.to_owned(), typeid);
        module.type_fields.push(fields);
        typeid
    }

    pub fn read_prelude_source(&mut self) {
        // get the leafpath root directory
        let leafpath = &self.environment.leafpath;

        // get the directory containing our entrypoint .lf file
        let current_dir = self.environment.entrypoint.parent().unwrap();

        let leafpath_prelude = leafpath.join("prelude");
        let current_dir_prelude = current_dir.join("prelude");

        if leafpath_prelude.exists() {
            self.tokenize_prelude(leafpath_prelude.as_path());
        }

        if current_dir_prelude.exists() {
            self.tokenize_prelude(current_dir_prelude.as_path());
        }
    }

    fn tokenize_prelude(&mut self, path: &Path) {
        for entry in path.read_dir().expect("Couldn't read prelude directory.") {
            let file_path = entry.expect("Prelude file path doesn't exist.").path();
            if file_path.extension() == Some(std::ffi::OsStr::new("lf")) {
                let mut source_code_buffer = Vec::new();
                std::fs::File::open(file_path)
                    .expect("Couldn't open file.")
                    .read_to_end(&mut source_code_buffer)
                    .expect("Couldn't read file.");

                self.tokenize(FileSource::Prelude, &source_code_buffer)
                    .expect("Tokenization failed.");
            }
        }
    }

    // We only have to return Functions because custom types only need to be indexed
    pub fn tokenize(
        &mut self,
        module_path: FileSource,
        source_code: &[u8],
    ) -> Result<usize, ParseError> {
        let fid = self.new_module(module_path.clone());

        let mut tokenizer = Tokenizer::from(source_code);
        loop {
            let (token_inner, source_index) = match tokenizer.next() {
                Some(t) => (t.inner, t.source_index),
                None => return Ok(fid),
            };
            match token_inner {
                RawToken::Header(h) => match h {
                    Header::Function => {
                        let mut funcb = FunctionBuilder::new().with_header(&mut tokenizer)?;
                        let body_entry = funcb
                            .parse_body(&mut tokenizer)
                            .map_err(|e| e.fallback(source_index))?;

                        funcb.body = Rc::new(body_entry);

                        self.new_function(fid, funcb);
                    }
                    Header::Operator => {
                        let mut opb = OperatorBuilder::new().with_header(&mut tokenizer)?;
                        let body_entry = opb
                            .parse_body(&mut tokenizer)
                            .map_err(|e| e.fallback(source_index))?;

                        opb.body = Rc::new(body_entry);

                        self.new_operator(fid, opb);
                    }
                    Header::Type => {
                        let (type_name, fields) = self.parse_type_decl(&mut tokenizer)?;

                        self.new_type(fid, type_name, fields);
                    }
                    Header::Use => {
                        let import = match tokenizer.next().map(|t| t.inner) {
                            Some(RawToken::Identifier(single)) => vec![single],
                            Some(RawToken::ExternalIdentifier(entries)) => entries,
                            None => {
                                return ParseFault::EndedWhileExpecting(vec![RawToken::Identifier(
                                    "identifier".into(),
                                )])
                                .to_err(tokenizer.index() - 1)
                                .into()
                            }
                            Some(other) => {
                                panic!("ET: Unexpected thing after `use` keyword: {:?}", other)
                            }
                        };

                        let file_path = {
                            if module_path == crate::entrypoint() {
                                leafmod::FileSource::try_from((
                                    import
                                        .iter()
                                        .map(|s| &**s)
                                        .collect::<Vec<&str>>()
                                        .as_slice(),
                                    &*self.environment,
                                ))
                                .unwrap()
                            } else {
                                let mut new_module_path = module_path.clone();
                                new_module_path.pop();
                                for level in import.iter() {
                                    new_module_path = new_module_path.join(level.clone());
                                }
                                new_module_path
                            }
                        };

                        let mut source_code = Vec::with_capacity(20);
                        let pathbuf = file_path.to_pathbuf(&self.environment);
                        File::open(pathbuf.clone())
                            .map_err(|e| {
                                ParseFault::ModuleLoadFailed(pathbuf.clone(), e.kind())
                                    .to_err(source_index)
                            })?
                            .read_to_end(&mut source_code)
                            .map_err(|e| {
                                ParseFault::ModuleLoadFailed(pathbuf, e.kind()).to_err(source_index)
                            })?;

                        let usefid = match self.tokenize(file_path.clone(), &source_code) {
                            Err(e) => return Err(e.with_source_code(source_code, file_path)),
                            Ok(fid) => fid,
                        };
                        self.modules[fid]
                            .imports
                            .insert(import.last().unwrap().clone(), usefid);
                    }
                },
                RawToken::NewLine => continue,
                _ => {
                    return ParseError::new(
                        source_index,
                        ParseFault::GotButExpected(
                            RawToken::Header(Header::Function),
                            vec![token_inner],
                        ),
                    )
                    .into()
                }
            }
        }
    }

    pub fn type_check(&mut self, fid: usize) -> Result<Type, ParseError> {
        TypeChecker::new(self, fid, "main", vec![])
            .map_err(|e| e.to_err(0))?
            .run()
    }

    fn parse_type_decl(
        &mut self,
        tokenizer: &mut Tokenizer,
    ) -> Result<(String, Vec<(String, Type)>), ParseError> {
        let first = match tokenizer.next() {
            None => {
                return ParseFault::EndedWhileExpecting(vec![RawToken::Identifier(
                    "custom type name".into(),
                )])
                .to_err(0)
                .into()
            }
            Some(t) => t,
        };
        let type_name = match first.inner {
            RawToken::Identifier(name) => name,
            _ => panic!("ERROR_TODO: Wanted type name, got {:?}", first),
        };
        let mut fields = Vec::new();
        loop {
            if tokenizer.next().map(|t| t.inner) != Some(RawToken::NewLine) {
                panic!("Expected newline")
            }
            tokenizer.skip_spaces_and_newlines();

            let next = tokenizer.next().expect("ERROR_TODO: File ended");
            let field_name = match next.inner {
                RawToken::Identifier(field_name) => field_name,
                RawToken::Header(h) => {
                    tokenizer.regress(h.as_str().len() + 1);
                    break;
                }
                _ => panic!("ERROR_TODO: Unexpected thingy in field decl, {:?}", next),
            };
            let next = tokenizer.next().expect("ERROR_TODO");
            match next.inner {
                RawToken::Identifier(type_name) => fields.push((
                    field_name.to_owned(),
                    Type::try_from(type_name.as_str()).unwrap(),
                )),
                _ => panic!(
                    "ERROR_TODO: Invalid syntax in field decleration, got {:?}",
                    next
                ),
            }
        }
        Ok((type_name, fields))
    }
}

impl fmt::Debug for Parser {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self
            .module_ids
            .iter()
            .map(|(mod_name, fid)| {
                //if *fid == PRELUDE_FID {
                //    String::new()
                // } else {
                format!(
                    "{}#{} {} {}\n{:?}",
                    Fg(color::Green),
                    fid,
                    mod_name,
                    Fg(color::Reset),
                    &self.modules[*fid]
                )
                // }
            })
            .collect::<Vec<String>>()
            .join("\n ---\n\n");
        f.write_str(&s)
    }
}

impl fmt::Debug for ParseModule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "IMPORTS:\n {}\nTYPES:\n {}\nFUNCTIONS:\n{}\nOPERATORS:\n{}",
            self.imports
                .iter()
                .map(|(name, fid)| format!(" {} -> {}", name, fid))
                .collect::<Vec<String>>()
                .join("\n"),
            self.types
                .iter()
                .map(|(tname, tid)| format!(
                    "  #{} {}\n{}",
                    tid,
                    tname,
                    self.type_fields[*tid]
                        .iter()
                        .map(|(f, t)| format!("      {} {}", f, t))
                        .collect::<Vec<String>>()
                        .join("\n")
                ))
                .collect::<Vec<String>>()
                .join("\n"),
            self.functions
                .values()
                .map(|same_name| same_name
                    .values()
                    .map(|(funcb, funcid)| format!("  #{} {:?}", funcid, funcb))
                    .collect::<Vec<String>>()
                    .join("\n"))
                .collect::<Vec<String>>()
                .join("\n"),
            self.operators
                .values()
                .map(|same_name| same_name
                    .values()
                    .map(|(opb, opid)| format!("  #{} {:?}", opid, opb))
                    .collect::<Vec<String>>()
                    .join("\n"))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}
