use crate::env::Environment;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::Path;

mod tokenizer;
pub use tokenizer::{is_valid_identifier, Header, Inlined, Key, RawToken, Token, Tokenizer};
mod function;
mod leafmod;
pub use function::{BodySource, FunctionBuilder};
pub use leafmod::FileSource;
mod r#type;
pub use r#type::Type;
mod checker;
pub mod flags;
use checker::TypeChecker;

pub struct Parser<'a> {
    pub module_ids: HashMap<FileSource, usize>,
    modules: Vec<ParseModule>,
    environment: &'a Environment,
}

#[derive(Default)]
struct ParseModule {
    // TODO: I probably want to convert Vec<Type> into a numeric representation like `0425` to save
    // heap allocations just for id lookups
    pub functions: HashMap<(String, Vec<Type>), (FunctionBuilder, usize)>,
    pub types: HashMap<String, usize>,
    pub type_fields: Vec<Vec<(String, Type)>>,
    pub imports: HashMap<String, usize>,
}

impl<'a> Parser<'a> {
    pub fn new(environment: &'a Environment) -> Self {
        Self {
            module_ids: HashMap::new(),
            modules: Vec::new(),
            environment,
        }
    }

    fn get_function(
        &self,
        fid: usize,
        name: &str,
        params: Vec<Type>,
    ) -> Option<&(FunctionBuilder, usize)> {
        self.modules
            .get(fid)?
            .functions
            .get(&(name.to_owned(), params))
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
                self.module_ids.insert(source, fid);
                self.modules.push(ParseModule::default());
                fid
            }
        }
    }
    fn new_function(&mut self, fid: usize, funcb: FunctionBuilder) -> usize {
        let module = &mut self.modules[fid];
        let funcid = module.functions.len();
        module.functions.insert(
            (
                funcb.name.clone(),
                funcb
                    .parameter_types
                    .iter()
                    .cloned()
                    .map(|(_flags, t)| t)
                    .collect(),
            ),
            (funcb, funcid),
        );
        funcid
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
    pub fn tokenize(&mut self, module_path: FileSource, source_code: &[u8]) -> Result<usize, ()> {
        let fid = self.new_module(module_path.clone());

        let mut tokenizer = Tokenizer::from(source_code);
        // let mut function_buf: Vec<FunctionBuilder> = Vec::new();
        loop {
            let token = match tokenizer.next() {
                Some(t) => t,
                None => return Ok(fid),
            };
            match token.inner {
                RawToken::Header(h) => match h {
                    Header::Function => {
                        let mut funcb = FunctionBuilder::new().with_header(&mut tokenizer)?;

                        let body_entry = funcb.parse_func(&mut tokenizer)?;

                        funcb.push(body_entry);

                        self.new_function(fid, funcb);
                    }
                    Header::Type => {
                        let (type_name, fields) = self.parse_type_decl(&mut tokenizer)?;

                        self.new_type(fid, type_name, fields);
                    }
                    Header::Use => {
                        let import = match tokenizer.next().map(|t| t.inner) {
                            Some(RawToken::Identifier(single)) => vec![single],
                            Some(RawToken::ExternalIdentifier(entries)) => entries,
                            None => panic!("ET: Nothing after `use` keyword"),
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
                                    self.environment,
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
                        }; // ET

                        let mut source_code = Vec::with_capacity(20);
                        File::open(file_path.to_pathbuf(self.environment))
                            .unwrap() // ET
                            .read_to_end(&mut source_code)
                            .unwrap(); // ET

                        let usefid = self.tokenize(file_path.clone(), &source_code)?;
                        self.modules[fid]
                            .imports
                            .insert(import.last().unwrap().clone(), usefid);
                    }
                },
                RawToken::NewLine => continue,
                _ => panic!("ERROR_TODO: Unexpected {:?}, wanted header", token),
            }
        }
    }

    pub fn type_check(&mut self, fid: usize) -> Result<Type, ()> {
        TypeChecker::new(self, fid, "main", Vec::new(), Type::Nothing).run()
    }

    fn parse_type_decl(
        &mut self,
        tokenizer: &mut Tokenizer,
    ) -> Result<(String, Vec<(String, Type)>), ()> {
        let first = tokenizer.next().expect("ERROR_TODO");
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
            /*
            if let RawToken::Identifier(type_name) = next.inner {
                fields.push((
                    field_name.to_owned(),
                    Type::try_from(type_name.as_str()).unwrap(),
                ))
            } else {
                panic!(
                    "ERROR_TODO: Invalid syntax in field decleration, got {:?}",
                    next
                );
            }
            */
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

impl fmt::Debug for Parser<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self
            .module_ids
            .iter()
            .map(|(mod_name, fid)| format!("#{} {}\n{:?}", fid, mod_name, &self.modules[*fid]))
            .collect::<Vec<String>>()
            .join("\n ---\n");
        f.write_str(&s)
    }
}

impl fmt::Debug for ParseModule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "imports:\n {}\ntypes:\n {}\nfunctions:\n{}",
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
                .map(|(funcb, funcid)| format!(
                    "  #{} {} ({:?} -> {:?})\n{:#?}",
                    funcid,
                    funcb.name,
                    funcb
                        .parameter_types
                        .iter()
                        .map(|(_flag, t)| t)
                        .collect::<Vec<&Type>>(),
                    funcb.returns,
                    funcb.body,
                ))
                .collect::<Vec<String>>()
                .join("\n"),
        )
    }
}
