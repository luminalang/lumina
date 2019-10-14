use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;

pub const MAIN_MODULE_ID: usize = 0;

mod tokenizer;
pub use tokenizer::{is_valid_identifier, Header, Key, RawToken, Token, Tokenizer};
mod function;
pub use function::{BodySource, FunctionBuilder};
mod r#type;
pub use r#type::Type;
pub mod flags;
use std::path::Path;
use std::io::Read;

pub struct Parser {
    module_ids: HashMap<String, usize>,
    modules: Vec<Option<ParseModule>>,
}

#[derive(Default)]
struct ParseModule {
    // TODO: I probably want to convert Vec<Type> into a numeric representation like `0425` to save
    // heap allocations just for id lookups
    functions: HashMap<(String, Vec<Type>, Type), usize>,
    types: HashMap<String, usize>,
    type_fields: Vec<Vec<(String, Type)>>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            module_ids: HashMap::new(),
            modules: Vec::new(),
        }
    }

    fn _get_function_id(
        &self,
        fid: usize,
        name: &str,
        params: Vec<Type>,
        returns: Type,
    ) -> Option<usize> {
        match self.modules.get(fid)? {
            None => panic!("Module {} used before created", name),
            Some(module) => module
                .functions
                .get(&(name.to_owned(), params, returns))
                .copied(),
        }
    }
    fn _get_type_id(&self, fid: usize, name: &str) -> Option<usize> {
        match self.modules.get(fid)? {
            None => panic!("Module {} used before created", name),
            Some(module) => module.types.get(name).copied(),
        }
    }

    fn new_module(&mut self, name: &str) -> usize {
        let fid = self.module_ids.len();
        assert_eq!(fid, self.modules.len());
        self.module_ids.insert(name.to_owned(), fid);
        self.modules.push(Some(ParseModule::default()));
        fid
    }
    fn new_function(&mut self, fid: usize, funcb: &FunctionBuilder) -> usize {
        match &mut self.modules[fid] {
            None => panic!("Module {} used before initialized", funcb.name),
            Some(module) => {
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
                        funcb.returns.clone(),
                    ),
                    funcid,
                );
                funcid
            }
        }
    }
    fn new_type(&mut self, fid: usize, name: String, fields: Vec<(String, Type)>) -> usize {
        match &mut self.modules[fid] {
            None => panic!("Module {} used before initialized", name),
            Some(module) => {
                let typeid = module.types.len();
                module.types.insert(name.to_owned(), typeid);
                module.type_fields.push(fields);
                typeid
            }
        }
    }

    pub fn read_prelude_source(&mut self) {
        let env_leafpath = std::env::var("LEAFPATH").expect("LEAFPATH variable not set.");
        // get path object from LEAFPATH environment variable string
        let leafpath = Path::new(&env_leafpath);

        // get current working directory from environment variable
        let current_dir = std::env::current_dir().expect("Current directory couldn't be read.");

        self.tokenize_prelude(leafpath.join("prelude").as_path());
        self.tokenize_prelude(current_dir.join("prelude").as_path());
    }

    fn tokenize_prelude(&mut self, path : &Path) {
        for entry in path.read_dir().expect("Couldn't read prelude directory.") {
            let file_path = entry.expect("Prelude file path doesn't exist.").path();
            if file_path.extension() == Some(std::ffi::OsStr::new("lf")) {
                let mut source_code_buffer = Vec::new();
                std::fs::File::open(file_path)
                    .expect("Couldn't open file.")
                    .read_to_end(&mut source_code_buffer)
                    .expect("Couldn't read file.");

                self.tokenize("prelude", &source_code_buffer)
                    .expect("Tokenization failed.");
            }
        }
    }

    // We only have to return Functions because custom types only need to be indexed
    pub fn tokenize(
        &mut self,
        mod_name: &str,
        source_code: &[u8],
    ) -> Result<Vec<FunctionBuilder>, ()> {
        let fid = self.new_module(mod_name);

        let mut tokenizer = Tokenizer::from(source_code);
        let mut function_buf: Vec<FunctionBuilder> = Vec::new();
        loop {
            let token = match tokenizer.next() {
                Some(t) => t,
                None => return Ok(function_buf),
            };
            match token.inner {
                RawToken::Header(h) => match h {
                    Header::Function => {
                        let mut funcb = FunctionBuilder::new().with_header(&mut tokenizer)?;

                        let body_entry = funcb.parse_func(&mut tokenizer)?;

                        funcb.push(body_entry);

                        self.new_function(fid, &funcb);
                        function_buf.push(funcb);
                    }
                    Header::Type => {
                        let (type_name, fields) = self.parse_type_decl(&mut tokenizer)?;

                        self.new_type(fid, type_name, fields);
                    }
                },
                RawToken::NewLine => continue,
                _ => panic!("ERROR_TODO: Unexpected {:?}, wanted header", token),
            }
        }
    }

    pub fn type_check(&mut self, mut functions: Vec<FunctionBuilder>) -> Result<(), ()> {
        for func in functions.iter_mut() {
            func.verify(&self)?;
        }
        Ok(())
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
                format!(
                    "#{} {}\n{:?}",
                    fid,
                    mod_name,
                    &self.modules[*fid].as_ref().unwrap()
                )
            })
            .collect::<Vec<String>>()
            .join("\n ---\n");
        f.write_str(&s)
    }
}

impl fmt::Debug for ParseModule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "types:\n {}\nfunctions:\n{}",
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
                .iter()
                .map(|((fname, params, returns), funcid)| format!(
                    "  #{} {} ({:?} -> {:?})",
                    funcid, fname, params, returns
                ))
                .collect::<Vec<String>>()
                .join("\n"),
        )
    }
}
