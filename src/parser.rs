use std::collections::HashMap;
use std::fmt;

pub const MAIN_MODULE_ID: usize = 0;

mod tokenizer;
pub use tokenizer::{Header, Key, RawToken, Token, Tokenizer};
mod function;
pub use function::FunctionBuilder;
mod r#type;
pub use r#type::Type;

pub struct Parser {
    module_ids: HashMap<String, usize>,
    modules: Vec<Option<ParseModule>>,
}

#[derive(Default)]
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

    fn _get_function_id(&self, fid: usize, name: &str) -> Option<usize> {
        match self.modules.get(fid)? {
            None => panic!("Module {} used before created", name),
            Some(module) => module.functions.get(name).copied(),
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
    fn new_function(&mut self, fid: usize, name: &str) -> usize {
        match &mut self.modules[fid] {
            None => panic!("Module {} used before initialized", name),
            Some(module) => {
                let funcid = module.functions.len();
                module.functions.insert(name.to_owned(), funcid);
                funcid
            }
        }
    }
    fn new_type(&mut self, fid: usize, name: &str) -> usize {
        match &mut self.modules[fid] {
            None => panic!("Module {} used before initialized", name),
            Some(module) => {
                let typeid = module.types.len();
                module.types.insert(name.to_owned(), typeid);
                typeid
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

                        while let Some(token) = tokenizer.next() {
                            if let RawToken::Header(h) = token.inner {
                                tokenizer.regress(h.as_str().len() + 1);
                                break;
                            }
                            funcb.push(token);
                        }

                        self.new_function(fid, &funcb.name);
                        function_buf.push(funcb);
                    }
                    Header::Type => {
                        unimplemented!("Custom Types");
                    }
                },
                RawToken::NewLine => continue,
                _ => panic!("ERROR_TODO: Unexpected {:?}, wanted header", token),
            }
        }
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
                .map(|(tname, tid)| format!("  #{} {}", tid, tname))
                .collect::<Vec<String>>()
                .join("\n"),
            self.functions
                .iter()
                .map(|(fname, funcid)| format!("  #{} {}", funcid, fname))
                .collect::<Vec<String>>()
                .join("\n"),
        )
    }
}
