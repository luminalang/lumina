use super::error::ParseError;
use crate::identifier::r#type::Type;
use crate::parser::function::{FunctionBuilder, FunctionHeader};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use termion::color;
use termion::color::Fg;

mod translator;
use translator::Translator;

pub struct Tagger {
    pub functions_ids: HashMap<String, usize>,
    pub function_headers: Vec<Rc<FunctionHeader>>,
    pub types: HashMap<String, (usize, Vec<(String, Type)>)>,
    pub imports: HashMap<std::ffi::OsString, PathBuf>,
}

impl fmt::Debug for Tagger {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let functions = self
            .functions_ids
            .iter()
            .map(|(_name, fid)| format!("    {}: {:?}", *fid, self.function_headers[*fid]))
            .collect::<Vec<String>>()
            .join("\n");
        let types = self
            .types
            .iter()
            .map(|(name, t)| format!("    {}: {:?}", name, t.1))
            .collect::<Vec<String>>()
            .join("\n");
        let imports = self
            .imports
            .iter()
            .map(|(name, imp)| format!("    {:?}: {:?}", name, imp.file_name()))
            .collect::<Vec<String>>()
            .join("\n");
        write!(
            f,
            "{green}Functions:{reset} \n{f}\n{green}Types:{reset} \n{t}\n{green}Imports:{reset} \n{i}\n",
            green = Fg(color::Green),
            reset = Fg(color::Reset),
            f = functions,
            t = types,
            i = imports
        )
    }
}

#[derive(Default)]
pub struct Index {
    file_ids: HashMap<PathBuf, usize>,
    files: Vec<Rc<RefCell<Tagger>>>,
}

impl fmt::Debug for Index {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ind = self
            .file_ids
            .iter()
            .enumerate()
            .map(|(fid, file)| {
                format!(
                    "({}) - {:?}:\n  {:?}",
                    fid,
                    file.0.file_name().unwrap(),
                    self.files[fid].borrow()
                )
            })
            .collect::<Vec<String>>()
            .join("\n");
        f.write_str(&ind)
    }
}

impl Tagger {
    fn new() -> Self {
        Tagger {
            functions_ids: HashMap::new(),
            function_headers: Vec::new(),
            types: HashMap::new(),
            imports: HashMap::new(),
        }
    }
    pub fn set_type(&mut self, w: &str, t: Vec<(String, Type)>) -> usize {
        let tid = self.types.len();
        self.types.insert(w.to_owned(), (tid, t));
        tid
    }
    pub fn get_type(&self, w: &str) -> &(usize, Vec<(String, Type)>) {
        match self.types.get(w) {
            Some(tid) => tid,
            None => panic!("Type {:?} does not exist yet", w),
        }
    }
    pub fn try_get_type(&self, w: &str) -> Option<&(usize, Vec<(String, Type)>)> {
        match self.types.get(w) {
            Some(tid) => Some(tid),
            None => None,
        }
    }
    pub fn get_func(&self, w: &str) -> usize {
        match self.functions_ids.get(w) {
            Some(fnid) => *fnid,
            None => panic!("Function {:?} does not exist yet", w),
        }
    }
    pub fn set_func(&mut self, func: FunctionHeader) -> usize {
        let funcid = self.functions_ids.len();
        self.functions_ids.insert(func.name.clone(), funcid);
        self.function_headers.push(Rc::new(func));
        assert_eq!(self.functions_ids.len(), self.function_headers.len());
        funcid
    }
    pub fn try_get_func(&self, w: &str) -> Option<usize> {
        match self.functions_ids.get(w) {
            Some(fnid) => Some(*fnid),
            None => None,
        }
    }
    pub fn get_func_header(&self, id: usize) -> Rc<FunctionHeader> {
        self.function_headers[id].clone()
    }

    pub fn get_import(&self, w: &std::ffi::OsStr) -> Option<&Path> {
        self.imports.get(w).map(|p| p.as_path())
    }
    pub fn import(&mut self, name: &std::ffi::OsStr, path: &Path) {
        self.imports.insert(name.to_owned(), path.to_owned());
    }
}

impl Index {
    pub fn new() -> Self {
        Index {
            file_ids: HashMap::new(),
            files: Vec::new(),
        }
    }

    pub fn get_file_or_create(&mut self, file: &Path) -> (usize, Rc<RefCell<Tagger>>) {
        match self.file_ids.get(file) {
            Some(fid) => (*fid, self.files[*fid].clone()),
            None => {
                let ptr = Rc::new(RefCell::new(Tagger::new()));
                let fid = self.file_ids.len();
                self.file_ids.insert(file.to_owned(), fid);
                self.files.push(ptr.clone());
                (fid, ptr)
            }
        }
    }
    pub fn get_file(&self, file: &Path) -> Rc<RefCell<Tagger>> {
        let fid = self.file_ids.get(file).expect("File used before created");
        self.files[*fid].clone()
    }
    pub fn get_file_from(&self, fid: usize) -> Rc<RefCell<Tagger>> {
        self.files[fid].clone()
    }

    pub fn try_get_file(&self, file: &Path) -> Option<(usize, Rc<RefCell<Tagger>>)> {
        self.file_ids
            .get(file)
            .map(|fid| (*fid, self.files[*fid].clone()))
    }

    pub fn optimize_func(
        &mut self,
        mut func: FunctionBuilder,
    ) -> Result<FunctionBuilder, ParseError> {
        let mut translator = Translator::new(self, &func.header.name);
        for (i, param) in func.header.parameters.iter().enumerate() {
            translator.tag_param(&func.header.parameter_names[i], param.clone())
        }
        for wheres in func.header.wheres.iter() {
            translator.tag_where(wheres.as_ref());
        }
        // TODO: Am i forgetting to translate the tokens in where statements?
        translator.translate_buf(&func.file, &mut func.tokens)?;

        Ok(func)
    }
}

enum Identified {
    Function(usize, usize, Rc<FunctionHeader>),
}
