use super::function::Function;
use super::r#type::Value;
use super::runner::Runner;
use crate::identifier::r#type::CustomType;
use crate::parser::ParseModule;

pub static mut RUNTIME: Runtime = Runtime {
    modules: Vec::new(),
};

#[derive(Debug)]
pub struct Runtime {
    modules: Vec<Module>,
}

impl Runtime {
    pub fn len(&self) -> usize {
        self.modules.len()
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl From<Vec<ParseModule>> for Runtime {
    fn from(mut modules: Vec<ParseModule>) -> Runtime {
        Runtime {
            modules: modules.drain(0..).map(|m| m.into()).collect(),
        }
    }
}

pub fn get<'a>(id: usize) -> Option<&'a Module> {
    unsafe { RUNTIME.modules.get(id) }
}

pub unsafe fn get_mut<'a>(id: usize) -> Option<&'a mut Module> {
    RUNTIME.modules.get_mut(id)
}

pub struct Module {
    pub functions: Vec<Function>,
    pub types: Vec<CustomType>,

    pub fid: usize,
}

pub fn dump_modules() {
    unsafe {
        for (i, m) in RUNTIME.modules.iter().enumerate() {
            eprintln!(
                "{} => \n  functions: {:?}\n  types: {:?}",
                i, m.functions, m.types,
            );
        }
    }
}

impl Module {
    pub fn with_fid(fid: usize) -> Self {
        Self {
            fid,
            functions: Vec::new(),
            types: Vec::new(),
        }
    }

    pub fn get_function(&self, id: usize) -> Function {
        self.functions[id].clone()
    }
    pub fn get_type(&self, id: usize) -> &CustomType {
        &self.types[id]
    }

    pub fn run(mut self, entrypoint: usize) -> Value {
        let func = &mut self.functions[entrypoint];
        Runner::run(func, Vec::new())
    }
}

impl From<ParseModule> for Module {
    fn from(mut pmodule: ParseModule) -> Module {
        Module {
            fid: pmodule.fid,
            functions: pmodule.functions.drain(0..).map(|f| f.into()).collect(),
            types: Vec::new(), // TODO: I don't think runtime even needs a record of the types since we're fully static
        }
    }
}

use std::fmt;
impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut functions = String::new();
        for func in self.functions.iter() {
            functions.push_str(&format!("    (takes {}) {:?}\n", func.params, func.tokens));
        }
        let mut types = String::new();
        for typ in self.types.iter() {
            types.push_str(&format!("{:?}", typ));
        }
        write!(
            f,
            " - Module {}:\n  {}Functions:{}\n{}\n  {}Types:{}\n    {}",
            self.fid,
            termion::color::Fg(termion::color::Green),
            termion::color::Fg(termion::color::Reset),
            functions,
            termion::color::Fg(termion::color::Green),
            termion::color::Fg(termion::color::Reset),
            types
        )
    }
}
