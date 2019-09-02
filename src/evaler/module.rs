use super::function::Function;
use super::r#type::{CustomType, Value};
use super::runner::Runner;
use crate::error::Leaf;

pub static mut RUNTIME: Runtime = Runtime {
    modules: Vec::new(),
};

#[derive(Debug)]
pub struct Runtime {
    modules: Vec<Option<Module>>, // None are reserved spaces
}

impl Runtime {
    pub fn len(&self) -> usize {
        self.modules.len()
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

pub fn get<'a>(id: usize) -> Option<&'a Module> {
    unsafe { RUNTIME.modules.get(id).unwrap().as_ref() }
}
/*
pub fn last<'a>() -> Option<&'a Module> {
    unsafe { RUNTIME.modules.last().unwrap().as_ref() }
}
*/

pub unsafe fn get_mut<'a>(id: usize) -> Option<&'a mut Module> {
    match RUNTIME.modules.get_mut(id) {
        None => panic!("Module {} does not exist", id),
        Some(m) => match m {
            None => panic!("Module {} is not allocated", id),
            Some(m) => Some(m),
        },
    }
}

pub unsafe fn reserve_module_space() -> usize {
    let reserved_id = RUNTIME.modules.len();
    RUNTIME.modules.push(None);
    reserved_id
}
pub unsafe fn set(id: usize, module: Module) {
    RUNTIME.modules[id] = Some(module);
}

pub unsafe fn push_module(module: Module) {
    RUNTIME.modules.push(Some(module))
}

pub struct Module {
    pub functions: Vec<Function>,
    pub types: Vec<CustomType>,

    pub fileid: usize,
}

pub fn dump_modules() {
    unsafe {
        for (i, m) in RUNTIME.modules.iter().enumerate() {
            eprintln!(
                "{} => \n  functions: {:?}\n  types: {:?}",
                i,
                m.as_ref().map(|m| &m.functions).unwrap_or(&Vec::new()),
                m.as_ref().map(|m| &m.types).unwrap_or(&Vec::new()),
            );
        }
    }
}

impl Module {
    pub fn with_fileid(fileid: usize) -> Self {
        Self {
            fileid,
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
        Runner::run(func, &[])
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
            self.fileid,
            termion::color::Fg(termion::color::Green),
            termion::color::Fg(termion::color::Reset),
            functions,
            termion::color::Fg(termion::color::Green),
            termion::color::Fg(termion::color::Reset),
            types
        )
    }
}
