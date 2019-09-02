//use crate::evaler::function::BridgedFunction;
use crate::identifier::Identifier;
use crate::parser::index::*;
use std::path::Path;

pub enum Function {
    Raw(String),

    //Bridged(BridgedFunction),
    Internal(usize),
    External(usize, usize),
}

impl Identifier for Function {
    fn from(w: &str, call_fid: &Path, index: &mut Index) -> Self {
        let mut spl = w.splitn(2, ':');
        let first = spl.next().unwrap();
        match spl.next() {
            Some(second) => {
                if first == "rust" {
                    panic!("RUST BRIDGE NOT IMPLEMENTED");
                } else {
                    let (fid, tagstore) = index.get_file_or_create(Path::new(first));
                    let tagstore = tagstore.borrow_mut();
                    Function::External(fid, tagstore.get_func(second))
                }
            }
            None => {
                let (fid, tagstore) = index.get_file_or_create(call_fid);
                let tagstore = tagstore.borrow_mut();
                Function::External(fid, tagstore.get_func(first))
            }
        }
    }

    fn as_str(&self) -> Option<&str> {
        match self {
            Function::Raw(s) => Some(s),
            _ => panic!("Attempted to take function name after optimization"),
        }
    }

    fn get_fid_id(&self) -> (usize, usize) {
        match self {
            Function::External(fid, tid) => (*fid, *tid),
            _ => panic!("Attempted to use func as optimized before optimization"),
        }
    }
}
