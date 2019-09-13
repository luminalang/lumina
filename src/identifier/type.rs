use crate::identifier::Identifier;
use crate::parser::index::*;
use crate::parser::prelude;
use std::convert::TryFrom;
use std::path::Path;

#[derive(Clone, Debug, PartialEq)]
pub enum BaseType {
    Nothing = 0,
    Int = 1,
    Float = 2,
    Byte = 3,
    Bool = 4,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Raw(String),

    Base(BaseType),
    UnsafeValid,
    Generic(u8),
    List(Box<Type>),
    Closure(Box<(Vec<Type>, Type)>),
    Custom(usize, usize, Option<Box<Type>>),
}

#[derive(Debug)]
pub struct CustomType {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

impl Type {
    pub fn is_number(&self) -> bool {
        match self {
            Type::Base(BaseType::Int) => true,
            Type::Base(BaseType::Float) => true,
            _ => false,
        }
    }
}

impl TryFrom<&str> for Type {
    type Error = ();

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let s = s.trim();
        use BaseType::*;
        match s {
            "int" => Ok(Type::Base(Int)),
            "float" => Ok(Type::Base(Float)),
            "byte" => Ok(Type::Base(Byte)),
            "bool" => Ok(Type::Base(Bool)),
            "_" | "nothing" => Ok(Type::Base(Nothing)),
            _ => {
                if s.len() == 1 {
                    // Generics
                    // Convert ascii byte to index (a.. -> 0..)
                    let ident = s.bytes().next().unwrap() - 97;
                    Ok(Type::Generic(ident))
                } else if s.bytes().next() == Some(b'[') {
                    // Lists
                    let sub_t = Type::try_from(&s[1..s.len() - 2])?;
                    Ok(Type::List(Box::new(sub_t)))
                } else {
                    Err(())
                }
            }
        }
    }
}

impl From<&Type> for String {
    fn from(src: &Type) -> Self {
        use BaseType::*;
        match src {
            Type::Base(bt) => match bt {
                Int => "int".to_owned(),
                Nothing => "nothing".to_owned(),
                Float => "float".to_owned(),
                Byte => "byte".to_owned(),
                Bool => "bool".to_owned(),
            },
            Type::Generic(c) => ((*c + 97) as char).to_string(),
            Type::List(of_t) => {
                let a = &**of_t;
                let s: String = a.into();
                format!("[{}]", s)
            }
            Type::Custom(fid, id, typearg) => {
                format!("Struct({}:{}<{:?}>)", fid, id, typearg.as_ref().clone())
            }
            _ => panic!("Can not display {:?}", src),
        }
    }
}

fn get_typearg(from: &str) -> Option<&str> {
    let from = from.trim_end();
    if from.bytes().last() == Some(b'>') {
        let mut iter = from.splitn(1, '<');
        iter.next().unwrap();
        iter.next().map(|t_str| &t_str[0..t_str.len() - 1])
    } else {
        None
    }
}

impl Identifier for Type {
    fn from(w: &str, call_fid: &Path, index: &mut Index) -> Self {
        if let Ok(tid) = Type::try_from(w) {
            return tid;
        };
        let mut spl = w.splitn(2, ':');
        let first = spl.next().unwrap();
        let (module, typename, typearg) = match spl.next() {
            Some(second) => match get_typearg(second) {
                Some(typearg) => (
                    call_fid,
                    &second[0..second.len() - typearg.len() - 2],
                    Some(typearg),
                ),
                None => (call_fid, second, None),
            },
            None => match get_typearg(first) {
                Some(typearg) => (
                    call_fid,
                    &first[0..first.len() - typearg.len() - 2],
                    Some(typearg),
                ),
                None => (call_fid, first, None),
            },
        };

        let (fid, tagstore) = index.get_file_or_create(module);
        let tagstore = tagstore.borrow_mut();
        match tagstore.try_get_type(typename) {
            Some(local) => Type::Custom(
                fid,
                local.0,
                typearg.map(|a| Box::new(Identifier::from(a, call_fid, index))),
            ),
            None => Type::Custom(
                prelude::LEAF_PRIM_FID,
                index
                    .get_file(Path::new(prelude::LEAF_PRIM_FILE))
                    .borrow_mut()
                    .get_type(typename)
                    .0,
                typearg.map(|a| Box::new(Identifier::from(a, call_fid, index))),
            ),
        }
    }

    fn as_str(&self) -> Option<&str> {
        match self {
            Type::Raw(s) => Some(s),
            _ => panic!("Attempted to take custom type name after optimization"),
        }
    }

    fn get_fid_id(&self) -> (usize, usize) {
        match self {
            Type::Custom(fid, tid, _typearg) => (*fid, *tid),
            _ => panic!("Attempted to use type as optimized before optimization"),
        }
    }
}
