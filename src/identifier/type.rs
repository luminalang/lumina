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
    Generic(u8),
    List(Box<Type>),
    Closure(Box<(Vec<Type>, Type)>),
    Custom(usize, usize),
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
        use BaseType::*;
        match s {
            "int" => Ok(Type::Base(Int)),
            "float" => Ok(Type::Base(Float)),
            "byte" => Ok(Type::Base(Byte)),
            "bool" => Ok(Type::Base(Bool)),
            "_" | "nothing" => Ok(Type::Base(Nothing)),
            _ => {
                if s.len() == 1 {
                    let ident = s.bytes().next().unwrap() - 97;
                    Ok(Type::Generic(ident))
                } else if s.get(0..5) == Some("list<") {
                    let sub_t = Type::try_from(&s[5..s.len() - 2])?;
                    Ok(Type::List(Box::new(sub_t)))
                } else {
                    Err(())
                }
            }
        }
    }
}

impl Into<String> for &Type {
    fn into(self) -> String {
        use BaseType::*;
        match self {
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
            Type::Custom(fid, id) => format!("Struct({}:{})", fid, id),
            _ => panic!("Can not display {:?}", self),
        }
    }
}

impl Identifier for Type {
    fn from(w: &str, call_fid: &Path, index: &mut Index) -> Self {
        if let Ok(tid) = Type::try_from(w) {
            return tid;
        };
        let mut spl = w.splitn(2, ':');
        let first = spl.next().unwrap();
        let (module, typename) = match spl.next() {
            Some(second) => (Path::new(first), second),
            None => (call_fid, first),
        };

        let (fid, tagstore) = index.get_file_or_create(module);
        let tagstore = tagstore.borrow_mut();
        match tagstore.try_get_type(typename) {
            Some(local) => Type::Custom(fid, local.0),
            None => Type::Custom(
                prelude::LEAF_PRIM_FID,
                index
                    .get_file(Path::new(prelude::LEAF_PRIM_FILE))
                    .borrow_mut()
                    .get_type(typename)
                    .0,
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
            Type::Custom(fid, tid) => (*fid, *tid),
            _ => panic!("Attempted to use type as optimized before optimization"),
        }
    }
}
