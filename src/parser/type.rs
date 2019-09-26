use std::convert::TryFrom;
use std::fmt;

pub enum Type {
    Nothing,
    Int,
    Float,
    List(Box<Type>),
    Struct(i32, i32),
    Custom(String),
}

impl std::default::Default for Type {
    fn default() -> Self {
        Type::Nothing
    }
}

impl TryFrom<&str> for Type {
    type Error = ();

    fn try_from(source: &str) -> Result<Type, Self::Error> {
        if source.bytes().next() == Some(b'[') {
            // TODO: ERROR_TODO: []
            let inner = source[1..source.len() - 2].trim();
            return Ok(Type::List(Box::new(Type::try_from(inner)?)));
        }
        let r = match source {
            "int" => Type::Int,
            "float" => Type::Float,
            "nothing" => Type::Nothing,
            "_" => Type::Nothing,
            _ => return Err(()),
        };
        Ok(r)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Nothing => f.write_str("nothing"),
            Type::Int => f.write_str("int"),
            Type::Float => f.write_str("float"),
            Type::List(inner) => write!(f, "[{}]", inner.to_string()),
            Type::Struct(fid, tid) => write!(f, "Struct({}:{})", fid, tid),
            Type::Custom(name) => write!(f, "unevaluated type {}", name),
        }
    }
}
