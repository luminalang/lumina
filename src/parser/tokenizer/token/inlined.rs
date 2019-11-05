use std::convert::TryFrom;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Inlined {
    Int(i64),
    Float(f64),
    Bool(bool),
    Nothing,
}

impl TryFrom<&[u8]> for Inlined {
    type Error = ();

    fn try_from(bytes: &[u8]) -> Result<Inlined, ()> {
        let as_str = String::from_utf8(bytes.to_vec()).unwrap();
        if let Ok(int) = as_str.parse::<i64>() {
            return Ok(Inlined::Int(int));
        };
        if let Ok(float) = as_str.parse::<f64>() {
            return Ok(Inlined::Float(float));
        };
        if let Ok(boolean) = as_str.parse::<bool>() {
            return Ok(Inlined::Bool(boolean));
        }
        if bytes == b"_" {
            return Ok(Inlined::Nothing);
        }
        if bytes.first().copied() == Some(b'"') {
            unimplemented!("string literals");
        }
        if bytes.first().copied() == Some(b'\'') {
            unimplemented!("byte literals");
        }
        Err(())
    }
}

impl fmt::Display for Inlined {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Inlined::Int(n) => write!(f, "{}", n),
            Inlined::Float(n) => write!(f, "{}", n),
            Inlined::Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Inlined::Nothing => f.write_str("_"),
        }
    }
}
