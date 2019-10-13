use std::convert::TryFrom;

#[derive(Debug, PartialEq, Clone)]
pub enum Inlined {
    Int(i32),
    Float(f32),
    Bool(bool),
    Nothing,
    // String(Vec<u8>),
}

impl TryFrom<&[u8]> for Inlined {
    type Error = ();

    fn try_from(bytes: &[u8]) -> Result<Inlined, ()> {
        let as_str = String::from_utf8(bytes.to_vec()).unwrap();
        if let Ok(int) = as_str.parse::<i32>() {
            return Ok(Inlined::Int(int));
        };
        if let Ok(float) = as_str.parse::<f32>() {
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
