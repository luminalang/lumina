use std::convert::TryFrom;

#[derive(Debug)]
pub enum Inlined {
    Int(i32),
    Float(f32),
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
        Err(())
    }
}
