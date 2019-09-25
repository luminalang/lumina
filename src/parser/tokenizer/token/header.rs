use std::convert::TryFrom;

#[derive(Debug)]
pub enum Header {
    Function,
    Type,
}

impl TryFrom<&[u8]> for Header {
    type Error = ();

    fn try_from(bytes: &[u8]) -> Result<Header, Self::Error> {
        let res = match bytes {
            b"fn" => Header::Function,
            b"type" => Header::Type,
            _ => return Err(()),
        };
        Ok(res)
    }
}
