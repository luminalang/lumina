use std::convert::TryFrom;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Header {
    Function,
    Operator,
    Type,
    Use,
}

impl TryFrom<&[u8]> for Header {
    type Error = ();

    fn try_from(bytes: &[u8]) -> Result<Header, Self::Error> {
        let res = match bytes {
            b"fn" => Header::Function,
            b"type" => Header::Type,
            b"use" => Header::Use,
            b"operator" => Header::Operator,
            _ => return Err(()),
        };
        Ok(res)
    }
}

impl Header {
    pub fn as_str(&self) -> &str {
        match self {
            Header::Function => "fn",
            Header::Operator => "operator",
            Header::Type => "type",
            Header::Use => "use",
        }
    }
}

impl fmt::Display for Header {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Header::Function => f.write_str("fn"),
            Header::Operator => f.write_str("operator"),
            Header::Type => f.write_str("type"),
            Header::Use => f.write_str("use"),
        }
    }
}
