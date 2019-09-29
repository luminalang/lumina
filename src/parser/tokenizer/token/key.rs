use std::convert::TryFrom;

#[derive(Debug, PartialEq)]
pub enum Key {
    ParenOpen,
    ParenClose,
    Arrow,
    TypeSep,
    Pipe,
    Bar,
    ClosureMarker,
    ListOpen,
    ListClose,
    RecordOpen,
    RecordClose,
    Match,
    If,
    Elif,
    Else,
    Then,
    Colon,
    Where,
    PrimitiveExit,
}

impl TryFrom<&[u8]> for Key {
    type Error = ();

    fn try_from(bytes: &[u8]) -> Result<Key, Self::Error> {
        let res = match bytes {
            b"(" => Key::ParenOpen,
            b")" => Key::ParenClose,
            b"->" => Key::Arrow,
            b"::" => Key::TypeSep,
            b"<<" => Key::Pipe,
            b"|" => Key::Bar,
            b"#" => Key::ClosureMarker,
            b"[" => Key::ListOpen,
            b"]" => Key::ListClose,
            b"{" => Key::RecordOpen,
            b"}" => Key::RecordClose,
            b":" => Key::Colon,
            b"match" => Key::Match,
            b"if" => Key::If,
            b"elif" => Key::Elif,
            b"else" => Key::Else,
            b"then" => Key::Then,
            b"where" => Key::Where,
            b"exit" => Key::PrimitiveExit,
            _ => return Err(()),
        };
        Ok(res)
    }
}
