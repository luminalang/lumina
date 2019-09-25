use std::convert::TryFrom;

#[derive(Debug)]
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
            _ => return Err(()),
        };
        Ok(res)
    }
}
