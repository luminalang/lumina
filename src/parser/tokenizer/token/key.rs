use std::convert::TryFrom;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Key {
    ParenOpen,
    ParenClose,
    Arrow,
    Pipe,
    Bar,
    ClosureMarker,
    ListOpen,
    ListClose,
    RecordOpen,
    RecordClose,
    Match,
    Comma,
    If,
    Elif,
    Else,
    Then,
    And,
    Or,
    First,
    Colon,
    Where,
    PrimitiveExit,
    PrimitiveUnimplemented,
}

impl TryFrom<&[u8]> for Key {
    type Error = ();

    fn try_from(bytes: &[u8]) -> Result<Key, Self::Error> {
        let res = match bytes {
            b"(" => Key::ParenOpen,
            b")" => Key::ParenClose,
            b"->" => Key::Arrow,
            b"<<" => Key::Pipe,
            b"|" => Key::Bar,
            b"#" => Key::ClosureMarker,
            b"[" => Key::ListOpen,
            b"]" => Key::ListClose,
            b"{" => Key::RecordOpen,
            b"}" => Key::RecordClose,
            b":" => Key::Colon,
            b"," => Key::Comma,
            b"match" => Key::Match,
            b"if" => Key::If,
            b"elif" => Key::Elif,
            b"else" => Key::Else,
            b"then" => Key::Then,
            b"where" => Key::Where,
            b"exit" => Key::PrimitiveExit,
            b"or" => Key::Or,
            b"and" => Key::And,
            b"first" => Key::First,
            b"unimplemented" => Key::PrimitiveUnimplemented,
            _ => return Err(()),
        };
        Ok(res)
    }
}

impl Key {
    pub fn as_str(&self) -> &str {
        match self {
            Key::ParenOpen => "(",
            Key::ParenClose => ")",
            Key::Arrow => "->",
            Key::Pipe => "<<",
            Key::Bar => "|",
            Key::ClosureMarker => "#",
            Key::ListOpen => "[",
            Key::ListClose => "]",
            Key::RecordOpen => "{",
            Key::RecordClose => "}",
            Key::Colon => ":",
            Key::Comma => ",",
            Key::Match => "match",
            Key::If => "if",
            Key::Elif => "elif",
            Key::Else => "else",
            Key::Then => "then",
            Key::And => "and",
            Key::Or => "or",
            Key::First => "first",
            Key::Where => "where",
            Key::PrimitiveExit => "exit",
            Key::PrimitiveUnimplemented => "unimplemented",
        }
    }
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
