use std::convert::TryFrom;
use std::fmt;

mod header;
pub use header::Header;
mod key;
pub use key::Key;
mod inlined;
use crate::parser::flags;
pub use inlined::Inlined;

pub struct Token {
    source_index: usize,
    flags: flags::Flag,
    pub inner: RawToken,
}

impl Token {
    pub fn with_source_index(mut self, source_index: usize) -> Self {
        self.source_index = source_index;
        self
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {:?}", self.source_index, self.inner)
    }
}

impl TryFrom<&[u8]> for Token {
    type Error = ();

    fn try_from(bytes: &[u8]) -> Result<Token, Self::Error> {
        if bytes.is_empty() {
            return Err(());
        }
        let find_inner = || {
            if let Ok(t) = Header::try_from(bytes) {
                return RawToken::Header(t);
            }
            if let Ok(t) = Key::try_from(bytes) {
                return RawToken::Key(t);
            }
            if let Ok(t) = Inlined::try_from(bytes) {
                return RawToken::Inlined(t);
            }
            if bytes == b"\n" {
                return RawToken::NewLine;
            }

            RawToken::Identifier(String::from_utf8(bytes.to_vec()).unwrap())
        };

        let t = Token {
            inner: find_inner(),
            source_index: 0,
            flags: flags::Flag::default(),
        };
        Ok(t)
    }
}

#[derive(Debug, PartialEq)]
pub enum RawToken {
    Identifier(String),

    Header(Header),
    Key(Key),
    Inlined(Inlined),

    NewLine,
}

impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        self.inner.eq(&other.inner)
    }
}
