use crate::parser::body::{IfExpr, MatchExpr};
use crate::parser::{flags, Type};
use std::convert::TryFrom;
use std::fmt;

mod header;
pub use header::Header;
mod key;
pub use key::Key;
mod inlined;
pub use inlined::Inlined;
mod operator;
pub use operator::Operator;

#[derive(Clone, Default)]
pub struct Token {
    pub source_index: usize,
    flags: flags::Flag,
    pub inner: RawToken,
}

impl Token {
    pub fn new(inner: RawToken, source_index: usize) -> Self {
        Self {
            source_index,
            flags: flags::Flag::default(),
            inner,
        }
    }

    pub fn with_source_index(mut self, source_index: usize) -> Self {
        self.source_index = source_index;
        self
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {:#?}", self.source_index, self.inner)
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
            if let Ok(t) = Operator::try_from(bytes) {
                return RawToken::Operator(t);
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

#[derive(Debug, PartialEq, Clone)]
pub enum RawToken {
    Identifier(String),
    ExternalIdentifier(Vec<String>),

    Header(Header),
    Key(Key),
    Inlined(Inlined),
    Operation(Box<(Token, Token)>, Operator),
    Parameters(Vec<Token>),
    Parameterized(Box<Token>, Vec<Token>),

    Operator(Operator),
    IfExpression(IfExpr),
    MatchExpression(MatchExpr),
    FirstStatement(Vec<Token>),
    List(Vec<Token>),
    RustCall(u16, Type),

    NewLine,
}

impl Default for RawToken {
    fn default() -> Self {
        RawToken::NewLine
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        self.inner.eq(&other.inner)
    }
}

pub const ALLOWED_IDENTIFIER_CHARACTERS: &[u8] = b"abcdefghijklmnopqrstuvwxyz1234567890_";
pub fn is_valid_identifier(ident: &str) -> bool {
    for c in ident.bytes() {
        if !ALLOWED_IDENTIFIER_CHARACTERS.contains(&c) {
            return false;
        }
    }
    let trimmed = ident.trim();
    trimmed.parse::<i64>().is_err()
}
