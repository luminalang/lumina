use super::{tokenizer::Operator, Key, RawToken};
use std::convert::Into;
use std::io;
use std::path::PathBuf;

#[derive(Debug)]
pub struct ParseError {
    pub source_index: usize,
    pub variant: ParseFault,
}

impl ParseError {
    pub fn new(i: usize, err: ParseFault) -> Self {
        Self {
            source_index: i,
            variant: err,
        }
    }

    pub fn fallback(mut self, fallback: usize) -> Self {
        if self.source_index == 0 {
            self.source_index = fallback;
        }
        self
    }
}

impl<T> Into<Result<T, ParseError>> for ParseError {
    fn into(self) -> Result<T, ParseError> {
        Err(self)
    }
}

#[derive(Debug)]
pub enum ParseFault {
    EndedWhileExpecting(Vec<RawToken>),
    GotButExpected(RawToken, Vec<RawToken>),
    ModuleLoadNotFound(Vec<String>),
    ModuleLoadFailed(PathBuf, io::ErrorKind),
    NotValidType(String),
    MissingRightSideOperator(Box<(RawToken, Operator, RawToken)>),
    EndedMissingRightSideOperator(RawToken, Operator),
    InvalidIdentifier(String, IdentSource),
    InvalidPath(Vec<String>),
    BridgedWrongPathLen(Vec<String>),
    BridgedFunctionNotFound(String, u16),
    BridgedFunctionNoMode(u8),
    Unexpected(RawToken),
    Unmatched(Key),
    FirstStmNoThen,
    EmptyParen,
    IfMissingElse,
    IfMissingThen,
    IfDoubleElse,
    ListMissingClose,
    OpNoIdent,
    OpWantedIdent(RawToken),
    InvalidParameterName(String),
    PipeIntoVoid,
    EmptyListType,
    Internal,
}

#[derive(Debug)]
pub enum IdentSource {
    Module,
    FunctionDeclName,
    Ident,
}

impl ParseFault {
    pub fn as_err(self, i: usize) -> ParseError {
        ParseError::new(i, self)
    }
}
