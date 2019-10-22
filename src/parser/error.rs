use super::{
    tokenizer::Operator, FileSource, FunctionBuilder, Key, OperatorBuilder, RawToken, Type,
};
use std::collections::HashMap;
use std::convert::Into;
use std::fmt;
use std::io;
use std::ops::Add;
use std::path::PathBuf;

#[derive(Debug)]
pub struct ParseError {
    pub source_index: usize,
    pub variant: ParseFault,
    pub source_code: Option<Vec<u8>>,
    pub module_name: Option<FileSource>,
}

impl ParseError {
    pub fn new(i: usize, err: ParseFault) -> Self {
        Self {
            source_index: i,
            variant: err,
            source_code: None,
            module_name: None,
        }
    }

    pub fn fallback(mut self, fallback: usize) -> Self {
        if self.source_index == 0 {
            self.source_index = fallback;
        }
        self
    }

    pub fn with_source_code(mut self, source_code: Vec<u8>, source_name: FileSource) -> Self {
        self.source_code = Some(source_code);
        self.module_name = Some(source_name);
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
    FnTypeReturnMismatch(FunctionBuilder, Type),
    OpTypeReturnMismatch(OperatorBuilder, Type),
    FunctionNotFound(String, usize),
    OperatorNotFound(String, usize),
    FunctionVariantNotFound(
        String,
        Vec<Type>,
        HashMap<Vec<Type>, (FunctionBuilder, usize)>,
    ),
    OperatorVariantNotFound(
        String,
        [Type; 2],
        HashMap<[Type; 2], (OperatorBuilder, usize)>,
    ),
    Internal,
}

#[derive(Debug)]
pub enum IdentSource {
    Module,
    FunctionDeclName,
    Ident,
}

impl ParseFault {
    pub fn to_err(self, i: usize) -> ParseError {
        ParseError::new(i, self)
    }
}

impl fmt::Display for ParseFault {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TODO: show error")
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let source = self.source_code.as_ref().unwrap();
        let (raw_line, arrow) = locate_line(source, self.source_index);
        let line = String::from_utf8(raw_line.to_vec()).unwrap();
        let module_path = self.module_name.as_ref().unwrap();

        write!(
            f,
            "{}: {}\n{}\n{}",
            module_path,
            self.variant,
            line.trim(),
            std::iter::repeat(' ')
                .take(if arrow == 0 { 0 } else { arrow - 1 })
                .collect::<String>()
                .add("^")
        )
    }
}

fn locate_line(source: &[u8], index: usize) -> (&[u8], usize) {
    let mut i = index;
    let start_i = loop {
        let c = source[i];
        if c == b'\n' || i == 0 {
            break i + 1;
        }
        i -= 1;
    };

    let mut i = index;
    let end_i = loop {
        let c = match source.get(i) {
            None => break i,
            Some(c) => *c,
        };
        if c == b'\n' {
            break i;
        }
        i += 1;
    };
    (&source[start_i..end_i], index - start_i)
}
