use super::{tokenizer::Operator, FileSource, FunctionBuilder, Key, Parser, RawToken, Type};
use std::convert::Into;
use std::fmt;
use std::io;
use std::ops::Add;
use std::path::PathBuf;
use termion::color;

#[derive(Debug)]
pub struct ParseError {
    pub source_index: usize,
    pub variant: ParseFault,
    pub source_code: Option<Vec<u8>>,
    pub module_name: Option<FileSource>,
    pub parser: Option<Parser>,
}

impl ParseError {
    pub fn new(i: usize, err: ParseFault) -> Self {
        Self {
            source_index: i,
            variant: err,
            source_code: None,
            module_name: None,
            parser: None,
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

    pub fn with_parser(mut self, parser: Parser) -> Self {
        self.parser = Some(parser);
        self
    }
}

impl<'a, T> Into<Result<T, ParseError>> for ParseError {
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
    OpTypeReturnMismatch(FunctionBuilder, Type),
    FunctionNotFound(String, usize),
    OperatorNotFound(String, usize),
    FunctionVariantNotFound(String, Vec<Type>, usize),
    OperatorVariantNotFound(String, [Type; 2], usize),
    Internal,
}

#[derive(Debug)]
pub enum IdentSource {
    Module,
    FunctionDeclName,
    Ident,
}

impl<'a> ParseFault {
    pub fn to_err(self, i: usize) -> ParseError {
        ParseError::new(i, self)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let parser = self
            .parser
            .as_ref()
            .expect("Parser not appended to error in final scope");
        let source = self
            .source_code
            .as_ref()
            .expect("Source code not appended to error in final scope");
        if self.source_index != 0 {
            let (raw_line, arrow, line_number) = locate_line(source, self.source_index);
            let line = String::from_utf8(raw_line.to_vec()).unwrap();
            let module_path = self
                .module_name
                .as_ref()
                .expect("Module name not appended to error in final scope");

            write!(
                f,
                "{g}leaf{n} {}{g}:{n}\n {y}{}{n} | {}\n{}\n",
                module_path
                    .to_pathbuf(&parser.environment)
                    .to_string_lossy(),
                line_number,
                line,
                std::iter::repeat(' ')
                    .take(arrow + 3 + line_number.to_string().len())
                    .collect::<String>()
                    .add(color::Red.fg_str())
                    .add("-^-")
                    .add(color::Reset.fg_str()),
                y = color::Yellow.fg_str(),
                g = color::Green.fg_str(),
                n = color::Reset.fg_str(),
            )?;
        }

        use ParseFault::*;
        match &self.variant {
            InvalidPath(entries) => write!(f, "`{}` is not a valid module path", entries.join(":")),
            BridgedWrongPathLen(entries) => write!(f, "`{}` wrong length of path", entries.join(":")),
            BridgedFunctionNotFound(ident, param_amount) => write!(f, "No bridged function named `{}` takes {} parameters", ident, param_amount),
            BridgedFunctionNoMode(c) => write!(f, "Bridged path mode doesn't exist, got `{}`", c),
            Unexpected(t) => write!(f, "Unexpected {}", t),
            Unmatched(k) => write!(f, "Unmatched {}", k),
            EndedWhileExpecting(expected) => match expected.len() {
                0 => panic!("None expected"),
                1 => write!(f, "I was expecting {} but the file ended here", expected[0]),
                2 => write!(
                    f,
                    "I was expecting {} or {} but the file ended here",
                    expected[0], expected[1]
                ),
                3 => write!(
                    f,
                    "I was expecting {} or {} or {} but the file ended here",
                    expected[0], expected[1], expected[2]
                ),
                _ => write!(
                    f,
                    "The file ended but I was expecting any of: \n{}",
                    expected
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join("\n  ")
                ),
            },
            GotButExpected(got, expected) => match expected.len() {
                0 => panic!("None expected"),
                1 => write!(f, "I was expecting {} but got {}", expected[0], got),
                2 => write!(
                    f,
                    "I was expecting {} or {} but got {}",
                    expected[0], expected[1], got
                ),
                _ => write!(
                    f,
                    "Got {} but I was expecting any of: \n{}",
                    got,
                    expected
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join("\n  ")
                ),
            },
            FunctionNotFound(ident, fid) => {
                let module = &parser.modules[*fid];
                write!(
                    f,
                    "Function `{}` not found in {} (or prelude)",
                    ident, module.module_path
                )
            }
            OperatorNotFound(ident, fid) => {
                let module = &parser.modules[*fid];
                write!(
                    f,
                    "Operator `{}` not found in {} (or prelude)",
                    ident, module.module_path
                )
            }
            FunctionVariantNotFound(ident, params, fid) => {
                let module = &parser.modules[*fid];
                let variants = &module.functions[ident];
                match variants.len() {
                    1 => {
                        let (wanted_params, (wfuncb, _)) = variants.iter().next().unwrap();
                        let mut mismatches = Vec::with_capacity(2);
                        for (i, param) in wanted_params.iter().enumerate() {
                            let got = match params.get(i) {
                                None => break, // TODO: What if `got` has less parmater then `wanted`?
                                Some(t) => t,
                            };
                            if param != got {
                                mismatches.push(i);
                            }
                        }
                        if mismatches.len() == 1 {
                            let i = mismatches[0];
                            write!(
                                f,
                                "Type mismatch. Wanted `{}` but got `{}`\n {}\n {}",
                                wanted_params[i],
                                params[i],
                                format_function_header(ident, Some(&params), None),
                                format_function_header(&wfuncb.borrow().name, Some(&wanted_params) ,None),
                            )
                        } else {
                            write!(f, "No function named `{}` takes these parameters\n  {}\n perhaps you meant to use?\n  {}",
                                ident,
                                format_function_header(ident, Some(&params), None),
                                format_function_header(&wfuncb.borrow().name, Some(&wanted_params), None),
                                )
                        }
                    }
                    _ => {
                        write!(f, "No function named `{}` takes these parameters\n  {}\n i did however find these variants\n  {}",
                            ident,
                            format_function_header(ident, Some(&params), None),
                            variants.values().map(|(fb, _)| format_function_header(&fb.borrow().name, Some(&fb.borrow().parameter_types), None)).collect::<Vec<String>>().join("\n  ")
                            )
                    },
                }
            }
            OperatorVariantNotFound(ident, params, fid) => {
                let module = &parser.modules[*fid];
                let variants = &module.functions[ident];
                match variants.len() {
                    1 => {
                        let (wanted_params, (wfuncb, _)) = variants.iter().next().unwrap();
                        let mut mismatches = Vec::with_capacity(2);
                        for (i, param) in wanted_params.iter().enumerate() {
                            let got = match params.get(i) {
                                None => break, // TODO: What if `got` has less parmater then `wanted`?
                                Some(t) => t,
                            };
                            if param != got {
                                mismatches.push(i);
                            }
                        }
                        if mismatches.len() == 1 {
                            let i = mismatches[0];
                            write!(
                                f,
                                "Type mismatch. Wanted `{}` but got `{}`\n {}\n {}",
                                wanted_params[i],
                                params[i],
                                format_operator_header(ident, &params[0], &params[1]),
                                format_operator_header(&wfuncb.borrow().name, &wanted_params[0], &wanted_params[1]),
                            )
                        } else {
                            write!(f, "No operator named `{}` takes these parameters\n  {}\n perhaps you meant to use?\n  {}",
                                ident,
                                    format_operator_header(ident, &params[0], &params[1]),
                                    format_operator_header(&wfuncb.borrow().name, &wanted_params[0], &wanted_params[1]),
                                )
                        }
                    }
                    _ => {
                        write!(f, "No operator named `{}` takes these parameters\n  {}\n i did however find these variants\n  {}",
                            ident,
                            format_operator_header(ident, &params[0], &params[1]),
                            variants.values().map(|(fb, _)| format_operator_header(&fb.borrow().name, &fb.borrow().parameter_types[0], &fb.borrow().parameter_types[1])).collect::<Vec<String>>().join("\n  ")
                            )
                    },
                }
            }
            ModuleLoadNotFound(entries) => write!(
                f,
                "Module `{}` not found in project folder or leafpath",
                entries.join(":")
            ),
            ModuleLoadFailed(path, err) => write!(
                f,
                "Found but unable to read {}: {:?}",
                path.to_string_lossy(),
                err,
            ),
            NotValidType(ident) => write!(f, "`{}` is not a valid type identifier", ident),
            MissingRightSideOperator(box (_left, op, right)) => write!(
                f,
                "Missing the right value for the operator `{}`, instead got this `{}`",
                op, right
            ),
            EndedMissingRightSideOperator(_left, op) => write!(f, "The function ended but I was still looking for the right side value for the operator `{}`", op),
            InvalidIdentifier(ident, identsource) => {
                write!(f, "`{}` is not a valid identifier", ident)?;
                match identsource {
                    IdentSource::Module => write!(f, "for a module name"),
                    IdentSource::FunctionDeclName => write!(f, "for a function"),
                    IdentSource::Ident => Ok(()),
                }
            },
            FirstStmNoThen => {
                write!(f, "This first statement doesn't have a `then` branch. I was looking for something ressembling\n first ...\n then  ...")
            },
            EmptyParen => write!(f, "Empty parenthesis aren't allowed. For unit value use the type `nothing` and value `_`"),
            IfMissingThen => write!(f, "This if expression doesn't have a `then` branch, I was looking for something ressembling\n if ...\n  then ...\n  else ..."),
            IfMissingElse => write!(f, "This if expression doesn't have an `else` branch, I was looking for something ressembling\n if ...\n  then ...\n  else ..."),
            IfDoubleElse => write!(f, "This if expression has two `else` branches, how would I know which one to use?"),
            ListMissingClose => write!(f, "This list open is missing a matching `]` to close it"),
            OpNoIdent => write!(f, "You need to provide an identifier for this operator"),
            OpWantedIdent(a) => write!(f, "Wanted identifier for the operator but got `{}`", a),
            InvalidParameterName(name) => write!(f, "`{}` is not a valid identifier for a parmater", name),
            PipeIntoVoid => write!(f, "This pipe doesn't lead to anywhere, perhaps you need to remove it?"),
            EmptyListType => write!(f, "I know that this is a list but you need to say what type the contents of the list will be\n such as [a] or [int]"),
            FnTypeReturnMismatch(funcb, got) => write!(f, "This function returns the wrong value. Acording to its type signature it should return `{}`\n  {}\nbut instead it returns `{}`",
                funcb.returns,
                format_function_header(&funcb.name, None, Some(&funcb.returns)),
                got,
            ),
            OpTypeReturnMismatch(opb, got) => write!(f, "This operator returns the wrong value. Acording to its type signature it should return `{}`\n  {}\nbut instead it returns `{}`",
                opb.returns,
                format_operator_header(&opb.name, &opb.parameter_types[0], &opb.parameter_types[1]),
                got,
            ),
            Internal => write!(f, "Internal leaf error"),
        }
    }
}

fn format_function_header(name: &str, params: Option<&[Type]>, returns: Option<&Type>) -> String {
    let mut buf = String::with_capacity(10);
    buf.push_str(color::Yellow.fg_str());
    buf.push_str("fn ");
    buf.push_str(color::Reset.fg_str());
    buf.push_str(name);
    buf.push_str(" (");
    if let Some(params) = params {
        buf.push_str(color::Yellow.fg_str());
        for param in params {
            buf.push_str(&param.to_string());
            buf.push(' ');
        }
        buf.push_str(color::Reset.fg_str());
        buf.push_str("-> ");
    }
    buf.push_str(color::Yellow.fg_str());
    match returns {
        Some(t) => buf.push_str(&t.to_string()),
        None => buf.push_str("..."),
    }
    buf.push_str(color::Reset.fg_str());
    buf.push(')');

    buf
}

// TODO: Replicate optional return/param like format_function_header

fn format_operator_header(opname: &str, left: &Type, right: &Type) -> String {
    format!(
        "{}operator{} {} ({}{} ... {}{})",
        color::Yellow.fg_str(),
        color::Reset.fg_str(),
        opname,
        color::Green.fg_str(),
        left,
        right,
        color::Reset.fg_str(),
    )
}

fn locate_line(source: &[u8], index: usize) -> (&[u8], usize, usize) {
    let mut line_number = 1;
    for (i, c) in source.iter().enumerate() {
        if i == index {
            break;
        }
        if *c == b'\n' {
            line_number += 1;
        }
    }

    // Pretty dirty hack but if the error is on the actual newline character itself we want to
    // technically search for the start of the previous line
    let mut i = if source[index] == b'\n' {
        index - 1
    } else {
        index
    };
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
    (&source[start_i..end_i], index - start_i, line_number)
}
