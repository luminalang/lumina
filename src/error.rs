use crate::parser::tokenizer::token::Token;
use std::fmt;
use std::path::PathBuf;

#[derive(Debug)]
pub enum Leaf {
    // Parser errors
    ExHeader(Token),
    ExFnName(Token),
    ExFnReturn(Token),
    ExParam(Token),
    ExParamType(Token),
    RustCallNonum,
    RustCallInvUse(String, String),
    UnexInFnHeader(char),
    TypeNotFound(String),
    FuncNotFound(String),
    ModuleNotFound(String),
    MalfFnHeader,
    IllegalCharacter(String),
    FileNotFound(PathBuf),
    FilePermissionDenied(PathBuf),
    FileUnknownError(PathBuf),

    // Generic errors
    Expected(Vec<Token>, Token),
    Unexpected(Token),

    // Evaler errors

    // Other
    Internal(String),
    Unimplemented(String),
}

impl fmt::Display for Leaf {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Leaf::ExHeader(tok) => write!( f, "Expected beginning of function, type or conversion. got: `{}`", tok),
            Leaf::ExFnName(tok) => write!( f, "Expected a function name, got: `{}`", tok),
            Leaf::ExFnReturn(tok) => write!( f, "Expected function return type, got `{}`", tok),
            Leaf::ExParam(tok) => write!(f, "Expected function parameters, newline or return type signature. got: `{}`", tok),
            Leaf::ExParamType(tok) => write!(f, "Exepcted function parameter type or `->`. got: `{}`", tok),
            Leaf::RustCallNonum => write!(f, "rust:call<n> expects a number"),
            Leaf::RustCallInvUse(a, b) => write!(f, "rust builtin invalid usage: {}:{}", a, b), 
            Leaf::UnexInFnHeader(tok) => write!( f, "Unexpected `{}` in function header", tok),
            Leaf::Unexpected(tok) => write!( f, "Unexpected `{}`", tok),
            Leaf::TypeNotFound(name) => write!( f, "Type not found: {}", name),
            Leaf::FuncNotFound(name) => write!(f, "Function not found: {}", name),
            Leaf::ModuleNotFound(name) => write!(f, "Module not found: {}", name),
            Leaf::MalfFnHeader => write!( f, "Malformed function header"),
            Leaf::FileNotFound(file) => write!(f, "File {} not found", file.file_name().unwrap().to_str().unwrap()),
            Leaf::FilePermissionDenied(file) => write!(f, "Could not open {:#?}: Permission denied", file.file_name().unwrap()),
            Leaf::FileUnknownError(file) => write!(f, "Could not open {:#?}", file.file_name().unwrap()),
            Leaf::IllegalCharacter(s) => write!(f, "Identifier `{}` contains an illegal character", s),
            Leaf::Internal(s) => write!(f, "leaf has suffered an internal error. If you have the time then please report the error to github.com/simvux/leaf\n  error: {}", s),
            Leaf::Unimplemented(s) => write!(f, "Unimplemented feature: {}", s),
            Leaf::Expected(one_of, got) => {
                let middle =  match one_of.len() {
                    1 => format!("`{}`", one_of[0].to_string()),
                    2 => format!("either `{}` or `{}`", one_of[0].to_string(), one_of[1].to_string()),
                    3 => format!("either `{}` or `{}` or `{}`", one_of[0].to_string(), one_of[1].to_string(), one_of[2].to_string()),
                    _ => format!("Expected any of \n  {}\n", one_of.iter().map(|a| a.to_string()).collect::<Vec<String>>().join("\n  ")),
                };
                write!(f, "Expected {} got `{}`", middle, got)
            }
        }
    }
}

pub trait Error {
    fn leaf(&self) -> &Leaf;
    fn header(&self) -> String;
    fn source(&self) -> Option<String>;
}

impl fmt::Display for dyn Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("{} {}", self.header(), self.leaf());
        match self.source() {
            None => {}
            Some(src) => {
                s.push('\n');
                s.push_str(&src)
            }
        }
        f.write_str(&s)
    }
}
