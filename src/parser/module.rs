use super::function::FunctionBuilder;
use super::tokenizer::{token::Token, Tokenizer};
use crate::identifier::r#type::CustomType;
use std::fmt;

pub struct ParseModule {
    pub functions: Vec<FunctionBuilder>,
    pub types: Vec<CustomType>,
    pub tokenizer: Tokenizer,

    pub fid: usize,
}

impl ParseModule {
    pub fn with_fid(fid: usize, tokenizer: Tokenizer) -> Self {
        Self {
            fid,
            functions: Vec::new(),
            tokenizer,
            types: Vec::new(),
        }
    }
}

impl fmt::Debug for ParseModule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut functions = String::new();
        for func in self.functions.iter() {
            functions.push_str(&format!(
                "{:?}\n  {:#?}\n",
                func.header,
                func.tokens
                    .iter()
                    .map(|a| &a.inner)
                    .collect::<Vec<&Token>>()
            ));
        }
        let mut types = String::new();
        for typ in self.types.iter() {
            types.push_str(&format!("{:?}", typ));
        }
        write!(
            f,
            " - Module {}:\n  {}Functions:{}\n{}\n  {}Types:{}\n    {}",
            self.fid,
            termion::color::Fg(termion::color::Green),
            termion::color::Fg(termion::color::Reset),
            functions,
            termion::color::Fg(termion::color::Green),
            termion::color::Fg(termion::color::Reset),
            types
        )
    }
}
