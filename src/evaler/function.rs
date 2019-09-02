use super::runner::tokenbuffer::TokenBuffer;
use crate::parser::function::FunctionBuilder;
use crate::parser::tokenizer::token::Token;
use std::sync::Arc;

#[derive(Debug)]
pub struct Function {
    pub tokens: Arc<Vec<Token>>,
    pub wheres: Arc<Vec<Vec<Token>>>,
    pub params: u8,

    r_ip: usize,
}

impl From<FunctionBuilder<'_>> for Function {
    fn from(mut func: FunctionBuilder) -> Self {
        Self {
            tokens: Arc::new(func.tokens.drain(0..).map(|tt| tt.untrack_t()).collect()),
            wheres: Arc::new(
                // Removes source tracking
                func.wheres
                    .drain(0..)
                    .map(|mut vectt| vectt.drain(0..).map(|tt| tt.untrack_t()).collect())
                    .collect(),
            ),
            params: func.header.parameters.len() as u8,
            r_ip: std::usize::MAX,
        }
    }
}

impl Clone for Function {
    fn clone(&self) -> Self {
        Self {
            tokens: self.tokens.clone(),
            wheres: self.wheres.clone(),
            params: self.params,
            r_ip: std::usize::MAX,
        }
    }
}

impl TokenBuffer for &mut Function {
    #[inline]
    fn next_token(&mut self) -> Option<&Token> {
        if self.r_ip == std::usize::MAX {
            self.r_ip = 0
        } else {
            self.r_ip += 1;
        }
        self.tokens.get(self.r_ip)
    }
    fn reset(&mut self) {
        self.r_ip = std::usize::MAX;
    }
}

impl Function {
    pub fn dump_tokens(&self) {
        println!("Function with {:?} params", self.params);
        for t in self.tokens.iter() {
            println!("{:?}", t);
        }
        if !self.wheres.is_empty() {
            println!("  where");
            for (i, ts) in self.wheres.iter().enumerate() {
                print!("    {} = ", i);
                for t in ts {
                    print!(" {:?} ", t);
                }
                println!();
            }
        }
    }

    pub fn new(params: u8, tokens: Vec<Token>, wheres: Vec<Vec<Token>>) -> Self {
        Self {
            wheres: Arc::new(wheres),
            tokens: Arc::new(tokens),
            params,

            r_ip: std::usize::MAX,
        }
    }
}
