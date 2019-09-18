// Final pass. Here we convert the parse tokens into entities and apply final optimizations.
// Everything's already type checked.

use crate::evaler::runner::{rustbridge, Entity};
use crate::parser::tokenizer::{token::Key, token::Token, Tracked};
use std::iter::Peekable;

struct Optimizer<S>
where
    S: Iterator<Item = Token>,
{
    pending: Peekable<S>,
    complete: Vec<Entity>,
}

pub fn from_tbuf(mut tbuf: Vec<Tracked<Token>>) -> Vec<Entity> {
    let mut optimizer = Optimizer {
        complete: Vec::with_capacity(tbuf.len()),
        pending: tbuf.drain(0..).map(|tt| tt.inner).peekable(),
    };

    loop {
        let r#continue = optimizer.walk();
        if !r#continue {
            break;
        }
    }
    optimizer.finalize()
}

impl<S: Iterator<Item = Token>> Optimizer<S> {
    fn finalize(mut self) -> Vec<Entity> {
        self.complete.shrink_to_fit();
        self.complete
    }

    fn walk(&mut self) -> bool {
        let t = match self.pending.next() {
            None => return false,
            Some(t) => t,
        };
        let entity = self.optimize(t);
        self.complete.push(entity);
        true
    }

    fn optimize(&mut self, t: Token) -> Entity {
        match t {
            Token::Function(fid, funcid, mut params) => {
                let mut param_buf = Vec::with_capacity(params);
                while params > 0 {
                    let next = self.pending.next().unwrap();
                    let param = self.optimize(next);
                    param_buf.push(param);
                    params -= 1;
                }
                Entity::Function(fid, funcid, param_buf)
            }
            Token::Recurse(mut params) => {
                let mut param_buf = Vec::with_capacity(params);
                while params > 0 {
                    let next = self.pending.next().unwrap();
                    let param = self.optimize(next);
                    param_buf.push(param);
                    params -= 1;
                }
                Entity::Recurse(param_buf)
            }
            // Token::Recurse()
            Token::K(Key::Operator(op)) => {
                let left = self.complete.pop().unwrap();
                let next = self.pending.next().unwrap();
                let right = self.optimize(next);
                Entity::Operation(Box::new((left, right)), op)
            }
            Token::BridgedFunction(funcid) => {
                let mut params = rustbridge::get_function_parameter_amount(funcid);
                let mut param_buf = Vec::with_capacity(params);
                while params > 0 {
                    let next = self.pending.next().unwrap();
                    let param = self.optimize(next);
                    param_buf.push(param);
                    params -= 1;
                }
                Entity::BridgedFunction(funcid, param_buf)
            }
            Token::Group(tokens) => Entity::Group(from_tbuf(tokens)),
            Token::Finished(entity) => entity, // TODO: Dive
            _ => panic!(
                "Unexpected token escaped into final optimization stage: {:?}",
                t
            ),
        }
    }
}
