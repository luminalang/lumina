use super::{BodySource, Mode, SimpleSource, WalkResult};
use crate::parser::tokenizer::{Key, RawToken, Token};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct IfExpr {
    pub branches: Vec<(Token, Token)>,
    pub else_branch: Box<Token>,
}

pub fn build<S: BodySource + ?Sized>(source: &mut S) -> Result<IfExpr, ()> {
    let mut expr = IfExpr::default();

    loop {
        let raw_cond = gather_cond(source)?;
        let condition = match SimpleSource::new(&raw_cond).walk(Mode::Neutral) {
            Ok(WalkResult::Value(v)) => v,
            Ok(was) => panic!("ET: ?, {:?}", was),
            Err(e) => return Err(e),
        };

        let (brk, raw_eval) = gather_eval(source)?;
        let evaluation = match SimpleSource::new(&raw_eval).walk(Mode::Neutral) {
            Ok(WalkResult::Value(v)) => v,
            Ok(was) => panic!("ET: ?, {:?}", was),
            Err(e) => return Err(e),
        };
        expr.branches.push((condition, evaluation));

        match brk {
            Else => {
                let (brk, raw_eval) = gather_eval(source)?;
                let else_evaluation = match SimpleSource::new(&raw_eval).walk(Mode::Neutral) {
                    Ok(WalkResult::Value(v)) => v,
                    Ok(was) => panic!("ET: ?, {:?}", was),
                    Err(e) => return Err(e),
                };
                expr.else_branch = Box::new(else_evaluation);

                match brk {
                    Else => panic!("ET: Multiple `else` in a row"),
                    Elif => panic!("ET: Elif after `else`"),
                    EOF => return Ok(expr),
                }
            }
            Elif => (),
            EOF => panic!("ET: Missing `else` (i think?)"),
        }
    }
}

enum IfBreak {
    Elif,
    Else,
    EOF,
}
use IfBreak::*;

fn gather_cond<S: BodySource + ?Sized>(source: &mut S) -> Result<Vec<Token>, ()> {
    // If there's an entire if statement inside of this ones condition then we need a way to skip
    // past `then` keywords.
    let mut nested: usize = 0;

    let mut raw_tokens = Vec::new();
    loop {
        let next = source.next();
        match next {
            None => panic!("ET: missing `then`"),
            Some(t) => match t.inner {
                RawToken::Header(_) | RawToken::Key(Key::Where) => panic!("ET: missing `then`"),
                RawToken::NewLine => {}
                RawToken::Key(Key::If) => {
                    nested += 1;
                    raw_tokens.push(t);
                }
                RawToken::Key(Key::Then) => {
                    if nested == 0 {
                        return Ok(raw_tokens);
                    } else {
                        raw_tokens.push(t);
                    }
                }
                RawToken::Key(Key::Else) => {
                    if nested == 0 {
                        panic!("Unexpected `else`");
                    } else {
                        nested -= 1;
                        raw_tokens.push(t);
                    }
                }
                _ => raw_tokens.push(t),
            },
        }
    }
}

fn gather_eval<S: BodySource + ?Sized>(source: &mut S) -> Result<(IfBreak, Vec<Token>), ()> {
    // If there's nested if statements then we need to permit skipping `elif`
    // and `else` keywords.
    let mut nested: usize = 0;

    let mut raw_tokens = Vec::new();
    loop {
        let next = source.next();
        match next {
            None => return Ok((EOF, raw_tokens)),
            Some(t) => match t.inner {
                RawToken::Header(_) | RawToken::Key(Key::Where) => {
                    source.undo();
                    return Ok((EOF, raw_tokens));
                }
                RawToken::Key(Key::If) => {
                    nested += 1;
                    raw_tokens.push(t)
                }
                RawToken::Key(Key::Elif) => {
                    if nested == 0 {
                        return Ok((Elif, raw_tokens));
                    } else {
                        raw_tokens.push(t);
                    }
                }
                RawToken::Key(Key::Else) => {
                    if nested == 0 {
                        return Ok((Else, raw_tokens));
                    } else {
                        nested -= 1;
                        raw_tokens.push(t);
                    }
                }
                RawToken::NewLine => {}
                _ => raw_tokens.push(t),
            },
        }
    }
}
