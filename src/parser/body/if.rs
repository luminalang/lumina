use super::{BodySource, Mode, ParseError, ParseFault, SimpleSource, WalkResult};
use crate::parser::tokenizer::{Key, RawToken, Token};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct IfExpr {
    pub branches: Vec<(Token, Token)>,
    pub else_branch: Box<Token>,
}

pub fn build<S: BodySource + ?Sized>(source: &mut S) -> Result<IfExpr, ParseError> {
    let mut expr = IfExpr::default();

    loop {
        let raw_cond = gather_cond(source)?;
        let condition = match SimpleSource::new(&raw_cond).walk(Mode::Neutral) {
            Ok(WalkResult::Value(v)) => v,
            Ok(was) => panic!("If Condition didn't result in an value, got: {:?}", was),
            Err(e) => return Err(e),
        };

        let (brk, raw_eval) = gather_eval(source);
        let evaluation = match SimpleSource::new(&raw_eval).walk(Mode::Neutral) {
            Ok(WalkResult::Value(v)) => v,
            Ok(was) => panic!("ET: ?, {:?}", was),
            Err(e) => return Err(e),
        };
        expr.branches.push((condition, evaluation));

        match brk {
            Else => {
                let (brk, raw_eval) = gather_eval(source);
                let else_evaluation = match SimpleSource::new(&raw_eval).walk(Mode::Neutral) {
                    Ok(WalkResult::Value(v)) => v,
                    Ok(was) => panic!("?, {:?}", was),
                    Err(e) => return Err(e),
                };
                expr.else_branch = Box::new(else_evaluation);

                match brk {
                    // I think the body walker will just catch these instead
                    // update: didn't catch the else branch, guess I'll handle them here
                    Else => return ParseFault::IfDoubleElse.to_err(0).into(),
                    Elif => panic!("Elif after else"),
                    EOF => return Ok(expr),
                }
            }
            Elif => (),
            EOF => return ParseFault::IfMissingElse.to_err(0).into(),
        }
    }
}

enum IfBreak {
    Elif,
    Else,
    EOF,
}
use IfBreak::*;

fn gather_cond<S: BodySource + ?Sized>(source: &mut S) -> Result<Vec<Token>, ParseError> {
    // If there's an entire if statement inside of this ones condition then we need a way to skip
    // past `then` keywords.
    let mut nested: usize = 0;

    let mut raw_tokens = Vec::new();
    loop {
        let next = source.next();
        match next {
            None => return ParseFault::IfMissingThen.to_err(0).into(),
            Some(t) => match t.inner {
                RawToken::Header(_) | RawToken::Key(Key::Where) => {
                    return ParseFault::IfMissingThen.to_err(0).into()
                }
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
                        return ParseFault::Unexpected(RawToken::Key(Key::Else))
                            .to_err(t.source_index)
                            .into();
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

fn gather_eval<S: BodySource + ?Sized>(source: &mut S) -> (IfBreak, Vec<Token>) {
    // If there's nested if statements then we need to permit skipping `elif`
    // and `else` keywords.
    let mut nested: usize = 0;

    let mut raw_tokens = Vec::new();
    loop {
        let next = source.next();
        match next {
            None => return (EOF, raw_tokens),
            Some(t) => match t.inner {
                RawToken::Header(_) | RawToken::Key(Key::Where) => {
                    source.undo();
                    return (EOF, raw_tokens);
                }
                RawToken::Key(Key::If) => {
                    nested += 1;
                    raw_tokens.push(t)
                }
                RawToken::Key(Key::Elif) => {
                    if nested == 0 {
                        return (Elif, raw_tokens);
                    } else {
                        raw_tokens.push(t);
                    }
                }
                RawToken::Key(Key::Else) => {
                    if nested == 0 {
                        return (Else, raw_tokens);
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
