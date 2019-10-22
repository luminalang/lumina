use super::{BodySource, Mode, ParseError, ParseFault, SimpleSource, WalkResult};
use crate::parser::tokenizer::{Key, RawToken, Token};

pub fn build<S: BodySource + ?Sized>(source: &mut S) -> Result<Vec<Token>, ParseError> {
    let mut segments: Vec<Token> = Vec::new();
    let mut add_seg = |raw: Vec<Token>| -> Result<(), ParseError> {
        let segment = SimpleSource::new(&raw).walk(Mode::Neutral)?;
        match segment {
            WalkResult::Value(v) => segments.push(v),
            _ => panic!("Segment didn't result in value"),
        }
        Ok(())
    };

    let mut on_last_branch = false;
    loop {
        let (brk, raw_tokens) = gather_segm(source);
        add_seg(raw_tokens)?;
        match brk {
            EOF => {
                if on_last_branch {
                    return Ok(segments);
                } else {
                    return Err(
                        ParseFault::FirstStmNoThen.to_err(segments.last().unwrap().source_index)
                    );
                }
            }
            And => {
                continue;
            }
            Then => {
                on_last_branch = true;
                continue;
            }
        }
    }
}

enum FirstBreak {
    And,
    Then,
    EOF,
}
use FirstBreak::*;

fn gather_segm<S: BodySource + ?Sized>(source: &mut S) -> (FirstBreak, Vec<Token>) {
    let mut raw_tokens = Vec::new();
    loop {
        let next = source.next();
        match next {
            None => {
                return (EOF, raw_tokens);
            }
            Some(t) => match t.inner {
                RawToken::Key(Key::And) => {
                    return (And, raw_tokens);
                }
                RawToken::Key(Key::Then) => {
                    return (Then, raw_tokens);
                }
                RawToken::Header(_) | RawToken::Key(Key::Where) => {
                    source.undo();
                    return (EOF, raw_tokens);
                }
                _ => raw_tokens.push(t),
            },
        }
    }
}
