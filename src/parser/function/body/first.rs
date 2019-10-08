use super::{BodySource, Mode, SimpleSource, WalkResult};
use crate::parser::tokenizer::{Key, RawToken, Token};

pub fn build<S: BodySource + ?Sized>(source: &mut S) -> Result<Vec<Token>, ()> {
    let mut segments: Vec<Token> = Vec::new();
    let mut add_seg = |raw: Vec<Token>| -> Result<(), ()> {
        let segment = SimpleSource::new(&raw).walk(Mode::Neutral)?;
        match segment {
            WalkResult::Value(v) => segments.push(v),
            _ => unimplemented!(),
        }
        Ok(())
    };

    let mut on_last_branch = false;
    loop {
        let (brk, raw_tokens) = gather_segm(source)?;
        add_seg(raw_tokens)?;
        match brk {
            EOF => {
                if on_last_branch {
                    return Ok(segments);
                } else {
                    panic!("ET: Unexpected end, missing `then` part of `first` statement");
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

fn gather_segm<S: BodySource + ?Sized>(source: &mut S) -> Result<(FirstBreak, Vec<Token>), ()> {
    let mut raw_tokens = Vec::new();
    loop {
        let next = source.next();
        match next {
            None => {
                return Ok((EOF, raw_tokens));
            }
            Some(t) => match t.inner {
                RawToken::Key(Key::And) => {
                    return Ok((And, raw_tokens));
                }
                RawToken::Key(Key::Then) => {
                    return Ok((Then, raw_tokens));
                }
                RawToken::Header(_) => {
                    source.undo();
                    return Ok((EOF, raw_tokens));
                }
                _ => raw_tokens.push(t),
            },
        }
    }
}
