use super::{BodySource, Mode, ParseError, ParseFault, WalkResult};
use crate::parser::tokenizer::{Key, RawToken, Token};

pub fn build<S: BodySource + ?Sized>(source: &mut S) -> Result<(String, Token), ParseError> {
    let t = match source.next() {
        None => {
            return ParseFault::EndedWhileExpecting(vec![RawToken::Identifier(
                "where statement identifier".into(),
            )])
            .to_err(0)
            .into()
        }
        Some(t) => t,
    };
    let identifier = match t.inner {
        RawToken::Identifier(ident) => ident,
        _ => {
            return ParseFault::GotButExpected(
                t.inner,
                vec![RawToken::Identifier("where statement identifier".into())],
            )
            .to_err(t.source_index)
            .into()
        }
    };
    let t = match source.next() {
        None => {
            return ParseFault::EndedWhileExpecting(vec![RawToken::Key(Key::Colon)])
                .to_err(0)
                .into()
        }
        Some(t) => t,
    };
    match t.inner {
        RawToken::Key(Key::Colon) => {}
        _ => {
            return ParseFault::GotButExpected(t.inner, vec![RawToken::Key(Key::Colon)])
                .to_err(t.source_index)
                .into()
        }
    };
    let v = source.walk(Mode::Neutral)?;
    match v {
        WalkResult::Value(t) => Ok((identifier, t)),
        _ => panic!("{:?}", v),
    }
}
