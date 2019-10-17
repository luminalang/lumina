use super::{BodySource, Mode, WalkResult};
use crate::parser::tokenizer::{Key, RawToken, Token};

pub fn build<S: BodySource + ?Sized>(source: &mut S) -> Result<(String, Token), ()> {
    let identifier = match source.next().map(|t| t.inner) {
        Some(RawToken::Identifier(ident)) => ident,
        Some(other) => panic!("Unexpected {:?}, wanted identifier", other),
        None => panic!("ET: Wanted where statement identifier"),
    };
    match source.next().map(|t| t.inner) {
        Some(RawToken::Key(Key::Colon)) => {}
        Some(other) => panic!("Unexpected {:?}, wanted identifier", other),
        None => panic!("ET: Wanted where statement identifier"),
    };
    let v = source.walk(Mode::Neutral)?;
    match v {
        WalkResult::Value(t) => Ok((identifier, t)),
        _ => panic!("{:?}", v),
    }
}
