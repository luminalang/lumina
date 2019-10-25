use super::{BodySource, Mode, ParseError, ParseFault, SimpleSource, WalkResult};
use crate::parser::tokenizer::{Key, RawToken, Token};

pub fn build<S: BodySource + ?Sized>(source: &mut S) -> Result<Vec<Token>, ParseError> {
    let mut entries: Vec<Token> = Vec::new();
    'list: loop {
        let mut raw_tokens: Vec<Token> = Vec::new();
        loop {
            let next = source.next();
            match next {
                None => return ParseFault::ListMissingClose.to_err(0).into(),
                Some(t) => match t.inner {
                    RawToken::Key(Key::Comma) => {
                        let entry = SimpleSource::new(&raw_tokens).walk(Mode::Neutral)?;
                        match entry {
                            WalkResult::Value(v) => entries.push(v),
                            _ => panic!("{:?}", entry),
                        }
                        continue 'list;
                    }
                    RawToken::Key(Key::ListClose) => {
                        let entry = SimpleSource::new(&raw_tokens).walk(Mode::Neutral)?;
                        match entry {
                            WalkResult::Value(v) => entries.push(v),
                            WalkResult::EOF => break 'list,
                            _ => panic!("{:?}", entry),
                        }
                        break 'list;
                    }
                    RawToken::Header(_) | RawToken::Key(Key::Where) => {
                        return ParseFault::ListMissingClose.to_err(0).into()
                    }
                    _ => raw_tokens.push(t),
                },
            }
        }
    }
    Ok(entries)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tokenizer::{Inlined, Tokenizer};

    #[test]
    fn list_parser_values() {
        let source_code: &[u8] = b"1, 2]";
        let mut entries = build(&mut Tokenizer::from(source_code)).unwrap();
        assert_eq!(
            entries
                .drain(0..)
                .map(|t| t.inner)
                .collect::<Vec<RawToken>>(),
            vec![
                RawToken::Inlined(Inlined::Int(1)),
                RawToken::Inlined(Inlined::Int(2))
            ]
        );
    }
}
