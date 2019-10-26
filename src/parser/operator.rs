use super::{
    body::BodySource, FunctionBuilder, Key, ParseError, ParseFault, RawToken, Tokenizer, Type,
};
use std::convert::TryFrom;

impl FunctionBuilder {
    pub fn with_header_operator(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ParseError> {
        let first = match tokenizer.next() {
            None => return ParseFault::OpNoIdent.to_err(0).into(),
            Some(t) => t,
        };
        match first.inner {
            RawToken::Operator(op) => self.name = op.identifier,
            _ => {
                return ParseFault::OpWantedIdent(first.inner)
                    .to_err(first.source_index)
                    .into()
            }
        };
        self.with_types_operator(tokenizer)
    }

    fn with_types_operator(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ParseError> {
        let t = match tokenizer.next() {
            None => {
                return ParseFault::EndedWhileExpecting(vec![RawToken::Key(Key::ParenOpen)])
                    .to_err(0)
                    .into()
            }
            Some(t) => t,
        };
        match t.inner {
            RawToken::Key(Key::ParenOpen) => {}
            _ => {
                return ParseFault::GotButExpected(t.inner, vec![RawToken::Key(Key::ParenOpen)])
                    .to_err(t.source_index)
                    .into()
            }
        }
        let mut next_ident = || -> Result<(Type, usize), ParseError> {
            let next = match tokenizer.next() {
                None => {
                    return ParseFault::EndedWhileExpecting(vec![RawToken::Identifier(
                        "type expected to the left side of the operator".into(),
                    )])
                    .to_err(0)
                    .into();
                }
                Some(t) => t,
            };
            let source_index = next.source_index;
            if let RawToken::Identifier(ident) = next.inner {
                Ok((
                    Type::try_from(ident.as_str()).map_err(|e| e.to_err(source_index))?,
                    next.source_index,
                ))
            } else {
                ParseFault::GotButExpected(
                    next.inner,
                    vec![RawToken::Identifier(
                        "type expected to the left side of the operator".into(),
                    )],
                )
                .to_err(next.source_index)
                .into()
            }
        };

        let (left, _) = next_ident()?;
        let (returns, _) = next_ident()?;
        let (right, right_source_index) = next_ident()?;
        let t = match tokenizer.next() {
            Some(t) => t,
            None => {
                return ParseFault::EndedWhileExpecting(vec![RawToken::Key(Key::ParenClose)])
                    .to_err(right_source_index)
                    .into()
            }
        };
        match t.inner {
            RawToken::Key(Key::ParenClose) => {}
            _ => {
                return ParseFault::GotButExpected(t.inner, vec![RawToken::Key(Key::ParenClose)])
                    .to_err(right_source_index)
                    .into()
            }
        }

        self.parameter_types = vec![left, right];
        self.parameter_names = vec!["left".into(), "right".into()];
        self.returns = returns;
        Ok(self)
    }
}
