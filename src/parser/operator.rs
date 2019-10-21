use super::{
    body, body::BodySource, checker::Typeable, tokenizer::Operator, Header, Key, ParseError,
    ParseFault, RawToken, Token, Tokenizer, Type,
};
use std::convert::TryFrom;
use std::fmt;
use std::rc::Rc;

#[derive(Default)]
pub struct OperatorBuilder {
    pub name: Operator,
    pub parameter_types: [Type; 2],
    pub returns: Type,
    pub body: Rc<Token>,
    pub wheres: Vec<(String, Token)>,
}

impl OperatorBuilder {
    pub fn new() -> Self {
        OperatorBuilder {
            name: Operator::default(),
            parameter_types: <[Type; 2]>::default(),
            returns: Type::Nothing,
            body: Rc::new(Token::new(RawToken::NewLine, 0)),
            wheres: Vec::new(),
        }
    }

    pub fn with_header(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ParseError> {
        let first = match tokenizer.next() {
            None => return ParseFault::OpNoIdent.as_err(0).into(),
            Some(t) => t,
        };
        match first.inner {
            RawToken::Operator(op) => self.name = op,
            _ => {
                return ParseFault::OpWantedIdent(first.inner)
                    .as_err(first.source_index)
                    .into()
            }
        };
        self.with_types(tokenizer)
    }

    fn with_types(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ParseError> {
        let t = match tokenizer.next() {
            None => {
                return ParseFault::EndedWhileExpecting(vec![RawToken::Key(Key::ParenOpen)])
                    .as_err(0)
                    .into()
            }
            Some(t) => t,
        };
        match t.inner {
            RawToken::Key(Key::ParenOpen) => {}
            _ => {
                return ParseFault::GotButExpected(t.inner, vec![RawToken::Key(Key::ParenOpen)])
                    .as_err(t.source_index)
                    .into()
            }
        }
        let mut next_ident = || -> Result<(Type, usize), ParseError> {
            let next = match tokenizer.next() {
                None => {
                    return ParseFault::EndedWhileExpecting(vec![RawToken::Identifier(
                        "type expected to the left side of the operator".into(),
                    )])
                    .as_err(0)
                    .into();
                }
                Some(t) => t,
            };
            let source_index = next.source_index;
            if let RawToken::Identifier(ident) = next.inner {
                Ok((
                    Type::try_from(ident.as_str()).map_err(|e| e.as_err(source_index))?,
                    next.source_index,
                ))
            } else {
                ParseFault::GotButExpected(
                    next.inner,
                    vec![RawToken::Identifier(
                        "type expected to the left side of the operator".into(),
                    )],
                )
                .as_err(next.source_index)
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
                    .as_err(right_source_index)
                    .into()
            }
        };
        match t.inner {
            RawToken::Key(Key::ParenClose) => {}
            _ => {
                return ParseFault::GotButExpected(t.inner, vec![RawToken::Key(Key::ParenClose)])
                    .as_err(right_source_index)
                    .into()
            }
        }

        self.parameter_types = [left, right];
        self.returns = returns;
        Ok(self)
    }

    pub fn parse_body(&mut self, tokenizer: &mut Tokenizer) -> Result<Token, ParseError> {
        let entry = self.parse_body_tokens(tokenizer)?;
        self.parse_body_wheres(tokenizer)?;
        Ok(entry)
    }

    fn parse_body_tokens(&mut self, tokenizer: &mut Tokenizer) -> Result<Token, ParseError> {
        let entry = tokenizer.walk(body::Mode::Neutral)?;
        match entry {
            body::WalkResult::Value(v) => Ok(v),
            _ => panic!("{:?}", entry),
        }
    }

    fn parse_body_wheres(&mut self, tokenizer: &mut Tokenizer) -> Result<(), ParseError> {
        loop {
            let next = match tokenizer.next() {
                None => {
                    tokenizer.undo();
                    return Ok(());
                }
                Some(t) => t,
            };
            match next.inner {
                RawToken::Key(Key::Where) => {
                    let (name, t) = body::r#where::build(tokenizer)?;
                    self.wheres.push((name, t));
                }
                RawToken::Header(_) => {
                    tokenizer.undo();
                    return Ok(());
                }
                _ => {
                    return ParseFault::GotButExpected(
                        next.inner,
                        vec![
                            RawToken::Header(Header::Function),
                            RawToken::Key(Key::Where),
                        ],
                    )
                    .as_err(next.source_index)
                    .into()
                }
            }
        }
    }
}
impl Typeable for OperatorBuilder {
    fn get_parameter(&self, ident: &str) -> Option<usize> {
        match ident {
            "left" => Some(0),
            "right" => Some(1),
            _ => None,
        }
    }
    fn get_parameter_type(&self, pid: usize) -> &Type {
        &self.parameter_types[pid]
    }
    fn get_return(&self) -> &Type {
        &self.returns
    }
    fn entry_point(&self) -> Rc<Token> {
        self.body.clone()
    }
}

impl fmt::Debug for OperatorBuilder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let where_statements = self
            .wheres
            .iter()
            .map(|(name, entry)| format!("where {}: {:?}", name, entry))
            .collect::<Vec<String>>()
            .join("\n  ");
        write!(
            f,
            "fn {} ({} {} {})\n{:#?}{}",
            self.name.identifier,
            self.parameter_types[0],
            self.returns,
            self.parameter_types[1],
            self.body,
            where_statements,
        )
    }
}
