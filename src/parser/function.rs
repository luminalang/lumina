use super::{
    body, body::BodySource, Key, ParseError, ParseFault, RawToken, Token, Tokenizer, Type,
};
use std::convert::TryFrom;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

#[derive(Default, Clone)]
pub struct FunctionBuilder {
    pub name: String,
    pub parameter_names: Vec<String>,
    pub parameter_types: Vec<Type>,
    pub returns: Type,
    pub body: Token,
    pub wheres: Vec<(String, Token)>,
}

impl PartialEq for FunctionBuilder {
    fn eq(&self, other: &FunctionBuilder) -> bool {
        self.name == other.name
            && self.parameter_types == other.parameter_types
            && self.returns == other.returns
    }
}
impl Eq for FunctionBuilder {}
impl Hash for FunctionBuilder {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.parameter_types.hash(state);
        self.returns.hash(state);
    }
}

impl FunctionBuilder {
    pub fn new() -> Self {
        FunctionBuilder {
            name: String::new(),
            parameter_names: Vec::new(),
            parameter_types: Vec::new(),
            returns: Type::default(),
            body: Token::new(RawToken::NewLine, 0),
            wheres: Vec::new(),
        }
    }

    pub fn with_header(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ParseError> {
        let first = match tokenizer.next() {
            None => {
                return ParseFault::EndedWhileExpecting(vec![RawToken::Identifier(
                    "function name".into(),
                )])
                .to_err(0)
                .into()
            }
            Some(t) => t,
        };
        let name = match first.inner {
            RawToken::Identifier(name) => name,
            _ => {
                return ParseFault::GotButExpected(
                    first.inner,
                    vec![RawToken::Identifier("function name".into())],
                )
                .to_err(first.source_index)
                .into()
            }
        };
        self.name = name;

        let finished = match tokenizer.next().map(|t| (t.inner, t.source_index)) {
            // It has no parameters and returns Type::Nothing
            Some((RawToken::NewLine, _)) => self,
            // It has return value but no parameters
            Some((RawToken::Key(Key::ParenOpen), source)) => self
                .with_return(tokenizer)
                .map_err(|e| e.fallback(source))?,
            // It has parameters
            Some((RawToken::Identifier(ident), source)) => {
                tokenizer.regress(ident.len());
                self.with_parameter_names(tokenizer)
                    .map_err(|e| e.fallback(source))?
                    .with_parameter_types(tokenizer)
                    .map_err(|e| e.fallback(source))?
            }
            Some((RawToken::Operator(op), source)) => {
                return ParseFault::InvalidParameterName(op.identifier)
                    .to_err(source)
                    .into();
            }
            Some((rt, source)) => {
                return ParseFault::GotButExpected(
                    rt,
                    vec![
                        RawToken::Key(Key::ParenOpen),
                        RawToken::Identifier("parameter name".into()),
                    ],
                )
                .to_err(source)
                .into()
            }
            None => {
                return ParseFault::EndedWhileExpecting(vec![
                    RawToken::Key(Key::ParenOpen),
                    RawToken::Identifier("parameter name".into()),
                ])
                .to_err(0)
                .into()
            }
        };
        Ok(finished)
    }

    fn with_parameter_names(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ParseError> {
        loop {
            let next = match tokenizer.next() {
                Some(t) => t,
                None => {
                    return ParseFault::EndedWhileExpecting(vec![
                        RawToken::Key(Key::ParenOpen),
                        RawToken::Identifier("parameter name".into()),
                    ])
                    .to_err(tokenizer.index() - 1)
                    .into()
                }
            };
            match next.inner {
                RawToken::Identifier(name) => self.parameter_names.push(name),
                RawToken::Key(Key::ParenOpen) => return Ok(self),
                _ => {
                    return ParseFault::GotButExpected(
                        next.inner,
                        vec![
                            RawToken::Identifier("parameter name".into()),
                            RawToken::Key(Key::ParenOpen),
                        ],
                    )
                    .to_err(next.source_index)
                    .into()
                }
            }
        }
    }

    fn err_type_expecting(&self) -> Vec<RawToken> {
        if self.parameter_types.is_empty() {
            vec![RawToken::Identifier("parameter type".into())]
        } else {
            vec![
                RawToken::Identifier("parameter type".into()),
                RawToken::Key(Key::Arrow),
            ]
        }
    }

    fn with_parameter_types(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ParseError> {
        loop {
            let (next_inner, next_source_index) = match tokenizer.next() {
                Some(t) => (t.inner, t.source_index),
                None => {
                    return ParseFault::EndedWhileExpecting(self.err_type_expecting())
                        .to_err(tokenizer.index() - 1)
                        .into();
                }
            };
            match next_inner {
                RawToken::Identifier(name) => self
                    .parameter_types
                    .push(Type::try_from(name.as_str()).map_err(|e| e.to_err(next_source_index))?),
                RawToken::Key(Key::ListOpen) => self
                    .parameter_types
                    .push(Type::List(Box::new(self.parse_list_type(tokenizer)?))),
                RawToken::Key(Key::ParenClose) => return Ok(self),
                RawToken::Key(Key::ParenOpen) => self
                    .parameter_types
                    .push(Type::Function(Box::new(self.parse_param_type(tokenizer)?))),
                RawToken::Key(Key::Arrow) => return self.with_return(tokenizer),
                _ => {
                    return ParseFault::GotButExpected(next_inner, self.err_type_expecting())
                        .to_err(next_source_index)
                        .into()
                }
            }
        }
    }

    fn parse_list_type(&self, tokenizer: &mut Tokenizer) -> Result<Type, ParseError> {
        let (next, source_index) = match tokenizer.next() {
            None => {
                return ParseFault::Unmatched(Key::ParenOpen)
                    .to_err(tokenizer.index() - 1)
                    .into()
            }
            Some(t) => (t.inner, t.source_index),
        };
        let r#type = match next {
            RawToken::Key(Key::ListOpen) => Type::List(Box::new(self.parse_list_type(tokenizer)?)),
            RawToken::Identifier(name) => {
                Type::try_from(name.as_str()).map_err(|e| e.to_err(source_index))?
            }
            _ => {
                return ParseFault::Unmatched(Key::ParenOpen)
                    .to_err(source_index)
                    .into()
            }
        };
        if let Some(t) = tokenizer.next() {
            match t.inner {
                RawToken::Key(Key::ListClose) => Ok(r#type),
                _ => ParseFault::GotButExpected(t.inner, vec![RawToken::Key(Key::ListClose)])
                    .to_err(t.source_index)
                    .into(),
            }
        } else {
            ParseFault::Unmatched(Key::ListOpen)
                .to_err(source_index)
                .into()
        }
    }

    fn parse_param_type(&self, tokenizer: &mut Tokenizer) -> Result<(Vec<Type>, Type), ParseError> {
        let mut buf = Vec::new();
        loop {
            let (next_inner, next_source_index) = match tokenizer.next() {
                Some(t) => (t.inner, t.source_index),
                None => {
                    return ParseFault::EndedWhileExpecting(self.err_type_expecting())
                        .to_err(tokenizer.index() - 1)
                        .into();
                }
            };
            match next_inner {
                RawToken::Identifier(name) => buf
                    .push(Type::try_from(name.as_str()).map_err(|e| e.to_err(next_source_index))?),
                RawToken::Key(Key::ListOpen) => {
                    buf.push(Type::List(Box::new(self.parse_list_type(tokenizer)?)))
                }
                RawToken::Key(Key::ParenOpen) => {
                    buf.push(Type::Function(Box::new(self.parse_param_type(tokenizer)?)))
                }
                RawToken::Key(Key::Arrow) => {
                    let returns = (buf, self.parse_return_type(tokenizer)?);
                    return Ok(returns);
                }
                _ => {
                    return ParseFault::GotButExpected(next_inner, self.err_type_expecting())
                        .to_err(next_source_index)
                        .into()
                }
            }
        }
    }

    fn parse_return_type(&self, tokenizer: &mut Tokenizer) -> Result<Type, ParseError> {
        let (next_inner, next_source_index) = match tokenizer.next() {
            Some(t) => (t.inner, t.source_index),
            None => {
                return ParseFault::EndedWhileExpecting(vec![RawToken::Identifier(
                    "return type".into(),
                )])
                .to_err(tokenizer.index() - 1)
                .into()
            }
        };
        let r#type = match next_inner {
            RawToken::Identifier(name) => {
                Type::try_from(name.as_str()).map_err(|e| e.to_err(next_source_index))?
            }
            RawToken::Key(Key::ParenOpen) => {
                Type::Function(Box::new(self.parse_param_type(tokenizer)?))
            }
            RawToken::Key(Key::ListOpen) => Type::List(Box::new(self.parse_list_type(tokenizer)?)),
            _ => {
                return ParseFault::GotButExpected(
                    next_inner,
                    vec![RawToken::Identifier("return type".into())],
                )
                .to_err(next_source_index)
                .into()
            }
        };
        let after = match tokenizer.next() {
            None => {
                return ParseFault::EndedWhileExpecting(vec![RawToken::Key(Key::ParenClose)])
                    .to_err(next_source_index)
                    .into()
            }
            Some(t) => t,
        };
        match after.inner {
            RawToken::Key(Key::ParenClose) => Ok(r#type),
            _ => ParseFault::GotButExpected(after.inner, vec![RawToken::Key(Key::ParenClose)])
                .to_err(after.source_index)
                .into(),
        }
    }

    fn with_return(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ParseError> {
        self.returns = self.parse_return_type(tokenizer)?;
        Ok(self)
    }

    pub fn parse_body(&mut self, tokenizer: &mut Tokenizer) -> Result<Token, ParseError> {
        let entry = self.parse_body_tokens(tokenizer)?;
        self.parse_body_wheres(tokenizer)?;
        Ok(entry)
    }

    pub fn parse_body_tokens(&mut self, tokenizer: &mut Tokenizer) -> Result<Token, ParseError> {
        let entry = tokenizer.walk(body::Mode::Neutral)?;
        match entry {
            body::WalkResult::Value(v) => Ok(v),
            body::WalkResult::EOF => Ok(Token::new(RawToken::Unimplemented, 0)),
            _ => panic!("Body ended without value: {:?}", entry),
        }
    }

    pub fn parse_body_wheres(&mut self, tokenizer: &mut Tokenizer) -> Result<(), ParseError> {
        loop {
            let next = match tokenizer.next() {
                Some(t) => t,
                None => {
                    tokenizer.undo();
                    return Ok(());
                }
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
                            RawToken::Key(Key::Where),
                            RawToken::Header(super::Header::Function),
                        ],
                    )
                    .to_err(next.source_index)
                    .into()
                }
            }
        }
    }
    pub fn get_parameter(&self, ident: &str) -> Option<usize> {
        for (i, n) in self.parameter_names.iter().enumerate() {
            if n == ident {
                return Some(i);
            }
        }
        None
    }
    pub fn get_parameter_type(&self, pid: usize) -> &Type {
        &self.parameter_types[pid]
    }
    pub fn check_return(&self, got: &Type) -> Result<(), ParseFault> {
        if *got != self.returns && self.returns != Type::Nothing {
            return Err(ParseFault::FnTypeReturnMismatch(
                Box::new(self.clone()),
                got.clone(),
            ));
        }
        Ok(())
    }
}

impl fmt::Debug for FunctionBuilder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let types = self
            .parameter_types
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>();
        let annotation = if types.is_empty() {
            format!("{}", self.returns)
        } else {
            format!("{} -> {}", types.join(" "), self.returns)
        };
        let where_statements = self
            .wheres
            .iter()
            .map(|(name, entry)| format!("where {}: {:?}", name, entry))
            .collect::<Vec<String>>()
            .join("\n  ");
        write!(
            f,
            "fn {} {} ({})\n{:#?}{}",
            self.name,
            self.parameter_names.join(" "),
            annotation,
            self.body,
            where_statements,
        )
    }
}
