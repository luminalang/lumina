pub use super::FunctionBuilder;
use crate::parser::tokenizer::{is_valid_identifier, Key, Operator, RawToken, Token, Tokenizer};

#[derive(PartialEq, Debug)]
pub enum Mode {
    Neutral,
    Operator(Token, Operator),
    Parameters(Vec<Token>),
}

#[derive(Debug)]
pub enum WalkResult {
    Value(Token),
    CloseParen(Option<Token>),
    EOF,
}

impl FunctionBuilder {
    pub fn walk_body(&mut self, mode: Mode, tokenizer: &mut Tokenizer) -> Result<WalkResult, ()> {
        tokenizer.skip_spaces_and_newlines();
        let token = match tokenizer.next() {
            Some(t) => t,
            None => {
                return match mode {
                    Mode::Parameters(v) => {
                        let source = v[0].source_index;
                        Ok(WalkResult::Value(Token::new(
                            RawToken::Parameters(v),
                            source,
                        )))
                    }
                    _ => Ok(WalkResult::EOF),
                }
            }
        };

        match token.inner {
            RawToken::Header(h) => match mode {
                Mode::Operator(_v, _op) => panic!("ET: No right side of operator"),
                Mode::Parameters(previous) => {
                    tokenizer.regress(h.as_str().len());
                    let source = previous[0].source_index;
                    Ok(WalkResult::Value(Token::new(
                        RawToken::Parameters(previous),
                        source,
                    )))
                }
                Mode::Neutral => {
                    tokenizer.regress(h.as_str().len());
                    Ok(WalkResult::EOF)
                }
            },
            RawToken::Key(Key::Pipe) => match mode {
                Mode::Neutral => panic!("ET: Pipe into void"),
                Mode::Parameters(mut previous) => {
                    let v = self.walk_body(Mode::Neutral, tokenizer)?;
                    let source = match v {
                        WalkResult::Value(t) => {
                            previous.push(t);
                            previous[0].source_index
                        }
                        WalkResult::EOF => previous
                            .get(0)
                            .map(|a| a.source_index)
                            .unwrap_or_else(|| tokenizer.index()),
                        WalkResult::CloseParen(t) => {
                            panic!("ET: Unexpected `]` with last being {:?}", t)
                        }
                    };
                    Ok(WalkResult::Value(Token::new(
                        RawToken::Parameters(previous),
                        source,
                    )))
                }
                Mode::Operator(left, op) => {
                    let operation = |left: Token, op, right| {
                        let source = left.source_index;
                        Token::new(RawToken::Operation(Box::new((left, right)), op), source)
                    };

                    match self.walk_body(Mode::Neutral, tokenizer)? {
                        WalkResult::Value(t) => {
                            let operation = operation(left, op, t);
                            self.handle_after(operation, tokenizer)
                        }
                        WalkResult::CloseParen(t) => {
                            // panic!("ET: Unexpected `)` with last being {:?}", t)
                            let t = t.expect("No right side of operator");
                            let operation = operation(left, op, t);
                            Ok(WalkResult::CloseParen(Some(operation)))
                        }
                        WalkResult::EOF => panic!("ET: No right side of operator"),
                    }
                }
            },
            RawToken::Key(Key::ParenOpen) => match mode {
                Mode::Neutral => {
                    let v = self.walk_body(Mode::Neutral, tokenizer)?;
                    match v {
                        WalkResult::CloseParen(Some(v)) => self.handle_after(v, tokenizer),
                        WalkResult::CloseParen(None) => {
                            panic!("ET: Empty parenthesis are not allowed (for unit value use `_`)")
                        }
                        _ => panic!("ET: Wanted `)`, got {:?}", v),
                    }
                }
                Mode::Parameters(mut previous) => {
                    let v = self.walk_body(Mode::Neutral, tokenizer)?;
                    match v {
                        WalkResult::CloseParen(Some(v)) => {
                            previous.push(v);
                            self.walk_body(Mode::Parameters(previous), tokenizer)
                        }
                        WalkResult::CloseParen(None) => {
                            panic!("ET: Empty parenthesis are not allowed (for unit value use `_`)")
                        }
                        _ => panic!("ET: Wanted `)`, got {:?}", v),
                    }
                }
                Mode::Operator(left, op) => {
                    let operation = |left: Token, op, right| {
                        let source = left.source_index;
                        Token::new(RawToken::Operation(Box::new((left, right)), op), source)
                    };

                    match self.walk_body(Mode::Neutral, tokenizer)? {
                        WalkResult::Value(t) => {
                            let operation = operation(left, op, t);
                            self.handle_after(operation, tokenizer)
                        }
                        WalkResult::CloseParen(t) => {
                            let operation = operation(left, op, t.unwrap());
                            self.handle_after(operation, tokenizer)
                        }
                        WalkResult::EOF => panic!("No right side of operator"),
                    }
                }
            },
            RawToken::Inlined(v) => {
                let source = token.source_index;
                let reconstruct = Token::new(RawToken::Inlined(v), source);

                match mode {
                    Mode::Neutral => self.handle_after(reconstruct, tokenizer),
                    Mode::Parameters(mut previous) => {
                        previous.push(reconstruct);
                        self.walk_body(Mode::Parameters(previous), tokenizer)
                    }
                    Mode::Operator(left, op) => {
                        let source = left.source_index;
                        let operation = Token::new(
                            RawToken::Operation(Box::new((left, reconstruct)), op),
                            source,
                        );
                        self.handle_after(operation, tokenizer)
                    }
                }
            }
            RawToken::Identifier(ident) => {
                if !is_valid_identifier(&ident) {
                    panic!("ET: Invalid identifier");
                };
                match mode {
                    Mode::Neutral => {
                        let want_params =
                            self.walk_body(Mode::Parameters(Vec::new()), tokenizer)?;
                        let source = token.source_index;
                        let make_parameterized =
                            |params| Token::new(RawToken::Parameterized(ident, params), source);
                        match want_params {
                            WalkResult::Value(v) => match v.inner {
                                RawToken::Parameters(params) => {
                                    self.handle_after(make_parameterized(params), tokenizer)
                                }
                                _ => panic!("Wanted parameters but got {:?}", v),
                            },
                            WalkResult::CloseParen(v) => match v.unwrap().inner {
                                RawToken::Parameters(params) => {
                                    Ok(WalkResult::CloseParen(Some(make_parameterized(params))))
                                }
                                _ => panic!("Wanted parameters but got {:?}", ()),
                            },
                            _ => panic!("Wanted parameters but got {:?}", want_params),
                        }
                    }
                    Mode::Parameters(mut previous) => {
                        let source = token.source_index;
                        let this = Token::new(RawToken::Constant(ident), source);
                        previous.push(this);
                        self.walk_body(Mode::Parameters(previous), tokenizer)
                    }
                    Mode::Operator(left, op) => {
                        tokenizer.regress(ident.as_str().len());
                        let source = left.source_index;
                        let v = self.walk_body(Mode::Neutral, tokenizer)?;
                        match v {
                            WalkResult::Value(v) => match &v.inner {
                                RawToken::Parameterized(_n, _p) => {
                                    let operation = Token::new(
                                        RawToken::Operation(Box::new((left, v)), op),
                                        source,
                                    );
                                    // Ok(WalkResult::Value(operation))
                                    self.handle_after(operation, tokenizer)
                                }
                                _ => panic!(),
                            },
                            _ => panic!("{:?}", v),
                        }
                    }
                }
            }
            RawToken::Key(Key::ParenClose) => match mode {
                Mode::Parameters(previous) => {
                    let t = Token::new(RawToken::Parameters(previous), token.source_index);
                    Ok(WalkResult::CloseParen(Some(t)))
                }
                Mode::Operator(_left, _op) => {
                    panic!("Unexpected `)`, missing right side of operator")
                }
                Mode::Neutral => Ok(WalkResult::CloseParen(None)),
            },
            RawToken::Operator(op) => match mode {
                Mode::Parameters(previous) => {
                    tokenizer.regress(op.identifier.len());
                    let t = Token::new(RawToken::Parameters(previous), token.source_index);
                    Ok(WalkResult::Value(t))
                }
                _ => unimplemented!(), // possible?
            },
            _ => panic!("ET: Unexpected {:?}; MODE:{:?}", token, mode),
        }
    }

    fn handle_after(&mut self, v: Token, tokenizer: &mut Tokenizer) -> Result<WalkResult, ()> {
        let next = tokenizer.next();
        match next.map(|t| t.inner) {
            None => {
                tokenizer.undo();
                Ok(WalkResult::Value(v))
            }
            Some(RawToken::Operator(op)) => self.walk_body(Mode::Operator(v, op), tokenizer),
            Some(RawToken::Key(Key::ParenClose)) => Ok(WalkResult::CloseParen(Some(v))),
            _ => {
                tokenizer.undo();
                Ok(WalkResult::Value(v))
            }
        }
    }
}
