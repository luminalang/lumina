pub use super::FunctionBuilder;
use crate::parser::tokenizer::{is_valid_identifier, Key, Operator, RawToken, Token, Tokenizer};

#[derive(PartialEq, Debug)]
pub enum Mode {
    Neutral,
    // AfterPipe,
    AfterParen,
    Operator(Token, Operator),
    Parameters(Vec<Token>),
    List(Vec<Token>),
}

impl FunctionBuilder {
    pub fn walk_body(
        &mut self,
        mode: Mode,
        tokenizer: &mut Tokenizer,
    ) -> Result<Option<Token>, ()> {
        tokenizer.skip_spaces_and_newlines();
        let mut token = match tokenizer.next() {
            Some(t) => t,
            None => {
                return match mode {
                    Mode::List(_) | Mode::AfterParen => panic!("Ran out of tokens in {:?}", mode),
                    Mode::Parameters(v) => {
                        let source = v[0].source_index;
                        Ok(Some(Token::new(RawToken::Parameters(v), source)))
                    }
                    _ => Ok(None),
                }
            }
        };

        // eprintln!("{:?}\nMODE:{:?}", token, mode);

        match token.inner {
            RawToken::Header(h) => match mode {
                Mode::AfterParen => panic!("ET: Unmatched `)`"),
                Mode::List(_) => panic!("ET: Unmatched `]`"),
                Mode::Parameters(previous) => {
                    tokenizer.regress(h.as_str().len());
                    token.inner = RawToken::Parameters(previous);
                    Ok(Some(token))
                }
                Mode::Operator(_v, _op) => panic!("ET: No right side of operator"),
                Mode::Neutral => {
                    tokenizer.regress(h.as_str().len());
                    panic!("Missing body? I think? Let's find out!");
                }
            },
            RawToken::Key(Key::Pipe) => match mode {
                Mode::Neutral => panic!("ET: Pipe into void"),
                Mode::Parameters(mut previous) => {
                    let v = self
                        .walk_body(Mode::Neutral, tokenizer)?
                        .expect("Nothing after pipe");
                    previous.push(v);
                    let source = previous[0].source_index;
                    Ok(Some(Token::new(RawToken::Parameters(previous), source)))
                }
                Mode::Operator(left, op) => {
                    let v = self
                        .walk_body(Mode::Neutral, tokenizer)?
                        .expect("Nothing after pipe");
                    Ok(Some(Token::new(
                        RawToken::Operation(Box::new((left, v)), op),
                        token.source_index,
                    )))
                }
                Mode::AfterParen => unimplemented!(),
                Mode::List(_) => panic!("Can this happen?"),
            },
            RawToken::Key(Key::ParenOpen) => {
                let v = self
                    .walk_body(Mode::AfterParen, tokenizer)?
                    .expect("No value in ()");
                tokenizer.skip_spaces_and_newlines();
                let after = match tokenizer.next() {
                    Some(a) => a,
                    None => return Ok(Some(v)),
                };
                // TODO: I think it's always supposed to be ParenClose here. Could I double check
                // that?
                match mode {
                    Mode::Operator(left, op) => {
                        match after.inner {
                            RawToken::Key(Key::ParenClose) => {}
                            _ => panic!("Expected `)` got: {:?}", after),
                        }
                        let operation = {
                            let source = token.source_index;
                            Token::new(RawToken::Operation(Box::new((left, v)), op), source)
                        };
                        println!("Constructed operation {:?}", &operation);
                        // Ok(Some(operation))
                        tokenizer.skip_spaces_and_newlines();
                        let after = match tokenizer.next() {
                            Some(a) => a,
                            None => return Ok(Some(operation)),
                        };
                        match after.inner {
                            RawToken::Operator(op) => {
                                self.walk_body(Mode::Operator(operation, op), tokenizer)
                            }
                            RawToken::Key(k) => {
                                unimplemented!();
                                tokenizer.regress(k.as_str().len());
                                Ok(Some(operation))
                            }
                            _ => unimplemented!(),
                        }
                    }
                    Mode::AfterParen => match after.inner {
                        RawToken::Key(Key::ParenClose) => Ok(Some(v)),
                        _ => panic!("{:?}", after),
                    },
                    Mode::Neutral => match after.inner {
                        RawToken::Key(Key::ParenClose) => Ok(Some(v)),
                        _ => panic!("Expected `)`"),
                    },
                    Mode::Parameters(mut previous) => {
                        previous.push(v);
                        match after.inner {
                            /*
                            RawToken::Operator(op) => {
                                self.walk_body(Mode::Parameters(previous), tokenizer)
                            }
                            RawToken::Key(Key::ParenClose) => {
                                token.inner = RawToken::Parameters(previous);
                                Ok(Some(token))
                            }
                            */
                            RawToken::Key(Key::ParenClose) => {
                                self.walk_body(Mode::Parameters(previous), tokenizer)
                            }
                            _ => panic!("Expected `)`; got {:?}", after),
                        }
                    }
                    Mode::List(mut entries) => match after.inner {
                        RawToken::Key(Key::ParenClose) => {
                            tokenizer.skip_spaces_and_newlines();
                            let after = match tokenizer.next() {
                                Some(a) => a,
                                None => panic!("ET: Missing `]`"),
                            };
                            match after.inner {
                                RawToken::Key(Key::Comma) => {
                                    entries.push(v);
                                    self.walk_body(Mode::List(entries), tokenizer)
                                }
                                RawToken::Operator(op) => {
                                    let entry = self
                                        .walk_body(Mode::Operator(v, op), tokenizer)?
                                        .expect("No right side of operator");
                                    entries.push(entry);
                                    self.walk_body(Mode::List(entries), tokenizer)
                                }
                                RawToken::Key(Key::ListClose) => {
                                    // tokenizer.regress(1);
                                    entries.push(v);
                                    let source = entries[0].source_index;
                                    Ok(Some(Token::new(RawToken::List(entries), source)))
                                }
                                _ => panic!("ET: Unexpected {:?}", after),
                            }
                        }
                        _ => panic!("{:?}", after),
                    },
                }
            }
            RawToken::Key(Key::ListOpen) => {
                let list = self
                    .walk_body(Mode::List(Vec::new()), tokenizer)?
                    .expect("Could not get list");
                match mode {
                    Mode::Operator(left, op) => {
                        let operation = {
                            let source = token.source_index;
                            Token::new(RawToken::Operation(Box::new((left, list)), op), source)
                        };
                        Ok(Some(operation))
                    }
                    Mode::Neutral => {
                        tokenizer.skip_spaces_and_newlines();
                        let after = match tokenizer.next() {
                            Some(a) => a,
                            None => return Ok(Some(list)),
                        };
                        match after.inner {
                            RawToken::Operator(op) => {
                                self.walk_body(Mode::Operator(list, op), tokenizer)
                            }
                            _ => panic!("{:?}", after),
                        }
                    }
                    Mode::Parameters(mut previous) => {
                        previous.push(list);
                        self.walk_body(Mode::Parameters(previous), tokenizer)
                    }
                    Mode::List(_) => {
                        panic!("Is this possible? Let's find out!");
                    }
                    Mode::AfterParen => {
                        tokenizer.skip_spaces_and_newlines();
                        let after = match tokenizer.next() {
                            None => panic!("Expected `)`"),
                            Some(t) => t,
                        };

                        match after.inner {
                            RawToken::Operator(op) => {
                                self.walk_body(Mode::Operator(list, op), tokenizer)
                            }
                            RawToken::Key(Key::ParenClose) => {
                                tokenizer.regress(1);
                                Ok(Some(list))
                            }
                            _ => panic!("{:?}", after),
                        }
                    }
                }
            }
            RawToken::Inlined(v) => match mode {
                Mode::Neutral | Mode::AfterParen => {
                    let source = token.source_index;
                    let reconstruct = Token::new(RawToken::Inlined(v), source);

                    let after = tokenizer.next();
                    match after.map(|t| t.inner) {
                        Some(RawToken::Operator(op)) => {
                            let entity = self
                                .walk_body(Mode::Operator(reconstruct, op), tokenizer)?
                                .expect("ET: Expected value after operator");
                            Ok(Some(entity))
                        }
                        Some(RawToken::Key(Key::ParenClose)) => {
                            // TODO: This check is wrong if we're on Mode::Neutral
                            tokenizer.regress(1);
                            Ok(Some(reconstruct))
                        }
                        _ => unimplemented!(""),
                    }
                }
                Mode::List(mut previous) => {
                    let source = token.source_index;
                    let reconstruct = Token::new(RawToken::Inlined(v), source);

                    let after = tokenizer.next();
                    match after.map(|t| t.inner) {
                        Some(RawToken::Key(Key::Comma)) => {
                            previous.push(reconstruct);
                            self.walk_body(Mode::List(previous), tokenizer)
                        }
                        Some(RawToken::Operator(op)) => {
                            let entity = self
                                .walk_body(Mode::Operator(reconstruct, op), tokenizer)?
                                .expect("ET: Expected value after operator");
                            previous.push(entity);
                            self.walk_body(Mode::List(previous), tokenizer)
                        }
                        Some(RawToken::Key(Key::ListClose)) => {
                            previous.push(reconstruct);
                            let source = previous[0].source_index;
                            Ok(Some(Token::new(RawToken::List(previous), source)))
                        }
                        Some(other) => panic!("ET: Unexpected {:?}", other),
                        None => panic!("ET: Unmatched `]`"),
                    }
                }
                Mode::Operator(left, op) => {
                    let source = token.source_index;
                    let operation = Token::new(
                        RawToken::Operation(
                            Box::new((left, Token::new(RawToken::Inlined(v), source))),
                            op,
                        ),
                        source,
                    );
                    let after = tokenizer.next();
                    match after.map(|t| t.inner) {
                        Some(RawToken::Operator(op)) => {
                            let entity =
                                self.walk_body(Mode::Operator(operation, op), tokenizer)?;
                            Ok(entity)
                        }
                        Some(RawToken::Key(Key::ListClose)) => {
                            tokenizer.regress(1);
                            Ok(Some(operation))
                        }
                        Some(RawToken::Key(Key::ParenClose)) => {
                            tokenizer.regress(1);
                            Ok(Some(operation))
                        }
                        _ => Ok(Some(operation)),
                    }
                }
                Mode::Parameters(mut previous) => {
                    let source = token.source_index;
                    previous.push(Token::new(RawToken::Inlined(v), source));
                    self.walk_body(Mode::Parameters(previous), tokenizer)
                }
            },
            RawToken::Key(Key::ListClose) => match mode {
                Mode::List(entries) => {
                    let source = entries[0].source_index;
                    Ok(Some(Token::new(RawToken::List(entries), source)))
                }
                Mode::Parameters(previous) => {
                    let source = previous[0].source_index;
                    Ok(Some(Token::new(RawToken::Parameters(previous), source)))
                }
                _ => panic!("Unexpected lists close, mode was {:?}", mode),
            },
            RawToken::Identifier(ident) => {
                let v = match self.walk_body(Mode::Parameters(Vec::new()), tokenizer)? {
                    None => {
                        token.inner = RawToken::Constant(ident);
                        return Ok(Some(token));
                    }
                    Some(t) => t,
                };

                match v.inner {
                    RawToken::Parameters(params) => {
                        token.inner = RawToken::Parameterized(ident, params);
                        Ok(Some(token))
                    }
                    _ => panic!("{:?}", v),
                }
            }
            _ => panic!("ET: Unexpected {:?}; MODE:{:?}", token, mode),
        }
    }
}
