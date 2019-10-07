pub use super::FunctionBuilder;
pub use crate::parser::tokenizer::Token;
use crate::parser::tokenizer::{is_valid_identifier, Key, Operator, RawToken};

mod first;
mod list;
mod r#match;

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

pub trait BodySource {
    fn next(&mut self) -> Option<Token>;
    fn undo(&mut self);

    fn walk(&mut self, mode: Mode) -> Result<WalkResult, ()> {
        // tokenizer.skip_spaces_and_newlines();
        let token = match self.next() {
            Some(t) => {
                if t.inner == RawToken::NewLine {
                    return self.walk(mode);
                } else {
                    t
                }
            }
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
            RawToken::Header(_) => match mode {
                Mode::Operator(_v, _op) => panic!("ET: No right side of operator"),
                Mode::Parameters(previous) => {
                    self.undo();
                    let source = previous[0].source_index;
                    Ok(WalkResult::Value(Token::new(
                        RawToken::Parameters(previous),
                        source,
                    )))
                }
                Mode::Neutral => {
                    self.undo();
                    Ok(WalkResult::EOF)
                }
            },
            RawToken::Key(Key::Pipe) => match mode {
                Mode::Neutral => panic!("ET: Pipe into void"),
                Mode::Parameters(mut previous) => {
                    let v = self.walk(Mode::Neutral)?;
                    let source = match v {
                        WalkResult::Value(t) => {
                            previous.push(t);
                            previous[0].source_index
                        }
                        WalkResult::EOF => {
                            previous.get(0).map(|a| a.source_index).unwrap_or_else(|| 0)
                        }
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

                    match self.walk(Mode::Neutral)? {
                        WalkResult::Value(t) => {
                            let operation = operation(left, op, t);
                            self.handle_after(operation)
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
                    let v = self.walk(Mode::Neutral)?;
                    match v {
                        WalkResult::CloseParen(Some(v)) => self.handle_after(v),
                        WalkResult::CloseParen(None) => {
                            panic!("ET: Empty parenthesis are not allowed (for unit value use `_`)")
                        }
                        _ => panic!("ET: Wanted `)`, got {:?}", v),
                    }
                }
                Mode::Parameters(mut previous) => {
                    let v = self.walk(Mode::Neutral)?;
                    match v {
                        WalkResult::CloseParen(Some(v)) => {
                            previous.push(v);
                            self.walk(Mode::Parameters(previous))
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

                    match self.walk(Mode::Neutral)? {
                        WalkResult::Value(t) => {
                            let operation = operation(left, op, t);
                            self.handle_after(operation)
                        }
                        WalkResult::CloseParen(t) => {
                            let operation = operation(left, op, t.unwrap());
                            self.handle_after(operation)
                        }
                        WalkResult::EOF => panic!("No right side of operator"),
                    }
                }
            },
            RawToken::Inlined(v) => {
                let source = token.source_index;
                let reconstruct = Token::new(RawToken::Inlined(v), source);

                match mode {
                    Mode::Neutral => self.handle_after(reconstruct),
                    Mode::Parameters(mut previous) => {
                        previous.push(reconstruct);
                        self.walk(Mode::Parameters(previous))
                    }
                    Mode::Operator(left, op) => {
                        let source = left.source_index;
                        let operation = Token::new(
                            RawToken::Operation(Box::new((left, reconstruct)), op),
                            source,
                        );
                        self.handle_after(operation)
                    }
                }
            }
            RawToken::Identifier(ident) => {
                if !is_valid_identifier(&ident) {
                    panic!("ET: Invalid identifier");
                };
                match mode {
                    Mode::Neutral => {
                        let want_params = self.walk(Mode::Parameters(Vec::new()))?;
                        let source = token.source_index;
                        let make_parameterized =
                            |params| Token::new(RawToken::Parameterized(ident, params), source);
                        match want_params {
                            WalkResult::Value(v) => match v.inner {
                                RawToken::Parameters(params) => {
                                    self.handle_after(make_parameterized(params))
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
                        self.walk(Mode::Parameters(previous))
                    }
                    Mode::Operator(left, op) => {
                        self.undo();
                        let source = left.source_index;
                        let v = self.walk(Mode::Neutral)?;
                        match v {
                            WalkResult::Value(v) => match &v.inner {
                                RawToken::Parameterized(_n, _p) => {
                                    let operation = Token::new(
                                        RawToken::Operation(Box::new((left, v)), op),
                                        source,
                                    );
                                    // Ok(WalkResult::Value(operation))
                                    self.handle_after(operation)
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
            RawToken::Operator(_) => match mode {
                Mode::Parameters(previous) => {
                    self.undo();
                    let t = Token::new(RawToken::Parameters(previous), token.source_index);
                    Ok(WalkResult::Value(t))
                }
                _ => unimplemented!(), // possible?
            },
            _ => panic!("ET: Unexpected {:?}; MODE:{:?}", token, mode),
        }
    }

    fn handle_after(&mut self, v: Token) -> Result<WalkResult, ()> {
        let next = self.next();
        match next.map(|t| t.inner) {
            None => {
                self.undo();
                Ok(WalkResult::Value(v))
            }
            Some(RawToken::Operator(op)) => self.walk(Mode::Operator(v, op)),
            Some(RawToken::NewLine) => self.handle_after(v),
            Some(RawToken::Key(Key::ParenClose)) => Ok(WalkResult::CloseParen(Some(v))),
            _ => {
                self.undo();
                Ok(WalkResult::Value(v))
            }
        }
    }
}
