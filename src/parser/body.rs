pub use super::FunctionBuilder;
use crate::evaler::bridge;
use crate::parser::tokenizer::{is_valid_identifier, Key, Operator, RawToken, Token};

mod first;
mod r#if;
pub use r#if::IfExpr;
mod list;
mod r#match;
pub use r#match::MatchExpr;
pub mod r#where;

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

pub struct SimpleSource<'a> {
    buf: &'a [Token],
    index: usize,
}

impl<'a> BodySource for SimpleSource<'a> {
    fn next(&mut self) -> Option<Token> {
        let v = self.buf.get(self.index).cloned();
        self.index += 1;
        v
    }
    fn undo(&mut self) {
        self.index -= 1;
    }
}

impl<'a> SimpleSource<'a> {
    pub fn new(buf: &'a [Token]) -> Self {
        Self { index: 0, buf }
    }
}

pub trait BodySource {
    fn next(&mut self) -> Option<Token>;
    fn undo(&mut self);

    fn walk(&mut self, mode: Mode) -> Result<WalkResult, ()> {
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
                        Ok(WalkResult::Value(Token::new(RawToken::Parameters(v), 0)))
                    }
                    _ => Ok(WalkResult::EOF),
                }
            }
        };

        match token.inner {
            RawToken::Header(_) | RawToken::Key(Key::Where) => match mode {
                Mode::Operator(_v, _op) => panic!("ET: No right side of operator"),
                Mode::Parameters(previous) => {
                    self.undo();
                    Ok(WalkResult::Value(Token::new(
                        RawToken::Parameters(previous),
                        0,
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
                    panic!("ET: Invalid identifier {:?}", &ident);
                };
                self.handle_ident(
                    mode,
                    Token::new(RawToken::Identifier(ident), token.source_index),
                )
            }
            RawToken::ExternalIdentifier(entries) => {
                if !is_valid_identifier(&entries[0]) {
                    panic!("ET: Invalid module identifier {:?}", &entries[0]);
                };
                if !is_valid_identifier(&entries[1]) {
                    panic!("ET: Invalid func identifier {:?}", &entries[1]);
                };
                let t =
                    if let Some((bridged_id, bridged_type)) = bridge::try_rust_builtin(&entries)? {
                        Token::new(
                            RawToken::RustCall(bridged_id, bridged_type),
                            token.source_index,
                        )
                    } else {
                        if entries.len() != 2 {
                            panic!("ET: a:b:c is not allowed");
                        }
                        Token::new(RawToken::ExternalIdentifier(entries), token.source_index)
                    };
                self.handle_ident(mode, t)
            }
            RawToken::Key(Key::ListOpen) => {
                let list = list::build(self)?;
                let v = Token::new(RawToken::List(list), token.source_index);
                match mode {
                    Mode::Neutral => self.handle_after(v),
                    Mode::Parameters(mut previous) => {
                        previous.push(v);
                        self.walk(Mode::Parameters(previous))
                    }
                    Mode::Operator(left, op) => {
                        let operation = Token::new(
                            RawToken::Operation(Box::new((left, v)), op),
                            token.source_index,
                        );
                        self.handle_after(operation)
                    }
                }
            }
            RawToken::Key(Key::First) => {
                let firststm = first::build(self)?;
                let v = Token::new(RawToken::FirstStatement(firststm), token.source_index);
                match mode {
                    Mode::Neutral => self.handle_after(v),
                    Mode::Parameters(mut previous) => {
                        previous.push(v);
                        self.walk(Mode::Parameters(previous))
                    }
                    Mode::Operator(left, op) => {
                        let operation = Token::new(
                            RawToken::Operation(Box::new((left, v)), op),
                            token.source_index,
                        );
                        self.handle_after(operation)
                    }
                }
            }
            RawToken::Key(Key::If) => {
                let ifexpr = r#if::build(self)?;
                let v = Token::new(RawToken::IfExpression(ifexpr), token.source_index);
                match mode {
                    Mode::Neutral => self.handle_after(v),
                    Mode::Parameters(mut previous) => {
                        previous.push(v);
                        self.walk(Mode::Parameters(previous))
                    }
                    Mode::Operator(left, op) => {
                        let operation = Token::new(
                            RawToken::Operation(Box::new((left, v)), op),
                            token.source_index,
                        );
                        self.handle_after(operation)
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

    fn handle_ident(&mut self, mode: Mode, token: Token) -> Result<WalkResult, ()> {
        match mode {
            Mode::Neutral => {
                let want_params = self.walk(Mode::Parameters(Vec::new()))?;
                let source = token.source_index;
                let make_parameterized = |params: Vec<Token>| {
                    if params.is_empty() {
                        token
                    } else {
                        Token::new(RawToken::Parameterized(Box::new(token), params), source)
                    }
                };
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
                previous.push(token);
                self.walk(Mode::Parameters(previous))
            }
            Mode::Operator(left, op) => {
                self.undo();
                let source = left.source_index;
                let v = self.walk(Mode::Neutral)?;
                match v {
                    WalkResult::Value(v) => match &v.inner {
                        RawToken::Parameterized(_n, _p) => {
                            let operation =
                                Token::new(RawToken::Operation(Box::new((left, v)), op), source);
                            self.handle_after(operation)
                        }
                        RawToken::Identifier(_n) => {
                            let operation =
                                Token::new(RawToken::Operation(Box::new((left, v)), op), source);
                            self.handle_after(operation)
                        }
                        _ => panic!("{:?}", &v.inner),
                    },
                    _ => panic!("{:?}", v),
                }
            }
        }
    }
}
