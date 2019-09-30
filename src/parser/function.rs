use super::flags::Flag;
use super::Key;
use super::RawToken;
use super::Token;
use super::Tokenizer;
use super::Type;
use std::convert::TryFrom;
use std::fmt;

mod checker;

#[derive(Default)]
pub struct FunctionBuilder {
    pub name: String,
    pub parameter_names: Vec<String>,
    pub parameter_types: Vec<(Flag, Type)>,
    pub returns: Type,
    pub body: Vec<Token>,
}

impl FunctionBuilder {
    pub fn new() -> Self {
        FunctionBuilder {
            name: String::new(),
            parameter_names: Vec::new(),
            parameter_types: Vec::new(),
            returns: Type::default(),
            body: Vec::new(),
        }
    }

    pub fn with_header(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ()> {
        let first = tokenizer.next();
        let name = match first.map(|a| a.inner) {
            Some(RawToken::Identifier(name)) => name,
            _ => return Err(()),
        };
        self.name = name;

        let finished = match tokenizer.next().map(|t| t.inner) {
            // It has no parameters and returns Type::Nothing
            Some(RawToken::NewLine) => self,
            // It has return value but no parameters
            Some(RawToken::Key(Key::ParenOpen)) => self.with_parameter_types(tokenizer)?,
            // It has parameters
            Some(RawToken::Identifier(ident)) => {
                tokenizer.regress(ident.len());
                self.with_parameter_names(tokenizer)?
                    .with_parameter_types(tokenizer)?
            }
            Some(RawToken::Operator(op)) => {
                panic!(
                    "ERROR_TODO: {:?} is not a valid parameter name",
                    op.identifier
                );
            }
            _ => panic!("ERROR_TODO: Unexpected thing after function name"),
        };
        Ok(finished)
    }

    fn with_parameter_names(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ()> {
        loop {
            let next = tokenizer.next();
            match next.map(|t| t.inner) {
                Some(RawToken::Identifier(name)) => self.parameter_names.push(name),
                Some(RawToken::Key(Key::ParenOpen)) => return Ok(self),
                Some(n) => panic!("ERROR_TODO: Unexpected thing in header: {:?}", n),
                None => panic!("ERROR_TODO: File ended"),
            }
        }
    }
    fn with_parameter_types(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ()> {
        loop {
            let next = tokenizer.next();
            match next.map(|t| t.inner) {
                Some(RawToken::Identifier(name)) => self
                    .parameter_types
                    .push((Flag::default(), Type::try_from(name.as_str())?)),
                Some(RawToken::Key(Key::ParenClose)) => return Ok(self),
                Some(RawToken::Key(Key::Arrow)) => return self.with_return(tokenizer),
                None => panic!("ERROR_TODO: File ended"),
                Some(n) => panic!("ERROR_TODO: Unexpected thing in header: {:?}", n),
            }
        }
    }

    fn with_return(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ()> {
        let next = tokenizer.next();
        match next.map(|t| t.inner) {
            Some(RawToken::Identifier(name)) => {
                self.returns = Type::try_from(name.as_str())?;
                match tokenizer.next().map(|t| t.inner) {
                    Some(RawToken::Key(Key::ParenClose)) => Ok(self),
                    Some(s) => panic!("ERROR_TODO: Expected ), got {:?}", s),
                    None => panic!("Umatched ("),
                }
            }
            Some(s) => panic!("ERROR_TODO: Did not expect to get an {:?} here", s),
            None => panic!("ERROR_TODO: File ended"),
        }
    }

    fn get_parameter(&self, ident: &str) -> Option<usize> {
        for (i, n) in self.parameter_names.iter().enumerate() {
            if n == ident {
                return Some(i);
            }
        }
        None
    }

    pub fn _reserve(&mut self, additional: usize) {
        self.body.reserve(additional)
    }
    pub fn push(&mut self, t: Token) {
        self.body.push(t);
    }

    pub fn parse_func_body(&mut self, tokenizer: &mut Tokenizer) -> Result<Token, ()> {
        Ok(self.walk_func_body(Bodymode::Neutral, tokenizer)?.unwrap())
    }

    fn walk_func_body(
        &mut self,
        mode: Bodymode,
        tokenizer: &mut Tokenizer,
    ) -> Result<Discovery<Token>, ()> {
        let mut token = match tokenizer.next() {
            Some(t) => t,
            None => {
                return match mode {
                    Bodymode::List | Bodymode::AfterParen => Err(()),
                    _ => Ok(Discovery::Deadend),
                }
            }
        };

        match token.inner {
            RawToken::Header(h) => {
                tokenizer.regress(h.as_str().len());
                Ok(Discovery::Deadend)
            }
            RawToken::Key(Key::ParenOpen) => {
                let mut buf = Vec::new();
                loop {
                    let v = self.walk_func_body(Bodymode::AfterParen, tokenizer)?;
                    match v {
                        Discovery::Some(v) => buf.push(v),
                        Discovery::Done | Discovery::Deadend => break,
                        Discovery::OperatorCrash => {
                            token.inner = RawToken::Group(buf);
                            return self.walk_func_body(Bodymode::Operator(token), tokenizer);
                        }
                    }
                }
                token.inner = RawToken::Group(buf);
                Ok(Discovery::Some(token))
            }
            RawToken::Key(Key::Pipe) => {
                let mut buf = Vec::new();
                loop {
                    let v = self.walk_func_body(Bodymode::AfterPipe, tokenizer)?;
                    match v {
                        Discovery::Some(v) => buf.push(v),
                        Discovery::Done | Discovery::Deadend => break,
                        Discovery::OperatorCrash => {
                            token.inner = RawToken::Group(buf);
                            return self.walk_func_body(Bodymode::Operator(token), tokenizer);
                        }
                    }
                }
                token.inner = RawToken::Group(buf);
                Ok(Discovery::Some(token))
            }
            RawToken::Key(Key::ParenClose) => match mode {
                Bodymode::AfterParen => Ok(Discovery::Done),
                Bodymode::Parameters => {
                    panic!("Unexpected `)` while looking for function parameters")
                }
                Bodymode::List => panic!("Unexpected `)` while looking for list entries"),
                _ => {
                    tokenizer.regress(1);
                    Ok(Discovery::Done)
                }
            },
            RawToken::NewLine => self.walk_func_body(mode, tokenizer),
            RawToken::Inlined(_) => {
                if mode == Bodymode::Parameters {
                    Ok(Discovery::Some(token))
                } else {
                    match self.walk_func_body(Bodymode::Operator(token.clone()), tokenizer)? {
                        Discovery::Done | Discovery::Deadend => Ok(Discovery::Some(token)),
                        Discovery::Some(t) => Ok(Discovery::Some(t)),
                        Discovery::OperatorCrash => {
                            panic!("TODO: Not sure if this is valid or not. Lets find out!")
                        }
                    }
                }
            }
            RawToken::Operator(op) => {
                if let Bodymode::Operator(left) = mode {
                    let mut buf = Vec::new();
                    loop {
                        let v = self.walk_func_body(Bodymode::AfterPipe, tokenizer)?;
                        match v {
                            Discovery::Some(v) => buf.push(v),
                            Discovery::Done | Discovery::Deadend => break,
                            Discovery::OperatorCrash => {
                                if buf.len() == 1 {
                                    token = buf.pop().unwrap();
                                } else {
                                    token.inner = RawToken::Group(buf);
                                }
                                let source = token.source_index;
                                let new = Token::new(
                                    RawToken::Operation(Box::new((left, token)), op),
                                    source,
                                );
                                return self.walk_func_body(Bodymode::Operator(new), tokenizer);
                            }
                        }
                    }
                    if buf.len() == 1 {
                        token = buf.pop().unwrap();
                    } else {
                        token.inner = RawToken::Group(buf);
                    }
                    let source = token.source_index;
                    let new = Token::new(RawToken::Operation(Box::new((left, token)), op), source);
                    Ok(Discovery::Some(new))
                } else if mode == Bodymode::Parameters {
                    panic!("ERROR_TODO. Operator without wrapper as argument to function");
                } else {
                    tokenizer.regress(op.identifier.len());
                    Ok(Discovery::OperatorCrash)
                }
            }
            RawToken::Identifier(unknown) => {
                if !super::is_valid_identifier(&unknown) {
                    panic!("ERROR_TODO: Invalid identifier {:?}", unknown);
                }

                // It's a parameter value
                if let Some(pid) = self.get_parameter(&unknown) {
                    return if mode == Bodymode::Parameters {
                        Ok(Discovery::Some(Token::new(
                            RawToken::Parameter(pid, self.parameter_types[pid].1.clone()),
                            token.source_index,
                        )))
                    } else {
                        match self.walk_func_body(
                            Bodymode::Operator(Token::new(
                                RawToken::Identifier(unknown.clone()),
                                token.source_index,
                            )),
                            tokenizer,
                        )? {
                            Discovery::Done | Discovery::Deadend => {
                                Ok(Discovery::Some(Token::new(
                                    RawToken::Identifier(unknown.clone()),
                                    token.source_index,
                                )))
                            }
                            Discovery::Some(t) => Ok(Discovery::Some(t)),
                            Discovery::OperatorCrash => {
                                panic!("TODO: Not sure if this is valid or not. Lets find out!")
                            }
                        }
                    };
                }
                // It isn't a parameter value
                match mode {
                    Bodymode::Operator(_left) => {
                        panic!("ERROR_TODO: {:?} is not a valid operator", unknown);
                    }
                    Bodymode::Parameters => {
                        // Its probably a constant (or where statement i guess)
                        Ok(Discovery::Some(Token::new(
                            RawToken::Identifier(unknown.clone()),
                            token.source_index,
                        )))
                    }
                    _ => {
                        let mut buf = Vec::new();
                        loop {
                            let v = self.walk_func_body(Bodymode::Parameters, tokenizer)?;
                            match v {
                                Discovery::Some(v) => buf.push(v),
                                Discovery::Done | Discovery::Deadend => break,
                                Discovery::OperatorCrash => {
                                    // panic!("ERROR_TODO: Hit operator without wrapper when looking for parameters");
                                    // Actually nvm, we can just assumed we're done with the arguments
                                    // here.
                                    break;
                                }
                            }
                        }
                        token.inner = RawToken::Parameterized(unknown, buf);
                        Ok(Discovery::Some(token))
                    }
                }
            }
            _ => panic!("Unhandled {:?}", token),
        }
    }

    pub fn verify(&mut self, _parser: &super::Parser) {}
}

#[derive(PartialEq, Debug)]
pub enum Bodymode {
    AfterPipe,
    AfterParen,
    Neutral,
    Parameters,
    Operator(Token),
    List,
}

#[derive(Debug)]
enum Discovery<T: std::fmt::Debug> {
    Some(T),
    OperatorCrash,
    Done,
    Deadend,
}

impl<T: std::fmt::Debug> Discovery<T> {
    fn unwrap(self) -> T {
        if let Discovery::Some(v) = self {
            v
        } else {
            panic!("{:?} on Discovery::Unwrap()", self)
        }
    }
}

impl fmt::Debug for FunctionBuilder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let types = self
            .parameter_types
            .iter()
            .map(|(_flags, t)| t.to_string())
            .collect::<Vec<String>>();
        let annotation = if types.is_empty() {
            format!("{}", self.returns)
        } else {
            format!("{} -> {}", types.join(" "), self.returns)
        };
        write!(
            f,
            "fn {} {} ({})\n{:#?}",
            self.name,
            self.parameter_names.join(" "),
            annotation,
            self.body
        )
    }
}
