use super::index::Index;
use super::tokenizer::token::{Key, Token};
use super::tokenizer::{Tokenizer, Tracked};
use crate::error::Leaf;
use crate::identifier::r#type::{BaseType, Type};
use crate::identifier::Identifier;
use std::fmt;
use std::path::Path;

pub mod checker;

pub struct FunctionBuilder<'a> {
    pub wheres: Vec<Vec<Tracked<Token>>>,
    pub tokens: Vec<Tracked<Token>>,
    pub header: FunctionHeader,
    pub file: &'a Path,
}

#[derive(Clone)]
pub struct FunctionHeader {
    pub parameter_names: Vec<String>,
    pub parameters: Vec<Type>,
    pub wheres: Vec<String>,
    pub returns: Type,
    pub name: String,
}

const STOPPERS: &[(Token, u8)] = &[
    (Token::Key(Key::HeaderFn), 3),
    (Token::Key(Key::HeaderType), 5),
    (Token::Key(Key::HeaderConvert), 8),
    (Token::EOF, 0),
];

impl<'a> FunctionBuilder<'a> {
    pub fn new(file: &'a Path) -> Self {
        Self {
            wheres: Vec::new(),
            tokens: Vec::new(),
            header: FunctionHeader::new(),
            file,
        }
    }
    pub fn from_raw_ir(
        file: &'a str,
        name: &str,
        mut tokens: Vec<Token>,
        returns: Type,
        parameters: Vec<Type>,
    ) -> Self {
        Self {
            wheres: Vec::new(),
            tokens: tokens.drain(0..).map(Tracked::new).collect(),
            file: Path::new(file),
            header: FunctionHeader {
                parameter_names: (0..parameters.len())
                    .map(|n| n.to_string())
                    .collect::<Vec<String>>(),
                parameters,
                wheres: Vec::new(),
                returns,
                name: name.to_owned(),
            },
        }
    }

    pub fn with_header(mut self, c: &mut Context) -> Result<Self, Leaf> {
        let token = c.tokenizer.next_token(true);
        match token {
            Token::Word(name) => self.header.name = name,
            _ => return Err(Leaf::ExFnName(token)),
        };
        super::is_valid_identifier(&self.header.name)?;

        self.get_params(c)
    }

    fn get_params(mut self, c: &mut Context) -> Result<Self, Leaf> {
        loop {
            let token = c.tokenizer.next_token(true);
            match token {
                Token::HeaderFnEndNoArgs => {
                    return Ok(self);
                }
                Token::Word(name) => {
                    super::is_valid_identifier(&name)?;
                    self.header.parameter_names.push(name)
                }
                Token::Key(Key::ParenOpen) => {
                    return self.get_param_types(c);
                }
                _ => return Err(Leaf::ExParam(token)),
            }
        }
    }

    fn get_param_types(mut self, c: &mut Context) -> Result<Self, Leaf> {
        loop {
            let token = c.tokenizer.next_token(true);
            match token {
                Token::Word(tname) => {
                    if c.tokenizer.last_char() == b')' {
                        self.header.returns = Identifier::from(&tname, c.file, c.index);
                        return Ok(self);
                    } else {
                        super::is_valid_identifier(&tname)?;
                        self.header
                            .parameters
                            .push(Identifier::from(&tname, c.file, c.index));
                    }
                }
                Token::Key(Key::HeaderArrow) => {
                    let token = c.tokenizer.next_token(false);
                    match token {
                        Token::Word(return_name) => {
                            super::is_valid_identifier(&return_name)?;
                            self.header.returns =
                                { Identifier::from(&return_name, c.file, c.index) };
                        }
                        _ => return Err(Leaf::ExFnReturn(token)),
                    }

                    if c.tokenizer.last_char() != b')' {
                        return Err(Leaf::UnexInFnHeader(c.tokenizer.last_char() as char));
                    }
                    return Ok(self);
                }
                _ => return Err(Leaf::ExParamType(token)),
            }
        }
    }

    pub fn build(mut self, c: &mut Context) -> Result<Self, Leaf> {
        self.tokens = self.build_buf(c, None)?;
        c.index
            .get_file(c.file)
            .borrow_mut()
            .set_func(self.header.clone());
        Ok(self)
    }
}

pub struct Context<'a> {
    tokenizer: &'a mut Tokenizer,
    index: &'a mut Index,
    file: &'a Path,
}
impl<'a> Context<'a> {
    pub fn new(file: &'a Path, tokenizer: &'a mut Tokenizer, index: &'a mut Index) -> Self {
        Self {
            tokenizer,
            index,
            file,
        }
    }
}

impl<'a> FunctionBuilder<'a> {
    fn build_buf(
        &mut self,
        c: &mut Context,
        stop_at: Option<&[Key]>,
    ) -> Result<Vec<Tracked<Token>>, Leaf> {
        let mut tbuf = Vec::new();
        loop {
            let mut token = Tracked {
                inner: c.tokenizer.next_token(false),
                position: c.tokenizer.linecount,
            };
            for (stopper, len) in STOPPERS.iter() {
                if stopper == &token.inner {
                    c.tokenizer.regress(*len as usize);
                    return Ok(tbuf);
                }
            }
            match &token.inner {
                Token::LambdaHeader(_) => {
                    // TODO: How does << work here?
                    let buf = self.build_buf(c, Some(&[Key::ParenOpen, Key::Pipe]))?;
                    let _position = buf[0].position;
                    // Invalid tracking
                    token.inner = Token::Lambda(
                        self.build_buf(c, Some(&[Key::ParenOpen, Key::Pipe]))?
                            .drain(0..)
                            .map(|tt| tt.untrack_t())
                            .collect(),
                    )
                }
                Token::Key(Key::Pipe) => {
                    token.inner = Token::TrackedGroup(self.build_buf(c, Some(&[Key::Pipe]))?)
                }
                Token::Key(Key::ParenOpen) => {
                    token.inner = Token::TrackedGroup(self.build_buf(c, Some(&[Key::ParenClose]))?)
                }
                Token::Key(Key::ElseIf) => {
                    c.tokenizer.regress(Key::ElseIf.to_string().len() + 1);
                    return Ok(tbuf);
                }
                Token::Key(Key::Where) => loop {
                    let identifier = if let Token::Word(w) = c.tokenizer.next_token(false) {
                        w
                    } else {
                        return Err(Leaf::Expected(
                            vec![Token::Word("<where identifier>".to_owned())],
                            token.inner,
                        ));
                    };
                    if c.tokenizer.next_token(false) != Token::Key(Key::Assign) {
                        return Err(Leaf::Expected(vec![Token::Key(Key::Assign)], token.inner));
                    }
                    let tokens = self.build_buf(c, Some(&[Key::Where]))?;
                    self.header.wheres.push(identifier.to_owned());
                    self.wheres.push(tokens);

                    let next = c.tokenizer.next_token(false);
                    if next != Token::Key(Key::Where) {
                        c.tokenizer.regress(next.to_string().len() + 1);
                        return Ok(tbuf);
                    }
                },
                Token::Key(Key::If) => {
                    let mut branches = Vec::new();
                    let conditional = self.build_buf(c, Some(&[Key::Then]))?;
                    //panic!("{}", self.tokenizer.next_token(false));
                    let next = c.tokenizer.next_token(false);
                    if next != Token::Key(Key::Then) {
                        return Err(Leaf::Expected(vec![Token::Key(Key::Then)], next));
                    }
                    let eval = self.build_buf(c, Some(&[Key::ElseIf, Key::Else]))?;
                    c.tokenizer.regress_to(b' ');
                    branches.push((conditional, eval));
                    let else_branch: Vec<Tracked<Token>>;
                    loop {
                        let next = c.tokenizer.next_token(false);
                        match next {
                            Token::Key(Key::Else) => {
                                else_branch = self.build_buf(c, None)?;
                                break;
                            }
                            Token::Key(Key::ElseIf) => {
                                let conditional = self.build_buf(c, Some(&[Key::Then]))?;
                                let next = c.tokenizer.next_token(false);
                                if next != Token::Key(Key::Then) {
                                    return Err(Leaf::Expected(vec![Token::Key(Key::Then)], next));
                                }
                                let eval = self.build_buf(c, Some(&[Key::ElseIf, Key::Else]))?;
                                c.tokenizer.regress_to(b' ');
                                branches.push((conditional, eval));
                                continue;
                            }
                            _ => {
                                return Err(Leaf::Expected(
                                    vec![Token::Key(Key::ElseIf), Token::Key(Key::Else)],
                                    next,
                                ))
                            }
                        }
                    }
                    token.inner = Token::TrackedIfStatement(branches, else_branch);
                }
                Token::Key(k) => {
                    if let Some(stopper) = stop_at {
                        if stopper.contains(k) {
                            c.tokenizer.regress(k.to_string().len() + 1);
                            return Ok(tbuf);
                        }
                    }
                }
                _ => {}
            }
            tbuf.push(token);
        }
    }
}

// FunctionHeader gets detatched in preperation for runtime when FunctionBuilder is converted into
// a Function
impl FunctionHeader {
    fn new() -> Self {
        Self {
            parameter_names: Vec::new(),
            parameters: Vec::new(),
            wheres: Vec::new(),
            returns: Type::Base(BaseType::Nothing),
            name: String::new(),
        }
    }
}

impl fmt::Debug for FunctionHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let param_types: Vec<String> = self
            .parameters
            .iter()
            .map(|t| t.into())
            .collect::<Vec<String>>();
        let returns: String = (&self.returns).into();
        let wheres = self.wheres.join(", ");
        write!(
            f,
            "{} {} ({} -> {}) {}",
            self.name,
            self.parameter_names.join(" "),
            param_types.join(" "),
            returns,
            if wheres.is_empty() {
                "".to_owned()
            } else {
                format!("where {}", wheres)
            }
        )
    }
}
