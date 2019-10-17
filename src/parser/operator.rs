use super::body;
use super::body::BodySource;
use super::flags::Flag;
use super::tokenizer::Operator;
use super::Key;
use super::RawToken;
use super::Token;
use super::Tokenizer;
use super::Type;
use std::convert::TryFrom;
use std::fmt;

#[derive(Default)]
pub struct OperatorBuilder {
    pub name: Operator,
    pub parameter_types: [Type; 2],
    pub returns: Type,
    pub body: Token,
    pub wheres: Vec<(String, Token)>,
}

impl OperatorBuilder {
    pub fn new() -> Self {
        OperatorBuilder {
            name: Operator::default(),
            parameter_types: <[Type; 2]>::default(),
            returns: Type::Nothing,
            body: Token::new(RawToken::NewLine, 0),
            wheres: Vec::new(),
        }
    }

    // operator + (int int int)
    pub fn with_header(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ()> {
        let first = tokenizer.next();
        match first.map(|a| a.inner) {
            Some(RawToken::Operator(op)) => self.name = op,
            Some(other) => panic!("ET: Wanted identifier got {:?}", other),
            None => panic!("ET: Missing identifier after `operator` keyword"),
        };
        self.with_types(tokenizer)
    }

    fn with_types(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ()> {
        match tokenizer.next().map(|t| t.inner) {
            Some(RawToken::Key(Key::ParenOpen)) => {}
            Some(other) => panic!("ET: Expected parenopen got {:?}", other),
            None => panic!("ET: Expected parenopen but got nothing"),
        }
        let mut next_ident = || -> Result<Type, ()> {
            let next = match tokenizer.next() {
                None => {
                    panic!("ET: Expected type for the left argument of operator but file ended")
                }
                Some(t) => t,
            };
            if let RawToken::Identifier(ident) = next.inner {
                Type::try_from(ident.as_str()).map_err(|_| ())
            } else {
                panic!(
                    "Expected type identifier for the left argument of operator but got {:?}",
                    next
                );
            }
        };

        let left = next_ident()?;
        let returns = next_ident()?;
        let right = next_ident()?;
        match tokenizer.next().map(|t| t.inner) {
            Some(RawToken::Key(Key::ParenClose)) => {}
            Some(other) => panic!("ET: Expected parenclose got {:?}", other),
            None => panic!("ET: Expected parenclose but got nothing"),
        }

        self.parameter_types = [left, right];
        self.returns = returns;
        Ok(self)
    }

    pub fn parse_body(&mut self, tokenizer: &mut Tokenizer) -> Result<Token, ()> {
        let entry = self.parse_body_tokens(tokenizer)?;
        self.parse_body_wheres(tokenizer)?;
        Ok(entry)
    }

    fn parse_body_tokens(&mut self, tokenizer: &mut Tokenizer) -> Result<Token, ()> {
        let entry = tokenizer.walk(body::Mode::Neutral)?;
        match entry {
            body::WalkResult::Value(v) => Ok(v),
            _ => panic!("{:?}", entry),
        }
    }

    fn parse_body_wheres(&mut self, tokenizer: &mut Tokenizer) -> Result<(), ()> {
        loop {
            let next = tokenizer.next();
            match next.map(|t| t.inner) {
                Some(RawToken::Key(Key::Where)) => {
                    let (name, t) = body::r#where::build(tokenizer)?;
                    self.wheres.push((name, t));
                }
                None | Some(RawToken::Header(_)) => {
                    tokenizer.undo();
                    return Ok(());
                }
                Some(v) => panic!("ET: Unexpected {:?}", v),
            }
        }
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
