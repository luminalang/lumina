use super::body;
use super::body::BodySource;
use super::flags::Flag;
use super::Key;
use super::RawToken;
use super::Token;
use super::Tokenizer;
use super::Type;
use std::convert::TryFrom;
use std::fmt;

#[derive(Default)]
pub struct FunctionBuilder {
    pub name: String,
    pub parameter_names: Vec<String>,
    pub parameter_types: Vec<(Flag, Type)>,
    pub returns: Type,
    pub body: Token,
    pub wheres: Vec<(String, Token)>,
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

    pub fn with_header(mut self, tokenizer: &mut Tokenizer) -> Result<Self, ()> {
        let first = tokenizer.next();
        let name = match first.map(|a| a.inner) {
            Some(RawToken::Identifier(name)) => name,
            _ => panic!("ET: Missing function name"),
        };
        self.name = name;

        let finished = match tokenizer.next().map(|t| t.inner) {
            // It has no parameters and returns Type::Nothing
            Some(RawToken::NewLine) => self,
            // It has return value but no parameters
            Some(RawToken::Key(Key::ParenOpen)) => self.with_return(tokenizer)?,
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

    pub fn get_parameter(&self, ident: &str) -> Option<usize> {
        for (i, n) in self.parameter_names.iter().enumerate() {
            if n == ident {
                return Some(i);
            }
        }
        None
    }

    pub fn parse_body(&mut self, tokenizer: &mut Tokenizer) -> Result<Token, ()> {
        let entry = self.parse_body_tokens(tokenizer)?;
        self.parse_body_wheres(tokenizer)?;
        Ok(entry)
    }

    pub fn parse_body_tokens(&mut self, tokenizer: &mut Tokenizer) -> Result<Token, ()> {
        let entry = tokenizer.walk(body::Mode::Neutral)?;
        match entry {
            body::WalkResult::Value(v) => Ok(v),
            _ => panic!("{:?}", entry),
        }
    }

    pub fn parse_body_wheres(&mut self, tokenizer: &mut Tokenizer) -> Result<(), ()> {
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
