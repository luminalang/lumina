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
    pub parameter_types: Vec<Type>,
    pub returns: Type,
    generics: Vec<(u16, Type)>,
    body: Vec<Token>,
}

impl FunctionBuilder {
    pub fn new() -> Self {
        FunctionBuilder {
            name: String::new(),
            parameter_names: Vec::new(),
            parameter_types: Vec::new(),
            returns: Type::default(),
            generics: Vec::new(),
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
                Some(RawToken::Identifier(name)) => {
                    self.parameter_types.push(Type::try_from(name.as_str())?)
                }
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

    pub fn _reserve(&mut self, additional: usize) {
        self.body.reserve(additional)
    }

    pub fn push(&mut self, t: Token) {
        self.body.push(t);
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
