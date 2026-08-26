use super::{Expr, Parser, Token, Type};
use lumina_key as key;
use lumina_key::Map;
use lumina_util::{Highlighting, Tr};
use std::fmt;

#[derive(Debug)]
pub struct Declaration<'a> {
    pub name: Tr<&'a str>,
    pub dst: Tr<Type<'a>>,
    pub attributes: Vec<Tr<Expr<'a>>>,
    pub type_params: Map<key::Generic, &'a str>,
}

impl<'a> Parser<'a> {
    pub fn alias(&mut self, attributes: Vec<Tr<Expr<'a>>>) -> Option<Declaration<'a>> {
        let name = self.expect_name("alias name")?;

        let type_params = self.type_declaration_parameters();

        self.expect(Token::Equal)?;
        let dst = self.type_newline_sensitive()?;
        Some(Declaration { name, dst, attributes, type_params })
    }
}

impl<'a> fmt::Display for Declaration<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {} {} {}",
            "alias".keyword(),
            self.name,
            '='.symbol(),
            &self.dst
        )
    }
}
