use super::{select, Expr, Parser, Type, T};
use lumina_util::{Highlighting, Span, Tr};
use std::fmt;

#[derive(Debug)]
pub struct Declaration<'a> {
    pub span: Span,
    pub name: &'a str,
    pub type_: Option<Tr<Type<'a>>>,
    pub value: Tr<Expr<'a>>,
    pub public: bool,
}

impl<'a> Parser<'a> {
    pub fn val(&mut self) -> Option<Declaration<'a>> {
        let name = self.expect_name("value declaration")?;

        select! { self, "a value or type annotation";
            T::Equal => self.finalize_val(name, None),
            T::As    => {
                let type_ = self.type_with_params()?;
                self.expect(T::Equal)
                    .and_then(|_| self.finalize_val(name, Some(type_)))
            }
        }
    }

    fn finalize_val(
        &mut self,
        name: Tr<&'a str>,
        type_: Option<Tr<Type<'a>>>,
    ) -> Option<Declaration<'a>> {
        self.expr().map(|value| Declaration {
            name: *name,
            type_,
            span: name.span.extend(value.span),
            value,
            public: false,
        })
    }
}

impl<'a> fmt::Display for Declaration<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let val = "val".keyword();
        let as_ = "as".keyword();

        let type_ = match &self.type_ {
            Some(t) => format!(" {as_} {}", t.type_()),
            None => "".into(),
        };
        write!(f, "{val} {}{type_} = {}", self.name, self.value)
    }
}
