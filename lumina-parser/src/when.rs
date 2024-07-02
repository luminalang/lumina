use super::{select, Parser, Type, T};
use itertools::Itertools;
use lumina_util::{Highlighting, Span, Tr};
use std::fmt;

#[derive(Clone, Debug, Default)]
pub struct Constraints<'a> {
    pub generics: Vec<(Span, &'a str, Vec<Tr<Type<'a>>>)>,
}

impl<'a> Constraints<'a> {
    pub fn empty() -> Self {
        Self { generics: vec![] }
    }
}

impl<'a> Parser<'a> {
    pub fn when(&mut self) -> Option<Constraints<'a>> {
        let mut generics = vec![];

        loop {
            match self.bind() {
                Some(bind) => generics.push(bind),
                None => {
                    self.recover_for([T::Comma, T::NewLines], true);
                }
            }

            match self.lexer.peek_line_sensitive().0 {
                T::NewLines | T::Comma => {
                    self.progress();
                    if self.lexer.peek().0 != T::Path {
                        break Some(Constraints { generics });
                    }
                }
                _ => break Some(Constraints { generics }),
            }
        }
    }

    fn bind(&mut self) -> Option<(Span, &'a str, Vec<Tr<Type<'a>>>)> {
        let name = self.expect_name("generic to constrain")?;
        self.expect(T::Can)?;
        self.traits_for_can()
            .map(|(cons, end)| (name.span.extend(end), name.value, cons))
    }

    pub fn traits_for_can(&mut self) -> Option<(Vec<Tr<Type<'a>>>, Span)> {
        let mut constraints = vec![];
        loop {
            match self.type_newline_sensitive() {
                Some(t) => constraints.push(t),
                None => {
                    self.recover_for([T::NewLines, T::Operator, T::Comma], true);
                }
            };
            let src = self.lexer.source();
            select! { self, "`+`, new line, or next item", span sensitive: true peeked: true;
                T::NewLines | T::Equal => break Some((constraints, span.move_indice(-1))),
                T::Comma => break Some((constraints, span)),
                T::EOF => break Some((constraints, span.move_indice(-1))),
                T::Operator if span.get_str(src) == "+" => {
                    self.progress();
                    continue
                },
                t if t.is_header() => break Some((constraints, span.move_indice(-1))),
            }
        }
    }
}

impl<'a> fmt::Display for Constraints<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.generics.is_empty() {
            return Ok(());
        }

        writeln!(f, "{}", "when".keyword())?;

        self.generics
            .iter()
            .format_with("\n", |(_, gen, cons), f| {
                f(&format_args!(
                    "  {gen} {} {}",
                    "gen".keyword(),
                    cons.iter().format(" + ")
                ))
            })
            .fmt(f)
    }
}
