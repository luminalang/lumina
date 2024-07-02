use super::{select, when, Expr, Parser, Pattern, Span, Tr, Type, T};
use itertools::Itertools;
use lumina_util::{Highlighting, Identifier, Spanned};
use std::fmt;

#[derive(Debug, Clone)]
pub struct Declaration<'a> {
    pub header: Header<'a>,
    pub body: Option<Body<'a>>,
    pub attributes: Vec<Tr<Expr<'a>>>,
}

#[derive(Clone, Debug)]
pub struct Header<'a> {
    pub when: when::Constraints<'a>,
    pub typing: Option<Typing<'a>>,
    pub params: Vec<Tr<Pattern<'a>>>,
    pub name: Tr<&'a str>,
}

#[derive(Clone, Debug)]
pub struct Typing<'a> {
    pub span: Span,
    pub ptypes: Vec<Tr<Type<'a>>>,
    pub returns: Tr<Type<'a>>,
}

#[derive(Clone, Debug)]
pub struct Body<'a> {
    pub expr: Tr<Expr<'a>>,
    pub where_binds: Vec<Declaration<'a>>,
}

impl<'a> Parser<'a> {
    pub fn func(
        &mut self,
        when: when::Constraints<'a>,
        where_kw: Option<Span>,
        attributes: Vec<Tr<Expr<'a>>>,
    ) -> Option<Declaration<'a>> {
        let (identifier, span) = select! { self, "function name", span;
            T::Path => (Identifier::parse(self.take(span)).unwrap(), span),
            T::Operator => {
                let str = self.take(span);
                let ident = if str == ":" {
                    Identifier::from_raw(":")
                } else {
                    Identifier::identifier(str)
                };
                (ident, span)
            },
        };
        let name = identifier.as_name().unwrap_or_else(|| {
            self.err_expected_but_got(span, "a name", "a path");
            identifier.split_last().1
        });

        let params = match self.pat_params() {
            Some(params) => params,
            None => {
                self.recover_for([T::Equal, T::As], true);
                vec![]
            }
        };

        let mut header = Header { when, typing: None, params, name: name.tr(span) };

        let body = select! { self, "`=` or `as`", span;
             T::Equal => self.func_body(where_kw),
             T::As => {
                 let (typing, encountered_eq) = self.func_typing(span)?;
                 header.typing = Some(typing);

                 if encountered_eq {
                     self.func_body(where_kw)
                 } else {
                     select! { self, "`=` or end of declaration" peeked: true;
                         T::Equal => self.next_then(|parser| parser.func_body(where_kw)),
                         T::EOF => None,
                         t if t.is_header() => None,
                     }
                 }
             }
        };

        Some(Declaration { header, body, attributes })
    }

    fn func_body(&mut self, where_kw: Option<Span>) -> Option<Body<'a>> {
        let Some(expr) = self.expr() else {
            self.recover_next_toplevel();
            return None;
        };

        let where_binds = match self.next_is(|t| t == T::Where) {
            Some(span) => {
                if let Some(previous) = where_kw {
                    self.err_nested_where(previous, span);
                    self.recover_next_toplevel();
                    vec![]
                } else {
                    self.where_binds(span)
                }
            }
            None => vec![],
        };

        Some(Body { expr, where_binds })
    }

    fn func_typing(&mut self, start: Span) -> Option<(Typing<'a>, bool)> {
        let mut ptypes = vec![];

        loop {
            match self.type_with_params() {
                Some(t) => ptypes.push(t),
                None => {
                    ptypes.push(Type::Poison.tr(start));
                    self.recover_for([T::Equal, T::Arrow, T::Comma], false);
                }
            }

            let shorthand_return = |mut ptypes: Vec<_>, eq, end| {
                let returns = ptypes.remove(0);
                Some((Typing { span: start.extend(end), ptypes, returns }, eq))
            };

            select! { self, "`->`, `=` or `,`", span peeked: true;
                T::Equal if ptypes.len() == 1 => {
                    self.progress();
                    return shorthand_return(ptypes, true, span);
                },
                T::Arrow => {
                    self.progress();
                    break;
                },
                T::Comma => {
                    self.progress();
                    continue;
                },
                _ if ptypes.len() == 1 => {
                    return shorthand_return(ptypes, false, span);
                }
            }
        }

        self.type_with_params().map(|returns| {
            (
                Typing { span: start.extend(returns.span), ptypes, returns },
                false,
            )
        })
    }

    fn where_binds(&mut self, kw_span: Span) -> Vec<Declaration<'a>> {
        let base_indent = self.lexer.current_indent();

        let mut where_binds = Vec::with_capacity(1);

        loop {
            let ((t, span), indent) = self.lexer.peek_with_indent();
            match t {
                // TODO: There's some wonkiness with the syntax here when you consider that the
                // `impl` block can also have an indent requirement.
                //
                // We should probably pass that information along to here so we can create an error for it.
                T::Fn if belongs_to_where(base_indent, indent) => {
                    self.progress();
                    match self.func(when::Constraints::empty(), Some(kw_span), vec![]) {
                        Some(decl) => where_binds.push(decl),
                        None => {
                            self.recover_next_toplevel();
                            continue;
                        }
                    }
                }
                T::OpenAttribute if belongs_to_where(base_indent, indent) => {
                    unimplemented!("ET: attributes for where bindings");
                }
                _ if t.is_header() => break where_binds,
                other => {
                    self.err_unexpected_token(
                        (other, span),
                        "function or other top-level declaration",
                    );
                    self.recover_next_toplevel();
                    continue;
                }
            }
        }
    }
}

pub fn belongs_to_where(kw: u16, indent: u16) -> bool {
    indent >= kw || (kw != 0 && indent == kw)
}

impl<'a> Header<'a> {
    pub fn poisoned(name: Tr<&'a str>) -> Self {
        Header {
            when: when::Constraints::empty(),
            name,
            params: vec![],
            typing: None,
        }
    }
}

impl<'a> fmt::Display for Declaration<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let eq = "=".symbol();

        write!(f, "{}", self.header,)?;

        if let Some(body) = &self.body {
            write!(
                f,
                " {eq}\n  {}",
                body.expr.to_string().lines().format("\n  ")
            )
        } else {
            Ok(())
        }
    }
}

impl<'a> fmt::Display for Header<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.when.generics.is_empty() {
            writeln!(f, "{}", &self.when)?;
        }

        let fn_ = "fn".keyword();
        let as_ = "as".keyword();

        let annotation = match &self.typing {
            None => "".into(),
            Some(typing) => format!(" {as_} {}", typing),
        };

        write!(
            f,
            "{fn_} {}{}{annotation}",
            self.name.path(),
            if self.params.is_empty() {
                format!("")
            } else {
                format!(" {}", self.params.iter().format(" "))
            },
        )
    }
}

impl<'a> fmt::Display for Typing<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.ptypes.is_empty() {
            self.returns.fmt(f)
        } else {
            write!(f, "{} -> {}", self.ptypes.iter().format(", "), self.returns)
        }
    }
}
