use super::{func, select, when, Expr, Parser, Span, Tr, Type, T};
use itertools::Itertools;
use key::Map;
use lumina_key as key;
use lumina_util::Highlighting;
use std::fmt;

#[derive(Debug)]
pub struct Declaration<'a> {
    pub header: Header<'a>,
    pub attributes: Vec<Tr<Expr<'a>>>,

    pub methods: Map<key::Method, func::Declaration<'a>>,
    pub associations: Map<key::AssociatedType, Association<'a>>,
}

#[derive(Debug)]
pub struct Header<'a> {
    pub trait_: Tr<Type<'a>>,
    pub impltor: Tr<Type<'a>>,
    pub default: bool,
    pub span: Span,
    pub when: when::Constraints<'a>,
}

#[derive(Debug, Clone)]
pub struct Association<'a> {
    pub span: Span,
    pub name: &'a str,
    pub type_: Option<Tr<Type<'a>>>,
}

impl<'a> Parser<'a> {
    pub fn r#impl(
        &mut self,
        start: Span,
        default: bool,
        when: when::Constraints<'a>,
        attributes: Vec<Tr<Expr<'a>>>,
    ) -> Option<Declaration<'a>> {
        let trait_ = self.type_with_params()?;
        self.expect(T::For)?;
        let impltor = self.type_newline_sensitive()?;

        select! { self, "new line", _span sensitive: true;
            T::NewLines => {}
        };

        let (methods, associations) = self.methods_and_associations()?;

        Some(Declaration {
            header: Header {
                span: start.extend(impltor.span),
                trait_,
                default,
                impltor,

                when,
            },

            attributes,
            methods,
            associations,
        })
    }

    pub fn methods_and_associations(
        &mut self,
    ) -> Option<(
        Map<key::Method, func::Declaration<'a>>,
        Map<key::AssociatedType, Association<'a>>,
    )> {
        let mut methods = Map::new();
        let mut associations = Map::new();

        let mut attributes = vec![];

        loop {
            let ((t, span), indent) = self.lexer.peek_with_indent();
            match t {
                T::OpenAttribute if indent > 1 => {
                    self.progress();
                    if let Some(attr) = self.attribute(span) {
                        attributes.extend(attr);
                    }
                }
                T::When if indent > 1 => {
                    self.progress();
                    let constraints = self.when().unwrap_or_else(|| {
                        self.recover_until(T::is_header, false);
                        when::Constraints::empty()
                    });

                    if self.expect(T::Fn).is_none() {
                        self.recover_until(T::is_header, false);
                        continue;
                    }

                    let Some(header) =
                        self.func(constraints, None, std::mem::take(&mut attributes))
                    else {
                        self.recover_until(T::is_header, false);
                        continue;
                    };

                    methods.push(header);
                }
                T::Fn if indent > 1 => {
                    self.progress();

                    let attr = std::mem::take(&mut attributes);

                    let Some(header) = self.func(when::Constraints::empty(), None, attr) else {
                        self.recover_until(T::is_header, false);
                        continue;
                    };

                    methods.push(header);
                }
                T::Type if indent > 1 => {
                    self.progress();
                    let Some(association) = self.trait_association() else {
                        self.recover_until(T::is_header, false);
                        continue;
                    };
                    associations.push(association);
                }
                T::EOF => break,
                t if t.is_header() => {
                    if indent > 1 {
                        self.err_invalid_trait_member(span);
                        self.recover_until(T::is_header, false);
                    } else {
                        break;
                    }
                }
                _ => {
                    self.progress();
                    self.err_unexpected_token((t, span), "method or associated type declaration");
                    self.recover_until(T::is_header, false);
                }
            }
        }

        Some((methods, associations))
    }

    fn trait_association(&mut self) -> Option<Association<'a>> {
        let Tr { mut span, value: name } = self.expect_name("associated type name")?;

        let type_ = select! { self, "associated type" sensitive: true;
            T::NewLines => {
                if self.next_is(|t| t == T::Equal).is_some() {
                    // The `None` of `Association` doesn't mean poison, it means unannotated. So; we do
                    // still want to return early on error here.
                    let t = self.type_with_params()?;
                    span = span.extend(t.span);

                    Some(t)
                } else {
                    None
                }
            },
            T::Equal => {
                let t = self.type_with_params()?;
                span = span.extend(t.span);
                Some(t)
            }
        };

        Some(Association { name, type_, span })
    }
}

impl<'a> fmt::Display for Declaration<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.header.when.generics.is_empty() {
            writeln!(f, "{}", &self.header.when)?;
        }

        let impl_ = "impl".keyword();
        let for_ = "for".keyword();

        writeln!(
            f,
            "{impl_} {} {for_} {}",
            self.header.trait_.type_(),
            self.header.impltor.type_()
        )?;

        for assoc in self.associations.values() {
            writeln!(f, "  {assoc}")?;
        }
        if !self.associations.is_empty() {
            writeln!(f)?;
        }

        write!(
            f,
            "  {}",
            self.methods
                .values()
                .map(|f| f.to_string().lines().format("\n  ").to_string())
                .format("\n  ")
        )
    }
}

impl<'a> fmt::Display for Association<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", "type".keyword(), self.name)?;

        if let Some(t) = &self.type_ {
            write!(f, " {} {}", '='.symbol(), t.type_())
        } else {
            Ok(())
        }
    }
}
