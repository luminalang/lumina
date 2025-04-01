use itertools::Itertools;
use lumina_util::{Identifier, Span, Spanned, Tr};
use std::fmt;

mod lexer;
pub use lexer::Token;
use lexer::{Lexer, Token as T};
use tracing::trace;

mod expr;
pub use expr::{Expr, Literal};
pub mod func;
pub mod r#impl;
pub mod pat;
pub use pat::Pattern;
mod recover;
mod shared;
pub use shared::{AnnotatedPath, CurlyInit, Field, Fields, ListLength};
pub mod ty;
pub use ty::Type;
pub mod alias;
mod error;
pub use error::Error;
pub mod r#use;
pub mod val;
pub mod when;

#[cfg(test)]
mod tests;

#[derive(Clone)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    errors: Vec<Error>,
}

// I recommend just pretending this macro is compiler magic and not looking at it's horrific implementation
#[macro_export]
macro_rules! select {
    ($parser:expr, $exp:literal $(,$span:ident)? $($flagk:ident: $flagv:expr)*; $($token:pat $( if $guard:expr )? => $then:expr),+ $(,)?) => {
        {
            let settings = crate::AnyOfSettings {
                $($flagk: $flagv,)*
                ..Default::default()
            };

            let (t, _select_span) = if settings.sensitive { $parser.lexer.peek_line_sensitive() } else { $parser.lexer.peek() };

            tracing::trace!("selecting upon {t:?}");

            $( let $span = _select_span; )?

            match t {
                $($token $(if $guard)? => {
                    if !settings.peeked {
                        $parser.progress();
                    }
                    $then
                },)+
                #[allow(unreachable_patterns)]
                other => {
                    tracing::trace!("unexpected token {other:?}; starting recovery");

                    $parser.err_unexpected_token((t, _select_span), $exp);
                    if !settings.recovery {
                        return None;
                    }

                    let recovered_to = $parser.recover_until(|t| match t {
                        $($token $(if $guard)? => true,)*
                        _ => false,
                    }, settings.sensitive);

                    let (_, _select_span) = if settings.sensitive { $parser.lexer.peek_line_sensitive() } else { $parser.lexer.peek() };
                    $( let $span = _select_span; )?

                    match recovered_to {
                        $($token $(if $guard)? => {
                            if !settings.peeked {
                                $parser.progress();
                            }
                            $then
                        },)+
                        _ => return None,
                    }
                },
            }
        }
    };
}

struct AnyOfSettings {
    sensitive: bool,
    recovery: bool,
    peeked: bool,
}

impl Default for AnyOfSettings {
    fn default() -> Self {
        Self { sensitive: false, recovery: true, peeked: false }
    }
}

// NOTE: A parser method returning `Some(T)` doesn't mean that it was successfull. Just that if
// it did error it managed to recover.
//
// So; If we get a `None` we can assume that something rather severe has occoured and a normal
// response is to continue returning `None`.

fn wrap<'s, T>(f: impl Fn(T) -> Declaration<'s>, v: Option<T>) -> Option<Declaration<'s>> {
    Some(v.map(f).unwrap_or(Declaration::Failure))
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self { lexer: Lexer::new(src), errors: vec![] }
    }

    pub fn with_offset(mut self, offset: usize) -> Self {
        self.lexer.add_offset(offset);
        self
    }

    pub fn span(&mut self) -> Span {
        self.lexer.peek().1
    }

    pub fn skip_newlines(&mut self) {
        self.lexer.peek();
    }

    fn apply_attributes(
        &mut self,
        attribute: Vec<Tr<Expr<'a>>>,
        (span, mut decl): (Span, Declaration<'a>),
    ) -> Declaration<'a> {
        match &mut decl {
            Declaration::ModuleAttribute(_, attributes)
            | Declaration::Impl(r#impl::Declaration { attributes, .. })
            | Declaration::Type(ty::Declaration { attributes, .. })
            | Declaration::Alias(alias::Declaration { attributes, .. })
            | Declaration::Function(func::Declaration { attributes, .. }) => {
                attributes.extend(attribute);
                decl
            }
            Declaration::Failure => Declaration::Failure,
            _ if attribute.is_empty() => decl,
            _ => {
                self.errors
                    .push(Error::InvalidAttributes(span, attribute[0].span));
                decl
            }
        }
    }

    pub fn declaration(&mut self) -> Option<(Span, Declaration<'a>)> {
        let ((kw, span), ind) = self.lexer.peek_with_indent();
        trace!("attempting to get next declaration at {kw:?}");

        if ind > 0 && kw.is_header() {
            self.err_bad_indentation(span);
        }

        let opt = select! { self, "a top-level declaration";
            T::OpenAttribute => {
                self.attribute(span)
                    .and_then(|attribute| self.declaration().map(|decl| self.apply_attributes(attribute, decl)))
            },
            T::Pub => {
                let pub_expr = vec![Expr::name("pub".tr(span))];
                self.declaration().map(|(span, decl)|
                    match decl {
                        Declaration::Use(mut r#use) => {
                            r#use.public = true;
                            Declaration::Use(r#use)
                        }
                        Declaration::Val(mut val) => {
                            val.public = true;
                            Declaration::Val(val)
                        }
                        decl => self.apply_attributes(pub_expr, (span, decl)),
                    }
                )
            },
            T::OpenModuleAttribute => wrap(|attrs| Declaration::ModuleAttribute(span, attrs), self.attribute(span)),
            T::Fn => wrap(Declaration::Function, self.func(when::Constraints::empty(), None, vec![])),
            T::Alias => wrap(Declaration::Alias, self.alias(vec![])),
            T::Type => wrap(Declaration::Type, self.type_decleration(vec![])),
            T::Trait => wrap(Declaration::Type, self.r#trait(vec![])),
            T::Use => wrap(Declaration::Use, self.r#use()),
            T::Default => self.handle_default(when::Constraints::empty()),
            T::Impl => wrap(Declaration::Impl, self.r#impl(span, false, when::Constraints::empty(), vec![])),
            T::Val => wrap(Declaration::Val, self.val()),
            T::When => {
                let Some(when) = self.when() else {
                    self.recover_next_toplevel();
                    return self.declaration();
                };

                match self.declaration() {
                    Some((_, Declaration::Function(mut decl))) => {
                        decl.header.when.generics.extend(when.generics);
                        Some(Declaration::Function(decl))
                    },
                    Some((_, Declaration::Impl(mut decl))) => {
                        decl.header.when.generics.extend(when.generics);
                        Some(Declaration::Impl(decl))
                    },
                    Some((span, _)) => {
                        self.err_expected_but_got(span, "function or implementation declaration", "another declaration");
                        None
                    },
                    None => todo!(),
                }
            },
            T::EOF => None
        };

        opt.map(|decl| (span, decl))
    }

    fn attribute(&mut self, span: Span) -> Option<Vec<Tr<Expr<'a>>>> {
        self.shared_list(span, |parser| parser.expr(), None)
            .map(|(elems, _, _)| elems)
    }

    fn handle_default(&mut self, when: when::Constraints<'a>) -> Option<Declaration<'a>> {
        match self.lexer.peek() {
            (T::Impl, span) => self.next_then(|parser| {
                wrap(Declaration::Impl, parser.r#impl(span, true, when, vec![]))
            }),
            (kw, span) if kw.is_header() => {
                self.err_bad_default(span, kw);
                self.declaration().map(|(_, decl)| decl)
            }
            other => {
                self.err_unexpected_token(other, "the start of an implementation");
                self.recover_next_toplevel();
                None
            }
        }
    }

    pub fn try_take(&self, mut span: Span) -> Option<&'a str> {
        let src = self.lexer.source();
        span.indice -= self.lexer.span_offset as u32;
        span.try_get_str(src)
    }

    pub fn take(&self, mut span: Span) -> &'a str {
        let src = self.lexer.source();
        span.indice -= self.lexer.span_offset as u32;
        span.get_str(src)
    }
    pub fn taken(&self, span: Span) -> Tr<&'a str> {
        self.take(span).tr(span)
    }
    pub fn at(&self, i: u32) -> char {
        self.lexer.source()[i as usize..].chars().next().unwrap()
    }

    pub fn next_then<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.lexer.next();
        f(self)
    }

    pub fn next_is(&mut self, check: fn(Token) -> bool) -> Option<Span> {
        let (t, span) = self.lexer.peek();
        if check(t) {
            self.lexer.next();
            Some(span)
        } else {
            None
        }
    }

    pub fn progress(&mut self) {
        self.lexer.next();
    }

    pub fn consume<T>(&mut self, map: impl FnOnce(Span) -> T) -> T {
        let (_, span) = self.lexer.next();
        map(span)
    }

    #[must_use]
    #[track_caller]
    fn expect(&mut self, exp: Token) -> Option<Span> {
        let (t, span) = self.lexer.peek();
        if t == exp {
            self.lexer.next();
            Some(span)
        } else {
            self.err_unexpected_token((t, span), exp.describe());
            if self.recover_for([exp], exp == T::NewLines) == exp {
                Some(self.lexer.peek().1)
            } else {
                None
            }
        }
    }

    fn expect_name(&mut self, _: &str) -> Option<Tr<&'a str>> {
        let (t, span) = self.lexer.peek();
        if t != Token::Path && t != Token::Operator {
            self.err_unexpected_token((t, span), "an identifier");
            return None;
        }
        self.progress();

        let path = Identifier::parse(self.take(span)).unwrap();
        if let Some(name) = path.as_name() {
            Some(name.tr(span))
        } else {
            self.err_expected_but_got(span, "an identifier", "a full path");
            None
        }
    }

    pub fn into_errors(self) -> Vec<Error> {
        self.errors
    }
}

#[derive(Debug)]
pub enum Declaration<'a> {
    ModuleAttribute(Span, Vec<Tr<expr::Expr<'a>>>),
    Function(func::Declaration<'a>),
    Alias(alias::Declaration<'a>),
    Type(ty::Declaration<'a>),
    Impl(r#impl::Declaration<'a>),
    Use(r#use::Declaration<'a>),
    Val(val::Declaration<'a>),
    Failure,
}

// types: Location -> (TypeKey, KindKey)

impl<'a> fmt::Display for Declaration<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Declaration::ModuleAttribute(_, attribute) => {
                writeln!(f, "@![{}]", attribute.iter().format(", "))
            }
            Declaration::Alias(alias) => alias.fmt(f),
            Declaration::Function(func) => func.fmt(f),
            Declaration::Type(type_) => type_.fmt(f),
            Declaration::Impl(impl_) => impl_.fmt(f),
            Declaration::Use(use_) => use_.fmt(f),
            Declaration::Val(var) => var.fmt(f),
            Declaration::Failure => "???".fmt(f),
        }
    }
}
