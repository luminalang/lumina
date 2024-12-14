//! An attempt at parsing generalised token-tree-like structures which can then be converted into
//! expressions, types and patterns.

mod lexer;
pub use lexer::{Lexer, Token};
use lumina_util::{Error, Identifier, Span, Spanned, Tr};
mod construct;
mod fmt;

#[cfg(test)]
mod tests;

type Entities<'s> = Vec<Tr<Entity<'s>>>;

#[derive(Clone, Debug)]
pub enum Entity<'s> {
    Commented(&'s str, bool, Box<Tr<Self>>),
    // a b -> c
    //     ^^
    Delim(Span, &'s str, Box<[Tr<Self>; 2]>),
    Literal(Literal<'s>),
    Clause(&'s str, &'s str, Option<Box<Tr<Self>>>),
    Identifier(Identifier<'s>),
    Operator(Identifier<'s>),
    Keyword(&'s str, Box<Tr<Self>>),
    // `a b c`. Handles things like function application
    Sequence(Entities<'s>),
    Unary(Span, &'s str, Box<Tr<Self>>),

    /// Entities which are indented under this where-block
    Where(Entities<'s>),
    /// Entities which are indented under this impl-block
    Impl(Entities<'s>),
    /// Entities which are indented under this when-block
    When(Entities<'s>),

    Poison,
}

#[derive(Clone, Debug)]
pub enum Literal<'s> {
    Int(bool, u128),
    Float(f64),
    DoubleQuote(&'s str),
    SingleQuote(&'s str),
}

#[derive(Clone)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    errors: Vec<Error>,
}

enum Constr<'s> {
    Entity(Tr<Entity<'s>>),
    Delim {
        span: Span,
        middle: &'s str,
        rhs: Tr<Entity<'s>>,
    },
    Unhandled(Token),
    Failure(Span),
}

impl<'s> Parser<'s> {
    pub fn new(src: &'s str) -> Self {
        Parser { lexer: Lexer::new(src), errors: vec![] }
    }

    fn take(&self, span: Span) -> &'s str {
        span.get_str(self.lexer.source())
    }

    fn with_entity<T>(
        &mut self,
        con: Constr<'s>,
        and_then: impl FnOnce(&mut Self, Tr<Entity<'s>>) -> T,
    ) -> T {
        match con {
            Constr::Entity(v) => and_then(self, v),
            Constr::Failure(span) => and_then(self, Entity::Poison.tr(span)),
            Constr::Delim { span, middle, rhs } => {
                panic!("ET: incomplete, missing lhs for `{middle}`? (or just unexpected {middle}?)")
            }
            Constr::Unhandled(t) => panic!("??: {t:?}"),
        }
    }

    fn map_next_entity(
        &mut self,
        span: Span,
        f: impl FnOnce(&mut Self, Tr<Entity<'s>>) -> Entity<'s>,
    ) -> Constr<'s> {
        let inner = self.entity();
        self.with_entity(inner, |this, v| {
            let v = this.then_try_sequence(v);
            let v = f(this, v);
            singleton(span, v)
        })
    }

    fn next_then_kw(&mut self, span: Span, name: &'s str) -> Constr<'s> {
        self.lexer.next();
        self.map_next_entity(span, |this, v| Entity::Keyword(name, Box::new(v)))
    }

    fn next_then_delim(&mut self, span: Span, name: &'s str) -> Constr<'s> {
        self.lexer.next();
        let rhs = self.entity();
        self.with_entity(rhs, |this, rhs| {
            let rhs = this.then_try_sequence(rhs);
            Constr::Delim { span, middle: name, rhs }
        })
    }

    fn next_then_clause(&mut self, span: Span, open: &'s str, close: &'s str) -> Constr<'s> {
        self.lexer.next();
        let closet = match close {
            ")" => Token::CloseParen,
            "]" => Token::CloseList,
            "}" => Token::CloseCurly,
            _ => unreachable!(),
        };
        if let Some(end) = self.next_is(closet) {
            let span = span.extend(end);
            Constr::Entity(Entity::Clause(open, close, None).tr(span))
        } else {
            self.map_next_entity(span, |this, v| {
                this.expect(closet, |_| Entity::Clause(open, close, Some(Box::new(v))))
            })
        }
    }

    fn next_then_unary(&mut self, span: Span, name: &'s str) -> Constr<'s> {
        todo!();
    }

    fn expect<T>(&mut self, t: Token, then: impl FnOnce(&mut Self) -> T) -> T {
        let (got, _) = self.lexer.next();
        if got == t {
            then(self)
        } else {
            dbg!(got, &t);
            todo!("recovery");
            // actually; do we even want to `next` or just `peek`?
        }
    }

    fn next_is(&mut self, t: Token) -> Option<Span> {
        let (got, span) = self.lexer.peek();
        (got == t).then(|| {
            self.lexer.next();
            span
        })
    }

    fn everything(&mut self) -> Vec<Tr<Entity<'s>>> {
        let mut toplevel = Vec::with_capacity(5);

        loop {
            match self.entity() {
                Constr::Entity(entity) => toplevel.push(entity),
                Constr::Delim { span, middle, rhs } => {
                    panic!("error: unexpected {middle} {rhs:?}");
                }
                Constr::Failure(_) => {
                    todo!("recovery until keyword?");
                }
                Constr::Unhandled(Token::EOF) => break toplevel,
                Constr::Unhandled(t) => {
                    todo!("unexpected? i guess: {t:?}");
                }
            }
        }
    }

    fn entity(&mut self) -> Constr<'s> {
        let (token, span) = self.lexer.peek();
        match token {
            Token::NewLines => {
                self.lexer.next();
                self.entity()
            }
            Token::StringLiteral => {
                self.lexer.next();
                let data = self.take(span.move_indice(1).shortened(1));
                literal(span, Literal::DoubleQuote(data))
            }
            Token::CharLiteral => {
                self.lexer.next();
                let data = self.take(span.move_indice(1).shortened(1));
                literal(span, Literal::SingleQuote(data))
            }
            Token::Int => {
                self.lexer.next();
                let raw = self.take(span);
                let v = int(raw);
                singleton(span, v)
            }
            Token::Float => {
                self.lexer.next();
                let n = self.take(span).parse().unwrap();
                literal(span, Literal::Float(n))
            }
            Token::LineDocComment | Token::LineComment => {
                self.lexer.next();
                let comment = self.take(span);
                self.map_next_entity(span, |_, v| {
                    Entity::Commented(comment, token == Token::LineDocComment, Box::new(v))
                })
            }
            Token::If => self.next_then_kw(span, "if"),
            Token::Fn => self.next_then_kw(span, "fn"),
            Token::FnPtr => self.next_then_kw(span, "fnptr"),
            Token::Let => self.next_then_kw(span, "let"),
            Token::Do => self.next_then_kw(span, "do"),
            Token::Type => self.next_then_kw(span, "type"),
            Token::Use => self.next_then_kw(span, "use"),
            Token::Val => self.next_then_kw(span, "val"),
            Token::Trait => self.next_then_kw(span, "trait"),
            Token::Impl => self.next_then_kw(span, "impl"),

            Token::Can => self.next_then_delim(span, "can"),
            Token::As => self.next_then_delim(span, "as"),
            Token::Arrow => self.next_then_delim(span, "->"),
            Token::Then => self.next_then_delim(span, "then"),
            Token::Comma => self.next_then_delim(span, ","),
            Token::In => self.next_then_delim(span, "in"),
            Token::Symbol if self.take(span) == "=" => self.next_then_delim(span, "="),

            Token::OpenParen => self.next_then_clause(span, "(", ")"),
            Token::OpenList => self.next_then_clause(span, "[", "]"),
            Token::OpenCurly => self.next_then_clause(span, "{", "}"),

            Token::Where => todo!(),
            Token::When => todo!(),
            Token::Match => todo!("ok we probably do need a concepet of binding power"),

            Token::SemiColon => todo!("???"),

            Token::Symbol => {
                self.lexer.next();
                let str = self.take(span);

                if !self.is_whitespace(span.following(1))
                    && self.is_whitespace(Span::new(span.indice - 1, 1))
                {
                    // unary operator
                    let then = self.entity();
                    self.with_entity(then, |_, v| {
                        let uspan = span.extend(v.span);
                        let unary = Entity::Unary(span, str, Box::new(v));
                        singleton(uspan, unary)
                    })
                } else {
                    // binary operator
                    let ident = Identifier::parse(str).unwrap();
                    singleton(span, Entity::Operator(ident))
                }
            }
            Token::Path => {
                self.lexer.next();
                let str = self.take(span);
                let ident = Identifier::parse(str).unwrap();
                singleton(span, Entity::Identifier(ident))
            }
            Token::Error => todo!(),
            t => Constr::Unhandled(t),
            // _ => panic!("error? or: perhaps we should handle every case?"),
        }
    }

    fn is_whitespace(&self, span: Span) -> bool {
        self.take(span).chars().all(|c| c.is_whitespace())
    }

    fn then_try_sequence(&mut self, entity: Tr<Entity<'s>>) -> Tr<Entity<'s>> {
        let espan = entity.span;
        let mut seq = vec![entity];

        loop {
            match self.entity() {
                Constr::Entity(param) => seq.push(param),
                Constr::Delim { span, middle, rhs } => {
                    let lhs = seq_or_singleton(espan.extend_by_params(&seq), seq);
                    let fspan = espan.extend(rhs.span);
                    break Entity::Delim(span, middle, Box::new([lhs, rhs])).tr(fspan);
                }
                Constr::Failure(_) | Constr::Unhandled(_) => {
                    let span = espan.extend_by_params(&seq);
                    break seq_or_singleton(span, seq);
                }
            }
        }
    }
}

fn seq_or_singleton<'s>(span: Span, mut entities: Entities<'s>) -> Tr<Entity<'s>> {
    match entities.len() {
        0 => panic!("empty sequence"),
        1 => entities.remove(0),
        _ => Entity::Sequence(entities).tr(span),
    }
}

impl Token {
    // fn is_ender(&self) -> bool {
    //     matches!(
    //         self,
    //         Token::EOF
    //             | Token::CloseList
    //             | Token::CloseParen
    //             | Token::CloseCurly
    //             | Token::Else
    //             | Token::LineDocComment
    //             | Token::LineComment
    //     )
    // }
}

fn singleton<'s>(span: Span, v: Entity<'s>) -> Constr<'s> {
    Constr::Entity(v.tr(span))
}

fn literal<'s>(span: Span, v: Literal<'s>) -> Constr<'s> {
    Constr::Entity(Entity::Literal(v).tr(span))
}

fn int<'s>(raw: &'s str) -> Entity<'s> {
    let (sign, n) = if raw.as_bytes()[0] == b'-' {
        (true, raw[1..].parse::<u128>().unwrap())
    } else {
        (false, raw.parse::<u128>().unwrap())
    };
    Entity::Literal(Literal::Int(sign, n))
}
