use super::{select, ty, Expr, Parser, Pattern, Token, Type};
use itertools::Itertools;
use lumina_util::{Highlighting, Identifier, Span, Spanned, Tr};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CurlyResolve {
    WithTilde,
    WithBar,
    NoHeader,
}

#[derive(Clone, Debug)]
pub enum CurlyInit<'a> {
    Construct(Tr<Type<'a>>),
    Modify(Tr<Expr<'a>>),
    None,
}

pub struct Curly<'a, T> {
    pub init: CurlyInit<'a>,
    pub fields: Fields<'a, T>,
    pub span: Span,
}

pub trait CurlyValue<'a>: Sized + Clone + std::fmt::Debug {
    fn from_name(name: Tr<&'a str>) -> Self;
    fn from_field_access(names: Vec<Tr<&'a str>>) -> Self;
    fn parse(parser: &mut Parser<'a>) -> Option<Tr<Self>>;
    fn poison() -> Self;
}

impl<'a> CurlyValue<'a> for Expr<'a> {
    fn from_name(name: Tr<&'a str>) -> Self {
        Expr::Call(
            name.map(Identifier::from_raw).map(AnnotatedPath::without),
            vec![],
            0,
        )
    }
    fn from_field_access(names: Vec<Tr<&'a str>>) -> Self {
        let mut iter = names.into_iter();
        let init = Expr::name(iter.next().unwrap());
        iter.fold(init, |right: Tr<Self>, name| {
            let span = right.span.extend(name.span);
            Expr::FieldAccess(Box::new(right), name).tr(span)
        })
        .value
    }
    fn parse(parser: &mut Parser<'a>) -> Option<Tr<Self>> {
        parser.expr()
    }
    fn poison() -> Self {
        Expr::Poison
    }
}

impl<'a> CurlyValue<'a> for Pattern<'a> {
    fn from_name(name: Tr<&'a str>) -> Self {
        Pattern::Name(Identifier::from_raw(name.value), vec![])
    }
    fn from_field_access(_: Vec<Tr<&'a str>>) -> Self {
        panic!("ET: field access not allowed in patterns");
    }
    fn parse(parser: &mut Parser<'a>) -> Option<Tr<Self>> {
        parser.list_pattern()
    }
    fn poison() -> Self {
        Pattern::Poison
    }
}

pub type Fields<'a, T> = Vec<Field<'a, T>>;

#[derive(Clone, Debug)]
pub enum Field<'a, T> {
    Punned(Vec<Tr<&'a str>>),
    Value(Tr<T>),
    Assigned {
        field_path: Vec<Tr<&'a str>>,
        bind: Option<Tr<&'a str>>,
        value: Tr<T>,
    },
}

impl<'a, T: std::fmt::Debug> Field<'a, T> {
    pub fn name(&self) -> Tr<&'a str> {
        match self {
            Field::Value(a) => panic!("not a record: {:?}", a),
            Field::Punned(field_path) | Field::Assigned { field_path, .. } => field_path[0],
        }
    }
}

#[derive(Clone, Debug)]
pub struct AnnotatedPath<'a> {
    pub for_segments: Vec<(usize, ty::ForallAnnotation<'a>)>,
    pub path: Identifier<'a>,
}

impl<'a> AnnotatedPath<'a> {
    pub fn without(path: Identifier<'a>) -> Self {
        Self { path, for_segments: vec![] }
    }
}

impl<'a> Parser<'a> {
    // On the token `{`, we don't know whether to parse a type or an expression until the token
    // *afterwards*. So; this clones the lexer and goes further than LL(1)
    //
    // You might be terrified about this, and rightfully so. Any sane person would just design the
    // syntax to use a prefix or something instead.
    fn hack_resolve_curly_ambiguety(&mut self) -> CurlyResolve {
        let mut parser = self.clone();
        let stopped_at = parser.recover_until(
            |t| {
                [
                    Token::Bar,
                    Token::Tilde,
                    Token::CloseCurly,
                    Token::Comma,
                    Token::Equal,
                ]
                .contains(&t)
            },
            false,
        );
        match stopped_at {
            Token::Tilde => CurlyResolve::WithTilde,
            Token::Bar => CurlyResolve::WithBar,
            _ => CurlyResolve::NoHeader,
        }
    }

    pub fn shared_record<T: CurlyValue<'a>>(&mut self, start: Span) -> Option<Curly<'a, T>> {
        let resolve = self.hack_resolve_curly_ambiguety();
        let init = match resolve {
            CurlyResolve::WithBar => {
                let type_ = self.curly_type(start)?;
                CurlyInit::Construct(type_)
            }
            CurlyResolve::WithTilde => {
                let expr = self.curly_expr(start)?;
                CurlyInit::Modify(expr)
            }
            CurlyResolve::NoHeader => CurlyInit::None,
        };

        let (fields, end) = self.fields()?;

        Some(Curly { span: start.extend(end), init, fields })
    }

    fn curly_expr(&mut self, start: Span) -> Option<Tr<Expr<'a>>> {
        match self.expr() {
            Some(expr) => {
                self.expect(Token::Tilde)?;
                Some(expr)
            }
            None => match self.recover_for([Token::CloseCurly, Token::Tilde], false) {
                Token::Bar => self.next_then(|_| Some(Expr::Poison.tr(start))),
                _ => {
                    let other = self.lexer.peek();
                    self.err_unexpected_token(other, "`|` to modify the record");
                    None
                }
            },
        }
    }

    fn curly_type(&mut self, start: Span) -> Option<Tr<Type<'a>>> {
        self.skip_newlines();
        match self.type_with_params() {
            Some(type_) => {
                self.expect(Token::Bar)?;
                Some(type_)
            }
            None => match self.recover_for([Token::CloseCurly, Token::Bar], false) {
                Token::Tilde => self.next_then(|_| Some(Type::Poison.tr(start))),
                _ => {
                    let other = self.lexer.peek();
                    self.err_unexpected_token(other, "`.` to construct a record");
                    None
                }
            },
        }
    }

    fn fields<T: CurlyValue<'a>>(&mut self) -> Option<(Fields<'a, T>, Span)> {
        let mut fields = vec![];

        // edge-case for empty record
        if self.lexer.peek().0 == Token::CloseCurly {
            let (_, span) = self.lexer.next();
            return Some((fields, span));
        }

        loop {
            let ok = if let Some(field) = self.field() {
                fields.push(field);
                true
            } else {
                let got = self.lexer.peek();
                self.err_unexpected_token(got, "a record field");
                false
            };

            let ok = ok
                && match self.lexer.peek() {
                    (Token::CloseCurly, span) => return self.consume(|_| Some((fields, span))),
                    (Token::Comma, _) => {
                        self.progress();

                        // allow trailing comma
                        if let Some(span) = self.next_is(|t| t == Token::CloseCurly) {
                            break Some((fields, span));
                        }

                        true
                    }
                    got => {
                        self.err_unexpected_token(got, "`,` or `}`");
                        false
                    }
                };

            if !ok {
                match self.recover_for([Token::Comma, Token::CloseCurly], false) {
                    Token::Comma => {}
                    Token::CloseCurly => {
                        break self.consume(|span| Some((fields, span)));
                    }
                    _ => break None,
                }
            }
        }
    }

    fn field<T: CurlyValue<'a>>(&mut self) -> Option<Field<'a, T>> {
        match self.lexer.peek() {
            (Token::Path, span) => {
                self.progress();
                let mut field_path: Vec<Tr<&'a str>> = vec![self.taken(span)];

                loop {
                    match self.lexer.peek() {
                        (Token::Dot, _) => {
                            self.progress();

                            match self.lexer.peek() {
                                (Token::Path, span) => {
                                    self.progress();
                                    let name = self.taken(span);
                                    if !Identifier::parse(*name).unwrap().is_name() {
                                        self.err_unexpected_token(
                                            (Token::Path, span),
                                            "a field name",
                                        );
                                        continue;
                                    }
                                    field_path.push(name);
                                }
                                _ => break,
                            }
                        }
                        _ => break,
                    }
                }

                let bind = select! { self, "" peeked: true;
                    Token::At => {
                        self.lexer.next();
                        let span = self.expect(Token::Path)?;
                        let bind = self.taken(span);
                        self.expect(Token::Equal)?;
                        Some(bind)
                    },
                    Token::Equal => self.consume(|_| None),
                    Token::Comma => return Some(Field::Punned(field_path)),
                    Token::CloseCurly => return Some(Field::Punned(field_path)),
                    _ => return None,
                };

                T::parse(self).map(|value| Field::Assigned { field_path, bind, value })
            }
            _ => T::parse(self).map(Field::Value),
        }
    }

    pub fn shared_list<T: Clone, F>(
        &mut self,
        start: Span,
        mut f: F,
        poison: Option<T>,
    ) -> Option<(Vec<T>, Span)>
    where
        F: FnMut(&mut Parser<'a>) -> Option<T>,
    {
        let mut fields = vec![];

        if let Some(span) = self.next_is(|t| t == Token::CloseList) {
            return Some((fields, span));
        }

        let recovery = [Token::CloseList, Token::Comma];

        loop {
            match f(self) {
                None => {
                    if let Some(p) = poison.clone() {
                        fields.push(p);
                    }

                    match self.recover_for(recovery, false) {
                        Token::Comma => self.progress(),
                        Token::CloseList => {
                            let (_, end) = self.lexer.peek();
                            break Some((fields, end));
                        }
                        _ => {
                            self.err_unmatched(start, "the list");
                            return None;
                        }
                    }
                }
                Some(v) => fields.push(v),
            };

            select! { self, "`,` or `]`", span;
                Token::Comma => {
                    // allow trailing comma
                    if let Some(span) = self.next_is(|t| t == Token::CloseList) {
                        break Some((fields, span))
                    };
                },
                Token::CloseList => break Some((fields, span)),
            }
        }
    }

    pub fn shared_paren<T: Clone, F>(&mut self, mut f: F, poison: T) -> Option<(Vec<T>, Span)>
    where
        F: FnMut(&mut Parser<'a>) -> Option<T>,
    {
        let mut elements = vec![];

        if let Some(span) = self.next_is(|t| t == Token::CloseParen) {
            return Some((elements, span));
        }

        loop {
            let elem = f(self).unwrap_or_else(|| {
                self.recover_for([Token::Comma, Token::CloseParen], false);
                poison.clone()
            });
            elements.push(elem);

            select! { self, "a comma or closing parenthesis", span;
                Token::CloseParen => break Some((elements, span)),
                Token::Comma => continue
            }
        }
    }

    pub fn shared_anot_path(&mut self, mut path: Identifier<'a>) -> Option<AnnotatedPath<'a>> {
        let mut segment = path.segments() - 1;
        let mut for_segments: Vec<(usize, ty::ForallAnnotation<'a>)> = vec![];

        let hack_is_inbetween_segments = |self_: &Self, span: Span| {
            self_.take(span.move_indice(-1)) == "):"
                && matches!(span.following(1).try_get_str(self_.lexer.source()), Some(c) if c.is_ascii())
        };

        loop {
            let annotation = self.forall_annotation()?;
            for_segments.push((segment, annotation));

            match self.lexer.peek() {
                (Token::Operator, span) if hack_is_inbetween_segments(self, span) => {
                    self.progress();
                    let (span, has_more) = match self.lexer.next() {
                        (Token::Path, span) => (span, false),
                        (Token::AnnotatedPath, span) => (span.shortened(1), true),
                        other => unreachable!("hack_is_inbetween_segments should've made sure the next token a valid identifier: {other:?}"),
                    };

                    let next = Identifier::parse(self.take(span)).unwrap();
                    path += &next;

                    if !has_more {
                        break;
                    }

                    segment += 1;
                }
                _ => break,
            }
        }

        Some(AnnotatedPath { for_segments, path })
    }
}

impl<'a, T: fmt::Display> fmt::Display for Field<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Field::Punned(field_path) => field_path.iter().format(".").fmt(f),
            Field::Value(value) => value.fmt(f),
            Field::Assigned { field_path, bind, value } => write!(
                f,
                "{}{} {} {}",
                field_path.iter().format("."),
                if let Some(name) = bind {
                    format!(" {} {}", "@".symbol(), name)
                } else {
                    format!("")
                },
                '='.symbol(),
                value
            ),
        }
    }
}

impl<'a> fmt::Display for AnnotatedPath<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.path.fmt(f)?;
        if self.for_segments.is_empty() {
            return Ok(());
        }

        let op = '('.symbol();
        let cp = ')'.symbol();

        write!(
            f,
            "{op}{}{cp}",
            self.for_segments
                .iter()
                .format_with(" & ", |(segment, forall), f| f(&format_args!(
                    "{} => {}",
                    segment, &forall
                )))
        )
    }
}

impl<'a> fmt::Display for CurlyInit<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CurlyInit::None => Ok(()),
            CurlyInit::Modify(expr) => write!(f, "{expr} {} ", '~'.symbol()),
            CurlyInit::Construct(type_) => write!(f, "{} {} ", type_.type_(), '|'.symbol()),
        }
    }
}
