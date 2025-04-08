use itertools::Itertools;
use lexer::{Lexer, Token, TokenKind};
use lumina_util::{Highlighting, Identifier, Indentation, Span, Spanned, Tr};
use std::collections::HashMap;
use std::{cmp::Ordering, fmt};
use tracing::trace;

const TAB_WIDTH: usize = 2;

mod format;
mod meta;
pub use meta::Meta;
mod regroup;
pub use format::Formatter;
mod block;
mod lexer;
mod operator;
#[cfg(test)]
mod tests;

#[derive(Clone)]
pub struct Parser<'s> {
    pub lexer: Lexer<'s>,
    ctx: Vec<Context<'s>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Context<'s> {
    Header(&'s str),
    Indent(&'s str),
    Clause(&'s str),

    // Used for deciding whether `|` should be indent sensitive or not
    InMatch(Option<Span>),
    Operators(Tr<&'s str>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Entity<'s> {
    SingleQuote(&'s str),
    DoubleQuote(&'s str),

    Header(Tr<&'s str>, Box<Meta<Self>>),
    Headers(Vec<(Tr<&'s str>, Meta<Self>)>),
    Clause(&'s str, &'s str, Box<Meta<Self>>),
    Operators {
        lhs: Box<Meta<Self>>,
        operator: Tr<&'s str>,
        parts: Vec<Meta<Self>>,
    },
    DotAccess {
        lhs: Box<Meta<Self>>,
        field: Tr<&'s str>,
    },
    Unary(Tr<&'s str>, Box<Self>),
    Symbol(&'s str),
    IndentBlock(Tr<&'s str>, Vec<Meta<Self>>),

    Sequence(Vec<Meta<Self>>),

    Int(u128),
    Float(f64),
    Identifier(Identifier<'s>, Annotation<'s>),

    // Comment(&'s str),
    // Commented(&'s str, Box<Meta<Self>>),
    Missing,
    EOF,
}

type Annotation<'s> = HashMap<usize, Meta<Entity<'s>>>;

impl<'s> Parser<'s> {
    pub fn new(src: &'s str) -> Self {
        Self { lexer: Lexer::new(src, true), ctx: vec![] }
    }

    // TODO: we can probably remove `allow_missing` and just use `ctx`
    pub fn next(&mut self, allow_missing: bool) -> Meta<Entity<'s>> {
        let t = self.lexer.peek();
        let src = self.lexer.source();
        trace!("{t:?} : {}", self.take(t.span));

        let entity = match t.kind {
            TokenKind::If => {
                self.lexer.next();
                self.header_chain("if".tr(t.span), &[TokenKind::Then, TokenKind::Else])
            }
            TokenKind::Do => {
                self.lexer.next();
                self.header_chain("do".tr(t.span), &[TokenKind::Then])
            }
            TokenKind::Let => {
                self.lexer.next();
                self.header_chain("let".tr(t.span), &[TokenKind::In])
            }
            TokenKind::Alias => {
                self.lexer.next();
                self.header_chain("alias".tr(t.span), &[TokenKind::Equal])
            }
            TokenKind::Fn | TokenKind::FnPtr if is_outdated_closure_type(src, t.span) => {
                let kind = self.take(t.span);
                self.outdated_closure_type(kind, t.span)
            }
            TokenKind::Fn | TokenKind::FnPtr => {
                self.lexer.next();
                let kind = self.take(t.span).tr(t.span);
                self.header_chain(kind, &[TokenKind::As, TokenKind::Arrow, TokenKind::Equal])
            }
            TokenKind::Type => {
                self.lexer.next();
                self.header_chain("type".tr(t.span), &[TokenKind::Equal])
            }
            TokenKind::Pub => self.next_then(|this| this.header_no_ctx(t.span)),
            TokenKind::Symbol if self.take(t.span) == "\\" => {
                self.lexer.next();
                self.header_chain("\\".tr(t.span), &[TokenKind::Arrow])
            }
            TokenKind::Symbol if is_unary_operator(src, t.span) => {
                let entity = self.next_then(|this| this.unary(t.span));
                let entity = t.union(entity, |_, rhs| rhs.kind);
                return self.followups(entity);
            }
            TokenKind::Symbol
                if ["@", "@!"].contains(&self.take(t.span))
                    && self.take(t.span.following(1)) == "[" =>
            {
                let entity = self.next_then(|this| this.unary(t.span));
                let this = t.union(entity, |_, rhs| rhs.kind);
                return this;
            }
            TokenKind::Val => self.next_then(|this| this.header(t.span)),
            TokenKind::Then => self.handle_header_if_not_expected(&["if", "do"], t.span),
            TokenKind::In => self.handle_header_if_not_expected(&["let"], t.span),
            TokenKind::StringLiteral => {
                self.next_then(|this| this.quote(t.span, Entity::DoubleQuote))
            }
            TokenKind::CharLiteral => {
                self.next_then(|this| this.quote(t.span, Entity::SingleQuote))
            }
            TokenKind::Int => self.next_then(|this| this.int(t.span)),
            TokenKind::EOF => self.next_then(|_| Entity::EOF),
            TokenKind::Use | TokenKind::Equal => self.next_then(|this| this.header(t.span)),
            TokenKind::Match => self.next_then(|this| this.r#match(t.span)),
            TokenKind::OpenParen => {
                self.next_then(|this| this.clause(t.span, TokenKind::CloseParen, ")"))
            }
            TokenKind::OpenList => {
                self.next_then(|this| this.clause(t.span, TokenKind::CloseList, "]"))
            }
            TokenKind::OpenCurly => {
                self.next_then(|this| this.clause(t.span, TokenKind::CloseCurly, "}"))
            }
            TokenKind::Path => self.next_then(|this| this.path(t.span)),
            TokenKind::LineComment | TokenKind::LineDocComment => {
                return self.next(allow_missing);
            }
            TokenKind::When => self.next_then(|this| this.header(t.span)),
            TokenKind::Trait | TokenKind::Impl => self.next_then(|this| this.indent_block(t.span)),
            TokenKind::Where => self.handle_whereblock_if_not_expected(allow_missing, t.span),
            TokenKind::Comma | TokenKind::Symbol | TokenKind::Can => {
                self.handle_operator_if_not_expected(t.kind, t.span)
            }
            _ if allow_missing => Entity::Missing,
            other => todo!("{other:?}"),
        };

        let entity = t.map(|_| entity);

        self.followups(entity)
    }

    fn r#match(&mut self, span: Span) -> Entity<'s> {
        self.ctx.push(Context::InMatch(None));
        let then = self.next(true);
        self.ctx.pop().unwrap();
        Entity::Header("match".tr(span), Box::new(then))
    }

    fn int(&mut self, span: Span) -> Entity<'s> {
        let n = self.take(span).parse().unwrap();
        Entity::Int(n)
    }

    fn float(&mut self, span: Span) -> Entity<'s> {
        self.lexer.next();
        let n = self.take(span).parse().unwrap();
        Entity::Float(n)
    }

    fn field_name(&mut self) -> Option<Meta<&'s str>> {
        let t = self.lexer.peek();
        match t.kind {
            TokenKind::Path => {
                let raw = self.take(t.span);
                let ident = Identifier::parse(raw).unwrap();
                ident.as_name().map(|name| {
                    self.lexer.next();
                    t.map(|_| name)
                })
            }
            _ => None,
        }
    }

    fn followups(&mut self, mut lhs: Meta<Entity<'s>>) -> Meta<Entity<'s>> {
        while let Some(f) = self.get_followup() {
            trace!("attempting to handle followup {f:?} for {lhs}");

            match f {
                Followup::Parameter(param) => {
                    trace!("adding {param} to {lhs}");
                    lhs = Entity::merge(lhs, param);
                }
                Followup::FieldAccess(field) => {
                    lhs = lhs.union(field, |lhs, field| Entity::DotAccess {
                        lhs: Box::new(Meta::n(lhs.kind, lhs.span)),
                        field: field.kind.tr(field.span),
                    });
                }
                Followup::MaybeOperator(name) => match self.handle_operator_followup(lhs, name) {
                    Ok(nlhs) => lhs = nlhs,
                    Err(plhs) => {
                        lhs = plhs;
                        break;
                    }
                },
            }
        }

        lhs
    }

    fn in_ctx<T>(&mut self, ctx: Context<'s>, f: impl FnOnce(&mut Self) -> T) -> T {
        self.ctx.push(ctx);
        let rhs = f(self);
        self.ctx.pop().unwrap();
        rhs
    }

    fn get_followup(&mut self) -> Option<Followup<'s>> {
        let token = self.lexer.peek();
        let src = self.lexer.source();

        match token.kind {
            TokenKind::Symbol if is_field_access(src, token.span) => {
                self.lexer.next();
                let fname = self.field_name().unwrap();
                Some(Followup::FieldAccess(fname))
            }
            TokenKind::Symbol
                if ["@", "@!"].contains(&self.take(token.span))
                    && self.take(token.span.following(1)) == "[" =>
            {
                None
            }
            TokenKind::Symbol if is_unary_operator(src, token.span) => {
                self.lexer.next();
                let op = self.unary(token.span);
                Some(Followup::Parameter(op))
            }
            TokenKind::Arrow => self.header_followup(token.span, &["fn", "fnptr", "\\"]),
            TokenKind::Equal => self.header_followup(token.span, &["fn", "fnptr"]),
            TokenKind::As => self.header_followup(token.span, &["fn", "fnptr"]),
            TokenKind::Where => {
                if self.should_attach_where_bind() {
                    self.lexer.next();
                    let block = self.indent_block(token.span);
                    Some(Followup::Parameter(token.map(|_| block)))
                } else {
                    None
                }
            }
            TokenKind::Comma | TokenKind::Symbol | TokenKind::Can => {
                let op = self.take(token.span).tr(token.span);
                Some(Followup::MaybeOperator(op))
            }
            _ => self.try_parameter(token).map(Followup::Parameter),
        }
    }

    fn header_followup(&mut self, span: Span, blockers: &[&'static str]) -> Option<Followup<'s>> {
        let name = self.take(span);

        if self.is_in_ctx_header(blockers) {
            trace!("skipping `{name}` since we're in one of {blockers:?}");
            return None;
        }

        Some(self.accept_header(name.tr(span)))
    }

    fn should_attach_where_bind(&mut self) -> bool {
        for ctx in self.ctx.iter().rev() {
            match ctx {
                // We don't want this to be as a followup to the expression, we want it to be a
                // followup to the *function*.
                //
                // So; we don't check for `"fn"` here.
                Context::Indent("impl" | "trait") => break,

                // If we're inside a clause then this is an invalid but still an parseable where binding set
                Context::Clause(_) => break,

                Context::Operators(_)
                | Context::InMatch(_)
                | Context::Indent(_)
                | Context::Header(_) => return false,
            }
        }

        true
    }

    fn is_in_ctx_header(&self, check_for: &[&str]) -> bool {
        for ctx in self.ctx.iter().rev() {
            match ctx {
                Context::Header(header) if check_for.contains(header) => return true,
                Context::Header("let") => break,
                Context::Operators(_) | Context::InMatch(None) | Context::Header(_) => continue,
                Context::Indent(_) | Context::Clause(_) | Context::InMatch(_) => break,
            }
        }

        false
    }

    fn accept_header(&mut self, name: Tr<&'s str>) -> Followup<'s> {
        let token = self.lexer.next();
        self.ctx.push(Context::Header(*name));
        let rhs = self.next(true);
        let header = token.map(|_| Entity::Header(name, Box::new(rhs)));
        self.ctx.pop().unwrap();
        Followup::Parameter(header)
    }

    fn try_parameter(&mut self, token: Token) -> Option<Meta<Entity<'s>>> {
        trace!("param {token:?} : {}", self.take(token.span));

        let param = match token.kind {
            TokenKind::StringLiteral => {
                self.next_then(|this| this.quote(token.span, Entity::DoubleQuote))
            }
            TokenKind::CharLiteral => {
                self.next_then(|this| this.quote(token.span, Entity::SingleQuote))
            }
            TokenKind::Int => self.next_then(|this| this.int(token.span)),
            TokenKind::Float => self.float(token.span),
            TokenKind::OpenParen => {
                self.next_then(|this| this.clause(token.span, TokenKind::CloseParen, ")"))
            }
            TokenKind::OpenList => {
                self.next_then(|this| this.clause(token.span, TokenKind::CloseList, "]"))
            }
            TokenKind::OpenCurly => {
                self.next_then(|this| this.clause(token.span, TokenKind::CloseCurly, "}"))
            }
            TokenKind::Symbol if is_unary_operator(self.lexer.source(), token.span) => {
                return Some(self.unary(token.span));
            }
            TokenKind::Path => self.next_then(|this| this.path(token.span)),
            TokenKind::Fn if is_outdated_closure_type(self.lexer.source(), token.span) => {
                self.outdated_closure_type("fn", token.span)
            }
            TokenKind::FnPtr if is_outdated_closure_type(self.lexer.source(), token.span) => {
                self.outdated_closure_type("fnptr", token.span)
            }
            _ => return None,
        };

        let mut param = token.map(|_| param);

        let after = self.lexer.peek();
        match after.kind {
            TokenKind::Symbol => {
                let name = Meta::new(self.take(after.span), after.span, after.comment);
                if operator::PARAM_BINDED.contains(&name)
                    && self.take(after.span.extend_length(1)) != "@["
                    && self.take(after.span.extend_length(2)) != "@!["
                {
                    self.lexer.next();
                    param = self.handle_parameter_operator(param, name);
                }
            }
            _ => {}
        }

        Some(param)
    }

    fn outdated_closure_type(&mut self, name: &'s str, kwspan: Span) -> Entity<'s> {
        self.lexer.next(); // Skip `fn` or `fnptr`

        let token = self.lexer.next();
        assert_eq!(TokenKind::OpenParen, token.kind);

        // We use context as-if the syntax is the modern variant. Is this a bad idea?
        self.ctx.push(Context::Clause("("));
        self.ctx.push(Context::Header(name));
        let inner = self.header_chain_no_ctx(
            name.tr(token.span),
            &[TokenKind::As, TokenKind::Arrow, TokenKind::Equal],
        );
        assert_eq!(Some(Context::Header(name)), self.ctx.pop());
        assert_eq!(Some(Context::Clause("(")), self.ctx.pop());

        let after = self.lexer.next();
        if after.kind != TokenKind::CloseParen {
            todo!("recover for `)` same way as clause: {after:?}");
        }
        Entity::Clause("(", ")", Box::new(token.map(|_| inner)))
    }

    fn clause(&mut self, span: Span, close: TokenKind, closen: &'s str) -> Entity<'s> {
        trace!("entering {}{closen} clause", self.take(span));
        let open = self.take(span);

        {
            let token = self.lexer.peek();
            if token.kind == close {
                self.lexer.next();
                return Entity::Clause(open, closen, Box::new(Meta::n(Entity::Missing, span)));
            }
        }

        self.ctx.push(Context::Clause(open));
        let mut inner = self.next(false);

        loop {
            let token = self.lexer.peek();
            if token.kind == close {
                self.lexer.next();
                self.ctx.pop().unwrap();
                if !token.comment.is_null() {
                    inner =
                        Entity::merge(inner, Meta::new(Entity::Missing, token.span, token.comment));
                }
                return Entity::Clause(open, closen, Box::new(inner));
            } else {
                todo!("find the next `{closen}` at all cost: {token:?}");
                // construct a sequence for inner instead
                //
                // have Entity::Unmatched if it doesn't exist (or i guess just use Entity::Symbol)
            }
        }
    }

    fn path(&mut self, span: Span) -> Entity<'s> {
        let (mut segments, mut anotation) = (Vec::new(), HashMap::new());
        self.path_next(span, &mut segments, &mut anotation);
        let ident = Identifier::from_segments(segments);
        Entity::Identifier(ident, anotation)
    }

    fn path_next(&mut self, nspan: Span, path: &mut Vec<&'s str>, anot: &mut Annotation<'s>) {
        let name = self.take(nspan);
        path.push(name);

        let token = self.lexer.peek();

        match token.kind {
            TokenKind::OpenParen if nspan.following(1) == token.span => {
                self.lexer.next();
                self.annotation(token.span, path.len() - 1, anot);
            }
            _ => {}
        };

        let token = self.lexer.peek();

        match token.kind {
            TokenKind::Symbol
                if self.take(token.span) == ":"
                    && self.take(token.span.move_indice(-1)) != " :"
                    && self.take(token.span.move_indice(1)) != ": " =>
            {
                let time_machine = self.clone();

                self.lexer.next();
                let token = self.lexer.next();
                match token.kind {
                    TokenKind::Path => self.path_next(token.span, path, anot),
                    _ => *self = time_machine,
                }
            }
            _ => {}
        }
    }

    fn annotation(&mut self, span: Span, depth: usize, anot: &mut Annotation<'s>) {
        match self.clause(span, TokenKind::CloseParen, ")") {
            Entity::Clause(_, _, inner) => assert!(anot.insert(depth, *inner).is_none()),
            _ => panic!("non-clause returnt by clause tree parser"),
        }
    }

    fn quote<T>(&mut self, span: Span, con: impl FnOnce(&'s str) -> T) -> T {
        let text = self.take(Span::new(span.indice + 1, span.length - 2));
        con(text)
    }

    fn next_then<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.lexer.next();
        f(self)
    }

    fn header(&mut self, span: Span) -> Entity<'s> {
        let name = self.take(span).tr(span);
        self.ctx.push(Context::Header(*name));
        let then = self.next(false);
        self.ctx.pop().unwrap();
        Entity::Header(name, Box::new(then))
    }

    fn header_no_ctx(&mut self, span: Span) -> Entity<'s> {
        let name = self.take(span).tr(span);
        let then = self.next(false);
        Entity::Header(name, Box::new(then))
    }

    fn handle_header_if_not_expected(
        &mut self,
        can_belong_to: &[&'static str],
        span: Span,
    ) -> Entity<'s> {
        for ctx in self.ctx.iter().rev() {
            match ctx {
                Context::Operators(_) => {}
                Context::Clause(_) => break, // treat as unexpected if the most recent relevant was a clause
                Context::Header(name) if can_belong_to.contains(name) => return Entity::Missing,
                Context::Indent(_) | Context::Header(_) | Context::InMatch(_) => {}
            }
        }

        self.lexer.next();

        let and_then = self.in_context(Context::Header("then"), |this| this.next(false));

        let header = self.take(span).tr(span);

        Entity::Header(header, Box::new(and_then))
    }

    fn header_chain_no_ctx(&mut self, kw: Tr<&'s str>, more: &[TokenKind]) -> Entity<'s> {
        let and_then = self.next(false);

        let mut headers = vec![(kw, and_then)];

        let finalize = |headers: Vec<(Tr<&'s str>, Meta<Entity<'s>>)>| {
            trace!("finalizing header chain for {kw}");
            return Entity::Headers(headers);
        };

        for expected in more {
            let token = self.lexer.peek();
            if token.kind == *expected {
                self.lexer.next();
                let header = self.take(token.span).tr(token.span);
                let rhs = self.next(false);
                headers.push((header, rhs));
            } else {
                // If there's no other reasonable thing for this header to belong to;
                // then assume it belongs to the current context even if it's in a different order.
                for ctx in self.ctx.iter().rev().skip(1) {
                    match ctx {
                        Context::Header(_) => {
                            return finalize(headers);
                        }
                        Context::InMatch(_) | Context::Clause(_) | Context::Indent(_) => break,
                        Context::Operators(_) => continue,
                    }
                }
            }
        }

        finalize(headers)
    }

    fn header_chain(&mut self, kw: Tr<&'s str>, more: &[TokenKind]) -> Entity<'s> {
        trace!("going into chain of {kw} {more:?}");
        self.ctx.push(Context::Header(*kw));
        let entity = self.header_chain_no_ctx(kw, more);
        assert_eq!(Some(Context::Header(*kw)), self.ctx.pop());
        entity
    }

    fn in_context<T>(&mut self, ctx: Context<'s>, then: impl FnOnce(&mut Self) -> T) -> T {
        self.ctx.push(ctx);
        let v = then(self);
        self.ctx.pop();
        v
    }

    fn take(&self, span: Span) -> &'s str {
        span.get_str(self.lexer.source())
    }
}

impl<'s> Entity<'s> {
    fn merge(lhs: Meta<Entity<'s>>, rhs: Meta<Entity<'s>>) -> Meta<Entity<'s>> {
        lhs.union(rhs, |lhs, rhs| {
            let mut elems = match lhs.kind {
                Entity::Sequence(elems) => elems,
                _ => vec![lhs],
            };

            match rhs.kind {
                Entity::Sequence(relems) => elems.extend(relems),
                _ => elems.push(rhs),
            }

            Entity::Sequence(elems)
        })
    }
}

fn find_match_context<'a, 's: 'a>(iter: impl Iterator<Item = &'a mut Context<'s>>) -> Option<Span> {
    for ctx in iter {
        match ctx {
            Context::Clause(_) | Context::Indent(_) => break,
            Context::InMatch(span) => return Some(span.unwrap()),
            // TODO: Rather unsure about these
            Context::Operators(_) | Context::Header("->") => {}
            Context::Header(_) => break,
        }
    }

    None
}

#[derive(Debug)]
pub struct AmbigiousIndentation {
    pub previous: [Span; 2],
    pub span: Span,
}

#[derive(Debug)]
enum IndentCheck {
    NotAtStartOfLine,
    Ambigious(AmbigiousIndentation),
    DoEat,
    DoNotEat,
}

fn get_indent_level(src: &str, span: Span, ban_other_characters: bool) -> Option<usize> {
    let mut span_indent = 0;

    for c in src.as_bytes()[..span.indice as usize].iter().rev() {
        match c {
            b' ' => span_indent += 1,
            b'\t' => span_indent += TAB_WIDTH,
            b'\n' => break,
            _ if ban_other_characters => return None,
            _ => span_indent = 0,
        }
    }

    Some(span_indent)
}

fn is_unary_operator(src: &str, span: Span) -> bool {
    let valid_space = || span.following(1).get_str(src) != " ";

    match span.get_str(src) {
        name if name.chars().all(|c| "#*!-".contains(c)) && valid_space() => true,
        _ => false,
    }
}

fn is_field_access(src: &str, span: Span) -> bool {
    span.get_str(src) == "."
        && span
            .following(1)
            .get_str(src)
            .chars()
            .next()
            .unwrap()
            .is_ascii_alphabetic()
}

fn is_outdated_closure_type(src: &str, span: Span) -> bool {
    span.following(1).get_str(src) == "("
}

#[derive(Debug)]
enum Followup<'s> {
    Parameter(Meta<Entity<'s>>),
    FieldAccess(Meta<&'s str>),
    MaybeOperator(Tr<&'s str>),
}

impl<'s> fmt::Display for Entity<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Entity::SingleQuote(str) => write!(f, "\"{str}\""),
            Entity::DoubleQuote(str) => write!(f, "'{str}'"),
            Entity::Unary(op, rhs) => write!(f, "{op}{rhs}"),
            Entity::DotAccess { lhs, field } => write!(f, "{lhs}.{field}"),
            Entity::Header(header, then) => {
                write!(f, "Header({header})\n  {}", then.indent())
            }
            Entity::IndentBlock(name, entities) => {
                write!(f, "Indent({name})")?;
                for entity in entities {
                    write!(f, "\n  {}", entity.indent())?;
                }
                Ok(())
            }
            Entity::Symbol(name) => write!(f, "Symbol({name})"),
            // Entity::Comment(text) => write!(f, "//{text}"),
            // Entity::Commented(text, then) => write!(f, "//{text}\n{then}"),
            Entity::Sequence(elems) => {
                dbg!(&elems);
                write!(
                    f,
                    "[ {}\n]",
                    elems.iter().map(|elem| elem.indent()).format("\n, ")
                )
            }
            Entity::Operators { lhs, operator, parts } => {
                write!(f, "Operators({})\n  {}", parts.len(), lhs.indent())?;
                for (i, rhs) in parts.iter().enumerate() {
                    write!(f, "\nOp({i}, {operator})\n  {}", rhs.indent())?;
                }
                Ok(())
            }
            Entity::Identifier(ident, anot) if anot.is_empty() => write!(f, "{ident}"),
            Entity::Identifier(ident, anot) => {
                let mut segments = ident.as_slice().into_iter().enumerate();
                let (i, x) = segments.next().unwrap();

                x.fmt(f)?;

                let add_annotation = |f: &mut fmt::Formatter, i: usize| {
                    if let Some(assign) = anot.get(&i) {
                        let assign = assign.to_string();
                        write!(f, "(\n  {}\n)", assign.indent())?;
                    }
                    Ok(())
                };

                add_annotation(f, i)?;

                for (i, segment) in segments {
                    add_annotation(f, i)?;
                    write!(f, ":{segment}").unwrap();
                }

                Ok(())
            }
            Entity::Headers(vec) if vec.len() == 1 => {
                write!(f, "Header({})\n  {}", &vec[0].0, vec[0].1.indent())
            }
            Entity::Headers(vec) => {
                write!(f, "{}\n  {}", vec[0].0, vec[0].1.indent())?;
                for (header, then) in &vec[1..] {
                    write!(f, "\n{header}\n  {}", then.indent())?;
                }
                Ok(())
            }
            Entity::Clause(open, close, inner) => {
                write!(f, "Clause({open})\n  {}\n{close}", inner.indent())
            }
            Entity::Int(n) => n.fmt(f),
            Entity::Float(n) => n.fmt(f),
            Entity::Missing => Ok(()),
            Entity::EOF => write!(f, "<EOF>"),
        }
    }
}
