//! An attempt at parsing generalised token-tree-like structures which can then be converted into
//! expressions, types and patterns.

use bitflags::bitflags;
pub use format::Formatter;
pub use lexer::{Lexer, Token};
use lumina_util::{Identifier, Span, Spanned, Tr};
use std::cmp::Ordering;
use tracing::{trace, warn};

mod construct;
mod fmt;
mod format;
mod lexer;
mod type_fields;

#[cfg(test)]
mod tests;

type Entities<'s> = Vec<Tr<Entity<'s>>>;

#[derive(Clone, Debug)]
pub enum Entity<'s> {
    Commented(&'s str, bool, Box<Tr<Self>>),
    Literal(Literal<'s>),
    Clause(&'s str, &'s str, Option<Box<Tr<Entity<'s>>>>),
    Identifier(Identifier<'s>),

    Symbol(&'s str),
    // a.b
    DotPostfix(Box<[Tr<Self>; 2]>),
    Match(Span, Vec<Tr<Self>>),
    Headers(Vec<(&'s str, Span, Tr<Self>)>),
    Header(&'s str, Span, Box<Tr<Self>>),
    Unary(Span, &'s str, Box<Tr<Self>>),
    // `a b c`. Handles things like function application
    Sequence(Entities<'s>),

    /// Entities which are indented under this header
    IndentBlock(&'s str, Entities<'s>),

    Isolated(Token),
    Missing,
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
    matches: Vec<Option<Span>>,
}

impl<'s> Parser<'s> {
    pub fn new(src: &'s str) -> Self {
        Parser { lexer: Lexer::new(src, true), matches: vec![] }
    }

    fn take(&self, span: Span) -> &'s str {
        span.get_str(self.lexer.source())
    }

    pub fn everything(&mut self) -> Vec<Tr<Entity<'s>>> {
        let mut toplevel: Vec<Tr<Entity<'s>>> = Vec::with_capacity(5);

        loop {
            match self.entity() {
                Err(Token::EOF) => break toplevel,
                Err(t) => {
                    let span = self.lexer.peek().1;
                    panic!("unhandled: {t:?} {}", self.take(span))
                }
                Ok(entity) => toplevel.push(entity),
            }
        }
    }

    fn entity(&mut self) -> Result<Tr<Entity<'s>>, Token> {
        let (token, span) = self.lexer.peek();
        trace!("{} ∈ {:?} {span:?}", self.take(span), token);

        let lhs = match token {
            Token::NewLines => return self.next_then(|this| this.entity()),
            Token::StringLiteral => self.next_then_string_lit(span, Literal::DoubleQuote),
            Token::CharLiteral => self.next_then_string_lit(span, Literal::SingleQuote),
            Token::Int => self.next_then_lit(span, int),
            Token::Float => self.next_then_lit(span, |str| Literal::Float(str.parse().unwrap())),
            Token::LineComment | Token::LineDocComment => {
                self.handle_comment(span, token == Token::LineDocComment, |this| this.entity())?
            }
            Token::OpenParen => self.next_then(|this| this.clause(span, "(", ")")),
            Token::OpenList => self.next_then(|this| this.clause(span, "[", "]")),
            Token::OpenCurly => self.next_then(|this| this.clause(span, "{", "}")),
            Token::Symbol if is_unary_operator(self.lexer.source(), span) => {
                self.next_then(|this| this.unary(span))
            }
            Token::Use => self.next_then_header_and("use"),
            Token::Type => self.next_then_header_and("type"),
            Token::Fn if self.take(span.following(1)) == "(" => {
                self.next_then(|this| this.upgrade_old_fnty_syntax("fn"))
            }
            Token::FnPtr if self.take(span.following(1)) == "(" => {
                self.next_then(|this| this.upgrade_old_fnty_syntax("fnptr"))
            }
            Token::FnPtr => self.next_then(|this| this.fn_pointer(span)),
            Token::Fn => self.next_then_header_and("fn"),
            Token::Pub => self.next_then_header_and("pub"),
            Token::Equal => self.next_then_header_and("="),
            Token::As => self.next_then_header_and("as"),
            Token::Path => self.next_then(|this| this.path(span)),
            Token::Match => self.next_then(|this| this.r#match(span)),
            Token::Arrow => self.next_then_header_and("->"),
            Token::If => self.next_then(|this| this.r#if(span)),
            Token::Do => self.next_then(|this| this.r#do(span)),
            Token::Let => self.next_then(|this| this.r#let(span)),
            Token::Bar if self.matches.is_empty() => {
                self.next_then(|_| Entity::Symbol("|").tr(span))
            }
            Token::Where => self.next_then(|this| this.indent_block("where", span)),
            Token::When => self.next_then(|this| this.indent_block("when", span)),
            Token::Impl => self.next_then(|this| this.indent_block("impl", span)),
            Token::Symbol | Token::Comma | Token::Can | Token::Val => {
                self.lexer.next();
                Entity::Isolated(token).tr(span)
            }
            _ => return Err(token),
        };

        Ok(self.sequence(lhs))
    }

    fn upgrade_old_fnty_syntax(&mut self, kw: &'s str) -> Tr<Entity<'s>> {
        let (token, span) = self.lexer.next();
        assert_eq!(Token::OpenParen, token);
        let Tr { span, value: Entity::Clause("(", ")", and_then) } = self.clause(span, "(", ")")
        else {
            unreachable!();
        };

        let fn_ = Entity::Identifier(Identifier::from_raw(kw)).tr(span);

        let and_then = match and_then {
            Some(in_clause) => vec![fn_, *in_clause],
            None => vec![fn_, Entity::Symbol("->").tr(span)],
        };

        let seq = Entity::Sequence(and_then).tr(span);
        Entity::Clause("(", ")", Some(Box::new(seq))).tr(span)
    }

    fn sequence(&mut self, init: Tr<Entity<'s>>) -> Tr<Entity<'s>> {
        fn merge<'a>(buf: &mut Vec<Tr<Entity<'a>>>, elem: Tr<Entity<'a>>) {
            match elem.value {
                Entity::Sequence(elems) => buf.extend(elems),
                _ => buf.push(elem),
            }
        }

        let mut buf = vec![init];

        loop {
            match self.parameter() {
                // Check for `field.access`
                Some(Tr { value: Entity::Symbol("."), span })
                    if buf.last().unwrap().span.following(1) == span =>
                {
                    let rhs = self.parameter().unwrap_or_else(|| Entity::Missing.tr(span));
                    consume_lhs_to_apply_dotpostifx(&mut buf, rhs);
                }
                Some(v @ Tr { value: Entity::Symbol(_), .. }) if buf.len() > 1 => {
                    let buf = std::mem::take(&mut buf);
                    let lhs = Entity::processed_sequence(buf);
                    return self.operators(vec![lhs, v]);
                }
                Some(v @ Tr { value: Entity::Symbol(_), .. }) if buf.len() == 1 => {
                    buf.push(v);
                    return self.operators(buf);
                }
                Some(v) => merge(&mut buf, v),
                None => break Entity::processed_sequence(buf),
            }
        }
    }

    fn operators(&mut self, mut buf: Vec<Tr<Entity<'s>>>) -> Tr<Entity<'s>> {
        fn take_up_to_operator<'a>(buf: &mut Vec<Tr<Entity<'a>>>) -> Vec<Tr<Entity<'a>>> {
            if buf.is_empty() {
                return vec![];
            }

            let i = buf
                .iter()
                .rev()
                .take_while(|v| !matches!(v.value, Entity::Symbol(name) if ![","].contains(&name)))
                .count();

            if i == buf.len() {
                std::mem::take(buf)
            } else {
                buf.split_off(buf.len() - i - 1)
            }
        }

        loop {
            match self.parameter() {
                Some(Tr { value: Entity::Symbol("."), span })
                    if buf.last().unwrap().span.following(1) == span =>
                {
                    let rhs = self.parameter().unwrap_or_else(|| Entity::Missing.tr(span));
                    consume_lhs_to_apply_dotpostifx(&mut buf, rhs);
                }
                Some(v @ Tr { value: Entity::Symbol(_), .. }) => {
                    let lhs = take_up_to_operator(&mut buf);
                    buf.push(Entity::processed_sequence(lhs));
                    buf.push(v);
                }
                Some(v) => buf.push(v),
                None => {
                    let rhs = take_up_to_operator(&mut buf);
                    buf.push(Entity::processed_sequence(rhs));
                    break Entity::processed_sequence(buf);
                }
            }
        }
    }

    fn handle_comment(
        &mut self,
        span: Span,
        is_doc: bool,
        f: impl FnOnce(&mut Self) -> Result<Tr<Entity<'s>>, Token>,
    ) -> Result<Tr<Entity<'s>>, Token> {
        let reset = self.lexer.clone();
        self.lexer.next();
        match f(self) {
            Err(Token::EOF) => {
                let text = self.take(span).trim_start_matches('/');
                let missing = Box::new(Entity::Missing.tr(span));
                Ok(Entity::Commented(text, false, missing).tr(span))
            }
            Err(err) => {
                self.lexer = reset;
                Err(err)
            }
            Ok(v) => {
                let tspan = span.extend(v.span);
                let text = self.take(span).trim_start_matches('/');
                Ok(Entity::Commented(text, is_doc, Box::new(v)).tr(tspan))
            }
        }
    }

    fn parameter(&mut self) -> Option<Tr<Entity<'s>>> {
        let (token, span) = self.lexer.peek();
        trace!("{} p∈ {:?} {span:?}", self.take(span), token);

        let param = match token {
            Token::NewLines => return self.next_then(|this| this.parameter()),
            Token::StringLiteral => self.next_then_string_lit(span, Literal::DoubleQuote),
            Token::CharLiteral => self.next_then_string_lit(span, Literal::SingleQuote),
            Token::Int => self.next_then_lit(span, int),
            Token::Float => self.next_then_lit(span, |str| Literal::Float(str.parse().unwrap())),
            Token::LineComment | Token::LineDocComment => self
                .handle_comment(span, token == Token::LineDocComment, |this| {
                    this.parameter().ok_or(Token::NewLines)
                })
                .ok()?,
            Token::OpenParen => self.next_then(|this| this.clause(span, "(", ")")),
            Token::OpenList => self.next_then(|this| this.clause(span, "[", "]")),
            Token::OpenCurly => self.next_then(|this| this.clause(span, "{", "}")),
            Token::Fn if self.take(span.following(1)) == "(" => {
                self.next_then(|this| this.upgrade_old_fnty_syntax("fn"))
            }
            Token::FnPtr if self.take(span.following(1)) == "(" => {
                self.next_then(|this| this.upgrade_old_fnty_syntax("fnptr"))
            }
            Token::Symbol
                if is_unary_operator(self.lexer.source(), span)
                    && !self.is_attribute_header(span) =>
            {
                self.next_then(|this| this.unary(span))
            }
            Token::Arrow => self.next_then_header_and("->"),
            Token::Equal => self.next_then_header_and("="),
            // Token::Equal => self.next_then_header_and("="),
            Token::As => self.next_then_header_and("as"),
            Token::Symbol if self.is_attribute_header(span) => return None,
            Token::Symbol | Token::Comma => {
                self.next_then(|this| Entity::Symbol(this.take(span)).tr(span))
            }
            Token::Bar if self.matches.is_empty() => {
                self.next_then(|_| Entity::Symbol("|").tr(span))
            }
            Token::Path => self.next_then(|this| this.path(span)),
            Token::Can => {
                self.lexer.next();
                Entity::Isolated(token).tr(span)
            }
            _ => return None,
        };

        Some(param)
    }

    fn is_attribute_header(&self, span: Span) -> bool {
        self.take(Span::new(span.indice - 1, 1)) == "\n" || self.take(span.following(1)) == "["
    }

    fn next_then_string_lit(
        &mut self,
        span: Span,
        con: fn(&'s str) -> Literal<'s>,
    ) -> Tr<Entity<'s>> {
        self.lexer.next();
        let data = self.take(span.move_indice(1).shortened(1));
        Entity::Literal(con(data)).tr(span)
    }

    fn next_then_lit(&mut self, span: Span, con: fn(&'s str) -> Literal<'s>) -> Tr<Entity<'s>> {
        self.lexer.next();
        let data = self.take(span);
        Entity::Literal(con(data)).tr(span)
    }

    fn unary(&mut self, span: Span) -> Tr<Entity<'s>> {
        let name = self.take(span);

        match self.parameter() {
            Some(param) => {
                let span = span.extend(param.span);
                Entity::Unary(span, name, Box::new(param)).tr(span)
            }
            None => Entity::Symbol(name).tr(span),
        }
    }

    fn fn_pointer(&mut self, span: Span) -> Tr<Entity<'s>> {
        self.headers(span, &[])
    }

    fn path(&mut self, span: Span) -> Tr<Entity<'s>> {
        let ident = Identifier::parse(self.take(span)).unwrap();
        // TODO: TODO: Also check for type annotations here
        Entity::Identifier(ident).tr(span)
    }

    fn r#match(&mut self, span: Span) -> Tr<Entity<'s>> {
        trace!("getting match expression");
        self.matches.push(None);

        let matched = self.entity().expect("ET");

        match self.lexer.peek() {
            (Token::Bar, span) => {
                self.lexer.next();
                assert_eq!(
                    None,
                    std::mem::replace(self.matches.last_mut().unwrap(), Some(span))
                );
            }
            other => todo!("{other:?}"),
        }

        fn is_closest_indent(src: &str, matches: &[Option<Span>], span: Span) -> bool {
            let (x, xs) = matches.split_last().unwrap();
            let x = x.unwrap();

            let atlevel = find_indent_level(src.as_bytes(), span);
            let level = find_indent_level(src.as_bytes(), x);

            is_same_line(src, x, span)
                || level.abs_diff(atlevel)
                    < xs.iter()
                        .map(|y| atlevel.abs_diff(find_indent_level(src.as_bytes(), y.unwrap())))
                        .min()
                        .unwrap_or(u32::MAX)
        }

        let mut elems = vec![matched];
        loop {
            let branch = self.entity().expect("ET");
            elems.push(branch);

            match self.lexer.peek() {
                (Token::Bar, span) => {
                    if !(self.matches.len() == 1
                        || is_closest_indent(self.lexer.source(), &self.matches, span))
                    {
                        break;
                    }
                    self.lexer.next();
                }
                (_, _) => break,
            }
        }

        self.matches.pop().unwrap();

        let span = span.extend_by_params(&elems);
        Entity::Match(span, elems).tr(span)
    }

    fn type_decl(&mut self, span: Span) -> Tr<Entity<'s>> {
        // self.parameter();
        // We need the sum types to be edge-cased
        //
        // Hm... I feel like the ordinary stuff should be able to represent it well.
        //
        // How come we needed this edge-case?
        // can't we just detect whether we're in `match`. And if not; treat `|` a symbol?
        //
        // We already have `matches`
        todo!();
    }

    fn r#if(&mut self, span: Span) -> Tr<Entity<'s>> {
        self.headers(span, &[Token::Then, Token::Else])
    }

    fn r#do(&mut self, span: Span) -> Tr<Entity<'s>> {
        self.headers(span, &[Token::Then])
    }

    fn r#let(&mut self, span: Span) -> Tr<Entity<'s>> {
        self.headers(span, &[Token::In])
    }

    // Parse a tree of headers according to a set of expected followup tokens
    fn headers(&mut self, hspan: Span, stages: &[Token]) -> Tr<Entity<'s>> {
        trace!(
            "checking for more headers after `{}`, checks are: {stages:?}",
            self.take(hspan)
        );

        let header = self.take(hspan);
        let init = self.entity().expect("???");
        let mut headers = [(header, hspan, init)]
            .into_iter()
            .chain(stages.iter().map_while(|followup_header| {
                let (t, hspan) = self.lexer.peek();
                if t == *followup_header {
                    self.lexer.next();
                    let header = self.take(hspan);
                    let and_then = self.entity().expect("???");
                    Some((header, hspan, and_then))
                } else {
                    warn!("skipping followup {followup_header:?} as we encountered {t:?} instead");
                    None
                }
            }))
            .collect::<Vec<_>>();

        let span = hspan.extend(headers.last().unwrap().1);

        if headers.len() == 1 {
            let (name, span, and_then) = headers.remove(0);
            Entity::Header(name, span, Box::new(and_then))
        } else {
            Entity::Headers(headers)
        }
        .tr(span)
    }

    fn next_then_header_and(&mut self, name: &'s str) -> Tr<Entity<'s>> {
        trace!("header_and_then with {name}");

        let (_, span) = self.lexer.next();
        let inner = self.entity().unwrap_or_else(|_| Entity::Missing.tr(span));
        let span = span.extend(inner.span);
        Entity::Header(name, span, Box::new(inner)).tr(span)
    }

    fn next_then<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let t = self.lexer.next();
        trace!("consuming: {t:?}");
        f(self)
    }

    fn clause(&mut self, span: Span, open: &'s str, close: &'s str) -> Tr<Entity<'s>> {
        trace!("clause looking for {close}");

        let inner = self.entity().ok();
        let mut tspan = span;
        if let Some(Tr { span, .. }) = inner {
            tspan = tspan.extend(span);
        }

        match self.lexer.peek() {
            (_, span) if self.take(span) == close => {
                self.lexer.next();
            }
            other => {
                todo!("see if we can try to find it, and then use `Entity::Unknown`?: {other:?}");
            }
        }

        Entity::Clause(open, close, inner.map(Box::new)).tr(tspan)
    }

    fn indent_block(&mut self, kw: &'s str, mut kwspan: Span) -> Tr<Entity<'s>> {
        let ilevel = find_indent_level(self.lexer.source().as_bytes(), kwspan);

        let mut under_block = vec![];

        let mut construct = |under_block: Vec<Tr<Entity<'s>>>| {
            if let Some(entity) = under_block.last() {
                kwspan = kwspan.extend(entity.span);
            }
            Entity::IndentBlock(kw, under_block).tr(kwspan)
        };

        loop {
            let (_, span) = self.lexer.peek();
            let ilev = find_indent_level(self.lexer.source().as_bytes(), span);

            if ilev <= ilevel {
                break construct(under_block);
            }

            match self.entity() {
                Ok(entity) => under_block.push(entity),
                Err(_) => break construct(under_block),
            }
        }
    }
}

fn int<'s>(raw: &'s str) -> Literal<'s> {
    let (sign, n) = if raw.as_bytes()[0] == b'-' {
        (true, raw[1..].parse::<u128>().unwrap())
    } else {
        (false, raw.parse::<u128>().unwrap())
    };
    Literal::Int(sign, n)
}

fn find_indent_level(src: &[u8], span: Span) -> u32 {
    let mut offset: u32 = 0;

    loop {
        match src.get((span.indice - offset) as usize) {
            Some(b'\n') | None => break offset,
            Some(_) => offset += 1,
        }
    }
}

fn is_same_line(src: &str, x: Span, y: Span) -> bool {
    src.as_bytes()[x.indice as usize..=y.indice as usize]
        .iter()
        .all(|c| *c != b'\n')
}

fn is_unary_operator(src: &str, span: Span) -> bool {
    let after = span.following(1).get_str(src);
    ![" ", "\n", "\t"].contains(&after) && span.get_str(src) != "."
}

// NOTE: expects parent to already be skipping comments
fn is_valid_start_of_param(src: &str, t: Token, span: Span) -> bool {
    match t {
        Token::Int
        | Token::Float
        | Token::FnPtr
        | Token::StringLiteral
        | Token::CharLiteral
        | Token::Path
        | Token::OpenList
        | Token::OpenCurly
        | Token::OpenParen => true,
        Token::Symbol => is_unary_operator(src, span),
        _ => false,
    }
}

trait Constructors<'s> {
    fn seq(self, elem: Tr<Entity<'s>>) -> Tr<Entity<'s>>;
}

impl<'s> Constructors<'s> for Tr<Entity<'s>> {
    fn seq(mut self, Tr { value, span }: Tr<Entity<'s>>) -> Tr<Entity<'s>> {
        match &mut self.value {
            Entity::Sequence(elems) => {
                elems.push(Tr { value, span });
                self.span = self.span.extend(span);
                self
            }
            _ => {
                let sspan = self.span;
                Entity::Sequence(vec![self, Tr { value, span }]).tr(sspan.extend(span))
            }
        }
    }
}

impl<'s> Entity<'s> {
    pub fn split_header_seq<const N: usize>(
        &self,
        find: [&str; N],
    ) -> Option<([&[Tr<Entity<'s>>]; N], Tr<&Entity<'s>>)> {
        let mut entity = self.tr(Span::null());
        try_map(find, |name| match entity.value {
            Entity::Sequence(seq) => match &seq.split_last().unwrap() {
                (Tr { value: Entity::Header(n, _, and_then), .. }, seq) if *n == name => {
                    entity = (**and_then).as_ref();
                    Ok(*seq)
                }
                _ => Err(()),
            },
            Entity::Header(n, _, and_then) if *n == name => {
                entity = (**and_then).as_ref();
                Ok([].as_slice())
            }
            _ => Err(()),
        })
        .ok()
        .map(|seq| (seq, entity))
    }

    pub fn processed_sequence(mut entities: Vec<Tr<Entity<'s>>>) -> Tr<Entity<'s>> {
        if entities.len() == 1 {
            return entities.remove(0);
        }
        let span = Span::from_elems(&entities, |v| v.span);
        Entity::Sequence(entities).tr(span)
    }
}

fn consume_lhs_to_apply_dotpostifx<'s>(buf: &mut Vec<Tr<Entity<'s>>>, rhs: Tr<Entity<'s>>) {
    let lhs = buf.pop().unwrap();
    // let rhs = self.parameter().unwrap_or_else(|| Entity::Missing.tr(span));
    let tspan = lhs.span.extend(rhs.span);
    let new = Entity::DotPostfix(Box::new([lhs, rhs])).tr(tspan);
    buf.push(new);
}

// TODO: Remove this when real try_map is stable
pub fn try_map<T, R, E, const N: usize, F>(source: [T; N], mut cb: F) -> Result<[R; N], E>
where
    F: FnMut(T) -> Result<R, E>,
    R: Sized,
{
    use std::mem::MaybeUninit;

    struct Guard<T, const N: usize> {
        dst: [MaybeUninit<T>; N],
        initialized: usize,
    }

    impl<T, const N: usize> Drop for Guard<T, N> {
        fn drop(&mut self) {
            debug_assert!(self.initialized <= N);

            for i in 0..self.initialized {
                unsafe {
                    self.dst[i].assume_init_drop();
                }
            }
        }
    }

    let dst: [MaybeUninit<R>; N] = unsafe { MaybeUninit::uninit().assume_init() };
    let mut guard: Guard<R, N> = Guard { dst, initialized: 0 };

    for (i, s) in source.into_iter().enumerate() {
        match cb(s) {
            Ok(elem) => {
                guard.dst[i].write(elem);
                guard.initialized += 1;
            }
            Err(err) => {
                return Err(err);
            }
        }
    }

    guard.initialized = 0;
    Ok(unsafe { std::mem::transmute_copy::<[MaybeUninit<R>; N], [R; N]>(&guard.dst) })
}
