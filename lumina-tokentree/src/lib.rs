use itertools::Itertools;
use lexer::{Lexer, Token};
use lumina_util::{Identifier, Indentation, Span, Spanned, Tr};
use std::collections::HashMap;
use std::{cmp::Ordering, fmt};
use tracing::trace;

const TAB_WIDTH: usize = 2;

mod format;
mod regroup;
pub use format::Formatter;
mod lexer;

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

    Header(Tr<&'s str>, Box<Tr<Self>>),
    Headers(Vec<(Tr<&'s str>, Tr<Self>)>),
    Clause(&'s str, &'s str, Box<Tr<Self>>),
    Operators {
        lhs: Box<Tr<Self>>,
        operator: Tr<&'s str>,
        parts: Vec<Tr<Self>>,
    },
    DotAccess {
        lhs: Box<Tr<Self>>,
        field: Tr<&'s str>,
    },
    Unary(Tr<&'s str>, Box<Tr<Self>>),
    Symbol(&'s str),
    IndentBlock(Tr<&'s str>, Vec<Tr<Self>>),

    Sequence(Vec<Tr<Self>>),

    Int(u128),
    Float(f64),
    Identifier(Identifier<'s>, Annotation<'s>),

    Comment(&'s str),
    Commented(&'s str, Box<Tr<Self>>),
    Missing,
    EOF,
}

type Annotation<'s> = HashMap<usize, Tr<Entity<'s>>>;

#[derive(Clone)]
pub struct Parser<'s> {
    pub lexer: Lexer<'s>,
    ctx: Vec<Context<'s>>,
}

impl<'s> Parser<'s> {
    pub fn new(src: &'s str) -> Self {
        Self { lexer: Lexer::new(src, true), ctx: vec![] }
    }

    // TODO: we can probably remove `allow_missing` and just use `ctx`
    pub fn next(&mut self, allow_missing: bool) -> Tr<Entity<'s>> {
        let (token, span) = self.lexer.peek();
        trace!("{token:?} : {}", self.take(span));

        let entity = match token {
            Token::If => {
                self.lexer.next();
                self.header_chain("if".tr(span), &[Token::Then, Token::Else])
            }
            Token::Do => {
                self.lexer.next();
                self.header_chain("do".tr(span), &[Token::Then])
            }
            Token::Let => {
                self.lexer.next();
                self.header_chain("let".tr(span), &[Token::In])
            }
            Token::Alias => {
                self.lexer.next();
                self.header_chain("alias".tr(span), &[Token::Equal])
            }
            Token::Fn | Token::FnPtr if is_outdated_closure_type(self.lexer.source(), span) => {
                let kind = self.take(span);
                self.outdated_closure_type(kind, span)
            }
            Token::Fn | Token::FnPtr => {
                self.lexer.next();
                let kind = self.take(span).tr(span);
                self.header_chain(kind, &[Token::As, Token::Arrow, Token::Equal])
            }
            Token::Type => {
                self.lexer.next();
                self.header_chain("type".tr(span), &[Token::Equal])
            }
            Token::Pub => self.next_then(|this| this.header_no_ctx(span)),
            Token::Symbol if self.take(span) == "\\" => {
                self.lexer.next();
                self.header_chain("\\".tr(span), &[Token::Arrow])
            }
            Token::Symbol if is_unary_operator(self.lexer.source(), span) => self.unary(span),
            Token::Symbol
                if ["@", "@!"].contains(&self.take(span))
                    && self.take(span.following(1)) == "[" =>
            {
                self.unary(span)
            }
            Token::Val => self.next_then(|this| this.header(span)),
            Token::Then => self.handle_header_if_not_expected(&["if", "do"], span),
            Token::In => self.handle_header_if_not_expected(&["let"], span),
            Token::StringLiteral => self.next_then(|this| this.quote(span, Entity::DoubleQuote)),
            Token::CharLiteral => self.next_then(|this| this.quote(span, Entity::SingleQuote)),
            Token::Int => self.next_then(|this| this.int(span)),
            Token::EOF => Entity::EOF.tr(span),
            Token::Use | Token::Equal => self.next_then(|this| this.header(span)),
            Token::Match => self.next_then(|this| this.r#match(span)),
            Token::OpenParen => self.next_then(|this| this.clause(span, Token::CloseParen, ")")),
            Token::OpenList => self.next_then(|this| this.clause(span, Token::CloseList, "]")),
            Token::OpenCurly => self.next_then(|this| this.clause(span, Token::CloseCurly, "}")),
            Token::Path => self.next_then(|this| this.path(span)),
            Token::LineComment | Token::LineDocComment => {
                self.lexer.next();
                let text = self.take(span.move_indice(2));
                let under_comment = self.next(allow_missing);
                Entity::Commented(text, Box::new(under_comment)).tr(span)
            }
            Token::When => self.next_then(|this| this.header(span)),
            Token::Trait | Token::Impl => self.next_then(|this| this.indent_block(span)),
            Token::Where => self.handle_whereblock_if_not_expected(allow_missing, span),
            Token::Comma | Token::Symbol | Token::Can => {
                self.handle_operator_if_not_expected(token, span)
            }
            _ if allow_missing => Entity::Missing.tr(span),
            other => todo!("{other:?}"),
        };

        self.followups(entity)
    }

    fn unary(&mut self, span: Span) -> Tr<Entity<'s>> {
        self.lexer.next();

        let (token, pspan) = self.lexer.peek();

        let init = self
            .try_parameter(token, pspan)
            .unwrap_or_else(|| Entity::Missing.tr(span));

        (0..span.length as u32).rev().fold(init, |next, i| {
            let span = Span::new(span.indice + i, 1);
            let name = self.take(span).tr(span);
            let tspan = name.span.extend(next.span);
            Entity::Unary(name, Box::new(next)).tr(tspan)
        })
    }

    fn handle_whereblock_if_not_expected(
        &mut self,
        allow_missing: bool,
        span: Span,
    ) -> Tr<Entity<'s>> {
        for ctx in self.ctx.iter().rev() {
            match ctx {
                Context::Header("fn" | "fnptr") => {
                    return self.next_then(|this| this.indent_block(span))
                }
                // If we're inside a clause then this is an invalid but still an parseable where binding set
                Context::Clause(_) => return self.next_then(|this| this.indent_block(span)),

                Context::Operators(_)
                | Context::InMatch(_)
                | Context::Indent(_)
                | Context::Header(_) => break,
            }
        }

        if !allow_missing {
            return self.next_then(|this| this.indent_block(span));
        }

        assert!(allow_missing);
        Entity::Missing.tr(span)
    }

    fn indent_block(&mut self, span: Span) -> Tr<Entity<'s>> {
        let name = self.take(span);
        self.ctx.push(Context::Indent(name));
        let src = self.lexer.source();

        let mut buf = vec![];

        let finalize = |this: &mut Self, buf| {
            assert!(matches!(this.ctx.pop().unwrap(), Context::Indent(..)));
            return Entity::IndentBlock(name.tr(span), buf).tr(span);
        };

        loop {
            let (token, nspan) = self.lexer.peek();

            if token == Token::EOF {
                return finalize(self, buf);
            }

            let where_indent = get_indent_level(src, span, false).unwrap();
            if let Some(indent) = get_indent_level(src, nspan, true) {
                if indent <= where_indent {
                    return finalize(self, buf);
                }
            }
            // No need to handle `None` because we allow `where fn ...`

            let declaration = self.next(false);
            match &declaration.value {
                Entity::EOF => todo!(),
                _ => {}
            }
            buf.push(declaration);
        }
    }

    fn r#match(&mut self, span: Span) -> Tr<Entity<'s>> {
        self.ctx.push(Context::InMatch(None));
        let then = self.next(true);
        self.ctx.pop().unwrap();
        let tspan = span.extend(then.span);
        Entity::Header("match".tr(span), Box::new(then)).tr(tspan)
    }

    fn int(&mut self, span: Span) -> Tr<Entity<'s>> {
        let n = self.take(span).parse().unwrap();
        Entity::Int(n).tr(span)
    }

    fn float(&mut self, span: Span) -> Tr<Entity<'s>> {
        self.lexer.next();
        let n = self.take(span).parse().unwrap();
        Entity::Float(n).tr(span)
    }

    fn field_name(&mut self) -> Option<Tr<&'s str>> {
        match self.lexer.peek() {
            (Token::Path, span) => {
                let raw = self.take(span);
                let ident = Identifier::parse(raw).unwrap();
                ident.as_name().map(|name| {
                    self.lexer.next();
                    name.tr(span)
                })
            }
            _ => None,
        }
    }

    fn followups(&mut self, entity: Tr<Entity<'s>>) -> Tr<Entity<'s>> {
        let (token, span) = self.lexer.peek();

        trace!(
            "checking if {token:?} can be used as followup for lhs\n{entity}\nctx:\n{:?}",
            self.ctx
        );

        match token {
            Token::Symbol if is_field_access(self.lexer.source(), span) => {
                self.lexer.next();
                let entity = match self.field_name() {
                    None => add_parameter(entity, Entity::Symbol(".").tr(span)),
                    Some(field) => {
                        let tspan = entity.span.extend(field.span);
                        Entity::DotAccess { lhs: Box::new(entity), field }.tr(tspan)
                    }
                };
                self.followups(entity)
            }
            Token::Symbol if is_unary_operator(self.lexer.source(), span) => {
                let parameter = self.unary(span);
                let entity = add_parameter(entity, parameter);
                self.followups(entity)
            }
            Token::Symbol
                if ["@", "@!"].contains(&self.take(span))
                    && self.take(span.following(1)) == "[" =>
            {
                // TODO: we could fairly easily allow per-expr attributes here
                return entity;
            }
            Token::Comma | Token::Symbol | Token::Bar | Token::Can => {
                let op = self.take(span).tr(span);

                match self.operator(entity, op) {
                    Ok(operator) => self.followups(operator),
                    Err(unchanged) => unchanged,
                }
            }
            Token::LineDocComment | Token::LineComment => {
                // I'm pretty sure the comment buffer is the best solution.
                //
                // *BUT*; I also think the new code with followup enum is a lot prettier. So; I
                // kind of want to try to make that work.
                //
                // BUT; I never figured out why it didn't work...
                //
                // It did make me realise that my original code was also pretty sus.
                todo!("OK SO: ");

                // let reset = self.clone();

                // if self.followup_should_eat_comment(entity.span, span) {
                //     let text = self.take(span.move_indice(2));
                //     let entity = add_parameter(entity, Entity::Comment(text).tr(span));
                //     self.lexer.next();
                //     self.followups(entity)
                // } else {
                //     entity
                // }
            }
            Token::Arrow => self.arrow(span, entity),
            Token::Equal => self.equal(span, entity),
            Token::As => self.cast_as(span, entity),
            Token::Where => self.r#where(span, entity),
            _ => self.followups_try_parameter(token, span, entity),
        }
    }

    // HACK: parse ahead to figure out what this comment belongs to
    // fn followup_should_eat_comment(&self, at: Span, span: Span) -> bool {
    //     let src = self.lexer.source();
    //     // let mut lexer = self.lexer.clone();
    //     // let (_, aspan) = lexer.next();

    //     let mut ilevel = 0;
    //     for ctx in self.ctx.iter() {
    //         match ctx {
    //             Context::Indent(_) => ilevel += 2,
    //             _ => break,
    //         }
    //     }

    //     let alevel = get_indent_level(src, at, false).unwrap();
    //     let clevel = get_indent_level(src, span, false).unwrap();
    //     // let ilevel = get_indent_level(src, aspan, false).unwrap();
    //     // dbg!(clevel, ilevel, clevel + 1 > ilevel);

    //     dbg!(alevel, ilevel, clevel);

    //     if clevel <= ilevel {
    //         trace!("comment matches next item, ignoring");
    //         return false;
    //     }

    //     todo!("ok no, i think the `self.comments` buffer is a better idea. ");
    //     a

    //     println!("!!! {} ???", self.take(at));

    //     (alevel as isize).abs_diff(clevel as isize) < 2
    // }

    fn followups_try_parameter(
        &mut self,
        token: Token,
        span: Span,
        entity: Tr<Entity<'s>>,
    ) -> Tr<Entity<'s>> {
        match self.try_parameter(token, span) {
            Some(parameter) => {
                let entity = add_parameter(entity, parameter);
                self.followups(entity)
            }
            None => entity,
        }
    }

    fn is_not_in_item_ctx(&self) -> bool {
        for ctx in self.ctx.iter().rev() {
            match ctx {
                Context::Header(_) => continue,
                Context::Indent("trait" | "impl" | "where") => return true,
                Context::InMatch(Some(_)) => return false,
                Context::Operators(_) => continue,
                Context::Clause(_) | Context::InMatch(None) | Context::Indent(..) => return false,
            }
        }

        true
    }

    fn r#where(&mut self, span: Span, lhs: Tr<Entity<'s>>) -> Tr<Entity<'s>> {
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
                | Context::Header(_) => return lhs,
            }
        }

        self.next_then(|this| {
            let block = this.indent_block(span);
            Entity::merge(lhs, block)
        })
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

    fn accept_header(&mut self, name: Tr<&'s str>, lhs: Tr<Entity<'s>>) -> Tr<Entity<'s>> {
        self.lexer.next();
        self.ctx.push(Context::Header(*name));
        let rhs = self.next(true);
        let rspan = name.span.extend(rhs.span);
        let header = Entity::Header(name, Box::new(rhs)).tr(rspan);
        let applied = Entity::merge(lhs, header);
        self.ctx.pop().unwrap();
        self.followups(applied)
    }

    fn equal(&mut self, span: Span, lhs: Tr<Entity<'s>>) -> Tr<Entity<'s>> {
        if self.is_in_ctx_header(&["fn", "fnptr"]) {
            return lhs;
        }

        self.accept_header("=".tr(span), lhs)
    }

    fn cast_as(&mut self, span: Span, lhs: Tr<Entity<'s>>) -> Tr<Entity<'s>> {
        if self.is_in_ctx_header(&["fn", "fnptr"]) {
            return lhs;
        }

        self.accept_header("as".tr(span), lhs)
    }

    fn arrow(&mut self, span: Span, lhs: Tr<Entity<'s>>) -> Tr<Entity<'s>> {
        if self.is_in_ctx_header(&["fn", "fnptr", "\\"]) {
            return lhs;
        }

        self.accept_header("->".tr(span), lhs)
    }

    fn try_parameter(&mut self, token: Token, span: Span) -> Option<Tr<Entity<'s>>> {
        trace!("param {token:?} : {}", self.take(span));

        let param = match token {
            Token::StringLiteral => self.next_then(|this| this.quote(span, Entity::DoubleQuote)),
            Token::CharLiteral => self.next_then(|this| this.quote(span, Entity::SingleQuote)),
            Token::Int => self.next_then(|this| this.int(span)),
            Token::Float => self.float(span),
            Token::OpenParen => self.next_then(|this| this.clause(span, Token::CloseParen, ")")),
            Token::OpenList => self.next_then(|this| this.clause(span, Token::CloseList, "]")),
            Token::OpenCurly => self.next_then(|this| this.clause(span, Token::CloseCurly, "}")),
            Token::Symbol if is_unary_operator(self.lexer.source(), span) => self.unary(span),
            Token::Path => self.next_then(|this| this.path(span)),
            Token::LineComment | Token::LineDocComment => {
                let text = self.take(span.move_indice(2));
                let time_machine = self.lexer.clone();
                self.lexer.next();
                let (token, pspan) = self.lexer.peek();
                match self.try_parameter(token, pspan) {
                    Some(under_comment) => {
                        Entity::Commented(text, Box::new(under_comment)).tr(span)
                    }
                    None => {
                        self.lexer = time_machine;
                        return None;
                    }
                }
            }
            Token::Fn if is_outdated_closure_type(self.lexer.source(), span) => {
                self.outdated_closure_type("fn", span)
            }
            Token::FnPtr if is_outdated_closure_type(self.lexer.source(), span) => {
                self.outdated_closure_type("fnptr", span)
            }
            _ => return None,
        };

        Some(param)
    }

    fn outdated_closure_type(&mut self, name: &'s str, kwspan: Span) -> Tr<Entity<'s>> {
        self.lexer.next(); // Skip `fn` or `fnptr`
        self.lexer.next(); // Skip `(`
                           // TODO: There's no logical reason for why the lexer doesn't progress past the open parenthesis
                           // here. But for some reaosn it isn't. So; We're just calling next twice.
        let (token, span) = self.lexer.next();
        assert_eq!(Token::OpenParen, token);

        // We use context as-if the syntax is the modern variant. Is this a bad idea?
        self.ctx.push(Context::Clause("("));
        self.ctx.push(Context::Header(name));
        let inner =
            self.header_chain_no_ctx(name.tr(span), &[Token::As, Token::Arrow, Token::Equal]);
        assert_eq!(Some(Context::Header(name)), self.ctx.pop());
        assert_eq!(Some(Context::Clause("(")), self.ctx.pop());

        let after = self.lexer.next();
        if after.0 != Token::CloseParen {
            todo!("recover for `)` same way as clause: {after:?}");
        }
        let tspan = kwspan.extend(after.1);
        Entity::Clause("(", ")", Box::new(inner)).tr(tspan)
    }

    fn operator(
        &mut self,
        lhs: Tr<Entity<'s>>,
        operator: Tr<&'s str>,
    ) -> Result<Tr<Entity<'s>>, Tr<Entity<'s>>> {
        if (*operator == "@" || *operator == "@!") && self.is_not_in_item_ctx() {
            trace!("when lhs:\n{lhs}\n  deciding to REJECT operator {operator}");
            return Err(lhs);
        }

        let mut ctx_iter = self.ctx.iter_mut().rev();

        while let Some(ctx) = ctx_iter.next() {
            match ctx {
                // Use indentation rules to group nested `match`
                Context::InMatch(previous) if *operator == "|" => match previous {
                    &mut Some(recent) => match find_match_context(ctx_iter) {
                        // There's nested match expressions.
                        // Compare indentation to see which this should belong to.
                        Some(upper) => {
                            let src = self.lexer.source();
                            match bar_belongs_to_currents_span(src, operator.span, [upper, recent])
                            {
                                Ok(true) => break,
                                Ok(false) => {
                                    trace!("when lhs:\n{lhs}\n  deciding to REJECT operator {operator} due to indentation");
                                    return Err(lhs);
                                }
                                Err(ambigious) => {
                                    dbg!(&ambigious);
                                    todo!("have like an `Entity::AmbigiousBranch` or something: {ambigious:?}")
                                }
                            }
                        }
                        // There is no match nesting. Safely assume it belongs to the only one
                        None => break,
                    },
                    None => {
                        *previous = Some(operator.span);
                        break;
                    }
                },

                Context::Operators(left_operator) => {
                    return match tightness(*operator).cmp(&tightness(**left_operator)) {
                        Ordering::Equal | Ordering::Less => {
                            trace!("when lhs:\n{lhs}\n  deciding to REJECT operator {operator} due to precedence\n{:?}", self.ctx);
                            Err(lhs)
                        }
                        Ordering::Greater => Ok(self.eat_operator(lhs, operator)),
                    };
                }

                Context::Header("as") if *operator == "," => return Err(lhs),
                Context::Header("=") => break, // TODO: don't we want the opposite? to skip it?
                Context::Header(_) => {}
                Context::InMatch(_) | Context::Indent(_) | Context::Clause(_) => {
                    // This is the first operator in this followup chain
                    break;
                }
            }
        }

        Ok(self.eat_operator(lhs, operator))
    }

    fn eat_operator(&mut self, lhs: Tr<Entity<'s>>, operator: Tr<&'s str>) -> Tr<Entity<'s>> {
        self.lexer.next();

        trace!("when lhs:\n{lhs}\n  deciding to eat operator {operator}");

        self.ctx.push(Context::Operators(operator));
        let rhs = self.next(true);
        self.ctx.pop().unwrap();

        let tspan = lhs.span.extend(rhs.span);
        match lhs.value {
            Entity::Operators { lhs: llhs, operator: loperator, mut parts } => {
                // Separate out different kinds of operators into different entities
                if *loperator == *operator {
                    parts.push(rhs);
                    Entity::Operators { lhs: llhs, operator, parts }.tr(tspan)
                } else {
                    let lhs =
                        Entity::Operators { lhs: llhs, operator: loperator, parts }.tr(lhs.span);
                    Entity::Operators { lhs: Box::new(lhs), operator, parts: vec![rhs] }.tr(tspan)
                }
            }
            _ => Entity::Operators { lhs: Box::new(lhs), operator, parts: vec![rhs] }.tr(tspan),
        }
    }

    fn clause(&mut self, span: Span, close: Token, closen: &'s str) -> Tr<Entity<'s>> {
        trace!("entering {}{closen} clause", self.take(span));
        let open = self.take(span);

        match self.lexer.peek() {
            (token, cspan) if token == close => {
                self.lexer.next();
                return Entity::Clause(open, closen, Box::new(Entity::Missing.tr(span)))
                    .tr(span.extend(cspan));
            }
            _ => {}
        }

        self.ctx.push(Context::Clause(open));
        let mut inner = self.next(false);

        loop {
            match self.lexer.peek() {
                (token, cspan) if token == close => {
                    self.lexer.next();
                    self.ctx.pop().unwrap();
                    let tspan = span.extend(cspan);
                    return Entity::Clause(open, closen, Box::new(inner)).tr(tspan);
                }
                (Token::LineComment, cspan) => {
                    self.lexer.next();
                    let text = self.take(cspan.move_indice(2));
                    inner = Entity::merge(inner, Entity::Comment(text).tr(cspan));
                }
                other => {
                    todo!("find the next `{closen}` at all cost: {other:?}");
                    // construct a sequence for inner instead
                    //
                    // have Entity::Unmatched if it doesn't exist (or i guess just use Entity::Symbol)
                }
            }
        }
    }

    fn path(&mut self, span: Span) -> Tr<Entity<'s>> {
        let (mut segments, mut anotation) = (Vec::new(), HashMap::new());
        self.path_next(span, &mut segments, &mut anotation);
        let ident = Identifier::from_segments(segments);
        Entity::Identifier(ident, anotation).tr(span)
    }

    fn path_next(&mut self, nspan: Span, path: &mut Vec<&'s str>, anot: &mut Annotation<'s>) {
        let name = self.take(nspan);
        path.push(name);

        match self.lexer.peek() {
            (Token::OpenParen, ospan) if nspan.following(1) == ospan => {
                self.lexer.next();
                self.annotation(ospan, path.len() - 1, anot);
            }
            _ => {}
        };

        match self.lexer.peek() {
            (Token::Symbol, s)
                if self.take(s) == ":"
                    && self.take(s.move_indice(-1)) != " :"
                    && self.take(s.move_indice(1)) != ": " =>
            {
                let time_machine = self.lexer.clone();

                self.lexer.next();
                match self.lexer.next() {
                    (Token::Path, nspan) => self.path_next(nspan, path, anot),
                    _ => self.lexer = time_machine,
                }
            }
            _ => {}
        }
    }

    fn annotation(&mut self, span: Span, depth: usize, anot: &mut Annotation<'s>) {
        match self.clause(span, Token::CloseParen, ")").value {
            Entity::Clause(_, _, inner) => assert!(anot.insert(depth, *inner).is_none()),
            _ => panic!("non-clause returnt by clause tree parser"),
        }
    }

    fn quote<T>(&mut self, span: Span, con: impl FnOnce(&'s str) -> T) -> Tr<T> {
        let text = self.take(Span::new(span.indice + 1, span.length - 2));
        con(text).tr(span)
    }

    fn next_then<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.lexer.next();
        f(self)
    }

    fn header(&mut self, span: Span) -> Tr<Entity<'s>> {
        let name = self.take(span).tr(span);
        self.ctx.push(Context::Header(*name));
        let then = self.next(false);
        self.ctx.pop().unwrap();
        let tspan = name.span.extend(then.span);
        Entity::Header(name, Box::new(then)).tr(tspan)
    }

    fn header_no_ctx(&mut self, span: Span) -> Tr<Entity<'s>> {
        let name = self.take(span).tr(span);
        let then = self.next(false);
        let tspan = name.span.extend(then.span);
        Entity::Header(name, Box::new(then)).tr(tspan)
    }

    fn handle_operator_if_not_expected(&mut self, _token: Token, span: Span) -> Tr<Entity<'s>> {
        for ctx in self.ctx.iter().rev() {
            match ctx {
                Context::Header(_)
                | Context::Indent(_)
                | Context::Clause(_)
                | Context::InMatch(_) => {}

                // We expected something else here, and we expect the operator later
                Context::Operators(_) => return Entity::Missing.tr(span),
            }
        }

        self.lexer.next();
        let name = self.take(span);
        Entity::Symbol(name).tr(span)
    }

    fn handle_header_if_not_expected(
        &mut self,
        can_belong_to: &[&'static str],
        span: Span,
    ) -> Tr<Entity<'s>> {
        for ctx in self.ctx.iter().rev() {
            match ctx {
                Context::Operators(_) => {}
                Context::Clause(_) => break, // treat as unexpected if the most recent relevant was a clause
                Context::Header(name) if can_belong_to.contains(name) => {
                    return Entity::Missing.tr(span)
                }
                Context::Indent(_) | Context::Header(_) | Context::InMatch(_) => {}
            }
        }

        self.lexer.next();

        let and_then = self.in_context(Context::Header("then"), |this| this.next(false));
        let tspan = span.extend(and_then.span);

        let header = self.take(span).tr(span);

        Entity::Header(header, Box::new(and_then)).tr(tspan)
    }

    fn header_chain_no_ctx(&mut self, kw: Tr<&'s str>, more: &[Token]) -> Tr<Entity<'s>> {
        let and_then = self.next(false);

        let mut headers = vec![(kw, and_then)];

        let finalize = |headers: Vec<(Tr<&'s str>, Tr<Entity<'s>>)>| {
            trace!("finalizing header chain for {kw}");
            let tspan = kw.span.extend(headers[headers.len() - 1].1.span);
            return Entity::Headers(headers).tr(tspan);
        };

        for expected in more {
            let (token, span) = self.lexer.peek();
            if token == *expected {
                self.lexer.next();
                let header = self.take(span).tr(span);
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

    fn header_chain(&mut self, kw: Tr<&'s str>, more: &[Token]) -> Tr<Entity<'s>> {
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
    fn merge(lhs: Tr<Entity<'s>>, rhs: Tr<Entity<'s>>) -> Tr<Entity<'s>> {
        let tspan = lhs.span.extend(rhs.span);

        let mut elems = match lhs.value {
            Entity::Sequence(elems) => elems,
            _ => vec![lhs],
        };

        match rhs.value {
            Entity::Sequence(relems) => elems.extend(relems),
            _ => elems.push(rhs),
        }

        Entity::Sequence(elems).tr(tspan)
    }
}

fn tightness(op: &str) -> usize {
    match op {
        "," => 1,
        "|" => 2,
        "." => 3,
        "can" => 4,
        _ => 5,
    }
}

fn add_parameter<'s>(entity: Tr<Entity<'s>>, parameter: Tr<Entity<'s>>) -> Tr<Entity<'s>> {
    let tspan = entity.span.extend(parameter.span);
    match entity.value {
        Entity::Sequence(mut elems) => {
            elems.push(parameter);
            Entity::Sequence(elems).tr(tspan)
        }
        _ => Entity::Sequence(vec![entity, parameter]).tr(tspan),
    }
}

fn attach_comments<'s>(comments: &mut Vec<Tr<&'s str>>, entity: Tr<Entity<'s>>) -> Tr<Entity<'s>> {
    comments.drain(..).fold(entity, |entity, text| {
        Entity::Commented(*text, Box::new(entity)).tr(text.span)
    })
}

fn find_match_context<'a, 's: 'a>(iter: impl Iterator<Item = &'a mut Context<'s>>) -> Option<Span> {
    for ctx in iter {
        match ctx {
            Context::Clause(_) | Context::Indent(_) => break,
            Context::InMatch(span) => return Some(span.unwrap()),
            // TODO: Rather unsure about this one
            Context::Header(_) | Context::Operators(_) => {}
        }
    }

    None
}

#[derive(Debug)]
pub struct AmbigiousIndentation {
    pub previous: [Span; 2],
    pub span: Span,
}

fn bar_belongs_to_currents_span(
    src: &str,
    span: Span,
    [upper, current]: [Span; 2],
) -> Result<bool, AmbigiousIndentation> {
    let Some(span_indent) = get_indent_level(src, span, true) else {
        // It's not on a clean newline, so assume it belongs to the closest match expression
        return Ok(true);
    };

    let [u_indent, c_indent] = [upper, current].map(|sp| get_indent_level(src, sp, false).unwrap());

    match span_indent
        .abs_diff(u_indent)
        .cmp(&span_indent.abs_diff(c_indent))
    {
        Ordering::Greater => Ok(true),
        Ordering::Less => Ok(false),
        Ordering::Equal => Err(AmbigiousIndentation { previous: [upper, current], span }),
    }
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
            Entity::Comment(text) => write!(f, "//{text}"),
            Entity::Commented(text, then) => write!(f, "//{text}\n{then}"),
            Entity::Sequence(elems) => write!(
                f,
                "[ {}\n]",
                elems.iter().map(|elem| elem.indent()).format("\n, ")
            ),
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
