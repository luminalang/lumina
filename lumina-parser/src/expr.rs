use super::{select, AnnotatedPath, CurlyInit, Fields, Parser, Pattern, Token, Type, T};
use itertools::Itertools;
use lumina_util::{Highlighting, Identifier, Span, Spanned, Tr};
use std::fmt;

/// One weird syntactic quirk of using pipe operators in eagerly evaluated language is that the
/// meaning of `1 . (f 0)` and `1 . f 0` are suddenly different. So; Parenthesis become much more
/// significant. This flag is to mark how many times things are wrapped by parenthesis and thereby are to
/// be eagerly evaluated instead of further applicated
///
/// Should applicate returnt fn
/// `1 . ((\ -> #f))`
///
/// Should applicate returnt fn
/// `1 . ((\a -> #f) 0)`
///
/// Should *not* applicate returnt fn
/// `1 . (\a -> a)`
///
/// Should applicate returnt fn
/// `1 . (f 0)`
///
/// Should *not* applicate returnt fn
/// `1 . f 0`
pub type Sealed = u16;

#[derive(Clone, Debug)]
pub enum Expr<'a> {
    Lit(Literal<'a>),
    Call(Tr<AnnotatedPath<'a>>, Vec<Tr<Self>>, Sealed),

    DotPipe(Box<[Tr<Self>; 2]>),             // a . b
    FieldAccess(Box<Tr<Self>>, Tr<&'a str>), // a.b

    Lambda(
        Vec<Tr<Pattern<'a>>>,
        Vec<Tr<Self>>,
        Box<Tr<Expr<'a>>>,
        Sealed,
    ),
    CallExpr(Box<Tr<Self>>, Vec<Tr<Self>>, Sealed),
    Operators {
        init: Box<Tr<Self>>,
        ops: Vec<(Tr<&'a str>, Tr<Self>)>,
    },
    Match(Box<Tr<Self>>, Vec<(Tr<Pattern<'a>>, Tr<Self>)>),

    CastAs(Box<Tr<Self>>, Tr<Type<'a>>),

    List(Vec<Tr<Self>>),
    Tuple(Vec<Tr<Self>>),
    Record {
        init: Box<CurlyInit<'a>>,
        fields: Fields<'a, Self>,
    },
    If(Box<[Tr<Self>; 3]>),
    Do(Box<[Tr<Self>; 2]>),
    Let(Tr<Pattern<'a>>, Box<[Tr<Self>; 2]>),
    Pass(Box<Tr<Self>>),
    PassFptr(AnnotatedPath<'a>),
    Poison,
}

impl<'a> Expr<'a> {
    pub fn name(str: Tr<&'a str>) -> Tr<Self> {
        Self::namep(str, vec![])
    }

    pub fn namep(str: Tr<&'a str>, params: Vec<Tr<Self>>) -> Tr<Self> {
        Expr::Call(
            AnnotatedPath::without(Identifier::parse(*str).unwrap()).tr(str.span),
            params,
            0,
        )
        .tr(str.span)
    }
}

#[derive(Clone, Debug)]
pub enum Literal<'a> {
    Int(bool, u128),
    Float(f64),
    String(&'a str),
}

pub(super) struct ExprParser<'p, 'a> {
    parser: &'p mut Parser<'a>,
    indent_tracker: IndentTracker,
}

// Nested `match` expressions are ambigious. However; if the `match` *isn't* nested then we don't
// want to tighten the rules. So; We track information about match expressions in the callstack via
// this struct.
#[derive(Debug)]
struct IndentTracker {
    matches: Vec<MatchTracker>,
}

#[derive(Debug)]
struct MatchTracker {
    kw_span: Span,
    only_single_line: Option<bool>,
    bar_indent: u16,
    bar_line: u32,
    bar_span: Span,
}

#[derive(Debug, Clone)]
pub struct IndentConflict {
    pub kw: Span,
    pub bar: Span,
}

#[derive(Debug)]
struct MissingBar {
    kw: Span,
}

impl IndentTracker {
    fn new_match(&mut self, kw_span: Span) -> Result<(), MissingBar> {
        if let Some(previous) = self.matches.last() {
            if previous.bar_span.is_null() {
                return Err(MissingBar { kw: previous.kw_span });
            }
        }

        self.matches.push(MatchTracker {
            kw_span,
            only_single_line: None,
            bar_indent: 0,
            bar_line: 0,
            bar_span: Span::null(),
        });

        Ok(())
    }

    fn init_bar(
        &mut self,
        bar_indent: u16,
        bar_line: u32,
        bar_span: Span,
    ) -> Result<(), IndentConflict> {
        let mut only_single_line = None;

        for existing in self.matches.iter_mut().rev() {
            if existing.bar_line == bar_line {
                match &mut existing.only_single_line {
                    Some(true) => {
                        return Err(IndentConflict { kw: existing.kw_span, bar: existing.bar_span })
                    }
                    e @ Some(false) | e @ None => {
                        *e = Some(false);
                        only_single_line = Some(true);
                    }
                }
            }
        }

        let tracker = self.matches.last_mut().unwrap();
        tracker.bar_indent = bar_indent;
        tracker.only_single_line = only_single_line;
        tracker.bar_line = bar_line;
        tracker.bar_span = bar_span;

        Ok(())
    }

    fn check_ownership(&mut self, indent: u16, line: u32, span: Span) -> IndentOwnership {
        if self.matches.len() == 1 {
            return IndentOwnership::Current;
        }

        let this_match = self.matches.last_mut().unwrap();

        match &mut this_match.only_single_line {
            r @ None if line == this_match.bar_line => {
                *r = Some(line == this_match.bar_line);
                return IndentOwnership::Current;
            }
            r @ None => {
                *r = Some(false);
            }
            Some(true) if line == this_match.bar_line => return IndentOwnership::Current,
            Some(true) => return IndentOwnership::Other,
            Some(false) if line == this_match.bar_line => return IndentOwnership::InvalidSameLine,
            _ => {}
        }

        let this_bar_indent = this_match.bar_indent;
        let this_kw_span = this_match.kw_span;

        if line == this_match.bar_line {
            return IndentOwnership::Current;
        }

        for other in self.matches.iter().rev().skip(1) {
            let other_diff = other.bar_indent.abs_diff(indent);
            let this_diff = this_bar_indent.abs_diff(indent);
            if other_diff < this_diff {
                return IndentOwnership::Other;
            }

            if other_diff == this_diff {
                return IndentOwnership::InvalidSameDiff(DiffConflict {
                    outer_kw_span: other.kw_span,
                    inner_kw_span: this_kw_span,
                    this_bar_span: span,
                });
            }
        }

        IndentOwnership::Current
    }

    fn finish_match(&mut self) {
        debug_assert!(self.matches.pop().is_some());
    }
}

#[derive(Clone, Debug)]
pub struct DiffConflict {
    pub outer_kw_span: Span,
    pub inner_kw_span: Span,
    pub this_bar_span: Span,
}

enum IndentOwnership {
    Current,
    InvalidSameLine,
    InvalidSameDiff(DiffConflict),
    Other,
}

impl<'a> Parser<'a> {
    pub fn expr(&mut self) -> Option<Tr<Expr<'a>>> {
        ExprParser::new(self).expr()
    }
}

impl<'p, 'a> ExprParser<'p, 'a> {
    pub fn new(parser: &'p mut Parser<'a>) -> Self {
        Self { parser, indent_tracker: IndentTracker { matches: vec![] } }
    }

    pub fn expr(&mut self) -> Option<Tr<Expr<'a>>> {
        self.expr_without_followup()
            .and_then(|expr| self.expr_followup(expr))
    }

    fn expr_without_followup(&mut self) -> Option<Tr<Expr<'a>>> {
        select! {self.parser, "an expression", span;
            T::Int => self.expr_int(span),
            T::Float => self.expr_float(span),
            T::Path => self.expr_path(span, true),
            T::Default => self.expr_path(span, true),
            T::StringLiteral => self.expr_string(span),
            T::OpenParen => self.expr_parenthesis(span),
            T::OpenCurly => self.expr_record(span),
            T::OpenList => self.expr_list(span),
            T::If => self.expr_if(span),
            T::Do => self.expr_do(span),
            T::Let => self.expr_let(span),
            T::Match => {
                let res = self.expr_match(span);
                self.indent_tracker.finish_match();
                res
            },
            T::Backslash => self.expr_lambda(span),
            T::Square => self.expr_pass(span),
            T::SquareExt => self.expr_pass_fnptr(span),
            T::AnnotatedPath => self.expr_anot_path(span.extend_length(-1), true)
        }
    }

    pub fn expr_param(&mut self) -> Option<Tr<Expr<'a>>> {
        select! { self.parser, "an expression", span;
            T::Int => self.expr_int(span),
            T::Float => self.expr_float(span),
            T::Operator => self.expr_path(span, false),
            T::Path => self.expr_path(span, false),
            T::Default => self.expr_path(span, false),
            T::StringLiteral => self.expr_string(span),
            T::OpenParen => self.expr_parenthesis(span),
            T::OpenCurly => self.expr_record(span),
            T::OpenList => self.expr_list(span),
            T::Square => self.expr_pass(span),
            T::SquareExt => self.expr_pass_fnptr(span),
            T::AnnotatedPath => self.expr_anot_path(span.extend_length(-1), false)
        }
    }

    fn expr_followup(&mut self, left: Tr<Expr<'a>>) -> Option<Tr<Expr<'a>>> {
        match self.parser.lexer.peek() {
            (T::Operator, span) => {
                self.parser.lexer.next();
                let right = self.expr()?;

                let operator = self.parser.take(span);

                match right.value {
                    Expr::Operators { init, mut ops } => {
                        let mut new_ops = vec![(operator.tr(span), *init)];
                        new_ops.append(&mut ops);

                        let span = left.span.extend(new_ops.iter().last().unwrap().1.span);

                        let expr = Expr::Operators { init: Box::new(left), ops: new_ops }.tr(span);

                        self.expr_followup(expr)
                    }
                    _ => {
                        let total_span = left.span.extend(right.span);
                        let ops = vec![(operator.tr(span), right)];
                        let expr = Expr::Operators { init: Box::new(left), ops }.tr(total_span);
                        self.expr_followup(expr)
                    }
                }
            }

            (T::Dot, span) => {
                self.parser.progress();

                // even if we edge-case `).a` here; we still have `#.ident` that needs to be edge-cased
                let lspan = left.span;

                match self.parser.take(Span::new(span.indice - 1, 1)) {
                    " " | "\n" | "\t" => {
                        let rhs = self.expr_without_followup()?;
                        let span = lspan.extend(rhs.span);
                        let expr = Expr::DotPipe(Box::new([left, rhs])).tr(span);
                        self.expr_followup(expr)
                    }
                    "#" => unreachable!("this needs to be edge-cased in the parser for `Pass`"),
                    _ => match self.parser.lexer.peek() {
                        (T::Path, span) => {
                            self.parser.progress();
                            let expr = match Identifier::parse(self.parser.take(span))
                                .unwrap()
                                .as_name()
                            {
                                Some(name) => Expr::FieldAccess(Box::new(left), name.tr(span)),
                                None => {
                                    self.parser.err_expected_but_got(
                                        span,
                                        "a field name",
                                        "a path",
                                    );
                                    Expr::Poison
                                }
                            };

                            self.expr_followup(expr.tr(lspan.extend(span)))
                        }
                        (t, span) => {
                            self.parser
                                .err_expected_but_got(span, "a field name", t.describe());

                            Some(Expr::Poison.tr(span))
                        }
                    },
                }
            }

            (T::As, _) => {
                self.parser.progress();
                match self.parser.type_with_params() {
                    Some(ty) => {
                        let span = left.span.extend(ty.span);
                        Some(Expr::CastAs(Box::new(left), ty).tr(span))
                    }
                    None => None,
                }
            }

            // Is this safe to do? Feels risky somehow
            //
            // maybe we should also have a `left.can_return_function()`
            (t, _) if t.is_valid_start_of_expr_param() => {
                self.expr_params(left.span, |params| match left.value {
                    Expr::Lambda(patterns, p, body, seal) if seal < 2 && p.is_empty() => {
                        Expr::Lambda(patterns, params, body, seal)
                    }
                    other => Expr::CallExpr(Box::new(other.tr(left.span)), params, 0),
                })
            }

            _ => Some(left),
        }
    }

    fn expr_int(&mut self, span: Span) -> Option<Tr<Expr<'a>>> {
        let raw = self.parser.take(span);
        let parse = |span| self.parser.take(span).parse::<u128>().unwrap();
        let (sign, n) = if raw.as_bytes()[0] == b'-' {
            (true, parse(span.move_indice(1)))
        } else {
            (false, parse(span))
        };
        Some(Expr::Lit(Literal::Int(sign, n)).tr(span))
    }

    fn expr_float(&mut self, span: Span) -> Option<Tr<Expr<'a>>> {
        let n = self.parser.take(span).parse().unwrap();
        Some(Expr::Lit(Literal::Float(n)).tr(span))
    }

    fn expr_path(&mut self, span: Span, params: bool) -> Option<Tr<Expr<'a>>> {
        let str = self.parser.take(span);
        let path = Identifier::parse(str).unwrap();
        let apath = AnnotatedPath::without(path).tr(span);
        self.expr_path_params(apath, params)
    }

    fn expr_anot_path(&mut self, span: Span, params: bool) -> Option<Tr<Expr<'a>>> {
        let str = self.parser.take(span);
        let path = Identifier::parse(str).unwrap();
        self.parser
            .shared_anot_path(path)
            .and_then(|p| self.expr_path_params(p.tr(span), params))
    }

    fn expr_path_params(
        &mut self,
        apath: Tr<AnnotatedPath<'a>>,
        params: bool,
    ) -> Option<Tr<Expr<'a>>> {
        match self.parser.lexer.peek() {
            (T::Dot, _) if self.parser.take(apath.span.end()) == "." => {
                self.parser.progress();
                let aspan = apath.span;
                return self
                    .expr_path_field_accessors(true, Expr::Call(apath, vec![], 0).tr(aspan));
            }
            _ => {
                if params {
                    self.expr_params(apath.span, |params| Expr::Call(apath, params, 0))
                } else {
                    let span = apath.span;
                    Some(Expr::Call(apath, vec![], 0).tr(span))
                }
            }
        }
    }

    fn expr_path_field_accessors(&mut self, init: bool, lhs: Tr<Expr<'a>>) -> Option<Tr<Expr<'a>>> {
        match self.parser.lexer.peek() {
            (T::Path, span) => match Identifier::parse(self.parser.take(span)).unwrap().as_name() {
                None => {
                    self.parser
                        .err_expected_but_got(span, "field name", "a path");
                    return Some(Expr::Poison.tr(span));
                }
                Some(name) => {
                    self.parser.progress();
                    let span = lhs.span.extend(span);
                    let expr = Expr::FieldAccess(Box::new(lhs), name.tr(span)).tr(span);

                    match self.parser.lexer.peek() {
                        (T::Dot, _) if self.parser.take(span.end()) == "." => {
                            self.parser.progress();
                            return self.expr_path_field_accessors(false, expr);
                        }
                        _ => Some(expr),
                    }
                }
            },
            (t, span) if init => {
                self.parser
                    .err_expected_but_got(span, "field name", t.describe());
                return Some(Expr::Poison.tr(span));
            }
            _ => return Some(lhs),
        }
    }

    fn expr_params(
        &mut self,
        con_span: Span,
        constr: impl FnOnce(Vec<Tr<Expr<'a>>>) -> Expr<'a>,
    ) -> Option<Tr<Expr<'a>>> {
        let mut params = vec![];

        loop {
            let (t, span) = self.parser.lexer.peek();
            if !t.is_valid_start_of_expr_param() {
                let span = con_span.extend_by_params(&params);
                return Some(constr(params).tr(span));
            }

            let expr = self.expr_param().unwrap_or(Expr::Poison.tr(span));
            params.push(expr);
        }
    }

    fn expr_string(&mut self, span: Span) -> Option<Tr<Expr<'a>>> {
        let inner_span = span.move_indice(1).extend_length(-1);
        Some(Expr::Lit(Literal::String(self.parser.take(inner_span))).tr(span))
    }

    fn expr_parenthesis(&mut self, span: Span) -> Option<Tr<Expr<'a>>> {
        let (mut elems, end) = self
            .parser
            .shared_paren(Parser::expr, Expr::Poison.tr(span))?;

        if elems.len() == 1 {
            let mut value = elems.remove(0).value;
            self.hack_apply_seal(&mut value);
            Some(value.tr(span.extend(end)))
        } else {
            Some(Expr::Tuple(elems).tr(span.extend(span)))
        }
    }

    fn expr_record(&mut self, span: Span) -> Option<Tr<Expr<'a>>> {
        self.parser.shared_record(span).map(|curly| {
            Expr::Record { init: Box::new(curly.init), fields: curly.fields }.tr(curly.span)
        })
    }

    fn hack_apply_seal(&self, expr: &mut Expr<'a>) {
        match expr {
            Expr::Call(_, _, seal) | Expr::Lambda(_, _, _, seal) | Expr::CallExpr(_, _, seal) => {
                *seal += 1
            }
            _ => {}
        }
    }

    fn expr_list(&mut self, span: Span) -> Option<Tr<Expr<'a>>> {
        let (elems, end) =
            self.parser
                .shared_list(span, Parser::expr, Some(Expr::Poison.tr(span)))?;
        Some(Expr::List(elems).tr(span.extend(end)))
    }

    fn expr_if(&mut self, span: Span) -> Option<Tr<Expr<'a>>> {
        let (cond, _) = self.expr_followed_by(span, T::Then)?;
        let (truthy, _) = self.expr_followed_by(span, T::Else)?;

        self.expr()
            .map(|falsely| Expr::If(Box::new([cond, truthy, falsely])).tr(span))
    }

    fn expr_followed_by(&mut self, span: Span, then: T) -> Option<(Tr<Expr<'a>>, Span)> {
        let value = self.expr().unwrap_or_else(|| {
            self.parser.recover_for([then], false);
            Expr::Poison.tr(span)
        });

        self.parser.expect(then).map(|span| (value, span))
    }

    fn expr_do(&mut self, span: Span) -> Option<Tr<Expr<'a>>> {
        let (discarded, end) = self.expr_followed_by(span, T::Then)?;
        self.expr()
            .map(|kept| Expr::Do(Box::new([discarded, kept])).tr(span.extend(end)))
    }

    fn expr_let(&mut self, span: Span) -> Option<Tr<Expr<'a>>> {
        let pattern = self.parser.let_pattern()?;
        self.parser.expect(T::Equal)?;
        let (value, _) = self.expr_followed_by(span, T::In)?;
        self.expr()
            .map(|and_then| Expr::Let(pattern, Box::new([value, and_then])).tr(span))
    }

    fn expr_lambda(&mut self, span: Span) -> Option<Tr<Expr<'a>>> {
        let patterns = self.parser.pat_params(false)?;

        self.parser.expect(T::Arrow)?;

        self.expr()
            .map(|expr| Expr::Lambda(patterns, vec![], Box::new(expr), 0))
            .map(|lambda| lambda.tr(span))
    }

    fn expr_pass(&mut self, span: Span) -> Option<Tr<Expr<'a>>> {
        // Hack for being whitespace-sensitive in this unusual way
        if self.parser.take(span.following(1)) == " " {
            self.parser
                .err_unexpected_token((T::Square, span), "an expression");
            return None;
        }

        self.expr_pass_expr(span)
    }

    fn expr_pass_fnptr(&mut self, square_span: Span) -> Option<Tr<Expr<'a>>> {
        select! {self.parser, "an identifier", span;
            T::Path => {
                let fnptr_span = square_span.extend(span);
                let path = Identifier::parse(self.parser.take(span)).unwrap();
                Some(Expr::PassFptr(AnnotatedPath::without(path)).tr(fnptr_span))
            },
            T::AnnotatedPath => {
                let fnptr_span = square_span.extend(span);
                let path = Identifier::parse(self.parser.take(span)).unwrap();
                self.parser.shared_anot_path(path)
                    .map(|p| Expr::PassFptr(p).tr(fnptr_span))
            }
        }
    }

    pub fn expr_pass_expr(&mut self, span: Span) -> Option<Tr<Expr<'a>>> {
        // Hacky edge-case for partially applicating operators.
        //
        // An operator without a LHS isn't a valid expression normally but is valid here
        let mut parser = self.parser.clone();
        loop {
            match parser.lexer.next() {
                (Token::Operator, ospan) => {
                    // *self.parser = parser;
                    self.parser.progress();
                    let str = self.parser.take(ospan);
                    let identifier = Identifier::parse(str).unwrap();
                    let span = span.extend(ospan);
                    let call = Expr::Call(AnnotatedPath::without(identifier).tr(ospan), vec![], 0);
                    return Some(Expr::Pass(Box::new(call.tr(span))).tr(span));
                }
                (Token::OpenParen, _) => match parser.lexer.next() {
                    (Token::Operator, ospan) => {
                        self.parser.progress();
                        self.parser.progress();
                        let str = self.parser.take(ospan);
                        let identifier = Identifier::parse(str).unwrap();
                        let span = span.extend(ospan);
                        return self.expr_param().and_then(|rhs| {
                            self.parser.expect(Token::CloseParen).map(|_| {
                                let call = Expr::Call(
                                    AnnotatedPath::without(identifier).tr(ospan),
                                    vec![rhs],
                                    0,
                                );
                                Expr::Pass(Box::new(call.tr(span))).tr(span)
                            })
                        });
                    }
                    _ => break,
                },
                _ => break,
            }
        }

        self.expr_param()
            .map(Box::new)
            .map(Expr::Pass)
            .map(|expr| expr.tr(span))
    }

    fn expr_match(&mut self, kw_span: Span) -> Option<Tr<Expr<'a>>> {
        if let Err(err) = self.indent_tracker.new_match(kw_span) {
            self.parser.err_invalid_nested_match(err.kw, kw_span);
            return None;
        }

        let against = self.expr().or_else(|| {
            if self.parser.recover_for([T::Bar], false) != T::Bar {
                return None;
            }
            Some(Expr::Poison.tr(kw_span))
        })?;

        let ((t, span), indent) = self.parser.lexer.peek_with_indent();

        let line = self.parser.lexer.current_line();

        if t != T::Bar {
            self.parser
                .err_unexpected_token((t, span), "a branch for the match expression");
            return None;
        }
        self.parser.progress();

        if let Err(err) = self.indent_tracker.init_bar(indent, line, span) {
            self.parser.err_conflicting_bars(err);
            return None;
        }

        let mut branches = Vec::with_capacity(2);

        loop {
            let pattern = self.parser.match_pattern()?;

            self.parser.expect(T::Arrow)?;

            let branch = self.expr()?;

            branches.push((pattern, branch));

            let ((t, span), indent) = self.parser.lexer.peek_with_indent();
            let line = self.parser.lexer.current_line();

            match t {
                T::Bar => match self.indent_tracker.check_ownership(indent, line, span) {
                    IndentOwnership::Current => {
                        self.parser.progress();
                        continue;
                    }
                    IndentOwnership::InvalidSameLine => {
                        self.err_invalidly_placed_vertical_bar();
                        self.indent_tracker.finish_match();
                        return None;
                    }
                    IndentOwnership::InvalidSameDiff(conflict) => {
                        // TODO: we probably still want to run the exprs for errors. But then just
                        // discard the results.
                        self.parser.err_bad_bar_indent(span, conflict);
                        self.indent_tracker.finish_match();
                        return None;
                    }
                    IndentOwnership::Other => break,
                },
                _ => break,
            }
        }

        Some(Expr::Match(Box::new(against), branches).tr(kw_span))
    }

    fn err_invalidly_placed_vertical_bar(&self) {
        todo!();
    }
}

impl T {
    fn is_valid_start_of_expr_param(&self) -> bool {
        [
            T::Int,
            T::Float,
            T::Path,
            T::StringLiteral,
            T::OpenParen,
            T::OpenCurly,
            T::OpenList,
            T::Square,
            T::SquareExt,
            T::AnnotatedPath,
        ]
        .contains(&self)
    }
}

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn sealf(seal: &Sealed) -> String {
            if *seal > 0 {
                format!("{seal}^")
            } else {
                "".into()
            }
        }

        let op = '('.symbol();
        let cp = ')'.symbol();
        let oc = '{'.symbol();
        let cc = '}'.symbol();
        let ol = '['.symbol();
        let cl = ']'.symbol();
        let if_ = "if".keyword();
        let then_ = "then".keyword();
        let else_ = "else".keyword();
        let do_ = "do".keyword();
        let let_ = "let".keyword();
        let in_ = "in".keyword();
        let eq = "=".symbol();
        let arrow = "->".symbol();

        match self {
            Expr::Call(path, params, seal) if params.is_empty() => {
                write!(f, "{}{path}", sealf(seal))
            }
            Expr::Call(path, params, seal) => {
                write!(
                    f,
                    "{op}{}{path} {}{cp}",
                    sealf(seal),
                    params.iter().format(" ")
                )
            }
            Expr::FieldAccess(object, field) => write!(f, "{object}.{field}"),
            Expr::DotPipe(elems) => write!(f, "{} . {}", elems[0], elems[1]),
            Expr::Operators { init, ops } => write!(
                f,
                "{} {}",
                init,
                ops.iter()
                    .format_with(" ", |(op, right), f| f(&format_args!("{} {}", op, right)))
            ),
            Expr::CallExpr(expr, params, seal) => {
                write!(
                    f,
                    "{op}{}{expr} {}{cp}",
                    sealf(seal),
                    params.iter().format(" ")
                )
            }
            Expr::Lambda(patterns, params, expr, seal) if params.is_empty() => write!(
                f,
                "{op}{}\\{} {arrow} {}{cp}",
                if *seal > 1 { sealf(seal) } else { "".into() },
                patterns.iter().format(" "),
                expr
            ),
            Expr::Lambda(patterns, params, expr, seal) => write!(
                f,
                "{op}{op}{}\\{} {arrow} {}{cp} {}{cp}",
                sealf(seal),
                patterns.iter().format(" "),
                expr,
                params.iter().format(" ")
            ),
            Expr::Record { init, fields } => {
                write!(f, "{oc} {}{} {cc}", init, fields.iter().format(", "))
            }
            Expr::Lit(lit) => lit.fmt(f),
            // Expr::PipeLeft(exprs) => write!(f, "{} {pl} {}", exprs[0], exprs[1]),
            // Expr::PipeRight(exprs) => write!(f, "{} {pr} {}", exprs[0], exprs[1]),
            Expr::If(exprs) => {
                let cond = exprs[0].to_string();
                let then_expr = exprs[1].to_string();
                let else_expr = exprs[2].to_string();

                write!(f, "{if_}")?;
                if cond.contains('\n') {
                    write!(f, "\n  {op}{cond}{cp}")?;
                } else {
                    write!(f, " {cond}")?;
                }

                if then_expr.contains('\n') {
                    write!(f, "\n  {then_}\n    {}", then_expr.lines().format("\n    "))?;
                } else {
                    write!(f, "\n  {then_} {}", then_expr.lines().format("\n  "))?;
                }

                if else_expr.contains('\n') {
                    write!(f, "\n  {else_}\n    {}", else_expr.lines().format("\n    "))
                } else {
                    write!(f, "\n  {else_} {}", else_expr.lines().format("\n  "))
                }
            }
            Expr::Match(against, branches) => {
                let against = against.to_string();

                write!(f, "{}", "match".keyword())?;

                if against.contains('\n') {
                    write!(f, "\n  {}\n", against.lines().format("\n  "))
                } else {
                    write!(f, " {}\n", against)
                }?;

                write!(
                    f,
                    "{}",
                    branches.iter().format_with("\n", |(p, v), f| {
                        f(&format_args!("| {p} ->"))?;

                        let v = v.to_string();
                        if v.contains('\n') {
                            f(&format_args!(
                                "\n  {}",
                                v.to_string().lines().format("\n  ")
                            ))
                        } else {
                            f(&format_args!(" {v}"))
                        }
                    })
                )
            }

            Expr::Let(pattern, exprs) => {
                write!(
                    f,
                    "{let_} {} {eq} {}\n  {in_} {}",
                    pattern,
                    exprs[0],
                    exprs[1].to_string().lines().format("\n  ")
                )
            }
            Expr::Pass(inner) => write!(f, "{}{}", "#".symbol(), inner),
            Expr::PassFptr(fkey) => write!(f, "{}{}", "#!".symbol(), fkey),
            Expr::Do(exprs) => write!(f, "{do_} {} {then_} {}", exprs[0], exprs[1]),
            Expr::List(elems) => write!(f, "{ol}{}{cl}", elems.iter().format(", ")),
            Expr::Tuple(elems) => write!(f, "{op}{}{cp}", elems.iter().format(", ")),
            Expr::CastAs(v, ty) => write!(f, "{op}{v} {} {ty}{cp}", "as".keyword()),
            Expr::Poison => "???".fmt(f),
        }
    }
}

impl<'a> fmt::Display for Literal<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Int(true, n) => write!(f, "-{}", n),
            Literal::Int(false, n) => n.fmt(f),
            Literal::Float(n) => n.fmt(f),
            Literal::String(str) => write!(f, "\"{str}\""),
        }
    }
}
