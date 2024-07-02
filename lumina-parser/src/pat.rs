use super::{select, CurlyInit, Fields, Parser, Tr, T};
use itertools::Itertools;
use lumina_util::{Highlighting, Identifier, Span, Spanned};
use std::fmt;

#[derive(Clone, Debug)]
pub enum Pattern<'a> {
    Name(Identifier<'a>, Vec<Tr<Self>>),
    String(&'a str),
    Fields(Box<CurlyInit<'a>>, Fields<'a, Self>),
    List(Vec<Tr<Self>>),
    Tuple(Vec<Tr<Self>>),
    Int([Bound; 2]),
    Float(f64),
    Operators {
        init: Box<Tr<Self>>,
        ops: Vec<(&'a str, Tr<Self>)>,
    },
    Poison,
}

#[derive(Clone, PartialEq, Eq, Copy)]
pub enum Bound {
    Excess,
    Neg(u128),
    Pos(u128),
}

impl<'a> Parser<'a> {
    pub fn pattern_parameter(&mut self) -> Option<Tr<Pattern<'a>>> {
        self.pattern_without_followup(false)
    }

    pub fn let_pattern(&mut self) -> Option<Tr<Pattern<'a>>> {
        self.pattern(true)
    }

    pub fn match_pattern(&mut self) -> Option<Tr<Pattern<'a>>> {
        self.pattern(true)
    }

    pub fn list_pattern(&mut self) -> Option<Tr<Pattern<'a>>> {
        self.pattern(true)
    }

    fn pattern(&mut self, params: bool) -> Option<Tr<Pattern<'a>>> {
        self.pattern_without_followup(params)
            .and_then(|p| self.pat_followup(p))
    }

    fn pattern_without_followup(&mut self, params: bool) -> Option<Tr<Pattern<'a>>> {
        select! { self, "pattern", span;
            T::Int => self.pat_int(span),
            T::Float => self.pat_float(span),
            T::DotDot => self.pat_partial_int(Bound::Excess, span ),
            T::Path => self.pat_path(span, params),
            T::StringLiteral => self.pat_string(span),
            T::OpenParen => self.pat_paren(span),
            T::OpenCurly => self.pat_record(span),
            T::OpenList => self.pat_list(span)
        }
    }

    fn pat_followup(&mut self, left: Tr<Pattern<'a>>) -> Option<Tr<Pattern<'a>>> {
        // this is straight up copy-pasted from `expr_followup`. We can probably be a bit more
        // clever than that.
        match self.lexer.peek() {
            (T::Operator, span) => {
                self.progress();
                let right = self.pattern(true)?;

                let operator = self.take(span);

                match right.value {
                    Pattern::Operators { init, mut ops } => {
                        let mut new_ops = vec![(operator, *init)];
                        new_ops.append(&mut ops);
                        let span = left.span.extend(new_ops.iter().last().unwrap().1.span);
                        let pat =
                            Pattern::Operators { init: Box::new(left), ops: new_ops }.tr(span);
                        self.pat_followup(pat)
                    }
                    _ => {
                        let total_span = left.span.extend(right.span);
                        let ops = vec![(operator, right)];
                        let pat = Pattern::Operators { init: Box::new(left), ops }.tr(total_span);

                        self.pat_followup(pat)
                    }
                }
            }
            _ => Some(left),
        }
    }

    fn pat_int(&mut self, span: Span) -> Option<Tr<Pattern<'a>>> {
        // let left = self.take(span).parse::<u128>().unwrap();
        let left = self.int_to_bound(span);
        if let Some(dspan) = self.next_is(|t| t == T::DotDot) {
            self.pat_partial_int(left, span.extend(dspan))
        } else {
            let p = Pattern::Int([left, left]);
            Some(p.tr(span))
        }
    }

    fn int_to_bound(&self, span: Span) -> Bound {
        let raw = self.take(span);
        match raw.as_bytes()[0] {
            b'-' => Bound::Neg(raw[1..].parse().unwrap()),
            _ => Bound::Pos(raw.parse().unwrap()),
        }
    }

    fn pat_partial_int(&mut self, left: Bound, mut span: Span) -> Option<Tr<Pattern<'a>>> {
        let right = if let Some(rspan) = self.next_is(|t| t == T::Int) {
            let right = self.int_to_bound(rspan);
            span = span.extend(rspan);
            right
        } else {
            Bound::Excess
        };
        let p = Pattern::Int([left, right]);
        Some(p.tr(span))
    }

    fn pat_float(&mut self, span: Span) -> Option<Tr<Pattern<'a>>> {
        let n = self.take(span).parse().unwrap();
        let p = Pattern::Float(n);
        Some(p.tr(span))
    }

    fn pat_path(&mut self, span: Span, params: bool) -> Option<Tr<Pattern<'a>>> {
        let str = self.take(span);
        let path = Identifier::parse(str).unwrap();
        let params = if params { self.pat_params()? } else { vec![] };
        let span = span.extend_by_params(&params);
        let pat = Pattern::Name(path, params);
        Some(pat.tr(span))
    }

    fn pat_string(&mut self, span: Span) -> Option<Tr<Pattern<'a>>> {
        let str = self.take(span.move_indice(1).extend_length(-1));
        let pat = Pattern::String(str);
        Some(pat.tr(span))
    }

    fn pat_paren(&mut self, span: Span) -> Option<Tr<Pattern<'a>>> {
        let (mut elems, end) =
            self.shared_paren(|parser| parser.pattern(true), Tr::null(Pattern::Poison))?;

        if elems.len() == 1 {
            Some(elems.remove(0).value.tr(span.extend(end)))
        } else {
            Some(Pattern::Tuple(elems).tr(span.extend(end)))
        }
    }

    fn pat_record(&mut self, span: Span) -> Option<Tr<Pattern<'a>>> {
        let curly = self.shared_record(span)?;
        Some(Pattern::Fields(Box::new(curly.init), curly.fields).tr(curly.span))
    }

    fn pat_list(&mut self, span: Span) -> Option<Tr<Pattern<'a>>> {
        let (elems, end) = self.shared_list(
            span,
            |parser| parser.pattern(true),
            Some(Pattern::Poison.tr(span)),
        )?;

        Some(Pattern::List(elems).tr(span.extend(end)))
    }

    pub fn pat_params(&mut self) -> Option<Vec<Tr<Pattern<'a>>>> {
        let mut patterns = vec![];

        loop {
            let (t, span) = self.lexer.peek();
            if !t.is_valid_start_of_pattern_param() {
                break Some(patterns);
            }

            let expr = self
                .pattern_without_followup(false)
                .unwrap_or(Pattern::Poison.tr(span));

            patterns.push(expr);
        }
    }
}

impl T {
    pub fn is_valid_start_of_pattern_param(self) -> bool {
        match self {
            T::Float
            | T::Int
            | T::Path
            | T::OpenCurly
            | T::OpenParen
            | T::OpenList
            | T::FnOpenParen
            | T::FnPtrOpenParen => true,
            _ => false,
        }
    }
}

impl<'a> fmt::Display for Pattern<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = '('.symbol();
        let cp = ')'.symbol();
        let oc = '{'.symbol();
        let cc = '}'.symbol();
        let ol = '['.symbol();
        let cl = ']'.symbol();

        match self {
            Pattern::Name(name, params) if params.is_empty() => name.fmt(f),
            Pattern::Name(name, params) => {
                write!(f, "{op}{} {}{cp}", name, params.iter().format(" "))
            }
            Pattern::String(s) => write!(f, "\"{s}\""),
            Pattern::List(elems) => write!(f, "{ol}{}{cl}", elems.iter().format(", ")),
            Pattern::Fields(init, fields) => {
                write!(f, "{oc} {init}{} {cc}", fields.iter().format(", "))
            }
            Pattern::Int([Bound::Excess, Bound::Excess]) => {
                write!(f, "({})", "..".numeric())
            }
            Pattern::Int([start, end]) if start == end => start.numeric().fmt(f),
            Pattern::Int([start, end]) => format_args!("{}..{}", start, end).numeric().fmt(f),
            Pattern::Float(n) => n.numeric().fmt(f),
            Pattern::Tuple(elems) => {
                write!(f, "{op}{}{cp}", elems.iter().format(", "))
            }
            Pattern::Operators { init, ops } => write!(
                f,
                "{op}{} {}{cp}",
                init,
                ops.iter()
                    .format_with(" ", |(op, right), f| f(&format_args!("{} {}", op, right)))
            ),
            Pattern::Poison => "???".fmt(f),
        }
    }
}

impl fmt::Display for Bound {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Bound::Excess => Ok(()),
            Bound::Neg(n) => write!(f, "-{n}"),
            Bound::Pos(n) => n.fmt(f),
        }
    }
}

impl fmt::Debug for Bound {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{self}")
    }
}
