use super::{func::belongs_to_where, Parser, Token};
use lumina_util::Span;
use smallvec::SmallVec;

impl<'a> Parser<'a> {
    pub fn recover_next_toplevel(&mut self) {
        self.recover_until(|_| false, false);
    }

    pub fn recover_pattern_or_function_annotation(&mut self) -> Token {
        self.recover_until(
            |t| [Token::As, Token::Equal].contains(&t) || t.is_valid_start_of_pattern_param(),
            false,
        )
    }

    pub fn recover_until(&mut self, target: impl Fn(Token) -> bool, sensitive: bool) -> Token {
        Recover::new(self, target, sensitive).run()
    }

    pub fn recover_for<const N: usize>(&mut self, tokens: [Token; N], sensitive: bool) -> Token {
        Recover::new(self, |t| tokens.contains(&t), sensitive).run()
    }
}

struct Recover<'a, 'p, F: Fn(Token) -> bool> {
    parser: &'p mut Parser<'a>,
    target: F,

    newline_sensitive: bool,

    parenthesis: SmallVec<[Span; 2]>,
    curlies: SmallVec<[Span; 2]>,
    squares: SmallVec<[Span; 2]>,

    where_bind: Option<u16>,
}

impl<'a, 'p, F: Fn(Token) -> bool> Recover<'a, 'p, F> {
    fn new(parser: &'p mut Parser<'a>, target: F, newline_sensitive: bool) -> Self {
        Recover {
            parser,
            target,
            parenthesis: SmallVec::new(),
            newline_sensitive,
            curlies: SmallVec::new(),
            squares: SmallVec::new(),
            where_bind: None,
        }
    }

    fn run(&mut self) -> Token {
        loop {
            let (t, span) = if self.newline_sensitive {
                self.parser.lexer.peek_line_sensitive()
            } else {
                self.parser.lexer.peek()
            };
            match t {
                Token::CloseParen => {
                    self.parenthesis.pop();
                }
                Token::CloseList => {
                    self.squares.pop();
                }
                Token::CloseCurly => {
                    self.curlies.pop();
                }
                Token::OpenParen => self.parenthesis.push(span),
                Token::OpenList => self.squares.push(span),
                Token::OpenCurly => self.curlies.push(span),
                Token::Where => self.where_bind = Some(self.parser.lexer.current_indent()),

                Token::EOF => {
                    self.err_all_unmatched();
                    break t;
                }
                // edge-case to not break early if this `Fn` isn't top-level and belongs to `where`
                Token::Fn => {
                    self.err_all_unmatched();
                    if let Some(kw) = self.where_bind {
                        let indent = self.parser.lexer.current_indent();
                        if belongs_to_where(kw, indent) {
                            self.parser.lexer.next();
                            continue;
                        }
                    }
                    break t;
                }

                // edge-case for making `default` not act as top-level when used in attributes/expressions
                Token::Default
                    if !span
                        .get_line(self.parser.lexer.source())
                        .0
                        .trim_start()
                        .starts_with("default") => {}

                t if t.is_header() => {
                    self.err_all_unmatched();
                    break t;
                }

                _ => {}
            }

            if self.curlies.len() + self.parenthesis.len() + self.squares.len() == 0 {
                if (self.target)(t) {
                    break t;
                }
            }

            self.parser.lexer.next();
        }
    }

    fn err_all_unmatched(&mut self) {
        let parser = &mut self.parser;
        let mut unmatched_err = |span, kind: &str| {
            parser.err_unmatched(span, kind);
        };

        for unmatched in self.curlies.drain(..) {
            unmatched_err(unmatched, "record");
        }
        for unmatched in self.squares.drain(..) {
            unmatched_err(unmatched, "list");
        }
        for unmatched in self.parenthesis.drain(..) {
            unmatched_err(unmatched, "group");
        }
    }
}
