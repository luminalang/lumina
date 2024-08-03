use logos::{Lexer as LogosLexer, Logos, SpannedIter};
use lumina_util::Span;

#[derive(PartialEq, Debug, Clone, Copy, Logos)]
#[logos(subpattern name = "[_a-zA-Z][-_0-9a-zA-Z]*")]
#[logos(subpattern path = "(?&name)(:(?&name))+|(?&name)")]
pub enum Token {
    #[regex("\\n+")]
    NewLines,

    #[regex(r#""(\\[\\"]|[^"])*""#)]
    StringLiteral,

    #[regex("\\-?\\d+")]
    Int,
    #[regex("\\-?\\d+\\.\\d+")]
    Float,

    #[regex("//[^\n]*")]
    LineComment,

    #[regex("///[^\n]*")]
    LineDocComment,

    #[token("pub")]
    Pub,

    #[token("if")]
    If,
    #[token("|")]
    Bar,
    #[token("~")]
    Tilde,
    #[token("can")]
    Can,
    #[token("as")]
    As,
    #[token("=")]
    Equal,
    #[token("where")]
    Where,
    #[token("when")]
    When,
    #[token("match")]
    Match,
    #[token("for")]
    For,
    #[token("..")]
    DotDot,
    #[token("@")]
    At,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("{")]
    OpenCurly,
    #[token("[")]
    OpenList,
    #[token("]")]
    CloseList,
    #[token("}")]
    CloseCurly,
    #[token(",")]
    Comma,
    #[token("->")]
    Arrow,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("do")]
    Do,
    #[token("\\")]
    Backslash,
    #[token("#")]
    Square,
    #[token("#!")]
    SquareExt,
    #[token("fn")]
    Fn,
    #[token("fn(")]
    FnOpenParen,
    #[token("fnptr(")]
    FnPtrOpenParen,
    #[token("type")]
    Type,
    #[token("trait")]
    Trait,
    #[token("use")]
    Use,
    #[token("impl")]
    Impl,
    #[token("val")]
    Val,
    #[token("default")]
    Default,

    #[token("@[")]
    OpenAttribute,
    #[token("@![")]
    OpenModuleAttribute,

    #[regex(r#"[:\\!+/*&%@$?^~<>=|-]+"#)]
    Operator,

    #[token(".")]
    Dot,

    #[regex("_|(?&path)")]
    Path,

    #[regex("(?&path)\\(")]
    AnnotatedPath,

    #[error]
    #[regex(r"[ \t\f]+", logos::skip)]
    Error,

    EOF,
}

impl Token {
    pub fn is_header(self) -> bool {
        use Token as T;
        [
            T::OpenAttribute,
            T::OpenModuleAttribute,
            T::Fn,
            T::Val,
            T::Use,
            T::Type,
            T::Trait,
            T::Impl,
            T::When,
            T::Default,
            T::When,
            T::Pub,
            T::EOF,
        ]
        .contains(&self)
    }

    pub fn describe(self) -> &'static str {
        use Token as T;
        match self {
            T::Pub => "pub keyword",
            T::OpenAttribute => "start of attribute",
            T::OpenModuleAttribute => "start of attribute",
            T::Int | T::Float => "number",
            T::NewLines => "new line",
            T::Val => "static",
            T::LineDocComment | T::LineComment => "documentation comment",
            T::StringLiteral => "string literal",
            T::If => "start of if expression",
            T::Bar => "vertical bar",
            T::Can => "`can` keyword",
            T::As => "`as` keyword",
            T::Equal => "equal sign",
            T::Where => "`where` keyword",
            T::When => "`when` keyword",
            T::Match => "start of match expression",
            T::For => "`for` keyword",
            T::DotDot => "`..`",
            T::Dot => "dot pipe",
            T::At => "`@`",
            T::Then => "`then` keyword",
            T::Else => "`else` keyword",
            T::OpenParen => "`(`",
            T::CloseParen => "`)`",
            T::OpenCurly => "`{`",
            T::CloseCurly => "`}`",
            T::OpenList => "`[`",
            T::CloseList => "`]`",
            T::Comma => "comma",
            T::Arrow => "arrow",
            T::Let => "start of let binding",
            T::In => "`in` keyword",
            T::Do => "start of do expression",
            T::Backslash => "`\\`",
            T::Square => "`#`",
            T::SquareExt => "`#!`",
            T::Fn => "start of function declaration",
            T::FnOpenParen => "start of closure type",
            T::FnPtrOpenParen => "start of function pointer type",
            T::Type => "start of type declaration",
            T::Trait => "start of trait declaration",
            T::Use => "use declaration",
            T::Impl => "start of implementation block",
            T::Default => "`default` keyword",
            T::Operator => "operator",
            T::Tilde => "`~`",
            T::Path => "identifier",
            T::AnnotatedPath => "type annotated path",
            T::Error => "unknown token",
            T::EOF => "end of file",
        }
    }
}

pub struct Lexer<'src> {
    logos: SpannedIter<'src, Token>,
    indentation: u16,
    line: u32,

    pub span_offset: usize,

    // we implement our own since Token is Copy
    peeked: Option<(Token, Span)>,
}

impl<'src> Clone for Lexer<'src> {
    fn clone(&self) -> Self {
        Lexer {
            logos: self.logos.as_lexer().clone().spanned(),
            peeked: self.peeked,
            span_offset: self.span_offset,
            indentation: self.indentation,
            line: self.line,
        }
    }
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Lexer {
            logos: LogosLexer::new(src).spanned(),
            line: 1,
            span_offset: 0,
            indentation: 0,
            peeked: None,
        }
    }

    pub fn add_offset(&mut self, offset: usize) {
        self.span_offset += offset;
    }

    pub fn source(&self) -> &'src str {
        self.logos.as_lexer().source()
    }

    pub fn next(&mut self) -> (Token, Span) {
        self.next_with_indent().0
    }

    pub fn peek(&mut self) -> (Token, Span) {
        self.peek_with_indent().0
    }

    pub fn next_with_indent(&mut self) -> ((Token, Span), u16) {
        if let Some(t) = self.peeked.take() {
            (t, self.indentation)
        } else {
            let t = self.generate();
            (t, self.indentation)
        }
    }

    pub fn peek_with_indent(&mut self) -> ((Token, Span), u16) {
        if let Some(t) = self.peeked {
            if t.0 == Token::NewLines {
                self.next();
                return self.peek_with_indent();
            }
            (t, self.indentation)
        } else {
            let t = self.generate();
            self.peeked = Some(t);
            (t, self.indentation)
        }
    }

    pub fn peek_line_sensitive(&mut self) -> (Token, Span) {
        if let Some(t) = self.peeked {
            t
        } else {
            let t = match self.logos.next() {
                None => self.eof(),
                Some((Token::LineComment, _)) => self.peek_line_sensitive(),
                Some((t, mut range)) => {
                    if t == Token::NewLines {
                        self.register_newline(range.clone());
                    }
                    self.peeked = Some((t, Span::from(range.clone())));
                    range.start += self.span_offset;
                    range.end += self.span_offset;
                    (t, Span::from(range))
                }
            };
            self.peeked = Some(t);
            t
        }
    }

    fn generate(&mut self) -> (Token, Span) {
        match self.logos.next() {
            None => self.eof(),
            Some((Token::NewLines, span)) => {
                self.register_newline(span);
                self.generate()
            }
            Some((Token::LineComment, _)) => self.generate(),
            Some((t, mut range)) => {
                range.start += self.span_offset;
                range.end += self.span_offset;
                (t, Span::from(range))
            }
        }
    }

    pub fn current_line(&self) -> u32 {
        self.line
    }
    pub fn current_indent(&self) -> u16 {
        self.indentation
    }

    fn register_newline(&mut self, span: std::ops::Range<usize>) {
        let lexer = self.logos.as_lexer_mut();
        let src = lexer.source();

        self.line += 1;

        let mut spaces = 0;
        let mut tabs = 0;

        for byte in src[span.end..].bytes() {
            match byte {
                b' ' => spaces += 1,
                b'\t' => tabs += 1,
                _ => break,
            }
        }

        self.indentation = spaces + (tabs * 2);

        lexer.bump((spaces + tabs) as usize);
    }

    fn eof(&self) -> (Token, Span) {
        (
            Token::EOF,
            Span::new(self.source().len().checked_sub(1).unwrap_or(0) as u32, 1),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token as T;

    macro_rules! cmp {
        ($src:literal => $exp:expr) => {
            cmp! { $src => $exp, Token::EOF };
        };
        ($src:literal => $($exp:expr),+) => {
            let lexer = LogosLexer::<T>::new($src);
            let paired = lexer.clone().spanned().map(|(t, span)| (t, &$src[span])).collect::<Vec<_>>();
            let mut tokens = lexer.collect::<Vec<_>>();
            tokens.push(T::EOF);
            assert_eq!(tokens, [$($exp),+], "\n representation: {:?}", paired);
        };
    }

    #[test]
    fn regexes() {
        cmp! { "username" => T::Path };
        cmp! { "std:io:puts" => T::Path };
        cmp! { "user.position.point.x" => T::Path, T::Dot, T::Path, T::Dot, T::Path, T::Dot, T::Path, T::EOF };
        cmp! { "std:io:stdin.handle" => T::Path, T::Dot, T::Path, T::EOF };
        cmp! { "std:io:stdin.handle.socket" => T::Path, T::Dot, T::Path, T::Dot, T::Path, T::EOF };
        cmp! { "\"awefwafef ewa\n fewafewa\"" => T::StringLiteral };
        cmp! { "\\!+/*&%@$?^~<>=|-" => T::Operator };
        cmp! { "// test\n0" => T::LineComment, T::NewLines, T::Int, T::EOF };
        cmp! { "/// test\n0" => T::LineDocComment, T::NewLines, T::Int, T::EOF };
        cmp! { "0.0 12.34 10" => T::Float, T::Float, T::Int, T::EOF };
        cmp! { "fn( fn" => T::FnOpenParen, T::Fn, T::EOF };
        cmp! { "#0 #(f 1)" => T::Square, T::Int, T::Square, T::OpenParen, T::Path, T::Int, T::CloseParen, T::EOF };
        cmp! { "_" => T::Path };
    }

    #[test]
    fn record() {
        cmp! { "{ f 0 | a.b.c @ c = c, b = 0 }" => T::OpenCurly, T::Path, T::Int, T::Bar, T::Path, T::Dot, T::Path, T::Dot, T::Path, T::At, T::Path, T::Equal, T::Path, T::Comma, T::Path, T::Equal, T::Int, T::CloseCurly, T::EOF };
        cmp! { "{ Point int . x }" => T::OpenCurly, T::Path, T::Path, T::Dot, T::Path ,T::CloseCurly, T::EOF };
    }
}
