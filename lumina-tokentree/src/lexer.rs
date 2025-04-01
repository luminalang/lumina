use logos::{Lexer as LogosLexer, Logos, SpannedIter};
use lumina_util::Span;

fn find_str_end<'src>(end: u8, lex: &mut LogosLexer<'src, TokenKind>) {
    let mut i = 0;
    let bytes = lex.remainder().as_bytes();

    while let Some(&c) = bytes.get(i) {
        match c {
            b'\\' if bytes.get(i + 1) == Some(&end) => i += 2,
            b'\\' if bytes.get(i + 1) == Some(&b'\\') => i += 2,
            c if c == end => break,
            _ => i += 1,
        }
    }

    lex.bump(i + 1);
}

pub type Token = super::Meta<TokenKind>;

#[derive(PartialEq, Debug, Clone, Copy, Logos)]
#[logos(subpattern name = "[_a-zA-Z][-_0-9a-zA-Z]*")]
#[logos(subpattern path = "(?&name)(:(?&name))+|(?&name)")]
pub enum TokenKind {
    #[token("\"", |lex| find_str_end(34, lex))]
    StringLiteral,
    #[token("'", |lex| find_str_end(b'\'', lex))]
    CharLiteral,

    #[regex("\\d+")]
    Int,
    #[regex("\\d+\\.\\d+")]
    Float,

    #[regex("//[^\n]*")]
    LineComment,

    #[regex("///[^\n]*")]
    LineDocComment,

    #[token("=", priority = 3)]
    Equal,
    #[token("pub")]
    Pub,
    #[token("if")]
    If,
    #[token("can")]
    Can,
    #[token("as")]
    As,
    #[token("where")]
    Where,
    #[token("when")]
    When,
    #[token("match")]
    Match,
    // #[token("for")]
    // For,
    // #[token("..")]
    // DotDot,
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
    // #[token(";")]
    // SemiColon,
    // #[token("|")]
    // Bar,
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
    #[token("fn")]
    Fn,
    #[token("fnptr")]
    FnPtr,
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
    #[token("alias")]
    Alias,

    // Soft Keysymbols:
    // =
    // ..
    #[regex(r#"[\|:\\!+/*&%@$?^~<>=\-~\.@;#]+"#)]
    Symbol,
    // ok nope, i think the only reasonable way to do this is to remove `+` from operator regex as well and then merge the ops in the parser
    // #[regex(r#"[!#*-]"#, priority = 10, callback = is_unary_callback)]
    // Unary,

    // Soft Keywords:
    // default
    // for
    // pub
    // alias
    // as
    #[regex("_|(?&path)")]
    Path,

    #[regex(r"[ \n\t\f]+", logos::skip)]
    Skip,

    Error,
    EOF,
}

impl TokenKind {
    pub fn describe(self) -> &'static str {
        use TokenKind as T;
        match self {
            T::Int | T::Float => "number",
            T::Alias => "alias",
            T::Pub => "`pub` keyword",
            T::Equal => "`=`",
            T::Val => "static",
            // T::Bar => "`|`",
            T::LineDocComment | T::LineComment => "documentation comment",
            T::StringLiteral => "string literal",
            T::CharLiteral => "char literal",
            T::If => "start of if expression",
            T::Can => "`can` keyword",
            T::As => "`as` keyword",
            T::Where => "`where` keyword",
            T::When => "`when` keyword",
            T::Match => "start of match expression",
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
            T::Fn => "`fn`",
            T::FnPtr => "`fnptr`",
            T::Type => "start of type declaration",
            T::Trait => "start of trait declaration",
            T::Use => "use declaration",
            T::Impl => "start of implementation block",
            T::Symbol => "operator",
            // T::Unary => "unary operator",
            T::Path => "identifier",
            T::Skip => "...",
            T::Error => "unknown token",
            T::EOF => "end of file",
        }
    }
}

pub struct Lexer<'src> {
    logos: SpannedIter<'src, TokenKind>,
    indentation: u16,
    line: u32,

    keep_comments: bool,

    // we implement our own since Token is Copy
    peeked: Option<Token>,
}

impl<'src> Clone for Lexer<'src> {
    fn clone(&self) -> Self {
        Lexer {
            logos: self.logos.clone(),
            peeked: self.peeked,
            keep_comments: self.keep_comments,
            indentation: self.indentation,
            line: self.line,
        }
    }
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str, keep_comments: bool) -> Self {
        Lexer {
            logos: LogosLexer::new(src).spanned(),
            line: 1,
            keep_comments,
            indentation: 0,
            peeked: None,
        }
    }

    /// WARNING: Has to clear peek buffer
    pub fn bump(&mut self, n: usize) {
        self.peeked = None;
        self.logos.bump(n)
    }

    pub fn source(&self) -> &'src str {
        self.logos.source()
    }

    fn generate(&mut self) -> Token {
        let mut comment = Span::null();
        let (token, span) = self.logos_generate(&mut comment);
        Token::new(token, span, comment)
    }

    fn logos_generate(&mut self, comment: &mut Span) -> (TokenKind, Span) {
        match self.logos.next() {
            None => {
                let Token { span, kind: data, .. } = self.eof();
                (data, span)
            }
            Some((Err(()), span)) => (TokenKind::Error, Span::from(span)),
            Some((Ok(TokenKind::LineComment | TokenKind::LineDocComment), span)) => {
                if *comment == Span::null() {
                    *comment = Span::from(span);
                } else {
                    *comment = comment.extend(Span::from(span));
                }
                self.logos_generate(comment)
            }
            Some((Ok(t), span)) => (t, Span::from(span)),
        }
    }

    pub fn next(&mut self) -> Token {
        match self.peeked {
            None => self.generate(),
            Some(_) => std::mem::replace(&mut self.peeked, None).unwrap(),
        }
    }

    pub fn peek(&mut self) -> Token {
        match self.peeked {
            Some(t) => t,
            None => {
                let peeked = self.generate();
                self.peeked = Some(peeked);
                peeked
            }
        }
    }

    fn eof(&self) -> Token {
        Token::n(
            TokenKind::EOF,
            Span::new(self.source().len().checked_sub(1).unwrap_or(0) as u32, 1),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenKind as T;

    macro_rules! cmp {
        ($src:literal => $exp:expr) => {
            cmp! { $src => $exp, TokenKind::EOF };
        };
        ($src:literal => $($exp:expr),+) => {
            let mut lexer = Lexer::new($src, false);
            let iter = std::iter::from_fn(|| {
                let t = lexer.next();
                if t.kind == TokenKind::EOF {
                    None
                } else {
                    Some(t.kind)
                }
            });
            let mut tokens = iter.collect::<Vec<_>>();
            tokens.push(T::EOF);
            assert_eq!(tokens, [$($exp),+], "\n representation: {:?}", tokens);
        };
    }

    #[test]
    fn str_escapes() {
        cmp! { r#""hello" a"# => T::StringLiteral, T::Path, T::EOF };
        cmp! { r#""hello\\" a"# => T::StringLiteral, T::Path, T::EOF };
        cmp! { r#""hello\\\"" a"# => T::StringLiteral, T::Path, T::EOF };
    }

    #[test]
    fn regexes() {
        cmp! { "username" => T::Path };
        cmp! { "std:io:puts" => T::Path };
        cmp! { "user.position.point.x" => T::Path, T::Symbol, T::Path, T::Symbol, T::Path, T::Symbol, T::Path, T::EOF };
        cmp! { "std:io:stdin.handle" => T::Path, T::Symbol, T::Path, T::EOF };
        cmp! { "std:io:stdin.handle.socket" => T::Path, T::Symbol, T::Path, T::Symbol, T::Path, T::EOF };
        cmp! { "\"awefwafef ewa\n fewafewa\"" => T::StringLiteral };
        cmp! { "\\!+/*&%@$?^~<>=|-" => T::Symbol };
        cmp! { "// test\n0" => T::LineComment, T::Int, T::EOF };
        cmp! { "/// test\n0" => T::LineDocComment, T::Int, T::EOF };
        cmp! { "0.0 12.34 10" => T::Float, T::Float, T::Int, T::EOF };
        cmp! { "(fn fn" => T::OpenParen, T::Fn, T::Fn, T::EOF };
        cmp! { "#0 #(f 1)" => T::Symbol, T::Int, T::Symbol, T::OpenParen, T::Path, T::Int, T::CloseParen, T::EOF };
        cmp! { "_" => T::Path };
    }

    #[test]
    fn record() {
        cmp! { "{ f 0 | a.b.c @ c = c, b = 0 }" => T::OpenCurly, T::Path, T::Int, T::Symbol, T::Path, T::Symbol, T::Path, T::Symbol, T::Path, T::Symbol, T::Path, T::Symbol, T::Path, T::Comma, T::Path, T::Symbol, T::Int, T::CloseCurly, T::EOF };
        cmp! { "{ Point int . x }" => T::OpenCurly, T::Path, T::Path, T::Symbol, T::Path ,T::CloseCurly, T::EOF };
    }
}
