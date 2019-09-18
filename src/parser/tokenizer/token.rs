use super::Tracked;
use crate::evaler::runner::operator::{cond::Cond, list::List, math::Math, Operators};
use crate::evaler::runner::Entity;
use std::fmt;
use std::path::PathBuf;

type TrackedMore = Vec<Tracked<Token>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Word(String),
    LambdaHeader(String),
    Import(PathBuf),
    HeaderFnEndNoArgs,
    Group(TrackedMore),
    IfStatement(Vec<(TrackedMore, TrackedMore)>, TrackedMore),
    List(TrackedMore),
    ExternClosure(String, u16, TrackedMore),
    LocalClosure(TrackedMore),
    K(Key),
    Function(usize, usize, usize),
    Recurse(usize),
    BridgedFunction(u16),

    Finished(Entity),
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Key {
    If,
    Then,
    Else,
    ElseIf,
    ListOpen,
    ListClose,
    Or,
    And,
    Use,
    Comma,
    First,
    Where,
    Assign,
    TypeSep,
    LineSep,
    Match,
    Colon,
    HeaderFn,
    HeaderConvert,
    HeaderType,
    Pipe,
    ParenOpen,
    ParenClose,
    HeaderArrow,
    ClosureMarker,
    PrimitiveExit,
    Comment,

    Operator(Operators),
}

#[derive(Debug)]
pub enum Result<'a, T> {
    Complete(T),
    Multiple(&'a [T]),
    Single(T),
    Empty,
    Unmatched,
}

impl Key {
    // TODO: Would be pretty nice if I could automatically generate this based on fmt
    pub fn matches<'a>(c: u8) -> Result<'a, Key> {
        use self::Result::*;
        use Key::*;
        match c {
            b'i' => Single(If),
            b't' => Multiple(&[HeaderType, Then]),
            b'e' => Multiple(&[Else, ElseIf, PrimitiveExit]),
            b'o' => Single(Or),
            b'a' => Single(And),
            b'u' => Single(Use),
            b',' => Complete(Comma),
            b'.' => Single(Operator(Operators::List(List::Range))),
            b'f' => Multiple(&[First, HeaderFn]),
            b'w' => Single(Where),
            b'=' => Multiple(&[
                Assign,
                Operator(Operators::Cond(Cond::Eq)),
                Operator(Operators::Cond(Cond::GtE)),
            ]),
            b':' => Single(TypeSep),
            b'c' => Single(HeaderConvert),
            b'<' => Multiple(&[
                Pipe,
                Operator(Operators::Cond(Cond::Lt)),
                Operator(Operators::Cond(Cond::LtE)),
                Operator(Operators::List(List::Append)),
            ]),
            b'>' => Complete(Operator(Operators::Cond(Cond::Gt))),
            b'(' => Complete(ParenOpen),
            b')' => Complete(ParenClose),
            b'-' => Multiple(&[Operator(Operators::Math(Math::Sub)), HeaderArrow]),
            b'+' => Complete(Operator(Operators::Math(Math::Add))),
            b'/' => Complete(Operator(Operators::Math(Math::Div))),
            b'*' => Complete(Operator(Operators::Math(Math::Mul))),
            b'#' => Complete(ClosureMarker),
            b'|' => Single(Comment),
            b'[' => Complete(ListOpen),
            b']' => Complete(ListClose),
            _ => Unmatched,
        }
    }

    pub fn is(&self, chars: &[u8]) -> bool {
        let key = self.to_string();
        let key = key.as_bytes();

        let mut i = chars.len();
        if i != key.len() {
            return false;
        }

        while i > 0 {
            if chars[i - 1]
                != match key.get(i - 1) {
                    None => return false,
                    Some(v) => *v,
                }
            {
                return false;
            }
            i -= 1;
        }
        true
    }
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Key::*;
        let s = match self {
            If => "if",
            Then => "then",
            Else => "else",
            ElseIf => "elif",
            Or => "or",
            And => "and",
            Use => "use",
            Comma => ",",
            Colon => ":",
            First => "first",
            Where => "where",
            Assign => "=",
            TypeSep => "::",
            LineSep => "|",
            Match => "match",
            HeaderFn => "fn",
            HeaderConvert => "convert",
            HeaderType => "type",
            Pipe => "<<",
            ListOpen => "[",
            ListClose => "]",
            ParenOpen => "(",
            ParenClose => ")",
            HeaderArrow => "->",
            PrimitiveExit => "exit",
            ClosureMarker => "#",
            Comment => "||",
            Operator(op) => return op.fmt(f),
        };
        f.write_str(s)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::K(k) => k.fmt(f),
            Token::Import(source) => {
                let path_representation = source
                    .iter()
                    .map(|p| p.to_str().unwrap())
                    .collect::<Vec<&str>>()
                    .join(":");
                write!(f, "use {}", path_representation,)
            }
            Token::Finished(entity) => entity.fmt(f),
            Token::LambdaHeader(_) => write!(f, "lambda"),
            Token::IfStatement(_, _) => write!(f, "if statement"),
            Token::Word(s) => f.write_str(&s),
            Token::HeaderFnEndNoArgs => f.write_str("new line"),
            Token::EOF => write!(f, "end of file"),
            Token::Function(_, _, _) => write!(f, "function"),
            Token::Recurse(_) => write!(f, "recursive function call"),
            Token::BridgedFunction(_) => write!(f, "bridged function"),
            Token::List(_) => write!(f, "list"),
            Token::ExternClosure(_, _, _) => write!(f, "function closure"),
            Token::LocalClosure(_) => write!(f, "lambda closure"),
            Token::Group(buf) => write!(
                f,
                "Group<\n{:?}\n>",
                buf.iter()
                    .map(|t| t.inner.to_string())
                    .collect::<Vec<String>>(),
            ),
            // _ => write!(f, "{:#?}", self),
        }
    }
}
