use super::Tracked;
use crate::evaler::r#type::Value;
use crate::evaler::runner::operator::{cond::Cond, list::List, math::Math, Operators};
use std::fmt;
use std::path::PathBuf;

// TODO: I should consider reworking this into two enums, ParseToken and RuntimeToken. That should
// make the Tracked<> situation easier and I could just implement From<> Into<> between them.
//
// Could also prevent compiler failure-caused undefined behavior since I can know that no
// parse-time things like Word<S> or Key::If can't sneak into runtime
//
// Could also just create a trait for code reusability

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Parsetime
    Word(String),
    LambdaHeader(String),
    Import(PathBuf),
    HeaderFnEndNoArgs,
    TrackedGroup(Vec<Tracked<Token>>),
    TrackedIfStatement(
        Vec<(Vec<Tracked<Token>>, Vec<Tracked<Token>>)>,
        Vec<Tracked<Token>>,
    ),
    TrackedRuntimeList(Vec<Tracked<Token>>),
    TrackedExternClosure(String, u16, Vec<Tracked<Token>>),
    EOF,

    // Runtime
    LocalClosure(Vec<Token>),
    ExternClosure(usize, usize, Vec<Token>), // Vec<> here is a b in #(func a b)
    Key(Key),
    V(Value),
    LambdaValue(usize),
    ParamValue(usize),
    WhereValue(usize),
    Function(usize, usize),
    BridgedFunction(u16),
    Recurse,
    RuntimeList(Vec<Token>),
    Lambda(Vec<Token>),
    Group(Vec<Token>),
    IfStatement(Vec<(Vec<Token>, Vec<Token>)>, Vec<Token>),

    // Other
    AnyValue,
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
            Token::Key(k) => k.fmt(f),
            Token::Import(source) => {
                let path_representation = source
                    .iter()
                    .map(|p| p.to_str().unwrap())
                    .collect::<Vec<&str>>()
                    .join(":");
                write!(f, "use {}", path_representation,)
            }
            Token::Function(_, _) => write!(f, "function"),
            Token::BridgedFunction(_) => write!(f, "rust bridged function"),
            Token::IfStatement(_, _) => write!(f, "if statement"),
            Token::TrackedIfStatement(_, _) => write!(f, "if statement"),
            Token::Recurse => write!(f, "function"),
            Token::RuntimeList(_) => write!(f, "list"),
            Token::TrackedRuntimeList(_) => write!(f, "list"),
            Token::LambdaValue(_) => write!(f, "value"),
            Token::ParamValue(_) => write!(f, "parameter"),
            Token::WhereValue(_) => write!(f, "value"),
            Token::LambdaHeader(_) => write!(f, "lambda"),
            Token::Lambda(_) => write!(f, "lambda"),
            Token::Word(s) => f.write_str(&s),
            Token::HeaderFnEndNoArgs => f.write_str("new line"),
            Token::V(t) => write!(f, "{}", t.pretty_print()),
            Token::EOF => write!(f, "end of file"),
            Token::AnyValue => write!(f, "any value"),
            Token::Group(buf) => write!(
                f,
                "Group<\n{:?}\n>",
                buf.iter().map(|t| t.to_string()).collect::<Vec<String>>(),
            ),
            Token::TrackedGroup(buf) => write!(
                f,
                "TrackedGroup<\n{:?}\n>",
                buf.iter()
                    .map(|t| format!("{}:{:?}", t.position, t.inner))
                    .collect::<Vec<String>>(),
            ),
            _ => write!(f, "{:#?}", self),
        }
    }
}
