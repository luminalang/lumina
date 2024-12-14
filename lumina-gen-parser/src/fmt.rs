use super::*;
use itertools::Itertools;
use std::fmt;

impl<'s> fmt::Display for Entity<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Entity::Commented(comment, _, then) => {
                write!(f, "{comment}\n{then}")
            }
            Entity::Delim(_, delim, elems) => write!(f, "{}\n'{delim}'\n{}", elems[0], elems[1]),
            Entity::Operator(path) => path.fmt(f),
            Entity::Literal(lit) => lit.fmt(f),
            Entity::Sequence(elems) => write!(f, "{}", elems.iter().format(" ")),
            Entity::Clause(start, end, None) => write!(f, "{start}{end}"),
            Entity::Clause(start, end, Some(inner)) => {
                write!(f, "{start}\n  {}\n{end}", indent(inner, "\n  "))
            }
            Entity::Unary(_, name, then) => write!(f, "{name}{then}"),
            Entity::Identifier(apath) => write!(f, "{apath}"),
            Entity::Keyword(kw, then) => write!(f, "Keyword({kw})\n  {}", indent(then, "\n  "),),
            Entity::Where(where_) => write!(f, "where\n  {}", where_.iter().format("\n  ")),
            Entity::Impl(impl_) => write!(f, "impl\n  {}", impl_.iter().format("\n  ")),
            Entity::When(elems) => write!(f, "when\n  {}", elems.iter().format("\n  ")),
            Entity::Poison => "<POISON>".fmt(f),
        }
    }
}

fn indent(v: impl fmt::Display, str: &str) -> String {
    v.to_string().lines().format(str).to_string()
}

impl<'s> fmt::Display for Literal<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Int(true, n) => write!(f, "-{n}"),
            Literal::Int(false, n) => write!(f, "{n}"),
            Literal::Float(fl) => fl.fmt(f),
            Literal::DoubleQuote(inner) => write!(f, "\"{inner}\""),
            Literal::SingleQuote(inner) => write!(f, "'{inner}'"),
        }
    }
}
