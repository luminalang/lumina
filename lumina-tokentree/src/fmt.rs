use super::*;
use itertools::Itertools;
use std::fmt;

impl<'s> fmt::Display for Entity<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Entity::Commented(_, _, then) => {
                write!(
                    f,
                    "commented\n  {}",
                    then.to_string().lines().format("\n  ")
                )
            }
            Entity::Isolated(token) => {
                write!(f, "{token:?}")
            }
            Entity::Match(_, elems) => {
                write!(f, "match {}", &elems[0]).unwrap();
                elems.iter().skip(1).try_for_each(|elem| {
                    write!(f, "\n| {}", elem.to_string().lines().format("\n  "))
                })
            }
            Entity::DotPostfix(elems) => {
                write!(f, "{}.{}", elems[0], elems[1])
            }
            Entity::Symbol(op) => write!(f, "{op}"),
            Entity::Literal(lit) => write!(f, "{lit}"),
            Entity::Header(name, _, and_then) => write!(
                f,
                "{name}\n  {}",
                and_then.to_string().lines().format("\n  ")
            ),
            Entity::Headers(headers) => {
                for (name, _, and_then) in headers {
                    writeln!(
                        f,
                        "{name}\n  {}",
                        and_then.to_string().lines().format("\n  ")
                    )?;
                }
                Ok(())
            }
            Entity::Clause(start, _, Some(inner)) => {
                write!(
                    f,
                    "Clause({start})\n  {}",
                    inner.to_string().lines().format("\n  ")
                )
            }
            Entity::Clause(start, end, None) => {
                write!(f, "Clause({start}{end}) -")
            }
            // Entity::Clause(start, end, None) => write!(f, "{start}{end}"),
            // Entity::Clause(start, end, Some(inner)) => write!(f, "{start} {inner} {end}"),
            Entity::Identifier(ident) => write!(f, "{ident}"),
            Entity::Sequence(seq) => seq.iter().format("\n").fmt(f),
            Entity::Unary(_, op, then) => write!(f, "{op}{then}"),
            //Entity::Where(elems) => write!(f, "(where [{}])", elems.iter().format(", ")),
            //Entity::Impl(elems) => write!(f, "(impl [{}])", elems.iter().format(", ")),
            Entity::IndentBlock(name, elems) => {
                write!(f, "{name}\n  {}", elems.iter().format("\n  "))
            }
            Entity::Missing => write!(f, "<POISON>"),
        }
    }
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
