use super::*;
use itertools::Itertools;
use lumina_util::ParamFmt;
use std::fmt;

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Container(cont) => cont.fmt(f),
            Type::Prim(prim) => prim.fmt(f),
            Type::List(type_, params) | Type::Defined(type_, params) => {
                ParamFmt::new(type_, params).fmt(f)
            }
            Type::Generic(generic) => generic.fmt(f),
            Type::Self_ => "self".fmt(f),
        }
    }
}

impl fmt::Display for Prim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prim::Float => "float".fmt(f),
            Prim::Poison => "???".fmt(f),
            Prim::Bool => "bool".fmt(f),
            Prim::Never => "never".fmt(f),
            Prim::Int(true, bitsize) if *bitsize == Bitsize::default() => "int".fmt(f),
            Prim::Int(false, bitsize) if *bitsize == Bitsize::default() => "uint".fmt(f),
            Prim::Int(true, bitsize) => write!(f, "i{bitsize}"),
            Prim::Int(false, bitsize) => write!(f, "u{bitsize}"),
        }
    }
}

impl fmt::Display for IType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IType::Container(cont) => cont.fmt(f),
            IType::Prim(prim) => prim.fmt(f),
            IType::Defined(type_, params) | IType::List(type_, params) => {
                ParamFmt::new(&type_, params).fmt(f)
            }
            IType::InferringRecord(rvar) => rvar.fmt(f),
            IType::Generic(generic) => generic.fmt(f),
            IType::Self_ => "self".fmt(f),

            IType::Var(var) => var.fmt(f),
            IType::Field(var, field) => write!(f, "{var}[{field}]"),
        }
    }
}

impl<T: fmt::Display> fmt::Display for Container<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Container::Func(kind, ptypes, returns) => {
                write!(f, "{kind}({} -> {})", ptypes.iter().format(", "), returns)
            }
            Container::Tuple(elems) => write!(f, "({})", elems.iter().format(", ")),
            Container::Pointer(inner) => write!(f, "*{inner}"),
        }
    }
}

impl fmt::Display for FuncKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FuncKind::Closure => "fn".fmt(f),
            FuncKind::FnPointer => "fnptr".fmt(f),
        }
    }
}
