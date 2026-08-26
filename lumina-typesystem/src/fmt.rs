use super::*;
use itertools::Itertools;
use std::fmt;
use std::fmt::Write;

impl<Ident: fmt::Debug> fmt::Debug for Environment<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Environment {{")?;

        writeln!(f, "variables:")?;
        for (var, var_data) in self.variables.iter() {
            writeln!(f, "  {var} -> {var_data:?}")?;
        }

        writeln!(f, "applications:")?;
        for (appl, appl_data) in self.applications.iter() {
            writeln!(
                f,
                "  {appl} -> {} {}",
                appl_data.func,
                appl_data
                    .parameters
                    .as_slice(&self.var_pool)
                    .iter()
                    .format(" "),
            )?;
        }

        writeln!(f, "assignments:")?;
        for assgn_data in self.assignments.iter() {
            writeln!(f, "{} = {}", assgn_data.lhs, assgn_data.rhs)?;
        }

        writeln!(f, "same-as-unifications:")?;
        for (sameas, sameas_data) in self.same_as_unifications.iter() {
            writeln!(
                f,
                "  {sameas} -> {:?} {}",
                sameas_data.main,
                sameas_data
                    .members
                    .as_slice(&self.var_pool)
                    .iter()
                    .format(" ")
            )?;
        }

        write!(f, "}}")
    }
}

impl fmt::Debug for inf::Application {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {} ... {}", self.func, self.ret)
    }
}

impl<Ident: fmt::Debug> fmt::Debug for Variable<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} {{ {} }}{}",
            &self.info,
            self.has_fields
                .iter()
                .map(|field| format!("{} {}", field.name, field.field_type))
                .format(", "),
            match self.source {
                VariableSource::Expression => "",
                VariableSource::Signature => " ∀",
            },
        )
    }
}

impl<Ident: fmt::Debug> fmt::Debug for VariableInfo<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Applied { func, appl } => write!(f, "{appl}({func})"),
            Self::InferTo(new) => write!(f, "to({new})"),
            Self::Error => write!(f, "<error>"),
            Self::Const(const_) => write!(f, "{const_}"),
            Self::Unknown => write!(f, "_"),
            Self::Numeric => write!(f, "{{integer}}"),
            Self::Defined(ident, _) => write!(f, "{ident:?} ..."),
            Self::Tuple(elems) if elems.is_empty() => write!(f, "()"),
            Self::Tuple(_) => write!(f, "(...)"),
            Self::List(_) => write!(f, "[...]"),
            Self::Array { of, len } => write!(f, "[{of}; {len}]"),
            Self::Generic(generic) => write!(f, "'{generic}"),
            Self::Prim(prim) => prim.fmt(f),
            Self::Pointer(inner) => write!(f, "*{inner}"),
            Self::TypeResolvedFunction(name) => {
                write!(f, "type-resolved({name})")
            }
            Self::Function { kind, ret, .. } => write!(f, "({kind} ... -> {ret})"),
        }
    }
}

impl fmt::Display for ConstType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{int}"),
        }
    }
}

impl fmt::Debug for ConstType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{int}"),
        }
    }
}

// Some rather nasty code to do stateful `fmt::Display`
pub struct KnownTypeFormatter<'a, TY, IFMT, GFMT> {
    fmt_ident: &'a IFMT,
    fmt_generic: &'a GFMT,
    ty: &'a TY,
}

impl<'a, TY, IFMT, GFMT> KnownTypeFormatter<'a, TY, IFMT, GFMT> {
    pub fn new(fmt_ident: &'a IFMT, fmt_generic: &'a GFMT, ty: &'a TY) -> Self {
        Self { fmt_ident, fmt_generic, ty }
    }

    pub fn fork<NEW>(&self, new: &'a NEW) -> KnownTypeFormatter<'_, NEW, IFMT, GFMT> {
        KnownTypeFormatter {
            fmt_ident: self.fmt_ident,
            fmt_generic: self.fmt_generic,
            ty: new,
        }
    }
}

impl<'a, Ident, IFMT, GFMT> fmt::Display
    for KnownTypeFormatter<'a, (&'a [KnownType<Ident>], &'static str), IFMT, GFMT>
where
    IFMT: Fn(&Ident) -> String,
    GFMT: Fn(TaggedGeneric) -> String,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, param) in self.ty.0.iter().enumerate() {
            self.fork(param).fmt(f)?;
            if i != self.ty.0.len() - 1 {
                self.ty.1.fmt(f)?;
            }
        }

        Ok(())
    }
}

impl<'a, Ident, IFMT, GFMT> fmt::Display for KnownTypeFormatter<'a, KnownType<Ident>, IFMT, GFMT>
where
    IFMT: Fn(&Ident) -> String,
    GFMT: Fn(TaggedGeneric) -> String,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            KnownType::Error => "?".fmt(f),
            KnownType::Prim(prim) => prim.fmt(f),
            KnownType::Pointer(to) => {
                '*'.fmt(f)?;
                self.fork(&**to).fmt(f)
            }
            KnownType::Defined(ident, params) if params.is_empty() => {
                (self.fmt_ident)(ident).fmt(f)
            }
            KnownType::Defined(ident, params) => {
                write!(f, "({} ", (self.fmt_ident)(ident))?;
                self.fork(&(params.as_values_slice(), " ")).fmt(f)?;
                write!(f, ")")
            }
            KnownType::Tuple(elems) => {
                '('.fmt(f)?;
                self.fork(&(elems.as_slice(), ", ")).fmt(f)?;
                ')'.fmt(f)
            }
            KnownType::Generic(generic) => (self.fmt_generic)(*generic).fmt(f),
            KnownType::Array { of, len } => {
                '['.fmt(f)?;
                self.fork(&**of).fmt(f)?;
                "; ".fmt(f)?;
                self.fork(&**len).fmt(f)?;
                ']'.fmt(f)
            }
            KnownType::Const(const_) => const_.fmt(f),
            KnownType::List(of) => {
                '['.fmt(f)?;
                self.fork(&**of).fmt(f)?;
                write!(f, "]")
            }
            KnownType::Function { kind, params, ret } => {
                write!(f, "({kind} ")?;
                self.fork(&(params.as_slice(), " ")).fmt(f)?;
                write!(f, " -> ")?;
                self.fork(&**ret).fmt(f)?;
                ')'.fmt(f)
            }
        }
    }
}

impl fmt::Display for Prim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prim::Int(int_size) => int_size.fmt(f),
            Prim::Float => "f64".fmt(f),
            Prim::Bool => "bool".fmt(f),
            Prim::Self_ => "self".fmt(f),
        }
    }
}

impl<Ident: fmt::Display> fmt::Display for KnownType<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        KnownTypeFormatter {
            ty: self,
            fmt_ident: &|ident: &Ident| format!("{ident}"),
            fmt_generic: &|generic: TaggedGeneric| format!("{generic}"),
        }
        .fmt(f)
    }
}

impl fmt::Display for CallableKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CallableKind::Closure => "fn",
            CallableKind::FnPointer => "fnptr",
        }
        .fmt(f)
    }
}
