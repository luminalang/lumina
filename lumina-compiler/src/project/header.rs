//! Lumina header file for entire projec

use super::config;
use super::symbols;
use crate::hir::{Type, TypeKey};
use crate::key;
use itertools::Itertools;
use lumina_key::Map;
use lumina_key::SecondaryMap;
use lumina_typesystem as ts;
use lumina_typesystem::{Forall, TaggedGeneric};
use lumina_util::Highlighting;
use lumina_util::ParamFmt;
use serde::Serialize;
use std::fmt;

pub struct HeaderFile {
    // All function signatures including those that are for trait and impl methods
    pub(crate) function_signatures: SecondaryMap<key::Func, Option<FuncDef>>,
    pub type_signatures: Map<key::Type, TypeSig>,
    pub typedefs: Map<key::Type, TypeDef>,
    pub implementations: Map<key::Impl, ImplDef>,
    pub values: Map<key::Val, ValDef>,

    // Other projects used in this project, where each key::External corresponds to their
    // declaration order in `config.lm`.
    //
    // Any additional element after those specified in `config.lm` are indirect dependencies caused
    // by usage of public re-exports in one of the explicit externals.
    pub externals: Map<key::External, config::Dependency>,

    pub symbols: symbols::Unit,
    pub name: String,
}

#[derive(Clone)]
pub struct ValDef {
    pub initialiser: key::Func,
}

#[derive(Clone)]
pub struct FuncDef {
    pub name: String,
    pub sig: FuncSig<Type>,
    pub precedence: u32,
    pub method_of: Option<(key::Type, key::Method)>,
    pub file: key::File,
    pub has_body: bool,
}

pub struct ImplDef {
    pub forall: Forall<TypeKey>,
    pub trait_: TypeKey,
    pub trait_params: Vec<Type>,
    pub methods: Map<key::Method, Option<key::Func>>,
    pub self_: Type,
}

pub struct TypeSig {
    pub name: String,
    pub generics: Map<key::Generic, String>,
    pub file: key::File,
}

#[derive(Clone, Serialize)]
pub enum TypeDef {
    Sum {
        vtypes: Map<key::Variant, Vec<Type>>,
        vnames: Map<key::Variant, String>,
    },
    Struct {
        ftypes: Map<key::Field, Type>,
        fnames: Map<key::Field, String>,
    },
    Trait {
        functions: Map<key::Method, key::Func>,
    },
    Alias {
        for_: Type,
    },
}

#[derive(Debug, Clone, Serialize)]
pub struct FuncSig<Type> {
    pub forall: Forall<TypeKey>,
    pub params: Vec<Type>,
    pub ret: Type,
}

impl HeaderFile {
    pub fn new(name: impl Into<String>, files: usize) -> Self {
        HeaderFile {
            function_signatures: SecondaryMap::new(),
            type_signatures: Map::new(),
            typedefs: Map::new(),
            externals: Map::new(),
            implementations: Map::new(),
            values: Map::new(),
            symbols: symbols::Unit::new(files),
            name: name.into(),
        }
    }

    pub fn stdlib(&self) -> Option<symbols::Origin> {
        self.externals
            .iter()
            .find_map(|(ext, dep)| (dep.name == "std").then_some(symbols::Origin::Inter(ext)))
            .or_else(|| (self.name == "std").then_some(symbols::Origin::Intra))
    }

    pub fn set_func(
        &mut self,
        file: key::File,
        key: key::Func,
        name: String,
        sig: FuncSig<Type>,
        method_of: Option<(key::Type, key::Method)>,
        precedence: u32,
        has_body: bool,
    ) {
        let slot = &mut self.function_signatures[key];
        assert!(slot
            .replace(FuncDef { name, sig, method_of, file, precedence, has_body })
            .is_none());
    }

    pub fn take_func(&mut self, func: key::Func) -> Option<FuncDef> {
        self.function_signatures[func].take()
    }

    pub fn func(&self, key: key::Func) -> &FuncDef {
        self.function_signatures[key]
            .as_ref()
            .expect("function is unlowered")
    }

    pub fn resolve_impl_method(&self, impl_: key::Impl, func: key::Func) -> Option<key::Method> {
        self.implementations[impl_]
            .methods
            .iter()
            .find_map(|(method, f)| (Some(func) == *f).then_some(method))
    }

    pub fn as_trait(&self, ty: key::Type) -> Option<&Map<key::Method, key::Func>> {
        match &self.typedefs[ty] {
            TypeDef::Trait { functions } => Some(functions),
            _ => None,
        }
    }

    pub fn variant(&self, ty: key::Type, var: key::Variant) -> (&[Type], &str) {
        match &self.typedefs[ty] {
            TypeDef::Sum { vtypes, vnames } => (&vtypes[var], &vnames[var]),
            _ => panic!("cannot get {var} of non-sum {ty}"),
        }
    }

    pub fn field(&self, ty: key::Type, f: key::Field) -> (&Type, &str) {
        match &self.typedefs[ty] {
            TypeDef::Struct { ftypes, fnames } => (&ftypes[f], &fnames[f]),
            _ => panic!("cannot get {f} of non-struct {ty}"),
        }
    }

    pub fn method(&self, ty: key::Type, method: key::Method) -> key::Func {
        match &self.typedefs[ty] {
            TypeDef::Trait { functions, .. } => functions[method],
            _ => panic!("cannot get {method} of non-method {ty}"),
        }
    }
}

impl<T> FuncSig<T> {
    pub fn new(params: impl IntoIterator<Item = T>, ret: T) -> Self {
        Self {
            forall: Forall::new(),
            params: params.into_iter().collect(),
            ret,
        }
    }

    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> FuncSig<U> {
        FuncSig {
            forall: self.forall,
            ret: f(self.ret),
            params: self.params.into_iter().map(f).collect(),
        }
    }
}

fn fmt_when_item<Ident: fmt::Display>(
    names: &Map<key::Generic, String>,
    cons: &Map<key::Generic, Vec<(Ident, Map<key::Generic, ts::KnownType<Ident>>)>>,
) -> String {
    // TODO: Pretty print constraint types with a StaticTypeFormatter

    if !names.is_empty() {
        format!(
            "{} {} · ",
            "when".keyword(),
            names
                .iter()
                .map(|(g, name)| {
                    match cons.get(g) {
                        None => format!("{name}"),
                        Some(cons) if cons.is_empty() => format!("{name}"),
                        Some(cons) => format!(
                            "{name} {} {}",
                            "can".keyword(),
                            cons.iter()
                                .map(|(ident, params)| ParamFmt::new(
                                    ident,
                                    params.as_values_slice()
                                ))
                                .format(" + ")
                        ),
                    }
                })
                .format(", ")
        )
    } else {
        String::new()
    }
}

impl FuncDef {
    pub fn fmt<'a, IFMT: Fn(&TypeKey) -> String, GFMT: Fn(TaggedGeneric) -> String>(
        &self,
        fmt: ts::KnownTypeFormatter<'a, (), IFMT, GFMT>,
    ) -> String {
        format!(
            "{}{} {} {} {} -> {}",
            fmt_when_item(&self.sig.forall.names, &self.sig.forall.constraints)
                .lines()
                .format("\n  "),
            "fn".keyword(),
            self.name,
            "as".keyword(),
            fmt.fork(&(self.sig.params.as_slice(), " ")),
            &self.sig.ret
        )
    }
}

impl fmt::Display for FuncDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{} {} {} {}{} {}",
            fmt_when_item(&self.sig.forall.names, &self.sig.forall.constraints)
                .lines()
                .format("\n  "),
            "fn".keyword(),
            self.name,
            "as".keyword(),
            self.sig.params.iter().format(" "),
            if self.sig.params.is_empty() {
                "->"
            } else {
                " ->"
            },
            &self.sig.ret
        )
    }
}

impl fmt::Display for HeaderFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{\n")?;

        for (ext, dep) in self.externals.iter() {
            writeln!(f, "  {ext} -> {dep}")?;
        }
        if !self.externals.is_empty() {
            writeln!(f)?;
        }

        for sig in self.function_signatures.values() {
            match sig {
                Some(func) => writeln!(f, "  {}", func.to_string().lines().format("\n  ")),
                None => write!(f, "  <unlowered>"),
            }?;
        }
        if !self.function_signatures.is_empty() {
            writeln!(f)?;
        }

        for impldef in self.implementations.values() {
            fmt_when_item(&impldef.forall.names, &impldef.forall.constraints)
                .lines()
                .format("\n  ")
                .fmt(f)?;

            writeln!(
                f,
                "{} {} {} {}",
                "impl".keyword(),
                ParamFmt::new(&impldef.trait_, &impldef.trait_params),
                "for".keyword(),
                impldef.self_
            )?;
        }
        if !self.function_signatures.is_empty() {
            writeln!(f)?;
        }

        for ((tkey, tdef), tsig) in self.typedefs.iter().zip(self.type_signatures.values()) {
            fmt_when_item(&tsig.generics, &Map::<_, Vec<(TypeKey, _)>>::new())
                .lines()
                .format("\n  ")
                .fmt(f)?;

            match tdef {
                TypeDef::Sum { vtypes, vnames } => {
                    write!(f, "  {} {tkey}", "type".keyword())?;
                    writeln!(
                        f,
                        "\n  = {}",
                        vnames.values().zip(vtypes.values()).format_with(
                            " | ",
                            |(vname, vtys), f| f(&format_args!(
                                "{vname} {}",
                                vtys.iter().format(" ")
                            ))
                        )
                    )
                }
                TypeDef::Struct { ftypes, fnames } if ftypes.is_empty() => {
                    write!(f, "  {} {tkey}", "type".keyword())?;
                    writeln!(f, " {{}}")
                }
                TypeDef::Struct { ftypes, fnames } => {
                    write!(f, "  {} {tkey}", "type".keyword())?;
                    writeln!(
                        f,
                        " {{\n  {}\n}}",
                        ftypes
                            .values()
                            .zip(fnames.values())
                            .format_with("\n  ", |(name, ty), f| f(&format_args!("{name} {ty}")))
                    )
                }
                TypeDef::Trait { functions } => {
                    write!(f, "  {} {tkey}", "trait".keyword())?;
                    if !functions.is_empty() {
                        writeln!(
                            f,
                            "\n  {}",
                            functions.iter().format_with("\n  ", |(method, func), f| f(
                                &format_args!("{method} => {func}")
                            ))
                        )
                    } else {
                        writeln!(f)
                    }
                }
                TypeDef::Alias { for_ } => {
                    writeln!(f, "{} {tkey} = {for_}", "alias".keyword())
                }
            }?;
        }
        if !self.type_signatures.is_empty() {
            writeln!(f)?;
        }

        writeln!(f, "  {}", self.symbols.to_string().lines().format("\n  "))?;

        write!(f, "}}")
    }
}
