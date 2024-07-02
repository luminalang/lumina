use crate::ast::AST;
use crate::hir;
use crate::hir::HIR;
use crate::mir::TEnv;
use derive_new::new;
use itertools::Itertools;
use key::{Map, M};
use lumina_key as key;
use lumina_key::LinearFind;
use lumina_typesystem::{Container, Forall, Generic, IType, IntConstraint, RecordVar, Type};
use lumina_util::Highlighting;
use lumina_util::Tr;
use std::fmt;
use std::fmt::Display;
use tracing::info;

pub trait TyFormatted<'a, 's> {
    fn tyfmt(&self, state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

/// Format types with state that allows them to be user-presentable
#[derive(new, Clone)]
pub struct TyFmtState<'a, 's> {
    // ast: &'a AST<'s>,
    hir: &'a HIR<'s>,
    env: &'a TEnv<'s>,

    forall: Map<key::Generic, &'s str>,
    pforall: Map<key::Generic, &'s str>,

    #[new(default)]
    highlighting: bool,
    #[new(value = "true")]
    surface: bool,
}

impl<'a, 's, P: TyFormatted<'a, 's>, K: Into<key::TypeKind> + Clone> TyFormatted<'a, 's>
    for (M<K>, &[P])
{
    fn tyfmt(&self, state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self.0.value.clone().into() {
            key::TypeKind::Record(ty) => state.hir.records[self.0.module.m(ty)].0,
            key::TypeKind::Sum(ty) => state.hir.sums[self.0.module.m(ty)].0,
            key::TypeKind::Trait(ty) => state.hir.traits[self.0.module.m(ty)].0,
        };

        match self.1.len() {
            0 => write!(f, "{name}"),
            _ => {
                if state.surface {
                    write!(f, "{name} {}", state.elems(" ", false, self.1))
                } else {
                    if state.highlighting {
                        write!(
                            f,
                            "{}{name} {}{}",
                            '('.symbol(),
                            state.elems(" ", false, self.1),
                            ')'.symbol()
                        )
                    } else {
                        write!(f, "({name} {})", state.elems(" ", false, self.1))
                    }
                }
            }
        }
    }
}

impl<'a, 's, T: TyFormatted<'a, 's>> TyFormatted<'a, 's> for hir::Typing<T> {
    fn tyfmt(&self, mut state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self.params.values().map(|t| &t.value).collect::<Vec<_>>();
        write!(
            f,
            "{}{} {} {}{}",
            '('.symbol(),
            state.clone().elems(", ", true, &params),
            "->".symbol(),
            {
                state.surface = true;
                state.fmt(&*self.returns)
            },
            ')'.symbol(),
        )
    }
}

impl<'a, 's> TyFormatted<'a, 's> for (RecordVar, &'s str) {
    fn tyfmt(&self, state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (var, fname) = (self.0, self.1);
        match state.env.get_record(var) {
            lumina_typesystem::RecordAssignment::Ok(key, params) => {
                let ty = state.env.inst(&params, |inst| {
                    state.hir.fnames[*key]
                        .find(|name| fname == **name)
                        .map(|field| inst.apply(&state.hir.field_types[*key][field]))
                });

                match ty {
                    Some(ty) => state.fmt(&ty).fmt(f),
                    None => write!(f, "{var}.{fname}"),
                }
            }
            lumina_typesystem::RecordAssignment::Redirect(var) => state.fmt((*var, fname)).fmt(f),
            lumina_typesystem::RecordAssignment::NotRecord(ty) => state.fmt(ty).fmt(f),
            lumina_typesystem::RecordAssignment::Unknown(_) => write!(f, "{var}.{fname}"),
            lumina_typesystem::RecordAssignment::None => write!(f, "_"),
        }
    }
}

impl<'a, 's> TyFormatted<'a, 's> for IType {
    fn tyfmt(&self, state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IType::Container(con) => write!(f, "{}", state.fmt(con)),
            IType::Defined(ty, params) => state.fmt((*ty, params.as_slice())).fmt(f),
            IType::List(_, params) => {
                write!(
                    f,
                    "[{}]",
                    params.iter().map(|ty| state.clone().fmt(ty)).format(", ")
                )
            }
            IType::InferringRecord(rvar) => match state.env.get_record(*rvar) {
                lumina_typesystem::RecordAssignment::Ok(key, params) => {
                    state.fmt((*key, params.as_slice())).fmt(f)
                }
                lumina_typesystem::RecordAssignment::Redirect(var) => {
                    state.fmt(IType::InferringRecord(*var)).fmt(f)
                }
                lumina_typesystem::RecordAssignment::NotRecord(ty) => state.fmt(ty).fmt(f),
                _ => {
                    let fields = state.env.fields_of(*rvar);
                    write!(f, "{{{}}}", fields.map(|name| *name).format(", "))
                }
            },
            IType::Prim(prim) => write!(f, "{}", prim),
            IType::Generic(generic) => match generic.kind {
                lumina_typesystem::GenericKind::Lambda(_) => todo!(),
                lumina_typesystem::GenericKind::Entity => match state.forall.get(generic.key) {
                    None => write!(f, "ERR_MISSING_GENERIC:{generic}"),
                    Some(g) => g.fmt(f),
                },
                lumina_typesystem::GenericKind::Parent => state.pforall[generic.key].fmt(f),
            },
            IType::Self_ => write!(f, "self"),
            IType::Var(var) => match state.env.get(*var).value {
                Some(ty) => write!(f, "{}", state.fmt(&**ty)),
                None => match state.env.constraint_of(*var) {
                    None => "_".fmt(f),
                    Some(IntConstraint { .. }) => "int".fmt(f),
                },
            },
            IType::Field(var, field) => {
                let fname = state.env.name_of_field(*var, *field);
                state.fmt((*var, *fname)).fmt(f)
            }
        }
    }
}

impl<'a, 's> TyFormatted<'a, 's> for Type {
    fn tyfmt(&self, state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Container(con) => write!(f, "{}", state.fmt(con)),
            Type::Defined(ty, params) if params.is_empty() => {
                let name = match ty.value {
                    key::TypeKind::Record(record) => *state.hir.records[ty.module.m(record)].0,
                    key::TypeKind::Sum(sum) => *state.hir.sums[ty.module.m(sum)].0,
                    key::TypeKind::Trait(tr) => *state.hir.traits[ty.module.m(tr)].0,
                };
                write!(f, "{name}")
            }
            Type::List(_, params) => {
                write!(f, "[{}]", state.fmt(params.last().unwrap()))
            }
            Type::Defined(ty, params) => {
                let name = match ty.value {
                    key::TypeKind::Record(record) => *state.hir.records[ty.module.m(record)].0,
                    key::TypeKind::Sum(sum) => *state.hir.sums[ty.module.m(sum)].0,
                    key::TypeKind::Trait(tr) => *state.hir.traits[ty.module.m(tr)].0,
                };
                if state.surface {
                    write!(f, "{name} {}", state.elems(" ", false, params))
                } else {
                    if state.highlighting {
                        write!(
                            f,
                            "{}{name} {}{}",
                            '('.symbol(),
                            state.elems(" ", false, params),
                            ')'.symbol()
                        )
                    } else {
                        write!(f, "({name} {})", state.elems(" ", false, params))
                    }
                }
            }
            Type::Prim(prim) => write!(f, "{}", prim),
            Type::Generic(generic) => match generic.kind {
                lumina_typesystem::GenericKind::Lambda(_) => todo!(),
                lumina_typesystem::GenericKind::Entity => state.forall[generic.key].fmt(f),
                lumina_typesystem::GenericKind::Parent => state.pforall[generic.key].fmt(f),
            },
            Type::Self_ => write!(f, "self"),
        }
    }
}

fn iheader(signed: bool) -> &'static str {
    match signed {
        true => "i",
        false => "u",
    }
}

impl<'a, 's, T: TyFormatted<'a, 's>> TyFormatted<'a, 's> for Container<T> {
    fn tyfmt(&self, state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Container::Func(kind, ptypes, returns) if state.highlighting => {
                write!(
                    f,
                    "{}{}{} {} {}{}",
                    kind.keyword(),
                    '('.symbol(),
                    state.clone().elems(", ", true, ptypes),
                    "->".symbol(),
                    state.fmt(&**returns),
                    ')'.symbol(),
                )
            }
            Container::Func(kind, ptypes, returns) => {
                write!(
                    f,
                    "{kind}({} -> {})",
                    state.clone().elems(", ", true, ptypes),
                    state.fmt(&**returns)
                )
            }
            Container::Tuple(elems) if state.highlighting => write!(
                f,
                "{}{}{}",
                '('.symbol(),
                state.elems(", ", true, elems),
                ')'.symbol()
            ),
            Container::Tuple(elems) => write!(f, "({})", state.elems(", ", true, elems)),
            Container::Pointer(inner) if state.highlighting => {
                write!(f, "{}{}", '*'.symbol(), state.fmt(&**inner))
            }
            Container::Pointer(inner) => {
                write!(f, "*{}", state.fmt(&**inner))
            }
        }
    }
}

pub struct Args<'a, T>(&'static str, &'a [T]);

impl<'a, 's, 't, T: TyFormatted<'a, 's>> TyFormatted<'a, 's> for Args<'t, T> {
    fn tyfmt(&self, mut state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.1
                .iter()
                .map(|t| {
                    state.surface = true;
                    state.clone().fmt(t)
                })
                .format(self.0)
        )
    }
}

impl<'a, 's> TyFmtState<'a, 's> {
    pub fn fmt<T: TyFormatted<'a, 's>>(self, value: T) -> TyFormatter<'a, 's, T> {
        TyFormatter { value, state: self }
    }

    pub fn elems<'t, T: TyFormatted<'a, 's>>(
        mut self,
        sep: &'static str,
        surface: bool,
        values: &'t [T],
    ) -> TyFormatter<'a, 's, Args<'t, T>> {
        self.surface = surface;
        self.fmt(Args(sep, values))
    }

    pub fn function<T: TyFormatted<'a, 's>>(
        self,
        name: impl fmt::Display,
        typing: &hir::Typing<T>,
    ) -> String {
        format!(
            "{} {name} {} {}",
            "fn".keyword(),
            "as".keyword(),
            self.fmt(typing)
        )
    }
}

pub struct TyFormatter<'a, 's, T> {
    state: TyFmtState<'a, 's>,
    value: T,
}

impl<'a, 's, T: TyFormatted<'a, 's>> fmt::Display for TyFormatter<'a, 's, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.tyfmt(self.state.clone(), f)
    }
}

impl<'a, 's, T: TyFormatted<'a, 's>> TyFormatted<'a, 's> for &T {
    fn tyfmt(&self, state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        T::tyfmt(self, state, f)
    }
}
