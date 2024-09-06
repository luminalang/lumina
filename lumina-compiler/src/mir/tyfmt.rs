use crate::hir;
use crate::hir::HIR;
use crate::mir::TEnv;
use derive_new::new;
use key::{Map, M};
use lumina_key as key;
use lumina_typesystem::{Container, GenericKind, Inference, Static, Ty};
use lumina_util::Highlighting;
use std::fmt;
use std::fmt::Display;

pub trait TyFormatted<'a, 's> {
    fn tyfmt(&self, state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

trait FmtSpecial<'a, 's>: fmt::Display {
    fn specialfmt(&self, state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl<'a, 's> FmtSpecial<'a, 's> for Inference {
    fn specialfmt(&self, state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ty = state.env.get(*self);
        match ty {
            Ok(ty) => state.fmt(&**ty).fmt(f),
            Err(_) => "_".fmt(f),
        }
    }
}

impl<'a, 's> FmtSpecial<'a, 's> for Static {
    fn specialfmt(&self, _: TyFmtState<'a, 's>, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        unreachable!("special static: {self}");
    }
}

/// Format types with state that allows them to be user-presentable
#[derive(new, Clone)]
pub struct TyFmtState<'a, 's> {
    pub(crate) hir: &'a HIR<'s>,
    env: &'a TEnv<'s>,

    default_intsize: u8,

    forall: Map<key::Generic, &'s str>,
    pforall: Map<key::Generic, &'s str>,

    #[new(value = "true")]
    surface: bool,
}

impl<'a, 's, P: TyFormatted<'a, 's>, K: Into<key::TypeKind> + Clone> TyFormatted<'a, 's>
    for (M<K>, &[P])
{
    fn tyfmt(&self, state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self.0 .1.clone().into() {
            key::TypeKind::Record(ty) => state.hir.records[ty.inside(self.0 .0)].0,
            key::TypeKind::Sum(ty) => state.hir.sums[ty.inside(self.0 .0)].0,
            key::TypeKind::Trait(ty) => state.hir.traits[ty.inside(self.0 .0)].0,
        };

        let paren = !state.surface;
        Container::fmt_defined(
            *name,
            self.1,
            |ty| state.clone().fmts(ty).to_string(),
            f,
            paren,
        )
    }
}

impl<'a, 's, T: TyFormatted<'a, 's>> TyFormatted<'a, 's> for hir::Typing<T> {
    fn tyfmt(&self, state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self
            .params
            .values()
            .map(|t| &t.value)
            .chain(std::iter::once(&*self.returns))
            .collect::<Vec<_>>();
        Container::fmt_func("", &params, |ty| state.clone().fmts(ty).to_string(), f)
    }
}

impl<'a, 's, T: FmtSpecial<'a, 's>> TyFormatted<'a, 's> for Ty<T> {
    fn tyfmt(&self, state: TyFmtState<'a, 's>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Container(Container::Defined(key, _), params) => {
                state.fmt((*key, params.as_slice())).fmt(f)
            }
            Ty::Container(con, params) => {
                con.fmt(params, |ty| state.clone().fmts(ty).to_string(), f)
            }
            Ty::Generic(generic) => {
                let mut or_ugly = |forall: &Map<key::Generic, &str>, key| match forall
                    .has(key)
                    .then(|| &forall[key])
                {
                    None => write!(f, "ERR_MISSING_GENERIC:{generic}"),
                    Some(g) => g.fmt(f),
                };

                match generic.kind {
                    GenericKind::Lambda(_) => todo!(),
                    GenericKind::Entity => or_ugly(&state.forall, generic.key),
                    GenericKind::Parent => or_ugly(&state.pforall, generic.key),
                }
            }
            Ty::Special(special) => T::specialfmt(special, state, f),
            Ty::Int(size) if size.bits() == state.default_intsize => {
                if size.signed {
                    "int".fmt(f)
                } else {
                    "uint".fmt(f)
                }
            }
            ty if state.surface => write!(f, "{ty}"),
            ty => write!(f, "{}{ty}{}", '('.symbol(), ')'.symbol()),
        }
    }
}

impl<'a, 's> TyFmtState<'a, 's> {
    pub fn fmt<T: TyFormatted<'a, 's>>(self, value: T) -> TyFormatter<'a, 's, T> {
        TyFormatter { value, state: self }
    }

    pub fn fmts<T: TyFormatted<'a, 's>>(mut self, value: T) -> TyFormatter<'a, 's, T> {
        self.surface = true;
        TyFormatter { value, state: self }
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
            self.fmts(typing)
        )
    }
}

/// Stateful formatter for typed data
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
