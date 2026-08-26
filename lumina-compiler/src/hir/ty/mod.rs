use crate::{
    errors, key,
    project::symbols::{self},
    Context, InterResolved, TranslationUnit,
};
use lumina_key::SecondaryMap;
use lumina_typesystem::{KnownType, Var};
use lumina_util::Span;
use serde::Serialize;
use std::fmt;

mod constr;
mod inst;
mod lower;
pub use lower::{LoweringKind, TypeLower};
mod infer_and_check;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct TypeKey {
    pub origin: symbols::Origin,
    pub key: key::Type,
}
pub type Type = KnownType<TypeKey>;
pub type Env = lumina_typesystem::Environment<TypeKey>;

impl fmt::Display for TypeKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}·{}", self.origin, self.key)
    }
}
impl fmt::Debug for TypeKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}·{}", self.origin, self.key)
    }
}

pub struct StaticEnv {
    pub self_: Option<Type>,
    pub invalid_inference_reason_given: &'static str,
}

impl StaticEnv {
    pub fn new(invalid_inference_reason_given: &'static str) -> Self {
        Self { self_: None, invalid_inference_reason_given }
    }

    pub fn with_self(mut self, self_: Type) -> Self {
        self.self_ = Some(self_);
        self
    }
}

pub struct InferenceEnv {
    pub self_: Option<Var>,
    env: Env,
    pub spans: SecondaryMap<Var, Span>,
}

impl InferenceEnv {
    pub fn new() -> Self {
        Self {
            self_: None,
            env: Env::new(),
            spans: SecondaryMap::with_default(Span::null()),
        }
    }

    pub fn var_pool(&self) -> &key::ListPool<Var> {
        &self.env.var_pool
    }
}
