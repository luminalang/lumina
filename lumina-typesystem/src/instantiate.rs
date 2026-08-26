use super::{ConstType, Environment, GenericTag, KnownType, Prim, TaggedGeneric, VariableInfo};
use crate::key;
use crate::key::{EntityList, EntityRef, ListPool, ReservedValue};

#[derive(Debug, Clone)]
pub struct Annotation<T: EntityRef + ReservedValue> {
    pub block: EntityList<T>,
    pub item: EntityList<T>,
    pub lambda: EntityList<T>,

    pub self_: Option<T>,
}

impl<T: EntityRef + ReservedValue> Default for Annotation<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: EntityRef + ReservedValue> Annotation<T> {
    pub fn new() -> Self {
        Self {
            block: EntityList::new(),
            item: EntityList::new(),
            lambda: EntityList::new(),

            self_: None,
        }
    }

    pub fn item(item: EntityList<T>) -> Self {
        Self {
            item,
            block: EntityList::new(),
            lambda: EntityList::new(),
            self_: None,
        }
    }

    pub fn get(&self, generic: TaggedGeneric, pool: &ListPool<T>) -> Option<T> {
        match generic.tag {
            GenericTag::Trait | GenericTag::Impl => self.block.get(generic.key.0 as usize, pool),
            GenericTag::Func | GenericTag::Type => self.item.get(generic.key.0 as usize, pool),
            GenericTag::Lambda(_) => self.lambda.get(generic.key.0 as usize, pool),
        }
    }

    pub fn push(&mut self, tag: GenericTag, ty: T, pool: &mut ListPool<T>) -> key::Generic {
        let i = match tag {
            GenericTag::Trait | GenericTag::Impl => self.block.push(ty, pool),
            GenericTag::Func | GenericTag::Type => self.item.push(ty, pool),
            GenericTag::Lambda(_) => self.lambda.push(ty, pool),
        };

        key::Generic(i as u32)
    }
}

impl Annotation<key::Var> {
    pub fn get_with_env<Ident>(
        &self,
        generic: TaggedGeneric,
        env: &Environment<Ident>,
    ) -> Option<key::Var> {
        self.get(generic, &env.var_pool)
    }

    pub fn push_unknown<Ident: std::fmt::Debug>(
        &mut self,
        tag: GenericTag,
        env: &mut Environment<Ident>,
    ) -> key::Generic {
        let ty = env.unknown();
        self.push_with_env(tag, ty, env)
    }

    pub fn push_with_env<Ident>(
        &mut self,
        tag: GenericTag,
        ty: key::Var,
        env: &mut Environment<Ident>,
    ) -> key::Generic {
        self.push(tag, ty, &mut env.var_pool)
    }
}

pub trait InstableType<Ident>: Sized {
    fn inst(
        &self,
        env: &mut Environment<Ident>,
        anot: &Annotation<key::Var>,
        for_ident: &mut dyn FnMut(&Ident) -> Ident,
    ) -> key::Var;
}

impl<Ident: Clone + std::fmt::Display + std::fmt::Debug> InstableType<Ident> for &KnownType<Ident> {
    fn inst(
        &self,
        env: &mut Environment<Ident>,
        anot: &Annotation<key::Var>,
        for_ident: &mut dyn FnMut(&Ident) -> Ident,
    ) -> key::Var {
        match self {
            KnownType::Error => env.error(),
            KnownType::Defined(name, params) => {
                let type_params = env.instantiate_types(anot, params.values(), for_ident);
                env.defined(for_ident(name), type_params)
            }
            KnownType::List(inner_type) => {
                let inner_type = env.instantiate(anot, &**inner_type, for_ident);
                env.list(inner_type)
            }
            KnownType::Array { of, len } => {
                let of = env.instantiate(anot, &**of, for_ident);
                let len = env.instantiate(anot, &**len, for_ident);
                env.array(of, len)
            }
            KnownType::Const(const_) => match const_ {
                ConstType::Int(n) => env.const_int(*n),
            },
            KnownType::Tuple(elems) => {
                let elems = env.instantiate_types(anot, elems, for_ident);
                env.tuple(elems)
            }
            KnownType::Generic(generic) => match anot.get(*generic, &env.var_pool) {
                Some(ty) => ty,
                None => panic!("{generic} not annotated"),
            },
            KnownType::Prim(Prim::Self_) => match anot.self_ {
                Some(var) => var,
                None => env.prim(Prim::Self_), // TODO: Is this dangerous?
            },
            KnownType::Prim(prim) => env.prim(*prim),
            KnownType::Pointer(inner_type) => {
                let inner_type = env.instantiate(anot, &**inner_type, for_ident);
                env.pointer(inner_type)
            }
            KnownType::Function { kind, params, ret } => {
                let params = env.instantiate_types(anot, params, for_ident);
                let ret = env.instantiate(anot, &**ret, for_ident);
                env.function(*kind, params, ret)
            }
        }
    }
}

impl<Ident: Clone + std::fmt::Display + std::fmt::Debug> InstableType<Ident> for &key::Var {
    fn inst(
        &self,
        env: &mut Environment<Ident>,
        anot: &Annotation<key::Var>,
        for_ident: &mut dyn FnMut(&Ident) -> Ident,
    ) -> key::Var {
        match env.variables[**self].info.clone() {
            VariableInfo::InferTo(var) => (&var).inst(env, anot, for_ident),
            VariableInfo::Applied { .. } => todo!(
                "ok we probably need a helper method for figuring out whether it's poison or valid yield"
            ),
            VariableInfo::Prim(Prim::Self_) => match anot.self_ {
                Some(var) => var,
                None => **self,
            },
            VariableInfo::Error
            | VariableInfo::Unknown
            | VariableInfo::Numeric
            | VariableInfo::TypeResolvedFunction(_)
            | VariableInfo::Prim(_) => **self,
            VariableInfo::Generic(generic) => match generic.tag {
                GenericTag::Lambda(_) => match anot.get(generic, &env.var_pool) {
                    Some(mapped) => mapped,
                    None => panic!("{generic} not annotated"),
                },
                GenericTag::Trait | GenericTag::Impl | GenericTag::Func | GenericTag::Type => {
                    **self
                }
            },
            VariableInfo::Defined(name, params) => {
                let (new_params, changed) = env.instantiate_entities(anot, params, for_ident);
                if_changed(**self, changed, || {
                    env.defined(for_ident(&name), new_params)
                })
            }
            VariableInfo::Tuple(elems) => {
                let (new_elems, changed) = env.instantiate_entities(anot, elems, for_ident);
                if_changed(**self, changed, || env.tuple(new_elems))
            }
            VariableInfo::List(inner) => {
                let new_inner = env.instantiate(anot, &inner, for_ident);
                if_changed(**self, new_inner != inner, || env.list(new_inner))
            }
            VariableInfo::Array { of, len } => {
                let new_of = env.instantiate(anot, &of, for_ident);
                let new_len = env.instantiate(anot, &len, for_ident);
                if_changed(**self, new_of != of, || env.array(new_of, new_len))
            }
            VariableInfo::Const(const_) => match const_ {
                ConstType::Int(n) => env.const_int(n),
            },
            VariableInfo::Pointer(inner) => {
                let new_inner = env.instantiate(anot, &inner, for_ident);
                if_changed(inner, new_inner != inner, || env.pointer(new_inner))
            }
            VariableInfo::Function { kind, params: Some(params), ret } => {
                let (params, changed) = env.instantiate_entities(anot, params, for_ident);
                let new_ret = env.instantiate(anot, &ret, for_ident);
                if_changed(**self, changed || new_ret != ret, || {
                    env.function(kind, params, new_ret)
                })
            }
            VariableInfo::Function { kind, params: None, ret } => {
                let new_ret = env.instantiate(anot, &ret, for_ident);
                if_changed(**self, new_ret != ret, || {
                    env.unknown_function(kind, new_ret)
                })
            }
        }
    }
}

fn if_changed(old: key::Var, changed: bool, f: impl FnOnce() -> key::Var) -> key::Var {
    if changed { f() } else { old }
}

impl<Ident: Clone + std::fmt::Display + std::fmt::Debug> Environment<Ident> {
    pub fn instantiate(
        &mut self,
        anot: &Annotation<key::Var>,
        ty: impl InstableType<Ident>,
        for_ident: &mut dyn FnMut(&Ident) -> Ident,
    ) -> key::Var {
        ty.inst(self, anot, for_ident)
    }

    pub fn instantiate_types<T: InstableType<Ident>>(
        &mut self,
        anot: &Annotation<key::Var>,
        types: impl IntoIterator<Item = T>,
        for_ident: &mut dyn FnMut(&Ident) -> Ident,
    ) -> EntityList<key::Var> {
        let mut instantiated = EntityList::new();
        for ty in types.into_iter() {
            let param = self.instantiate(anot, ty, for_ident);
            instantiated.push(param, &mut self.var_pool);
        }
        instantiated
    }

    pub fn instantiate_entities(
        &mut self,
        anot: &Annotation<key::Var>,
        types: EntityList<key::Var>,
        for_ident: &mut dyn FnMut(&Ident) -> Ident,
    ) -> (EntityList<key::Var>, bool) {
        // Here we're sacrificing performance for the sake of cleaner vars. We first scan whether
        // any instantiation needs to be performed. And if not; we just re-use the existing
        // EntityList.
        let mut changed = false;
        self.for_vars_mut(types, |this, ty| {
            let new = this.instantiate(anot, &ty, for_ident);
            changed |= ty != new;
        });

        if changed {
            let mut list = EntityList::new();
            self.for_vars_mut(types, |this, ty| {
                let new = this.instantiate(anot, &ty, for_ident);
                list.push(new, &mut this.var_pool);
            });
            (list, true)
        } else {
            (types.clone(), false)
        }
    }
}
