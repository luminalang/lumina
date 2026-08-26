use super::{Environment, KnownType, Map, VariableInfo};
use crate::key;
use std::collections::HashMap;
use std::fmt;

/// Assume type inference has been completed and convert types to their known form.
pub struct Finalizer<'a, Ident> {
    assignments: HashMap<key::Var, KnownType<Ident>>,
    env: &'a mut Environment<Ident>,
}

impl<'a, Ident: Clone + fmt::Display + fmt::Debug> Finalizer<'a, Ident> {
    pub fn new(env: &'a mut Environment<Ident>) -> Self {
        Self { assignments: HashMap::new(), env }
    }

    pub fn finalize_all(&mut self) -> Map<key::Var, KnownType<Ident>> {
        self.env
            .variables
            .keys()
            .map(|var| self.var_to_known(var))
            .collect()
    }

    fn var(&mut self, var: key::Var) {
        if self.assignments.contains_key(&var) {
            return;
        }

        let info = self.env.variables[var].info.clone();
        let ty = match info {
            VariableInfo::Applied { func, appl } => {
                // todo!("do we want to first check whether parameters.is_empty else poison?")
                match self.env.get_return_type_of_var(func) {
                    Some(ret) => self.var_to_known(ret),
                    None if self.env.applications[appl].parameters.is_empty() => {
                        self.var_to_known(func)
                    }
                    None => KnownType::Error,
                }
            }
            VariableInfo::InferTo(var) => self.var_to_known(var),
            VariableInfo::Error => KnownType::Error,
            VariableInfo::Function { params: None, .. } => KnownType::Error,

            VariableInfo::Numeric | VariableInfo::Unknown => {
                panic!("type variable {var} remained un-inferred post type inference passes")
            }
            VariableInfo::Tuple(elems) => {
                let mut known = Vec::with_capacity(elems.len(&self.env.var_pool));
                for i in 0..elems.len(&self.env.var_pool) {
                    let var = elems.get(i, &self.env.var_pool).unwrap();
                    known.push(self.var_to_known(var));
                }

                KnownType::Tuple(known)
            }
            VariableInfo::Defined(name, params) => {
                let mut known = Map::new();
                for i in 0..params.len(&self.env.var_pool) {
                    let var = params.get(i, &self.env.var_pool).unwrap();
                    known.push(self.var_to_known(var));
                }

                KnownType::Defined(name, known)
            }
            VariableInfo::List(_) => KnownType::Error,
            VariableInfo::Array { of, len } => {
                let of = self.var_to_known(of);
                let len = self.var_to_known(len);
                KnownType::array(of, len)
            }
            VariableInfo::Const(const_) => KnownType::Const(const_),
            VariableInfo::Generic(generic) => KnownType::Generic(generic),
            VariableInfo::Prim(prim) => KnownType::Prim(prim),
            VariableInfo::Pointer(inner_type) => {
                let inner_type = self.var_to_known(inner_type);
                KnownType::Pointer(Box::new(inner_type))
            }
            VariableInfo::TypeResolvedFunction(_) => KnownType::Error,
            VariableInfo::Function { kind, params: Some(params), ret } => {
                let mut known = Vec::with_capacity(params.len(&self.env.var_pool));
                for i in 0..params.len(&self.env.var_pool) {
                    let var = params.get(i, &self.env.var_pool).unwrap();
                    known.push(self.var_to_known(var));
                }
                let ret = self.var_to_known(ret);

                KnownType::Function { kind, params: known, ret: Box::new(ret) }
            }
        };

        self.assignments.insert(var, ty);
    }

    fn var_to_known(&mut self, var: key::Var) -> KnownType<Ident> {
        self.var(var);
        self.assignments[&var].clone()
    }
}
