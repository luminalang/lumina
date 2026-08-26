//! Helper methods for iterating and getting information from a type environment

use super::{CallableKind, Environment, VariableInfo, inf};
use crate::key;
use crate::key::EntityList;

impl<Ident> Environment<Ident> {
    pub(crate) fn vars(&self) -> impl Iterator<Item = key::Var> + 'static {
        self.variables.keys()
    }

    pub(crate) fn expected_of_sameas(&self, sameas_key: key::SameasUnification) -> key::Var {
        match self.same_as_unifications[sameas_key].main {
            inf::SameasMain::List { elem, .. } => elem,
            inf::SameasMain::JoinExpression(var) => var,
        }
    }

    pub(crate) fn get_members(&self, sameas: key::SameasUnification) -> EntityList<key::Var> {
        self.same_as_unifications[sameas].members
    }

    pub(crate) fn get_defined(&self, var: key::Var) -> Option<(&Ident, EntityList<key::Var>)> {
        let var = self.follow(var);
        match &self.variables[var].info {
            VariableInfo::Defined(name, params) => Some((name, *params)),
            _ => None,
        }
    }

    pub(crate) fn get_defined_ident(&self, var: key::Var) -> Option<&Ident> {
        self.get_defined(var).map(|(ident, _)| ident)
    }

    pub(crate) fn has_fields(&self, var: key::Var) -> &[inf::HasField] {
        // let var = self.follow(var);
        &self.variables[var].has_fields
    }

    pub(crate) fn follow(&self, var: key::Var) -> key::Var {
        match &self.variables[var].info {
            VariableInfo::InferTo(var) => self.follow(*var),
            VariableInfo::Applied { func, appl } => {
                let appl = &self.applications[*appl];

                match &self.variables[*func].info {
                    VariableInfo::Function { ret, .. } => self.follow(*ret),
                    _ if appl.parameters.is_empty() => self.follow(appl.func),
                    _ => var,
                }
            }
            _ => var,
        }
    }

    pub fn get_appl_function(&self, appl: key::Application) -> key::Var {
        self.applications[appl].func
    }
}
