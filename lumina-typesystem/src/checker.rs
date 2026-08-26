use tracing::{error, info_span};

use super::*;
use crate::key;

pub struct Checker<'a, Ident> {
    assignments: &'a Map<key::Var, KnownType<Ident>>,
    env: &'a Environment<Ident>,

    errors: Vec<Error<Ident>>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Error<Ident> {
    Mismatch {
        expected: KnownType<Ident>,
        given: KnownType<Ident>,
        kind: MismatchKind,
        var: Var,
    },
    UnknownFunction(Var),
    NonFunctionApplication(Application, KnownType<Ident>),
    FunctionWrongParameterCount {
        expected: usize,
        given: usize,
        appl: Application,
    },
    DoesNotHaveFields(Var, KnownType<Ident>, Vec<String>),
    UnresolvedFunction {
        func: Var,
        dominant: Var,
    },
    CircularInference {
        func: Var,
    },
    NoListType(Var),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum MismatchKind {
    Parameter(key::Application, usize),
    SameasExpr(key::SameasUnification),
    SameasList(key::SameasUnification),
    Assignment,
}

impl<'a, Ident: std::fmt::Debug + std::fmt::Display + Clone + PartialEq + Eq> Checker<'a, Ident> {
    pub fn new(
        assignments: &'a Map<key::Var, KnownType<Ident>>,
        env: &'a Environment<Ident>,
    ) -> Self {
        Self { assignments, env, errors: vec![] }
    }

    pub fn type_check(&mut self) -> Vec<Error<Ident>> {
        for (&func, reason) in self.env.failed_receiver_lookups.iter() {
            match *reason {
                FailedReceiverLookup::Unresolved(dominant) => {
                    self.err(Error::UnresolvedFunction { func, dominant })
                }
                FailedReceiverLookup::CircularInference => {
                    self.err(Error::CircularInference { func })
                }
            }
        }

        for appl in self.env.applications.keys() {
            info_span!("", appl = appl.to_string(), check = "application");
            self.type_check_application(appl);
        }

        for (i, assignment) in self.env.assignments.iter().enumerate() {
            info_span!("", i = i, check = "assignment");
            self.type_check_assignment(*assignment);
        }

        for (i, ret) in self.env.returns_.iter().enumerate() {
            info_span!("", i = i, check = "return");
            self.type_check_return(*ret);
        }

        for var in self.env.vars() {
            if let VariableInfo::Function { params: None, .. } = &self.env.variables[var].info {
                error!("unknown-function remained uninferred");
                self.err(Error::UnknownFunction(var));
            }

            self.type_check_fields_and_lists(var);
        }

        for sameas_key in self.env.same_as_unifications.keys() {
            info_span!("", sameas = sameas_key.to_string(), check = "same-as");
            self.type_check_sameas(sameas_key);
        }

        std::mem::take(&mut self.errors)
    }

    fn type_check_sameas(&mut self, sameas_key: key::SameasUnification) {
        let sameas_unification = &self.env.same_as_unifications[sameas_key];

        let expected = &self.assignments[self.env.expected_of_sameas(sameas_key)];
        for i in 0..sameas_unification.members.len(&self.env.var_pool) {
            let var = sameas_unification
                .members
                .get(i, &self.env.var_pool)
                .unwrap();
            let given = &self.assignments[var];

            if *expected != *given {
                let message = match sameas_unification.main {
                    inf::SameasMain::List { .. } => MismatchKind::SameasList(sameas_key),
                    inf::SameasMain::JoinExpression(_) => MismatchKind::SameasExpr(sameas_key),
                };

                self.err_type_mismatch(expected, given, message, var);
            }
        }
    }

    fn type_check_application(&mut self, appl_key: key::Application) {
        let appl = &self.env.applications[appl_key];

        match &self.assignments[appl.func] {
            KnownType::Error => return,
            KnownType::Function { params, .. } => {
                let given_len = appl.parameters.len(&self.env.var_pool);
                if params.len() != given_len {
                    self.err(Error::FunctionWrongParameterCount {
                        appl: appl_key,
                        expected: params.len(),
                        given: given_len,
                    });

                    return;
                }

                for (i, expected) in params.iter().enumerate() {
                    let given_var = appl.parameters.get(i, &self.env.var_pool).unwrap();
                    let given = &self.assignments[given_var];
                    if expected != given {
                        let kind = MismatchKind::Parameter(appl_key, i);
                        self.err_type_mismatch(expected, given, kind, given_var)
                    }
                }
            }

            // Since we don't know ahead of time whether something with 0 parameters is a function
            // that takes 0 parameter or a value, we assume everything is the former and allow it here.
            _ if appl.parameters.is_empty() => {}

            _ => {
                self.err(Error::NonFunctionApplication(
                    appl_key,
                    self.assignments[appl.func].clone(),
                ));
                return;
            }
        };
    }

    fn type_check_assignment(&mut self, assgn: inf::Assignment) {
        let [expected, given] = [assgn.lhs, assgn.rhs].map(|var| &self.assignments[var]);
        if expected != given {
            let kind = MismatchKind::Assignment;
            self.err_type_mismatch(expected, given, kind, assgn.rhs);
        }
    }

    fn type_check_return(&mut self, ret: inf::Return) {
        let [expected, given] = [ret.expected, ret.expr].map(|var| &self.assignments[var]);
        if expected != given {
            let kind = MismatchKind::Assignment;
            self.err_type_mismatch(expected, given, kind, ret.expr);
        }
    }

    fn type_check_fields_and_lists(&mut self, var: key::Var) {
        let variable = &self.env.variables[var];

        if variable.has_fields.is_empty() {
            return;
        }

        info_span!(
            "",
            var = var.to_string(),
            check = format!("fields-and-lists")
        );

        if let VariableInfo::List(_) = &variable.info {
            self.err(Error::NoListType(var));
        }

        let type_ = &self.assignments[var];
        if matches!(type_, KnownType::Error) {
            return;
        }

        if !matches!(type_, KnownType::Defined(..)) {
            let fields = variable.has_fields.iter().map(|f| f.name.clone()).collect();
            self.err(Error::DoesNotHaveFields(var, type_.clone(), fields));
        }
    }

    fn err(&mut self, err: Error<Ident>) {
        self.errors.push(err);
    }

    fn err_type_mismatch(
        &mut self,
        expected: &KnownType<Ident>,
        given: &KnownType<Ident>,
        kind: MismatchKind,
        var: Var,
    ) {
        error!("type mismatch: {expected} != {given}, \"{kind:?}\"");
        self.errors.push(Error::Mismatch {
            expected: expected.clone(),
            given: given.clone(),
            kind,
            var,
        });
    }
}
