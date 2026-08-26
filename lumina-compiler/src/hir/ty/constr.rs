use super::{InferenceEnv, TypeKey};
use crate::errors;
use lumina_typesystem as ts;
use lumina_typesystem::{Application, Var};
use lumina_util::Span;

type Env = ts::Environment<TypeKey>;

impl InferenceEnv {
    pub fn define(&mut self, span: Span, f: impl FnOnce(&mut Env) -> Var) -> Var {
        let var = f(&mut self.env);
        self.spans[var] = span;
        #[cfg(debug_assertions)]
        if !span.is_null() {
            let name = errors::get_str(span);
            self.env.debug_strings[var] = name.to_string();
        }
        var
    }

    pub fn assign(&mut self, src: Var, target: Var) {
        self.env.assign(src, target);
    }

    pub fn assign_return(&mut self, expr: Var, expected: Var) {
        self.env.assign_return(expr, expected);
    }

    pub fn apply(&mut self, span: Span, var: Var) -> Application {
        let cappl = self.env.apply(var);
        let ret = self.env.get_return_type(cappl);
        if self.spans[ret].is_null() {
            self.spans[ret] = span;
        }
        cappl
    }

    pub fn apply_next_parameter(&mut self, appl: Application, ty: Var) {
        self.env.apply_next_parameter(appl, ty);
    }

    pub fn get_return_type(&mut self, appl: Application) -> Var {
        self.env.get_return_type(appl)
    }

    // pub fn get_return_type_of_var(&mut self, func: Var) -> Var {
    // self.env.get_return_type_of_var(func)
    // }

    pub fn get_return_type_of_var(&mut self, var: Var) -> Option<Var> {
        self.env.get_return_type_of_var(var)
    }

    pub fn list_sameas(&mut self, span: Span) -> (ts::SameasUnification, Var, Var) {
        let (sameas, list, inner) = self.env.list_sameas();
        self.spans[list] = span;
        self.spans[inner] = span;
        (sameas, list, inner)
    }
    pub fn expr_sameas(&mut self, span: Span, init: Option<Var>) -> (ts::SameasUnification, Var) {
        let (sameas, inner) = self.env.expr_sameas(init);
        self.spans[inner] = span;
        (sameas, inner)
    }

    pub fn add_sameas_member(&mut self, sameas: ts::SameasUnification, var: Var) {
        self.env.add_sameas_member(sameas, var);
    }

    pub fn enter_signature(&mut self) {
        self.env.enter_signature();
    }

    pub fn leave_signature_enter_expression(&mut self) {
        self.env.leave_signature_enter_expression();
    }

    #[allow(unused)]
    pub fn debug_print(&self) {
        self.env.debug_print();
    }

    pub fn poison(&mut self, var: Var) {
        self.env.poison(var)
    }
}
