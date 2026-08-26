mod checker;
mod finalize;
mod fmt;
mod forall;
mod getters;
mod inference_passes;
mod instantiate;
mod intsize;
mod key;

pub use checker::{Checker, Error, MismatchKind};
pub use finalize::Finalizer;
pub use fmt::KnownTypeFormatter;
pub use forall::find_generic;
pub use forall::{
    ArrayLen, Constraints, Forall, ForallEnv, GenericTag, TaggedGeneric, add_constraint, declare,
    implicitly_declare,
};
use inference_passes as inf;
pub use inference_passes::{
    InferenceUnifier, KnownTypeRoot, ReceiverLookupResult, Resolver, ResolverError,
};
pub use instantiate::{Annotation, InstableType};
pub use intsize::IntSize;
pub use key::{Application, SameasUnification, Var};
use lumina_key::{EntityList, ListPool, Map, SecondaryMap};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// The environment represents a single functions state as its in the process of being inferred and
/// type checked.
///
/// Each type that may not be fully known is represented as a type variable.
/// Type variables track information of what information about it is known, which functions its
/// applied to, what it's assigned to, etc.
pub struct Environment<Ident> {
    variables: Map<key::Var, Variable<Ident>>,

    applications: Map<key::Application, inf::Application>,
    assignments: Vec<inf::Assignment>,
    returns_: Vec<inf::Return>,
    same_as_unifications: Map<key::SameasUnification, inf::SameasUnification>,

    failed_receiver_lookups: HashMap<key::Var, FailedReceiverLookup>,

    pub var_pool: ListPool<key::Var>,

    current_source: VariableSource,

    pub debug_strings: SecondaryMap<key::Var, String>,
}

pub enum FailedReceiverLookup {
    Unresolved(key::Var),
    CircularInference,
}

pub struct Variable<Ident> {
    info: VariableInfo<Ident>,
    has_fields: Vec<inf::HasField>,
    source: VariableSource,

    constraints: Constraints<Ident>,
    // `f` is ambigious during inference as it could either have the signature
    // `(fn -> a)` or `a`.
    //
    // In this situation `Option<key::Var>` is set and an application is added.
    //
    // Before inference starts, all instances of `var` may then be substituted with this `key::Var`
    // if the applications hint towards it being `a` instead of `(fn -> a)`. Otherwise; it's
    // inferred as `(fn -> a)` and `key::Var` is moved to that `VariableInfo::Function`.

    // If this type variable is unknown but applicated, then we want all applications to have the
    // same tvar as the return type instead of creating a new one in each `inf::Application`.
    // application_return: Option<key::Var>,
    // TODO: Can we iterate the `var_pool` to visit all vars?
}

impl<Ident> Variable<Ident> {
    fn new(source: VariableSource, info: VariableInfo<Ident>) -> Self {
        Self { info, has_fields: vec![], constraints: vec![], source }
    }
}

#[derive(Clone, Deserialize, Serialize, Debug, Copy, PartialEq, Eq)]
pub enum CallableKind {
    Closure,
    FnPointer,
}

impl CallableKind {
    fn can_apply(&self, other: &CallableKind) -> bool {
        matches!(
            (self, other),
            (CallableKind::FnPointer, _) | (CallableKind::Closure, CallableKind::Closure)
        )
    }
}

#[derive(Clone, Deserialize, Serialize, Debug, Copy, PartialEq, Eq, Hash)]
pub enum Prim {
    Int(IntSize),
    Bool,
    Float,
    Self_,
}

/// The incomplete information we may hold of this variables inferred type
#[derive(Clone)]
enum VariableInfo<Ident> {
    /// Created from an invalid type/expression and intentionally ignored by checks.
    Error,
    Unknown,
    Numeric,
    Defined(Ident, EntityList<key::Var>),
    Tuple(EntityList<key::Var>),
    List(key::Var),
    Array {
        of: key::Var,
        len: key::Var,
    },
    Const(ConstType),
    Generic(TaggedGeneric),
    Prim(Prim),
    Pointer(key::Var),
    TypeResolvedFunction(String),
    Function {
        kind: CallableKind,
        params: Option<EntityList<key::Var>>,
        ret: key::Var,
    },
    Applied {
        func: key::Var,
        appl: key::Application,
    },
    InferTo(key::Var),
}

#[derive(Clone, Serialize, PartialEq, Eq)]
pub enum ConstType {
    Int(i128),
}

/// Whether this variable was created in the functions type signature or elsewhere in its body.
///
/// The information is relevant for deciding how to default an uninferred type variable.
#[derive(Clone, Copy, Debug)]
enum VariableSource {
    Expression,
    Signature,
}

#[derive(Clone, Debug, Serialize)]
pub enum KnownType<Ident> {
    Error,
    Defined(Ident, Map<key::Generic, Self>),
    List(Box<Self>),
    Array {
        of: Box<Self>,
        len: Box<Self>,
    },
    Const(ConstType),
    Tuple(Vec<Self>),
    Generic(TaggedGeneric),
    Prim(Prim),
    Pointer(Box<Self>),
    Function {
        kind: CallableKind,
        params: Vec<Self>,
        ret: Box<Self>,
    },
}

impl<Ident> KnownType<Ident> {
    pub fn record<const N: usize>(name: Ident, params: impl Into<Vec<Self>>) -> Self {
        KnownType::Defined(name, params.into().into())
    }

    pub fn list(inner: Self) -> Self {
        KnownType::List(Box::new(inner))
    }

    pub fn array(of: Self, len: Self) -> Self {
        KnownType::Array { len: Box::new(len), of: Box::new(of) }
    }

    pub fn tuple<const N: usize>(elems: [Self; N]) -> Self {
        KnownType::Tuple(elems.into())
    }

    pub fn generic(generic: TaggedGeneric) -> Self {
        KnownType::Generic(generic)
    }

    pub fn function<const N: usize>(kind: CallableKind, params: [Self; N], ret: Self) -> Self {
        KnownType::Function { kind, params: params.into(), ret: Box::new(ret) }
    }

    pub fn i(bits: u8) -> Self {
        KnownType::Prim(Prim::Int(IntSize::new(true, bits)))
    }

    pub fn pointer(inner: Self) -> Self {
        KnownType::Pointer(Box::new(inner))
    }

    pub fn default_unit_type() -> Self {
        KnownType::Tuple(vec![])
    }
}

impl<Ident: PartialEq> PartialEq for KnownType<Ident> {
    fn eq(&self, other: &Self) -> bool {
        if matches!(self, KnownType::Error) || matches!(other, KnownType::Error) {
            return true;
        }

        match (self, other) {
            (KnownType::Defined(a_name, a_params), KnownType::Defined(b_name, b_params)) => {
                a_name == b_name && a_params == b_params
            }
            (KnownType::List(a), KnownType::List(b)) => a == b,
            (
                KnownType::Array { of: a_of, len: a_len },
                KnownType::Array { of: b_of, len: b_len },
            ) => a_len == b_len && a_of == b_of,
            (KnownType::Tuple(a), KnownType::Tuple(b)) => a == b,
            (KnownType::Generic(a), KnownType::Generic(b)) => a == b,
            (KnownType::Prim(a), KnownType::Prim(b)) => a == b,
            (KnownType::Pointer(a), KnownType::Pointer(b)) => a == b,
            (KnownType::Error, _) | (_, KnownType::Error) => true,
            (KnownType::Const(a_const), KnownType::Const(b_const)) => a_const == b_const,
            (
                KnownType::Function { kind: a_kind, params: a_params, ret: a_ret },
                KnownType::Function { kind: b_kind, params: b_params, ret: b_ret },
            ) => a_kind == b_kind && a_params == b_params && a_ret == b_ret,
            _ => false,
        }
    }
}

impl<Ident: Eq> Eq for KnownType<Ident> {}

impl<Ident: std::fmt::Debug> Default for Environment<Ident> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Ident> Environment<Ident> {
    // fn visit_vars_list<F>(&self, vars: key::EntityList<key::Var>, f: &mut F)
    // where
    //     F: FnMut(&Self, key::Var),
    // {
    //     let mut iter = key::EntityIter::from(vars);
    //     while let Some(var) = iter.next(&self.var_pool) {
    //         self.visit_vars(None, var, f);
    //     }
    // }

    // fn visit_vars<F>(&self, mut root: Option<key::Var>, var: key::Var, f: &mut F)
    // where
    //     F: FnMut(&Self, key::Var),
    // {
    //     if root == Some(var) {
    //         return;
    //     } else if let None = root {
    //         root = Some(var);
    //     }

    //     f(self, var);

    //     for i in 0.. {
    //         let Some(has_field) = self.variables[var].has_fields.get(i) else {
    //             break;
    //         };

    //         self.visit_vars(root, has_field.field_type, f);
    //     }

    //     let (vars, var) = match &self.variables[var].info {
    //         VariableInfo::Tuple(elems) | VariableInfo::Defined(_, elems) => (elems.clone(), None),
    //         VariableInfo::Pointer(var)
    //         | VariableInfo::Function { params: None, ret: var, .. }
    //         | VariableInfo::List(var) => (key::EntityList::new(), Some(*var)),
    //         VariableInfo::Const(_)
    //         | VariableInfo::Numeric
    //         | VariableInfo::Unknown
    //         | VariableInfo::Error
    //         | VariableInfo::TypeResolvedFunction(_)
    //         | VariableInfo::Prim(_)
    //         | VariableInfo::Generic(_) => (EntityList::new(), None),

    //         &VariableInfo::Array { of, len } => {
    //             self.visit_vars(root, of, f);
    //             self.visit_vars(root, len, f);
    //             return;
    //         }
    //         VariableInfo::Function { params: Some(params), ret, .. } => {
    //             (params.clone(), Some(*ret))
    //         }
    //     };

    //     if let Some(var) = var {
    //         self.visit_vars(root, var, f);
    //     }

    //     let mut iter = key::EntityIter::from(vars);
    //     while let Some(var) = iter.next(&self.var_pool) {
    //         self.visit_vars(root, var, f);
    //     }
    // }
}

impl<Ident: std::fmt::Debug> Environment<Ident> {
    pub fn new() -> Self {
        Self {
            variables: Map::new(),

            same_as_unifications: Map::new(),
            applications: Map::new(),
            assignments: vec![],
            returns_: vec![],

            failed_receiver_lookups: HashMap::new(),

            var_pool: ListPool::new(),

            current_source: VariableSource::Signature,

            debug_strings: SecondaryMap::new(),
        }
    }

    pub fn poison(&mut self, var: key::Var) {
        self.variables[var].info = VariableInfo::Error;
    }

    pub fn in_signature(&self) -> bool {
        matches!(self.current_source, VariableSource::Signature)
    }

    pub fn push_constraint(
        &mut self,
        var: key::Var,
        ident: Ident,
        params: Map<key::Generic, KnownType<Ident>>,
    ) {
        self.variables[var].constraints.push((ident, params));
    }

    pub fn push_constraints(&mut self, var: key::Var, cons: Constraints<Ident>) {
        self.variables[var].constraints.extend(cons);
    }

    /// Marks the end of the function signatures.
    ///
    /// Type variables declared past this point will not be able to implicitly declare generics.
    pub fn leave_signature_enter_expression(&mut self) {
        self.current_source = VariableSource::Expression;
    }
    pub fn enter_signature(&mut self) {
        self.current_source = VariableSource::Signature;
    }

    pub fn i(&mut self, bits: u8) -> key::Var {
        self.prim(Prim::Int(IntSize::new(true, bits)))
    }
    pub fn u(&mut self, bits: u8) -> key::Var {
        self.prim(Prim::Int(IntSize::new(false, bits)))
    }

    pub fn const_int(&mut self, n: i128) -> key::Var {
        self.variables.push(Variable::new(
            self.current_source,
            VariableInfo::Const(ConstType::Int(n)),
        ))
    }

    pub fn int(&mut self, size: IntSize) -> key::Var {
        self.prim(Prim::Int(size))
    }

    pub fn prim(&mut self, prim: Prim) -> key::Var {
        self.variables
            .push(Variable::new(self.current_source, VariableInfo::Prim(prim)))
    }

    pub fn unknown(&mut self) -> key::Var {
        self.variables
            .push(Variable::new(self.current_source, VariableInfo::Unknown))
    }

    pub fn error(&mut self) -> key::Var {
        self.variables
            .push(Variable::new(self.current_source, VariableInfo::Error))
    }

    pub fn numeric(&mut self) -> key::Var {
        self.variables
            .push(Variable::new(self.current_source, VariableInfo::Numeric))
    }

    pub fn defined(&mut self, name: Ident, params: EntityList<key::Var>) -> key::Var {
        self.variables.push(Variable::new(
            self.current_source,
            VariableInfo::Defined(name, params),
        ))
    }

    pub fn tuple(&mut self, elements: EntityList<key::Var>) -> key::Var {
        self.variables.push(Variable::new(
            self.current_source,
            VariableInfo::Tuple(elements),
        ))
    }

    /// Initialize a list where all members will inferred/checked to be the same type.
    pub fn list_sameas(&mut self) -> (key::SameasUnification, key::Var, key::Var) {
        let elem = self.unknown();
        let var = self.list(elem);
        let key = self.same_as_unifications.push(inf::SameasUnification {
            main: inf::SameasMain::List { elem, list: var },
            members: EntityList::new(),
        });
        (key, var, elem)
    }

    /// Initialize a branching expression where all members will infer/check to be the type of the
    /// expression.
    pub fn expr_sameas(&mut self, init: Option<Var>) -> (key::SameasUnification, key::Var) {
        let var = init.unwrap_or_else(|| self.unknown());
        let key = self.same_as_unifications.push(inf::SameasUnification {
            main: inf::SameasMain::JoinExpression(var),
            members: EntityList::new(),
        });
        (key, var)
    }

    pub fn add_sameas_member(&mut self, sameas: key::SameasUnification, var: key::Var) {
        self.same_as_unifications[sameas]
            .members
            .push(var, &mut self.var_pool);
    }

    pub fn add_field(&mut self, var: key::Var, field: impl Into<String>) -> key::Var {
        let field_type = self.unknown();
        self.variables[var]
            .has_fields
            .push(inf::HasField { name: field.into(), field_type });
        field_type
    }

    pub fn list(&mut self, element_type: key::Var) -> key::Var {
        self.variables.push(Variable::new(
            self.current_source,
            VariableInfo::List(element_type),
        ))
    }

    pub fn array(&mut self, of: key::Var, len: key::Var) -> key::Var {
        self.variables.push(Variable::new(
            self.current_source,
            VariableInfo::Array { of, len },
        ))
    }

    pub fn generic(&mut self, generic_key: TaggedGeneric) -> key::Var {
        self.variables.push(Variable::new(
            self.current_source,
            VariableInfo::Generic(generic_key),
        ))
    }

    pub fn function(
        &mut self,
        kind: CallableKind,
        params: EntityList<key::Var>,
        ret: key::Var,
    ) -> key::Var {
        // TODO: We can't do the same thing for type_resolved_function. Is that a problem?

        let params = Some(params);

        self.variables.push(Variable::new(
            self.current_source,
            VariableInfo::Function { kind, params, ret },
        ))
    }

    pub fn unknown_function(&mut self, kind: CallableKind, ret: key::Var) -> key::Var {
        self.variables.push(Variable::new(
            self.current_source,
            VariableInfo::Function { kind, params: None, ret },
        ))
    }

    pub fn type_resolved_function(&mut self, name: impl Into<String>) -> key::Var {
        self.variables.push(Variable::new(
            self.current_source,
            VariableInfo::TypeResolvedFunction(name.into()),
        ))
    }

    pub fn pointer(&mut self, inner: key::Var) -> key::Var {
        self.variables.push(Variable::new(
            self.current_source,
            VariableInfo::Pointer(inner),
        ))
    }

    /// Initialize function application.
    pub fn apply(&mut self, func: key::Var) -> key::Application {
        // Initialize a return type for the type variable if its not known.
        let ret = match &self.variables[func].info {
            VariableInfo::Function { ret, .. } => *ret,
            _ => self.variables.push(Variable::new(
                self.current_source,
                VariableInfo::Applied { func, appl: self.applications.next_key() },
            )),
        };

        self.applications
            .push(inf::Application { func, parameters: EntityList::new(), ret })
    }

    /// Add the next parameter to the function application.
    pub fn apply_next_parameter(&mut self, appl: key::Application, ty: key::Var) {
        let appl = &mut self.applications[appl];
        appl.parameters.push(ty, &mut self.var_pool);
    }

    pub fn assign(&mut self, src: key::Var, target: key::Var) {
        self.assignments
            .push(inf::Assignment { lhs: target, rhs: src });
    }

    pub fn assign_return(&mut self, expr: key::Var, expected: key::Var) {
        self.returns_.push(inf::Return { expr, expected });
    }

    // pub fn apply_or_yield(&self, appl: key::Application) -> Result<key::Var, key::Var> {
    //     let appl_data = &self.applications[appl];

    //     match self.get_return_type_of_var(appl_data.func) {
    //         Some(ret) => ret,
    //         None if appl_data.parameters.is_empty() => todo!(),
    //         None => (),
    //     }
    // }

    /// Get the return type of a function application.
    pub fn get_return_type(&self, appl: key::Application) -> key::Var {
        self.applications[appl].ret
    }

    pub fn get_return_type_of_var(&self, func: key::Var) -> Option<key::Var> {
        match &self.variables[func].info {
            VariableInfo::Function { ret, .. } => Some(*ret),
            VariableInfo::InferTo(var) => self.get_return_type_of_var(*var),
            VariableInfo::Applied { func, appl } => {
                let appl = &self.applications[*appl];

                match &self.variables[*func].info {
                    VariableInfo::Function { ret, .. } => Some(*ret),
                    _ if appl.parameters.is_empty() => Some(appl.func),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn for_vars_mut<F>(&mut self, list: EntityList<key::Var>, mut f: F)
    where
        F: FnMut(&mut Self, key::Var),
    {
        let len = list.len(&self.var_pool);
        for i in 0..len {
            let ty = list.get(i, &self.var_pool).unwrap();
            f(self, ty);
        }
    }

    pub fn set_closure_to_fnptr(&mut self, var: Var) {
        match &mut self.variables[var].info {
            VariableInfo::Function { kind: kind @ CallableKind::Closure, .. } => {
                *kind = CallableKind::FnPointer
            }
            other => panic!("cannot flip function kind for non-closure: {other:?}"),
        }
    }

    pub fn debug_print(&self) {
        for (var, variable) in self.variables.iter() {
            let name = &self.debug_strings[var];

            println!(
                "{var}{} -> {variable:#?}",
                if name.is_empty() {
                    "".into()
                } else {
                    format!(" {name}")
                }
            );
        }

        for (i, assignment) in self.assignments.iter().enumerate() {
            println!("assignment{i} -> {} ∈ {}", assignment.lhs, assignment.rhs);
        }

        for (appl, application) in self.applications.iter() {
            print!("{appl} -> {}", application.func);

            for p in application.parameters.as_slice(&self.var_pool) {
                print!(" {p}");
            }

            println!(" -> {}", application.ret);
        }

        for (s, sameas) in self.same_as_unifications.iter() {
            print!("{s} -> {:?}", sameas.main,);

            for m in sameas.members.as_slice(&self.var_pool) {
                print!(" {m}");
            }

            println!();
        }
    }
}
