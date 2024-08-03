use super::{Constraint, Forall, ForeignInst, IType, Inference, Static, Type};
use derive_new::new;
use lumina_key as key;
use lumina_key::{entity_impl, keys, LinearFind, Map, ModMap, M};
use lumina_util::{Span, Spanned, Tr};
use tracing::trace;

/// For bidirectional inference to work; we need indirection for types so that we can refer to the
/// same context from multiple locations.
///
/// The type environment contains all contexts that types direct to.
pub struct TEnv<'s> {
    pub self_: Option<IType>,
    pub(crate) records: Map<RecordVar, RecordVarInfo<'s>>,
    pub(crate) vars: Map<Var, VarInfo>,
    pub(crate) constraint_checks: Vec<(Var, Constraint<Inference>)>,
    pub(crate) concrete_constraint_checks: Vec<(Tr<Type>, Constraint<Static>)>,
}

#[derive(Debug, Clone)]
pub enum RecordError<'s> {
    DoesNotHaveFields(Span, M<key::Record>, Vec<Tr<&'s str>>),
    UnknownRecord(Span, Map<RecordVarField, Tr<&'s str>>),
    Ambigious(Span, Map<RecordVarField, Tr<&'s str>>, Vec<M<key::Record>>),
    NotARecord,
}

keys! {
    Var . "var",
    RecordVar . "rvar",
    RecordVarField . "rfvar",
    VarTraitConstraint . "vtrait"
}

pub struct RecordVarInfo<'s> {
    pub(crate) span: Span,
    pub(crate) assignment: RecordAssignment<'s>,
    pub(crate) fields: Map<RecordVarField, Tr<&'s str>>,
    pub(crate) verified: bool,
}

#[derive(Clone)]
pub enum RecordAssignment<'s> {
    Ok(M<key::Record>, Vec<IType>),

    Redirect(RecordVar),      // It was assigned to another record
    NotRecord(IType),         // The rvar ended up not being a record. This is a form of poison
    Unknown(RecordError<'s>), // The inference was forced via resolve but failed
    None,                     // The rvar has not yet been assigned to anything
}

#[derive(new)]
pub struct VarInfo {
    #[new(default)]
    pub(crate) assignment: Option<Tr<IType>>,
    pub(crate) span: Span,
    #[new(default)]
    pub(crate) int_constraint: Option<IntConstraint>,
}

#[derive(new, Debug, Clone, Copy)]
pub struct IntConstraint {
    pub min: Option<(bool, u128)>,
}

impl<'s> TEnv<'s> {
    pub fn new() -> Self {
        Self {
            self_: None,
            records: Map::new(),
            vars: Map::new(),

            constraint_checks: vec![],
            concrete_constraint_checks: vec![],
        }
    }

    pub fn unify_forall(&mut self, span: Span, forall: &Forall<'s, Static>) -> Vec<IType> {
        forall
            .generics
            .values()
            .map(|gdata| {
                if !gdata.trait_constraints.is_empty() {
                    panic!("unify short-hand used for foreign with constraints which need instantiation");
                };
                IType::infer(self.var(span))
            })
            .collect()
    }

    pub fn set_self(&mut self, ty: IType) {
        self.self_ = Some(ty);
    }

    pub fn assign(&mut self, var: Var, ty: Tr<IType>) {
        trace!("{var} -> {ty}");
        let previous = self.vars[var].assignment.replace(ty);
        assert!(previous.is_none(), "double assignment")
    }

    pub fn constraint_of(&self, var: Var) -> Option<IntConstraint> {
        self.vars[var].int_constraint
    }

    pub fn fields_of(&self, rvar: RecordVar) -> impl Iterator<Item = Tr<&'s str>> + '_ {
        self.records[rvar].fields.values().copied()
    }

    pub fn iter_var_fields(&self, rvar: RecordVar) -> impl Iterator<Item = RecordVarField> {
        self.records[rvar].fields.keys()
    }

    pub fn add_field(&mut self, rvar: RecordVar, name: Tr<&'s str>) -> RecordVarField {
        self.records[rvar]
            .fields
            .find(|n| *n == name)
            .unwrap_or_else(|| self.records[rvar].fields.push(name))
    }

    pub fn name_of_field(&self, rvar: RecordVar, field: RecordVarField) -> Tr<&'s str> {
        self.records[rvar].fields[field]
    }

    pub fn get(&self, var: Var) -> Tr<Option<Tr<&IType>>> {
        let span = self.vars[var].span;
        self.vars[var].assignment.as_ref().map(Tr::as_ref).tr(span)
    }

    pub fn get_record(&self, var: RecordVar) -> &RecordAssignment<'s> {
        &self.records[var].assignment
    }

    pub fn get_record_span(&self, var: RecordVar) -> Span {
        self.records[var].span
    }

    pub fn push_constraint(&mut self, ty: Tr<Type>, con: Constraint<Static>) {
        self.concrete_constraint_checks.push((ty, con))
    }

    pub fn push_iconstraint(&mut self, ty: Var, con: Constraint<Inference>) {
        self.constraint_checks.push((ty, con))
    }

    pub fn record(&mut self, span: Span) -> RecordVar {
        self.records.push(RecordVarInfo {
            fields: Map::new(),
            assignment: RecordAssignment::None,
            span,
            verified: false,
        })
    }

    pub fn var(&mut self, span: Span) -> Var {
        trace!("spawning {}", self.vars.next_key());
        self.vars.push(VarInfo::new(span))
    }

    pub fn int(&mut self, span: Span) -> Var {
        let var = self.var(span);
        self.vars[var].int_constraint = Some(IntConstraint::new(None));
        var
    }

    pub fn hint_min_int(&mut self, var: Var, signed: bool, n: u128) {
        match &mut self.vars[var].int_constraint {
            Some(IntConstraint { min: r @ None }) => *r = Some((signed, n)),
            None => {
                self.vars[var].int_constraint = Some(IntConstraint::new(Some((signed, n))));
            }
            _ => unreachable!(),
        }
    }
}
