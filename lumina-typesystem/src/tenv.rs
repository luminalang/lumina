use super::{Constraint, ForeignInst, IType, Type};
use derive_new::new;
use lumina_key as key;
use lumina_key::{entity_impl, keys, LinearFind, Map, ModMap, M};
use lumina_util::{Span, Spanned, Tr};
use tracing::trace;

/// For bidirectional inference to work; we need indirection for types so that we can refer to the
/// same context from multiple locations.
///
/// The type environment contains all contexts that types direct to.
#[derive(Debug)]
pub struct TEnv<'s> {
    pub(crate) self_: Option<IType>,
    pub(crate) records: Map<RecordVar, RecordVarInfo<'s>>,
    pub(crate) vars: Map<Var, VarInfo>,
    pub(crate) constraint_checks: Vec<(Var, Constraint<IType>)>,
    pub(crate) concrete_constraint_checks: Vec<(Span, Type, Constraint<Type>)>,
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

#[derive(Debug)]
pub struct RecordVarInfo<'s> {
    pub(crate) span: Span,
    pub(crate) assignment: RecordAssignment<'s>,
    pub(crate) fields: Map<RecordVarField, Tr<&'s str>>,
    pub(crate) verified: bool,
    // pub(crate) traits: Map<VarTraitConstraint, Constraint<IType>>, // we might not need this
    // since we more eagerly resolve
}

#[derive(Debug, Clone)]
pub enum RecordAssignment<'s> {
    Ok(M<key::Record>, Vec<IType>),

    Redirect(RecordVar),      // It was assigned to another record
    NotRecord(IType),         // The rvar ended up not being a record. This is a form of poison
    Unknown(RecordError<'s>), // The inference was forced via resolve but failed
    None,                     // The rvar has not yet been assigned to anything
}

#[derive(new, Debug)]
pub struct VarInfo {
    #[new(default)]
    pub(crate) assignment: Option<Tr<IType>>,
    pub(crate) span: Span,
    #[new(default)]
    pub(crate) int_constraint: Option<IntConstraint>,
    #[new(default)]
    pub(crate) traits: Map<VarTraitConstraint, Constraint<IType>>,
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

            // errors: Vec::new(),
            constraint_checks: Vec::new(),
            concrete_constraint_checks: Vec::new(),
        }
    }

    pub fn set_self(&mut self, ty: IType) {
        self.self_ = Some(ty);
    }

    pub fn assign(&mut self, var: Var, ty: Tr<IType>) {
        trace!("{var} -> {ty}");
        let previous = self.vars[var].assignment.replace(ty);
        assert!(previous.is_none(), "double assignment")
    }

    pub fn inst<Ty: Clone, T, F>(&self, params: &[Ty], and_then: F) -> T
    where
        F: FnOnce(ForeignInst<Ty>) -> T,
    {
        let finst = ForeignInst {
            generics: params.iter().cloned().collect(),
            pgenerics: Map::new(),
            self_: None,
        };
        and_then(finst)
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

    pub fn push_constraint(&mut self, ty: Tr<Type>, con: Constraint<Type>) {
        self.concrete_constraint_checks
            .push((ty.span, ty.value, con))
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
