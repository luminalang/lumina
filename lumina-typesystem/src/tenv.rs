use super::{Constraint, Forall, IType, Inference, Static};
use lumina_collections::{map_key_impl, Map, M};
use lumina_util::{Span, Spanned, Tr};
use tracing::trace;

/// For bidirectional inference to work; we need indirection for types so that we can refer to the
/// same context from multiple locations.
///
/// The type environment contains all contexts that types direct to.
pub struct TEnv<'s> {
    pub self_: Option<IType>,
    pub(crate) vars: Map<Var, VarInfo<'s>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Var(u32);
map_key_impl!(Var(u32), "var");

pub struct VarInfo<'s> {
    pub(crate) span: Span,
    pub(crate) assignment: Option<Tr<IType>>,
    pub(crate) int_constraint: Option<IntConstraint>,
    pub(crate) trait_constraints: Vec<Constraint<Inference>>,
    pub(crate) fields: Vec<(Tr<&'s str>, Var, Option<FieldMismatch>)>,
    pub(crate) field_of: Option<(Tr<&'s str>, Var)>,

    pub(crate) lift_to_generic: bool,
}

#[derive(Clone, Copy)]
pub(crate) struct FieldMismatch;

#[derive(Clone, Copy)]
pub struct IntConstraint {
    pub min: i128,
    pub max: u64,
}

impl<'s> VarInfo<'s> {
    pub fn new(span: Span) -> VarInfo<'s> {
        VarInfo {
            span,
            assignment: None,
            int_constraint: None,
            trait_constraints: vec![],
            fields: vec![],
            field_of: None,

            lift_to_generic: false,
        }
    }
}

impl<'s> TEnv<'s> {
    pub fn new() -> Self {
        Self { self_: None, vars: Map::new() }
    }

    // Clever, but not sure if we'll need it.
    // fn is_defaulted(&self, var: Var) -> bool {
    //     let vinfo = &self[var];
    //     Some(vinfo.span) == vinfo.assignment.as_ref().map(|tr| tr.span)
    // }

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

    pub(crate) fn assign(&mut self, var: Var, ty: Tr<IType>) {
        trace!("{var} -> {ty}");
        let previous = self.vars[var].assignment.replace(ty);
        assert!(previous.is_none(), "double assignment")
    }

    pub fn assign_simple(&mut self, var: Var, ty: Tr<IType>) {
        trace!("{var} -> {ty}");
        let vdata = &mut self.vars[var];
        assert!(vdata.fields.is_empty() && vdata.field_of.is_none());
        let previous = self.vars[var].assignment.replace(ty);
        assert!(previous.is_none(), "double assignment")
    }

    pub fn get(&self, var: Var) -> Result<&Tr<IType>, Span> {
        let vdata = &self.vars[var];
        vdata.assignment.as_ref().ok_or(vdata.span)
    }

    pub fn get_int_constraint(&self, var: Var) -> Option<IntConstraint> {
        self.vars[var].int_constraint
    }

    pub fn add_field(&mut self, var: Var, name: Tr<&'s str>) -> Var {
        debug_assert_eq!(
            self.vars[var].assignment, None,
            "cannot add fields to already assigned rvar"
        );

        self.vars[var]
            .fields
            .iter()
            .find_map(|(n, fieldvar, _)| (*n == name).then_some(*fieldvar))
            .unwrap_or_else(|| {
                let fieldvar = self.var(name.span);
                self.vars[var].fields.push((name, fieldvar, None));
                self.vars[fieldvar].field_of = Some((name, var));
                fieldvar
            })
    }

    pub fn add_trait_constraint(&mut self, var: Var, con: Constraint<Var>) {
        self.vars[var].trait_constraints.push(con);
    }

    pub fn next_key(&self) -> Var {
        Var(self.vars.len() as u32)
    }

    pub fn var(&mut self, span: Span) -> Var {
        trace!("spawning {}", self.next_key());
        let var = self.next_key();
        self.vars.push(VarInfo::new(span));
        var
    }

    pub fn enable_lift_to_generic(&mut self, var: Var) {
        self.vars[var].lift_to_generic = true;
    }

    pub fn merge_vars<const N: usize>(&mut self, span: Span, vars: [Var; N]) -> Var {
        let mut int_constraint = None;
        let mut trait_constraints = vec![];
        let mut fields = vec![];
        let mut field_of = None;
        let mut lift_to_generic = false;

        let nvar = self.next_key();
        trace!("merging {vars:?} -> {nvar}");

        for vinfo in self.vars.get_many_mut(vars) {
            assert!(vinfo.assignment.is_none());
            vinfo.assignment = Some(IType::infer(nvar).tr(span));

            lift_to_generic |= vinfo.lift_to_generic;

            if field_of.is_none() {
                if let Some(fvar) = vinfo.field_of {
                    field_of = Some(fvar);
                }
            }

            match (int_constraint.as_mut(), vinfo.int_constraint) {
                (None, None) => {}
                (None, Some(_)) => int_constraint = vinfo.int_constraint,
                (Some(con), Some(vcon)) => {
                    int_constraint = Some(IntConstraint {
                        min: con.min.min(vcon.min),
                        max: con.max.max(vcon.max),
                    })
                }
                (Some(_), None) => {}
            }
            trait_constraints.extend(std::mem::take(&mut vinfo.trait_constraints));
            fields.extend(std::mem::take(&mut vinfo.fields));
        }

        self.vars.push(VarInfo {
            span,
            assignment: None,
            int_constraint,
            trait_constraints,
            lift_to_generic,
            fields,
            field_of: None,
        });

        assert_eq!(nvar, Var(self.vars.len() as u32 - 1));

        nvar
    }

    pub fn int(&mut self, span: Span, min: i128, max: u64) -> Var {
        let var = self.var(span);
        self.vars[var].int_constraint = Some(IntConstraint { min, max });
        var
    }
}
