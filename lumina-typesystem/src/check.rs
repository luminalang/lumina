use super::{
    tenv::FieldMismatch, Compatibility as C, Constraint, Container, Forall, GetForall, GetImplData,
    IType, ImplIndex, IntConstraint, IntSize, Static, Transformer, Ty, Type, TypeSystem, Upgrade,
    Var,
};
use key::M;
use lumina_key as key;
use lumina_util::{Span, Spanned, Tr};
use smallvec::SmallVec;
use tracing::trace;

struct Unify {
    span: Span,
    assignments: SmallVec<[Var; 12]>,
    len_before_unifcation: usize,
}

impl<'a, 's> TypeSystem<'a, 's> {
    /// Unify two types
    ///
    /// Returns
    ///  * true:  the types successfully unified and the tenv might've been modified
    ///  * false: the types are incompatible and the tenv has not changed
    pub fn unify(&mut self, span: Span, got: &IType, exp: &IType) -> bool {
        let mut u = Unify {
            span,
            assignments: SmallVec::new(),
            len_before_unifcation: self.env.vars.len(),
        };

        let compatible = self.unify_(&mut u, got, exp);
        trace!("{got} ∈ {exp} = {compatible}");

        if !compatible {
            for var in u.assignments {
                self.env.vars[var].assignment = None;
            }

            self.env.vars.truncate(Var::from(u.len_before_unifcation));
        }

        compatible
    }

    fn unify_(&mut self, u: &mut Unify, got: &IType, exp: &IType) -> bool {
        match (got, exp) {
            (Ty::Container(gcon, gparams), Ty::Container(econ, eparams)) if gcon == econ => {
                self.unifys(u, gparams, eparams)
            }
            (Ty::Simple("self"), _) if self.env.self_.is_some() => {
                let got = self.env.self_.clone().unwrap();
                self.unify_(u, &got, exp)
            }
            (_, Ty::Simple("self")) if self.env.self_.is_some() => {
                let exp = self.env.self_.clone().unwrap();
                self.unify_(u, got, &exp)
            }
            (Ty::Generic(g), Ty::Generic(e)) => g == e,
            (Ty::Int(g), Ty::Int(e)) => g == e,
            (Ty::Simple(g), Ty::Simple(e)) => g == e,
            (Ty::Special(g), Ty::Special(e)) if g == e => true,
            (Ty::Special(var), exp) => self.unify_into_var(u, false, *var, exp),
            (got, Ty::Special(var)) => self.unify_into_var(u, true, *var, got),
            _ => false,
        }
    }

    fn unifyf(&mut self, u: &mut Unify, flip: bool, got: &IType, exp: &IType) -> bool {
        if flip {
            self.unify_(u, exp, got)
        } else {
            self.unify_(u, got, exp)
        }
    }

    fn unifys(&mut self, u: &mut Unify, gots: &[IType], exps: &[IType]) -> bool {
        gots.len() == exps.len() && gots.iter().zip(exps).all(|(g, e)| self.unify_(u, g, e))
    }

    fn unify_into_var(&mut self, u: &mut Unify, flip: bool, var: Var, ty: &IType) -> bool {
        // If the var is a field to a record and that record has-been/can-be inferred,
        // then use the known type from that instead of the var.
        if let Some((_, fty)) = self.try_get_field_if_is_field(var) {
            return self.unifyf(u, flip, &fty, ty);
        }

        // If this is a comparison of two vars, we merge them instead of assinging one to the other.
        if let Ty::Special(ovar) = ty {
            if var == *ovar {
                return true;
            }

            // Ditto for the other var
            if let Some((_, fty)) = self.try_get_field_if_is_field(*ovar) {
                return self.unify_into_var(u, flip, var, &fty);
            }

            if let Some(assigned) = self.env.vars[var].assignment.as_ref().cloned() {
                return self.unify_into_var(u, !flip, *ovar, &assigned);
            }

            if let Some(assigned) = self.env.vars[*ovar].assignment.as_ref().cloned() {
                return self.unify_into_var(u, flip, var, &assigned);
            }

            self.env.merge_vars(u.span, [var, *ovar]);
            return true;
        }

        if let Some(assigned) = self.env.vars[var].assignment.as_ref().cloned() {
            return self.unifyf(u, flip, &assigned, ty);
        }

        if !self.env.vars[var].fields.is_empty() {
            match ty {
                Ty::Container(
                    Container::Defined(M(module, key::TypeKind::Record(rkey)), _),
                    params,
                ) => {
                    let record = rkey.inside(*module);
                    u.assignments.push(var);
                    self.assign_record_to_rvar(u.span, var, record, params.to_vec());
                    return true;
                }
                _ => return false,
            }
        }

        u.assignments.push(var);
        self.env.assign(var, ty.clone().tr(u.span));
        true
    }

    /// Get the assigned type if exists or see if the type can be safely inferred by its constraints
    ///
    /// This method will eagerly infer a default int from int constraints
    pub fn try_get_known_type(&mut self, var: Var) -> Option<Tr<IType>> {
        if let Some((_, field)) = self.try_get_field_if_is_field(var) {
            let span = self.env.vars[var].span;
            return Some(field.tr(span));
        }

        if let Some(ty) = self.env.vars[var].assignment.clone() {
            return Some(ty);
        }

        if !self.env.vars[var].fields.is_empty() {
            if self.try_get_rvar(var).is_some() {
                return self.env.vars[var].assignment.clone();
            }
        }

        if let Some(intcon) = self.env.vars[var].int_constraint {
            let span = self.env.vars[var].span;
            let size = intcon.to_default_type(self.default_int_size);
            let ty = Ty::Int(size).tr(span);
            self.env.assign(var, ty.clone());
            return Some(ty);
        }

        None
    }

    pub fn call_as_function(
        &mut self,
        span: Span,
        ty: &IType,
        params: usize,
    ) -> Option<(Container, Vec<IType>, IType)> {
        match ty {
            Ty::Container(c @ (Container::FnPointer | Container::Closure), params) => {
                let (ret, ptypes) = params.split_last().unwrap();
                Some((*c, ptypes.to_vec(), ret.clone()))
            }
            Ty::Special(var) => {
                if let Some((_, fty)) = self.try_get_field_if_is_field(*var) {
                    return self.call_as_function(span, &fty, params);
                }

                match self.env.vars[*var].assignment.clone() {
                    Some(ty) => self.call_as_function(span, &ty, params),
                    None if params == 0 => None,
                    None => {
                        let params = (0..params)
                            .map(|_| self.env.var(span))
                            .map(Ty::Special)
                            .collect::<Vec<_>>();

                        let ret = Ty::Special(self.env.var(span));

                        self.env.vars[*var].assignment =
                            Some(Ty::closure(params.clone(), ret.clone()).tr(span));

                        Some((Container::Closure, params, ret))
                    }
                }
            }
            _ => None,
        }
    }
}

pub enum ConstraintError {
    GotInt {
        span: Span,
        iconstr: IntConstraint,
        expected: Type,
    },
    IntConstantNegativeUnsigned(Span, IntSize),
    IntConstantTooLarge(Span, i128, IntSize),
    Trait(Tr<Type>, Constraint<Static>),
    FieldType {
        exp: Type,
        got: Type,
        at: Span,
    },
    RecordNotFound(Vec<Tr<String>>),
    RecordAmbigous(Span, Vec<M<key::Record>>, Vec<Tr<String>>),
}

impl<'e, 's> TypeSystem<'e, 's> {
    pub fn check_all_constraints<'a, 't>(
        &self,
        index: &ImplIndex,
        in_trait: Option<(M<key::Trait>, &'t Forall<'s, Static>)>,
        lhs_forall: GetForall<'a, 't, 's>,
        get_impl_data: GetImplData<'a, 't, 's>,
    ) -> Vec<ConstraintError> {
        self.env
            .vars
            .iter()
            .flat_map(|(var, vdata)| {
                let mut errors = vec![];

                // Finaliser should've already defaulted all pending inference
                let ty = Upgrade(self.env).special(&var);
                let vspan = vdata.span;
                let tyspan = vdata.assignment.as_ref().unwrap().span;

                // Check int constraint
                if let Some(iconstr) = vdata.int_constraint {
                    if let Err(err) = self.check_int_constraint(vdata.span, &ty, iconstr) {
                        errors.push(err);
                    }
                }

                // Check record field constraints
                match &ty {
                    Ty::Container(
                        Container::Defined(M(module, key::TypeKind::Record(record)), _),
                        params,
                    ) => {
                        let key = record.inside(*module);
                        self.check_field_constraints(var, key, params, &mut errors);
                    }
                    Ty::Simple("poison") if !vdata.fields.is_empty() => {
                        assert_eq!(
                            tyspan, vdata.span,
                            "poison assigned to rvar outside of default"
                        );

                        let fields = vdata
                            .fields
                            .iter()
                            .map(|(name, _, _)| name.map(str::to_string))
                            .collect();

                        match self.find_record_by_fields(var).as_slice() {
                            [] => errors.push(ConstraintError::RecordNotFound(fields)),
                            [_] => {}
                            many => errors.push(ConstraintError::RecordAmbigous(
                                vspan,
                                many.to_vec(),
                                fields,
                            )),
                        }

                        return errors;
                    }
                    _ if !vdata.fields.is_empty() => {
                        // currently there's nothing stopping this from happening.
                        //
                        // is that bad? do we want to prevent it? maybe we're meant to just give a
                        // does_not_have_fields error instead
                        panic!("non-record assigned to rvar");
                    }
                    _ => {}
                }

                // Check trait constraints
                for con in &vdata.trait_constraints {
                    let con = Upgrade(self.env).transform_constraint(con);
                    let ok = C::constraint(index, in_trait, get_impl_data, lhs_forall, &ty, &con);

                    if !ok {
                        errors.push(ConstraintError::Trait(ty.clone().tr(tyspan), con));
                    }
                }

                errors
            })
            .collect()
    }

    fn check_int_constraint(
        &self,
        span: Span,
        ty: &Type,
        iconstr: IntConstraint,
    ) -> Result<(), ConstraintError> {
        match ty {
            Ty::Int(intsize) if !intsize.signed && iconstr.min < 0 => {
                Err(ConstraintError::IntConstantNegativeUnsigned(span, *intsize))
            }
            Ty::Int(intsize) if iconstr.max > intsize.max_value() => Err(
                ConstraintError::IntConstantTooLarge(span, iconstr.min, *intsize),
            ),
            Ty::Int(_) => Ok(()),
            _ => Err(ConstraintError::GotInt { span, iconstr, expected: ty.clone() }),
        }
    }

    fn check_field_constraints(
        &self,
        var: Var,
        record: M<key::Record>,
        params: &[Type],
        errors: &mut Vec<ConstraintError>,
    ) {
        for &(fname, fvar, mismatch) in &self.env.vars[var].fields {
            if let Some(FieldMismatch) = mismatch {
                let Tr { span, value } = self.env.vars[fvar].assignment.as_ref().unwrap();
                let exp = Upgrade(self.env).transform(value);
                let got = self.inst_field(record, &params, *fname).unwrap();
                errors.push(ConstraintError::FieldType { exp, got, at: *span });
            }
        }
    }
}
