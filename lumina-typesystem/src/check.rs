use super::{
    tenv, Container, GenericKind, GenericMapper, IType as Ty, Inference as Inf, IntConstraint,
    RecordAssignment, RecordError, RecordVar, Transformer, TypeSystem, Var,
};
use derive_more::From;
use lumina_key as key;
use lumina_key::{LinearFind, M};
use lumina_util::{Span, Spanned, Tr};
use std::collections::HashMap;
use tenv::RecordVarInfo;
use tracing::trace;
use tracing::warn;

#[derive(From, Debug)]
pub enum CheckResult<'s> {
    #[from]
    Record(RecordError<'s>),
    Mismatch {
        got: Tr<Ty>,
        exp: Tr<Ty>,
        ctx: &'static str,
    },
    Ok,
}

use CheckResult::Ok as ok;

impl<'s> CheckResult<'s> {
    pub fn fail(got: Tr<&Ty>, exp: Tr<&Ty>, ctx: &'static str) -> Self {
        Self::Mismatch { got: got.cloned(), exp: exp.cloned(), ctx }
    }

    pub fn is_ok(&self) -> bool {
        matches!(self, Self::Ok)
    }

    pub fn and_then(self, f: impl FnOnce() -> Self) -> Self {
        if matches!(self, CheckResult::Ok) {
            f()
        } else {
            self
        }
    }
}

impl<'a, 's> TypeSystem<'a, 's> {
    pub fn type_check(&mut self, got: Tr<&Ty>, exp: Tr<&Ty>) -> CheckResult<'s> {
        self.check(got, exp)
    }

    fn checkf(&mut self, flip: bool, got: Tr<&Ty>, exp: Tr<&Ty>) -> CheckResult<'s> {
        if flip {
            self.check(exp, got)
        } else {
            self.check(got, exp)
        }
    }

    fn check(&mut self, got: Tr<&Ty>, exp: Tr<&Ty>) -> CheckResult<'s> {
        trace!("{got} ∈ {exp}");

        match (*got, *exp) {
            // Dismissed
            (Ty::Simple("poison"), _) => ok,
            (_, Ty::Simple("poison")) => ok,

            // Concrete checks
            (Ty::Simple(gname), Ty::Simple(ename)) if gname == ename => ok,
            (Ty::Generic(ggen), Ty::Generic(egen)) if ggen == egen => ok,
            (Ty::Container(gcon, gparams), Ty::Container(econ, eparams))
                if (gcon == econ) && gparams.len() == eparams.len() =>
            {
                self.checks(got.span, exp.span, gparams, eparams)
            }
            // Edge-case for using a list with sugar against concrete type and vice-versa
            (
                Ty::Container(Container::List(gkey) | Container::Defined(gkey), gparams),
                Ty::Container(Container::List(ekey) | Container::Defined(ekey), eparams),
            ) if gkey == ekey => self.checks(got.span, exp.span, gparams, eparams),
            (Ty::Int(gsize), Ty::Int(esize)) if gsize == esize => ok,

            // Self Substitution
            (Ty::Simple("self"), _) if self.env.self_.is_some() => {
                let got = self.env.self_.clone().unwrap().tr(got.span);
                self.check(got.as_ref(), exp)
            }
            (_, Ty::Simple("self")) if self.env.self_.is_some() => {
                let exp = self.env.self_.clone().unwrap().tr(got.span);
                self.check(got, exp.as_ref())
            }

            // TVar direct checks
            (Ty::Special(ginf), Ty::Special(einf)) if ginf == einf => ok,

            // TVar merging
            (Ty::Special(Inf::Var(g)), Ty::Special(Inf::Var(e))) => {
                let [gdata, edata] = self.env.vars.get_many_mut([*g, *e]).unwrap();

                match (gdata.assignment.clone(), edata.assignment.clone()) {
                    (None, None) => match (gdata.int_constraint, edata.int_constraint) {
                        (_, None) => self.assign_var(true, *e, got),
                        (None, _) => self.assign_var(false, *g, exp),
                        (Some(IntConstraint { min }), Some(IntConstraint { min: emin })) => {
                            match (min, emin) {
                                (None, Some(_)) | (Some((false, _)), Some((true, _))) => {
                                    self.assign_var(false, *g, exp)
                                }
                                _ => self.assign_var(true, *e, got),
                            }
                        }
                    },
                    (Some(ty), None) => self.check(ty.as_ref(), exp),
                    (None, Some(ty)) => self.check(got, ty.as_ref()),
                    (Some(g), Some(e)) => self.check((&*g).tr(got.span), (&*e).tr(exp.span)),
                }
            }
            (Ty::Special(Inf::Record(grvar)), Ty::Special(Inf::Record(ervar))) => {
                self.merge_records((got.span, *grvar), (exp.span, *ervar))
            }
            (Ty::Special(Inf::Field(gvar, gfvar)), Ty::Special(Inf::Field(evar, efvar))) => {
                let [gfield, efield] = [
                    self.env.records[*gvar].fields[*gfvar],
                    self.env.records[*evar].fields[*efvar],
                ];
                self.check_fields((got.span, *gvar, gfield), (exp.span, *evar, efield))
            }

            // TVar assignments
            (_, Ty::Special(Inf::Var(evar))) => self.assign_var(true, *evar, got),
            (Ty::Special(Inf::Var(gvar)), _) => self.assign_var(false, *gvar, exp),
            (_, Ty::Special(Inf::Field(evar, efvar))) => {
                let fname = self.env.records[*evar].fields[*efvar];
                self.check_record_field(true, exp.span, (*evar, fname), got)
            }
            (Ty::Special(Inf::Field(gvar, gfvar)), _) => {
                let fname = self.env.records[*gvar].fields[*gfvar];
                self.check_record_field(false, got.span, (*gvar, fname), exp)
            }
            (Ty::Special(Inf::Record(rvar)), _) => self.check_rvar(false, got.span, *rvar, exp),
            (_, Ty::Special(Inf::Record(rvar))) => self.check_rvar(true, exp.span, *rvar, got),

            _ => CheckResult::fail(got, exp, ""),
        }
    }

    fn check_rvar(
        &mut self,
        flip: bool,
        span: Span,
        rvar: RecordVar,
        other: Tr<&Ty>,
    ) -> CheckResult<'s> {
        let assgn = self.env.records[rvar].assignment.clone();
        match assgn {
            RecordAssignment::Redirect(rvar) => self.check_rvar(flip, span, rvar, other),
            RecordAssignment::Ok(key, params) => {
                let ty = Ty::defined(key, params).tr(span);
                self.checkf(flip, ty.as_ref(), other)
            }
            RecordAssignment::None => self.assign_to_unassigned_rvar(flip, span, rvar, other),
            RecordAssignment::Unknown(..) => ok,
            RecordAssignment::NotRecord(_) => ok,
        }
    }

    fn assign_to_unassigned_rvar(
        &mut self,
        flip: bool,
        span: Span,
        rvar: RecordVar,
        other: Tr<&Ty>,
    ) -> CheckResult<'s> {
        let assignment = &mut self.env.records[rvar].assignment;
        assert!(matches!(assignment, RecordAssignment::None));
        match other.value {
            Ty::Container(Container::List(key) | Container::Defined(key), params) => {
                match key.value {
                    key::TypeKind::Record(rkey) => {
                        let key = key.module.m(rkey);
                        *assignment = RecordAssignment::Ok(key, params.clone());
                        ok
                    }
                    _ => {
                        warn!("Assuming we will error on the incorrect record assignment during type finalization");
                        *assignment = RecordAssignment::NotRecord(other.value.clone());
                        ok
                    }
                }
            }
            Ty::Special(Inf::Var(var)) => {
                let vdata = &self.env.vars[*var];
                match vdata.assignment.clone() {
                    Some(other) => self.assign_to_unassigned_rvar(flip, span, rvar, other.as_ref()),
                    None => match &vdata.int_constraint {
                        Some(IntConstraint { .. }) => todo!("error: or buffer?"),
                        None => {
                            self.env
                                .assign(*var, Ty::Special(Inf::Record(rvar)).tr(other.span));

                            ok
                        }
                    },
                }
            }
            Ty::Special(Inf::Field(orvar, ovarfield)) => {
                let name = self.env.name_of_field(*orvar, *ovarfield);
                let Some(ty) = self.force_record_field(other.span, (*orvar, name)) else {
                    return ok;
                };

                self.assign_to_unassigned_rvar(flip, span, rvar, (&ty).tr(other.span))
            }
            Ty::Special(Inf::Record(orvar)) if flip => {
                self.merge_records((other.span, *orvar), (span, rvar))
            }
            Ty::Special(Inf::Record(orvar)) => {
                self.merge_records((span, rvar), (other.span, *orvar))
            }
            _ => {
                warn!("Assuming we will error on the incorrect record assignment during type finalization");
                *assignment = RecordAssignment::NotRecord(other.value.clone());
                ok
            }
        }
    }

    fn merge_records(&mut self, got: (Span, RecordVar), exp: (Span, RecordVar)) -> CheckResult<'s> {
        let [grdata, erdata] = [&self.env.records[got.1], &self.env.records[exp.1]];

        match (&grdata.assignment, &erdata.assignment) {
            // Follow recursively
            (RecordAssignment::Redirect(newvar), _) => self.merge_records((got.0, *newvar), exp),
            (_, RecordAssignment::Redirect(newvar)) => self.merge_records(got, (exp.0, *newvar)),

            // Direct comparison between two records
            (RecordAssignment::Ok(gkey, gparams), RecordAssignment::Ok(ekey, eparams)) => {
                let got = Ty::defined(*gkey, gparams.to_vec()).tr(got.0);
                let exp = Ty::defined(*ekey, eparams.to_vec()).tr(exp.0);
                self.check(got.as_ref(), exp.as_ref())
            }

            // Move all field names from the unassigned to the assigned and then assign unassigned to assigned
            (RecordAssignment::Ok(key, params), RecordAssignment::None) => {
                let (key, params) = (*key, params.to_vec());
                let [grdata, erdata] = &mut self.env.records.get_many_mut([got.1, exp.1]).unwrap();
                erdata.assignment = RecordAssignment::Ok(key, params);
                for name in erdata.fields.values() {
                    grdata.fields.push(*name);
                }
                ok
            }
            (RecordAssignment::None, RecordAssignment::Ok(key, params)) => {
                let (key, params) = (*key, params.to_vec());
                let [grdata, erdata] = &mut self.env.records.get_many_mut([got.1, exp.1]).unwrap();
                grdata.assignment = RecordAssignment::Ok(key, params);
                for name in grdata.fields.values() {
                    erdata.fields.push(*name);
                }
                ok
            }

            // Create a new record var containing the sum of both and redirect both to it
            (RecordAssignment::None, RecordAssignment::None) => {
                let rvar = self.env.record(got.0);
                let [grdata, erdata, newrdata] =
                    &mut self.env.records.get_many_mut([got.1, exp.1, rvar]).unwrap();

                for name in grdata.fields.values().chain(erdata.fields.values()) {
                    newrdata.fields.push(*name);
                }

                grdata.assignment = RecordAssignment::Redirect(rvar);
                erdata.assignment = RecordAssignment::Redirect(rvar);

                ok
            }

            // Treated as poison
            (RecordAssignment::NotRecord(_), _) | (_, RecordAssignment::NotRecord(_)) => ok,
            (_, RecordAssignment::Unknown(_)) => ok,
            (RecordAssignment::Unknown(_), _) => ok,
        }
    }

    pub fn ascribe_record(&mut self, ty: Tr<&Ty>, span: Span, var: RecordVar) -> CheckResult<'s> {
        self.check_rvar(false, span, var, ty)
    }

    fn check_fields(
        &mut self,
        got: (Span, RecordVar, Tr<&'s str>),
        exp: (Span, RecordVar, Tr<&'s str>),
    ) -> CheckResult<'s> {
        let [grdata, erdata] = [&self.env.records[got.1], &self.env.records[exp.1]];

        match (&grdata.assignment, &erdata.assignment) {
            // Follow recursively
            (RecordAssignment::Redirect(newvar), _) => {
                self.check_fields((got.0, *newvar, got.2), exp)
            }
            (_, RecordAssignment::Redirect(newvar)) => {
                self.check_fields(got, (exp.0, *newvar, exp.2))
            }

            // Instantiate the fields from the known records and check those fields
            (RecordAssignment::Ok(gkey, gparams), RecordAssignment::Ok(ekey, eparams)) => {
                let Some((g, e)) = self
                    .inst_field_by_type_params(*gkey, got.2, gparams)
                    .zip(self.inst_field_by_type_params(*ekey, exp.2, eparams))
                else {
                    // poison or return CheckError::Record?
                    todo!();
                };
                let (got, exp) = (g.tr(got.0), e.tr(exp.0));
                self.check(got.as_ref(), exp.as_ref())
            }

            // Force early inference and then recurse
            (RecordAssignment::Ok(..), RecordAssignment::None) => {
                self.force_record_inference(exp.0, exp.1);
                self.check_fields(got, exp)
            }
            (RecordAssignment::None, RecordAssignment::Ok(..)) => {
                self.force_record_inference(got.0, got.1);
                self.check_fields(got, exp)
            }
            (RecordAssignment::None, RecordAssignment::None) => {
                self.force_record_inference(got.0, got.1);
                self.force_record_inference(exp.0, exp.1);
                self.check_fields(got, exp)
            }

            // Treated as poison
            (RecordAssignment::NotRecord(_), _) | (_, RecordAssignment::NotRecord(_)) => ok,
            (_, RecordAssignment::Unknown(_)) => ok,
            (RecordAssignment::Unknown(_), _) => ok,
        }
    }

    fn check_record_field(
        &mut self,
        flip: bool,
        span: Span,
        (var, fname): (RecordVar, Tr<&'s str>),
        other: Tr<&Ty>,
    ) -> CheckResult<'s> {
        match &self.env.records[var].assignment {
            RecordAssignment::Ok(key, params) => {
                match self.inst_field_by_type_params(*key, fname, params) {
                    Some(ty) => self.checkf(flip, (&ty).tr(span), other),
                    None => ok, // poison
                }
            }
            RecordAssignment::Redirect(var) => {
                self.check_record_field(flip, span, (*var, fname), other)
            }
            RecordAssignment::Unknown(_) | RecordAssignment::NotRecord(_) => ok,
            RecordAssignment::None => {
                self.force_record_inference(span, var);
                self.check_record_field(flip, span, (var, fname), other)
            }
        }
    }

    pub fn call_as_function(
        &mut self,
        ty: Tr<&Ty>,
        params: usize,
    ) -> Option<(Container, Vec<Ty>, Ty)> {
        match ty.value {
            Ty::Container(kind @ (Container::Closure | Container::FnPointer), params) => {
                let mut ptypes = params.clone();
                let ret = ptypes.pop().unwrap();
                Some((*kind, ptypes, ret))
            }
            Ty::Special(Inf::Var(var)) => {
                let idata = &mut self.env.vars[*var];
                match idata.assignment.clone() {
                    Some(ty) => self.call_as_function(ty.as_ref(), params),
                    None if params == 0 => None,
                    None => {
                        let (ptypes, returns) = self.generate_closure(ty.span, params);
                        self.env.assign(
                            *var,
                            Ty::closure(ptypes.clone(), returns.clone()).tr(ty.span),
                        );
                        Some((Container::Closure, ptypes, returns))
                    }
                }
            }
            Ty::Special(Inf::Field(var, field)) => {
                let fname = self.env.records[*var].fields[*field];
                self.call_field_as_function(ty.span, *var, fname, params)
            }
            _ => None,
        }
    }

    fn call_field_as_function(
        &mut self,
        span: Span,
        var: RecordVar,
        fname: Tr<&'s str>,
        params: usize,
    ) -> Option<(Container, Vec<Ty>, Ty)> {
        match self.env.records[var].assignment.clone() {
            RecordAssignment::Redirect(rvar) => {
                self.call_field_as_function(span, rvar, fname, params)
            }
            RecordAssignment::NotRecord(..) => None,
            RecordAssignment::Ok(key, rparams) => self
                .inst_field_by_type_params(key, fname, &rparams)
                .and_then(|ftype| self.call_as_function(ftype.tr(span).as_ref(), params)),
            RecordAssignment::None => {
                self.force_record_inference(span, var);
                self.call_field_as_function(span, var, fname, params)
            }
            RecordAssignment::Unknown(_) => None,
        }
    }

    pub fn inst_field_by_type_params(
        &self,
        key: M<key::Record>,
        fname: Tr<&'s str>,
        params: &[Ty],
    ) -> Option<Ty> {
        let finst = GenericMapper::from_types(GenericKind::Entity, params.iter().cloned());
        self.fnames[key]
            .find(|name| fname == *name)
            .map(|field| (&finst).transform(&self.ftypes[key][field]))
    }

    pub fn force_record_inference(&mut self, _from: Span, var: RecordVar) {
        let rdata = &mut self.env.records[var];

        match &rdata.assignment {
            RecordAssignment::Redirect(rvar) => {
                let rvar = *rvar;
                self.force_record_inference(_from, rvar)
            }
            RecordAssignment::NotRecord(..) | RecordAssignment::Ok(..) => {}
            RecordAssignment::None => {
                let span = rdata.span;
                match find_record_by_fields(span, self.field_lookup, rdata) {
                    Err(rerror) => {
                        rdata.assignment = RecordAssignment::Unknown(rerror);
                    }
                    Ok((key, _)) => {
                        let params = self.env.unify_forall(span, &self.records[key].1);
                        self.env.records[var].assignment = RecordAssignment::Ok(key, params);
                    }
                }
            }
            RecordAssignment::Unknown(..) => {}
        }
    }

    fn force_record_field(
        &mut self,
        span: Span,
        (var, fname): (RecordVar, Tr<&'s str>),
    ) -> Option<Ty> {
        match &self.env.records[var].assignment {
            RecordAssignment::Ok(key, params) => {
                self.inst_field_by_type_params(*key, fname, params)
            }
            RecordAssignment::Redirect(var) => self.force_record_field(span, (*var, fname)),
            RecordAssignment::Unknown(_) | RecordAssignment::NotRecord(_) => None,
            RecordAssignment::None => {
                self.force_record_inference(span, var);
                self.force_record_field(span, (var, fname))
            }
        }
    }

    fn generate_closure(&mut self, span: Span, params: usize) -> (Vec<Ty>, Ty) {
        let ptypes = (0..params)
            .map(|_| Ty::infer(self.env.var(span)))
            .collect::<Vec<_>>();
        let returns = Ty::infer(self.env.var(span));
        (ptypes, returns)
    }

    fn assign_var(&mut self, flip: bool, var: Var, other: Tr<&Ty>) -> CheckResult<'s> {
        let vdata = &mut self.env.vars[var];
        let vspan = vdata.span;

        match vdata.assignment.clone() {
            Some(exp) => {
                trace!("following {var} -> {exp}");
                self.checkf(flip, (&*exp).tr(vspan), other)
            }
            None => {
                self.env.assign(var, other.cloned());
                ok
            }
        }
    }

    fn checks(&mut self, gspan: Span, espan: Span, got: &[Ty], exp: &[Ty]) -> CheckResult<'s> {
        for (g, e) in got.iter().zip(exp) {
            match self.check(g.tr(gspan), e.tr(espan)) {
                CheckResult::Ok => {}
                result => return result,
            }
        }

        ok
    }
}

pub fn find_record_by_fields<'s>(
    span: Span,
    lookup: &HashMap<&'s str, Vec<M<key::Record>>>,
    rdata: &RecordVarInfo<'s>,
) -> Result<(M<key::Record>, Vec<Tr<&'s str>>), RecordError<'s>> {
    let mut possible = HashMap::new();

    let mut unknown = vec![];

    for name in rdata.fields.values() {
        match lookup.get(**name) {
            Some(records) if possible.is_empty() => {
                records.iter().copied().for_each(|r| {
                    possible.insert(r, 0);
                });
            }
            Some(records) => {
                possible.iter_mut().for_each(|(r, misses)| {
                    if !records.contains(r) {
                        *misses += 1;
                    }
                });
            }
            None => unknown.push(*name),
        }
    }

    let oks = possible.values().filter(|&&misses| misses == 0).count();

    match oks {
        0 => {
            let fields = rdata.fields.values().copied().collect();
            Err(RecordError::UnknownRecord(span, fields))
        }
        1 => {
            let key = *possible
                .iter()
                .find_map(|(k, misses)| (*misses == 0).then_some(k))
                .unwrap();

            Ok((key, unknown))
        }
        _ => {
            let oks = possible
                .iter()
                .filter(|(_, misses)| **misses == 0)
                .map(|(key, _)| *key)
                .collect::<Vec<_>>();

            let fields = rdata.fields.values().copied().collect();
            Err(RecordError::Ambigious(span, fields, oks))
        }
    }
}
