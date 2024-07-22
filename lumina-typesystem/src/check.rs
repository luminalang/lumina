use super::*;
use derive_more::From;
use lumina_key::LinearFind;
use lumina_util::{Span, Spanned, Tr};
use tenv::RecordVarInfo;
use tracing::trace;
use tracing::warn;
use IType as Ty;

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

pub enum NumError {
    NegativeForUnsigned(Span, Bitsize),
    NumberTooLarge(Span, u128, (bool, Bitsize)),
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
            // Dismissed cases
            (Ty::Prim(Prim::Poison), _) => ok,
            (_, Ty::Prim(Prim::Poison)) => ok,
            (Ty::Prim(Prim::Never), _) => ok,
            (_, Ty::Prim(Prim::Never)) => ok,

            // Concrete checks
            (
                Ty::List(gtype, gp) | Ty::Defined(gtype, gp),
                Ty::List(etype, ep) | Ty::Defined(etype, ep),
            ) if gtype == etype => self.checks(got.span, exp.span, gp, ep),
            (Ty::Prim(gprim), Ty::Prim(eprim)) if gprim == eprim => ok,
            (Ty::Generic(ggen), Ty::Generic(egen)) if ggen == egen => ok,
            (Ty::Self_, Ty::Self_) => ok,

            // Self Substitution
            (Ty::Self_, _) if self.env.self_.is_some() => {
                let got = self.env.self_.clone().unwrap().tr(got.span);
                self.check(got.as_ref(), exp)
            }
            (_, Ty::Self_) if self.env.self_.is_some() => {
                let exp = self.env.self_.clone().unwrap().tr(got.span);
                self.check(got, exp.as_ref())
            }

            (
                Ty::Container(Container::Func(gkind, gptypes, gret)),
                Ty::Container(Container::Func(ekind, eptypes, eret)),
            ) if ekind == gkind => self
                .checks(got.span, exp.span, gptypes, eptypes)
                .and_then(|| self.check((&**gret).tr(got.span), (&**eret).tr(exp.span))),

            (Ty::Container(Container::Tuple(gp)), Ty::Container(Container::Tuple(ep))) => {
                if ep.len() == gp.len() {
                    self.checks(got.span, exp.span, gp, ep)
                } else {
                    CheckResult::fail(got, exp, "tuples differ in amount of parameters")
                }
            }
            (Ty::Container(Container::Pointer(gp)), Ty::Container(Container::Pointer(ep))) => {
                self.check((&**gp).tr(got.span), (&**ep).tr(exp.span))
            }

            // TVar direct checks
            (Ty::Var(gvar), Ty::Var(evar)) if gvar == evar => ok,
            (Ty::InferringRecord(grvar), Ty::InferringRecord(ervar)) if grvar == ervar => ok,
            (Ty::Field(gvar, gfvar), Ty::Field(evar, efvar)) if evar == gvar && gfvar == efvar => {
                ok
            }

            // TVar merging
            (Ty::Var(g), Ty::Var(e)) => {
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
            (Ty::InferringRecord(grvar), Ty::InferringRecord(ervar)) => {
                self.merge_records((got.span, *grvar), (exp.span, *ervar))
            }
            (Ty::Field(gvar, gfvar), Ty::Field(evar, efvar)) => {
                let [gfield, efield] = [
                    self.env.records[*gvar].fields[*gfvar],
                    self.env.records[*evar].fields[*efvar],
                ];
                self.check_fields((got.span, *gvar, gfield), (exp.span, *evar, efield))
            }

            // TVar assignments
            (_, Ty::Var(evar)) => self.assign_var(true, *evar, got),
            (Ty::Var(gvar), _) => self.assign_var(false, *gvar, exp),
            (_, Ty::Field(evar, efvar)) => {
                let fname = self.env.records[*evar].fields[*efvar];
                self.check_record_field(true, exp.span, (*evar, fname), got)
            }
            (Ty::Field(gvar, gfvar), _) => {
                let fname = self.env.records[*gvar].fields[*gfvar];
                self.check_record_field(false, got.span, (*gvar, fname), exp)
            }
            (Ty::InferringRecord(rvar), _) => self.check_rvar(false, got.span, *rvar, exp),
            (_, Ty::InferringRecord(rvar)) => self.check_rvar(true, exp.span, *rvar, got),

            _ => CheckResult::fail(got, exp, ""),
        }
    }

    fn check_rvar(
        &mut self,
        flip: bool,
        span: Span,
        rvar: RecordVar,
        other: Tr<&IType>,
    ) -> CheckResult<'s> {
        let assgn = self.env.records[rvar].assignment.clone();
        match assgn {
            tenv::RecordAssignment::Redirect(_) => todo!(),
            tenv::RecordAssignment::Ok(key, params) => {
                let ty = IType::defined(key, params).tr(span);
                self.checkf(flip, ty.as_ref(), other)
            }
            tenv::RecordAssignment::None => self.assign_to_unassigned_rvar(flip, span, rvar, other),
            tenv::RecordAssignment::Unknown(..) => ok,

            // TODO: should we treat this as poison instead?
            tenv::RecordAssignment::NotRecord(ty) => self.checkf(flip, (&ty).tr(span), other),
        }
    }

    fn assign_to_unassigned_rvar(
        &mut self,
        flip: bool,
        span: Span,
        rvar: RecordVar,
        other: Tr<&IType>,
    ) -> CheckResult<'s> {
        let assignment = &mut self.env.records[rvar].assignment;
        assert!(matches!(assignment, RecordAssignment::None));
        match other.value {
            IType::List(key, params) | IType::Defined(key, params) => match key.value {
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
            },
            IType::Var(var) => {
                let vdata = &self.env.vars[*var];
                match vdata.assignment.clone() {
                    Some(other) => self.assign_to_unassigned_rvar(flip, span, rvar, other.as_ref()),
                    None => match &vdata.int_constraint {
                        Some(IntConstraint { .. }) => todo!("error: or buffer?"),
                        None => {
                            self.env
                                .assign(*var, IType::InferringRecord(rvar).tr(other.span));

                            ok
                        }
                    },
                }
            }
            IType::Field(_, _) => todo!("follow"),
            IType::InferringRecord(orvar) if flip => {
                self.merge_records((other.span, *orvar), (span, rvar))
            }
            IType::InferringRecord(orvar) => self.merge_records((span, rvar), (other.span, *orvar)),
            _ => {
                warn!("Assuming we will error on the incorrect record assignment during type finalization");
                *assignment = RecordAssignment::NotRecord(other.value.clone());
                ok
            }
        }
    }

    fn lambda_hint_from_lowest(lambdas: &[Option<key::Lambda>]) -> Option<key::Lambda> {
        lambdas.iter().min().copied().unwrap_or(None)
    }

    fn merge_records(&mut self, got: (Span, RecordVar), exp: (Span, RecordVar)) -> CheckResult<'s> {
        let [grdata, erdata] = [&self.env.records[got.1], &self.env.records[exp.1]];

        match (&grdata.assignment, &erdata.assignment) {
            // Follow recursively
            (RecordAssignment::Redirect(newvar), _) => self.merge_records((got.0, *newvar), exp),
            (_, RecordAssignment::Redirect(newvar)) => self.merge_records(got, (exp.0, *newvar)),

            // Direct comparison between two records
            (RecordAssignment::Ok(gkey, gparams), RecordAssignment::Ok(ekey, eparams)) => {
                let got = IType::defined(*gkey, gparams.to_vec()).tr(got.0);
                let exp = IType::defined(*ekey, eparams.to_vec()).tr(exp.0);
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

    pub fn ascribe_record(
        &mut self,
        ty: Tr<&IType>,
        span: Span,
        var: RecordVar,
    ) -> CheckResult<'s> {
        let rdata = &self.env.records[var];

        match rdata.assignment.clone() {
            RecordAssignment::Ok(key, params) => {
                let got = IType::defined(key, params).tr(span);
                self.check(got.as_ref(), ty)
            }
            RecordAssignment::Redirect(var) => self.ascribe_record(ty, span, var),

            RecordAssignment::None => match ty.value {
                IType::List(kind, params) | IType::Defined(kind, params) => match kind.value {
                    key::TypeKind::Record(key) => {
                        let key = kind.module.m(key);
                        self.env.records[var].assignment =
                            RecordAssignment::Ok(key, params.clone());
                        ok
                    }
                    _ => CheckResult::Record(RecordError::NotARecord),
                },
                IType::Var(v) => match self.env.vars[*v].assignment.clone() {
                    Some(ty) => self.ascribe_record(ty.as_ref(), span, var),
                    None => ok, // we don't need to do anything on `{ _ | ... }`
                },
                IType::Self_ => match self.env.self_.clone() {
                    Some(sty) => {
                        let ty = sty.tr(ty.span);
                        self.ascribe_record(ty.as_ref(), span, var)
                    }
                    None => panic!("ET: `Self` not allowed in this context"),
                },
                other @ (IType::Field(..) | IType::InferringRecord(..)) => {
                    panic!("Type ascription by unsupported type: {other}")
                }
                _ => CheckResult::Record(RecordError::NotARecord),
            },

            // Treated as poison
            RecordAssignment::NotRecord(_) => ok,
            RecordAssignment::Unknown(_) => ok,
        }
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
        other: Tr<&IType>,
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
        ty: Tr<&IType>,
        params: usize,
    ) -> Option<Result<(FuncKind, Vec<IType>, IType), CheckResult<'s>>> {
        match ty.value {
            IType::Container(Container::Func(kind, ptypes, returns)) => {
                Some(Ok((*kind, ptypes.clone(), (**returns).clone())))
            }
            IType::Var(var) => {
                let idata = &mut self.env.vars[*var];
                match idata.assignment.clone() {
                    Some(ty) => self.call_as_function(ty.as_ref(), params),
                    None if params == 0 => None,
                    None => {
                        let kind = FuncKind::Closure;
                        let (ptypes, returns) = self.generate_closure(ty.span, params);
                        self.env.assign(
                            *var,
                            IType::closure(ptypes.clone(), returns.clone()).tr(ty.span),
                        );
                        Some(Ok((kind, ptypes, returns)))
                    }
                }
            }
            IType::Field(var, field) => {
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
    ) -> Option<Result<(FuncKind, Vec<IType>, IType), CheckResult<'s>>> {
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
            RecordAssignment::Unknown(_) => {
                None
                // todo!("poison: {rerror:?}");
                // but how will we actually poison?
                //
                // we probably need to make the call_*_as_function return type more special
            }
        }
    }

    pub fn inst_field_by_type_params(
        &self,
        key: M<key::Record>,
        fname: Tr<&'s str>,
        params: &[IType],
    ) -> Option<IType> {
        let finst = ForeignInst::from_type_params(params);
        self.fnames[key]
            .find(|name| fname == *name)
            .map(|field| finst.apply(&self.ftypes[key][field]))
    }

    pub fn force_record_inference(&mut self, _from: Span, var: RecordVar) {
        let rdata = &mut self.env.records[var];

        match &rdata.assignment {
            RecordAssignment::Redirect(rvar) => {
                let rvar = *rvar;
                self.force_record_inference(_from, rvar)
            }
            // The reason we don't actually return the resulting information is because the parent branch
            // is already forced to handle the `NotRecord` condition. So; we end up with less
            // verbosity by just not returning anything and recursing in the parent function
            // instead.
            RecordAssignment::NotRecord(..) | RecordAssignment::Ok(..) => {}
            RecordAssignment::None => {
                let span = rdata.span;
                match find_record_by_fields(span, self.field_lookup, rdata) {
                    Err(rerror) => {
                        rdata.assignment = RecordAssignment::Unknown(rerror);
                    }
                    Ok((key, _)) => {
                        let params = self.records[key]
                            .1
                            .keys()
                            .map(|_| IType::Var(self.env.var(span)))
                            .collect::<Vec<_>>();
                        self.env.records[var].assignment = RecordAssignment::Ok(key, params);
                    }
                }
            }
            RecordAssignment::Unknown(..) => {}
        }
    }

    fn as_record(&self, var: Var) -> Option<(M<key::Record>, &[IType])> {
        dbg!(&var);
        match self.env.vars[var].assignment.as_ref() {
            Some(Tr {
                value:
                    IType::Defined(M { value: key::TypeKind::Record(key), module }, params)
                    | IType::List(M { value: key::TypeKind::Record(key), module }, params),
                ..
            }) => Some((module.m(*key), params)),
            Some(Tr { value: IType::Var(var), .. }) => self.as_record(*var),
            _ => None,
        }
    }

    fn generate_closure(&mut self, span: Span, params: usize) -> (Vec<IType>, IType) {
        let ptypes = (0..params)
            .map(|_| IType::Var(self.env.var(span)))
            .collect::<Vec<_>>();
        let returns = IType::Var(self.env.var(span));
        (ptypes, returns)
    }

    fn assign_var(&mut self, flip: bool, var: Var, other: Tr<&IType>) -> CheckResult<'s> {
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
