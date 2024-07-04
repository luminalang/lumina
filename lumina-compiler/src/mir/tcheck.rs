//! Various abstractions around type checking

use super::tyfmt::TyFmtState;
use super::Verify;
use crate::prelude::ModMap;
use crate::{ast, hir};
use itertools::Itertools;
use lumina_key as key;
use lumina_typesystem::{CheckResult, IType, TypeSystem};
use lumina_util::{Span, Tr};
use owo_colors::OwoColorize;
use std::fmt;
use tracing::trace;
use tracing::warn;

impl<'a, 's> Verify<'a, 's> {
    pub fn type_check_and_emit(&mut self, got: Tr<&IType>, exp: Tr<&IType>) -> bool {
        let mut system = self.type_system();
        let result = system.type_check(got, exp);
        let ok = matches!(result, lumina_typesystem::CheckResult::Ok);
        self.emit_type_errors((got, exp, result));
        ok
    }

    pub fn type_check_and_emit_application(
        &mut self,
        span: Span,
        got: &[Tr<IType>],
        exp: &[Tr<IType>],
    ) -> bool {
        let len_ok = got.len() == exp.len();

        let mut errors = got
            .iter()
            .zip(exp)
            .enumerate()
            .map(|(i, (g, e))| {
                let mut system = self.type_system();
                let result = system.type_check(g.as_ref(), e.as_ref());
                let ok = result.is_ok();
                trace!(
                    "param {i} {} ∈ {}, ok: {ok}",
                    self.ty_formatter().fmt(&**g),
                    self.ty_formatter().fmt(&**e)
                );
                (g.as_ref(), e.as_ref(), result)
            })
            .filter(|(_, _, result)| !result.is_ok())
            .collect::<Vec<_>>();

        let tyfmt = self.ty_formatter();

        match (errors.as_slice(), len_ok) {
            ([], true) => true,
            ([], false) => {
                let error = self
                    .hir
                    .sources
                    .error("missing function argument")
                    .m(self.module());

                // if got.len().abs_diff(exp.len()) == 1 {
                //     error.eline(
                //         span,
                //         format!(
                //             "missing argument of type `{}`",
                //             tyfmt.fmt(&*exp[exp.len() - 1])
                //         ),
                //     )
                // } else {
                warn!("TODO: show full typings");
                error
                    .eline(
                        span,
                        format!("expected {} arguments, was given {}", exp.len(), got.len()),
                    )
                    // }
                    .emit();
                false
            }
            ([_], true) => {
                self.emit_type_errors(errors.remove(0));
                false
            }
            (_, _) => {
                warn!("TODO: merge the errors");
                for error in errors.into_iter() {
                    self.emit_type_errors(error);
                }
                false
            }
        }
    }

    pub fn emit_type_errors(&self, (g, e, result): (Tr<&IType>, Tr<&IType>, CheckResult<'s>)) {
        use lumina_typesystem::CheckResult as Error;

        let tfmt = self.ty_formatter();

        match result {
            Error::Record(rerr) => {
                emit_record_error(&self.hir.sources, self.module(), &self.hir.records, rerr)
            }
            Error::Mismatch { ctx, .. } => {
                self.emit_type_mismatch(g.span, ctx, tfmt.clone().fmt(&*g), tfmt.fmt(&*e));
            }
            Error::Ok => {}
            _ => todo!(),
        }
    }

    pub fn emit_type_mismatch(
        &self,
        span: Span,
        ctx: &str,
        got: impl fmt::Display,
        exp: impl fmt::Display,
    ) {
        let m = self.module();
        self.error("type mismatch")
            .m(m)
            .eline(span, ctx)
            .text(format!("{}      {}", "got".purple(), got))
            .text(format!("{} {}", "expected".purple(), exp))
            .emit()
    }
}

pub fn emit_record_error<'s, T>(
    sources: &ast::Sources,
    module: key::Module,
    records: &ModMap<key::Record, (Tr<&'s str>, T)>,
    err: lumina_typesystem::RecordError<'s>,
) {
    match err {
        lumina_typesystem::RecordError::DoesNotHaveFields(_, _, _) => todo!(),
        lumina_typesystem::RecordError::UnknownRecord(span, fields) => sources
            .error("unknown record")
            .m(module)
            .eline(
                span,
                format!(
                    "no record in scope with the fields {}",
                    fields.values().format(", ")
                ),
            )
            .emit(),
        lumina_typesystem::RecordError::Ambigious(span, fields, possibilities) => {
            match possibilities.as_slice() {
                [left, right] => sources
                    .error("ambigious record")
                    .m(module)
                    .eline(
                        span,
                        format!(
                            "record could infer to either {} or {}",
                            records[*left].0, records[*right].0,
                        ),
                    )
                    .text(format!(
                        "inferred to have the fields {{{}}}",
                        fields.values().format(", ")
                    ))
                    .emit(),
                many => {
                    let mut err = sources
                        .error("ambigious record")
                        .m(module)
                        .eline(span, "record could infer into any of");
                    for key in many {
                        let name = records[*key].0;
                        err = err.text(format!("  {name}"));
                    }
                    err.emit()
                }
            }
            sources
                .error("type mismatch")
                .m(module)
                .eline(
                    span,
                    format!("a record with the fields {}", fields.values().format(", ")),
                )
                .emit();
            todo!();
        }
        lumina_typesystem::RecordError::NotARecord => todo!(),
    }
}

pub struct SameAsCheck<'s> {
    kind: &'static str,
    types: Vec<Tr<IType>>,
    // if we know there's a match between two branches, we assume the type of one of those branches
    // is the correct one and check everything else against it.
    prioritised: Option<usize>,

    mismatches: Vec<(Tr<IType>, Tr<IType>, &'static str, usize)>,
    rerrs: Vec<lumina_typesystem::RecordError<'s>>,
}

impl<'s> SameAsCheck<'s> {
    pub fn new(kind: &'static str) -> Self {
        SameAsCheck {
            kind,
            types: vec![],
            prioritised: None,
            mismatches: Vec::new(),
            rerrs: vec![],
        }
    }

    pub fn include<'a>(&mut self, mut ts: TypeSystem<'a, 's>, ty: Tr<IType>) {
        match self.types.len() {
            0 => {
                trace!("initialising same_as checker with {ty}");
                self.types.push(ty)
            }
            _ => match self.prioritised {
                Some(i) => {
                    let exp = self.types[i].as_ref();

                    trace!("checking {ty} against previous same_as {exp}");

                    let result = ts.type_check(ty.as_ref(), exp);

                    self.include_result(result);
                    self.types.push(ty);
                }
                None => {
                    let mut kept_result = None;

                    trace!(
                        "trying to find a match for {ty} in previous types {:#?}",
                        &self.types
                    );

                    for (i, exp) in self.types.iter().enumerate() {
                        let result = ts.type_check(ty.as_ref(), exp.as_ref());

                        if result.is_ok() {
                            self.prioritised = Some(i);
                            self.types.push(ty);
                            return;
                        } else if kept_result.is_none() {
                            kept_result = Some(result);
                        }
                    }

                    self.include_result(kept_result.unwrap());

                    self.types.push(ty);
                }
            },
        }
    }

    fn include_result(&mut self, result: CheckResult<'s>) {
        match result {
            CheckResult::Record(rerr) => self.rerrs.push(rerr),
            CheckResult::Mismatch { got, exp, ctx } => {
                self.mismatches.push((got, exp, ctx, self.types.len()))
            }
            CheckResult::Ok => {}
        }
    }

    pub fn finalize<'t>(
        &mut self,
        module: key::Module,
        tfmt: TyFmtState<'t, 's>,
        sources: &ast::Sources,
    ) -> Tr<IType> {
        for rerr in self.rerrs.iter() {
            todo!("{rerr:?}");
        }

        let mut err = sources.error("type mismatch").m(module);

        match self.mismatches.as_slice() {
            [] => {}
            [(got, exp, ctx, _)] => err
                .eline(got.span, *ctx)
                .text(format!(
                    "{}      {}",
                    "got".purple(),
                    tfmt.clone().fmt(&**got)
                ))
                .text(format!("{} {}", "expected".purple(), tfmt.fmt(&**exp)))
                .emit(),
            many => {
                let pri = &self.types[self.prioritised.unwrap_or(0)];
                err = err.iline(
                    pri.span,
                    format!("expected type is {}", tfmt.clone().fmt(&**pri)),
                );
                many.iter()
                    .fold(err, |err, (got, _, _, _)| {
                        err.eline(got.span, format!("{}", tfmt.clone().fmt(&**got)))
                    })
                    .text(match self.kind {
                        "" => "all must be of the same type".into(),
                        kind => format!("{kind} must be of the same type"),
                    })
                    .emit()
            }
        }

        // match self.kind {
        //     "match patterns" if self.errors.len() > 1 => {
        //         err = err.eline();
        //     }
        //     "match expressions" if self.errors.len() > 1 => todo!(),
        //     _ => {
        //         todo!();
        //     }
        // }

        return self.types.remove(self.prioritised.unwrap_or(0));
    }
}
