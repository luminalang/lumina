//! Various abstractions around type checking

use super::tyfmt::TyFmtState;
use super::Verify;
use crate::{ast, hir, Target};
use key::M;
use lumina_key as key;
use lumina_typesystem::{IType, TEnv, TypeSystem};
use lumina_util::{Span, Spanned, Tr};
use owo_colors::OwoColorize;
use std::collections::HashMap;
use std::fmt;
use tracing::trace;
use tracing::warn;

impl<'s> hir::HIR<'s> {
    pub(super) fn type_system<'a>(
        &'a self,
        env: &'a mut TEnv<'s>,
        target: Target,
        field_lookup: &'a HashMap<&'s str, Vec<M<key::Record>>>,
    ) -> TypeSystem<'a, 's> {
        TypeSystem::new(
            env,
            target.int_size(),
            &self.fnames,
            &self.field_types,
            &self.records,
            field_lookup,
        )
    }
}

impl<'a, 's> Verify<'a, 's> {
    pub fn type_check_and_emit(&mut self, got: Tr<&IType>, exp: Tr<&IType>) -> bool {
        let env = &mut self.tenvs[self.current.fkey];
        let fields = &self.field_lookup[self.current.fkey.0];
        let mut ts = self.hir.type_system(env, self.target, fields);

        let ok = ts.unify(got.span, *got, *exp);
        if !ok {
            let tfmt = self.ty_formatter();
            self.emit_type_mismatch(got.span, "", tfmt.clone().fmt(*got), tfmt.fmt(*exp));
        }
        ok
    }

    pub fn check_tuple_index(&mut self, object: Tr<&IType>, i: Tr<usize>) -> IType {
        let env = &mut self.tenvs[self.current.fkey];
        let fields = &self.field_lookup[self.current.fkey.0];
        let mut ts = self.hir.type_system(env, self.target, fields);
        match ts.check_tuple_index(object, *i) {
            Some(ty) => ty,
            None => {
                self.error("type mismatch")
                    .m(self.module())
                    .eline(
                        i.span,
                        format!(
                            "attempted to access tuple field of non-tuple type {}",
                            self.ty_formatter().fmt(object.value)
                        ),
                    )
                    .emit();

                IType::poison()
            }
        }
    }

    pub fn type_check_same<T, F>(
        &mut self,
        name: &'static str,
        ivar: Option<Tr<lumina_typesystem::Var>>,
        elems: &[T],
        mut f: F,
    ) -> Tr<IType>
    where
        F: FnMut(&mut Self, &T) -> Tr<IType>,
    {
        let mut checker = SameAsCheck::new(name);

        for elem in elems {
            let ty = f(self, elem);
            checker.include(self.type_system(), ty);
        }

        if let Some(ivar) = ivar {
            // Include the inner typevar to make sure it's assigned post this point
            checker.include(self.type_system(), ivar.map(IType::infer));
        }

        checker.finalize(self.module(), self.ty_formatter(), &self.hir.sources)
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
            .filter_map(|(i, (g, e))| {
                let env = &mut self.tenvs[self.current.fkey];
                let fields = &self.field_lookup[self.current.fkey.0];
                let mut ts = self.hir.type_system(env, self.target, fields);

                let ok = ts.unify(g.span, &g, &e);
                trace!(
                    "param {i} {} ∈ {}, ok: {ok}",
                    self.ty_formatter().fmt(&**g),
                    self.ty_formatter().fmt(&**e)
                );
                (!ok).then_some((g.as_ref(), e.as_ref()))
            })
            .collect::<Vec<_>>();

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
                let (got, exp) = errors.remove(0);
                let tfmt = self.ty_formatter();
                self.emit_type_mismatch(got.span, "", tfmt.clone().fmt(*got), tfmt.fmt(*exp));
                false
            }
            (_, _) => {
                warn!("TODO: merge the errors");
                for (got, exp) in errors.into_iter() {
                    let tfmt = self.ty_formatter();
                    self.emit_type_mismatch(got.span, "", tfmt.clone().fmt(*got), tfmt.fmt(*exp));
                }
                false
            }
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

pub struct SameAsCheck {
    kind: &'static str,

    // A pair of types and how many types unified into each type
    types: Vec<(Tr<IType>, usize)>,
}

impl SameAsCheck {
    pub fn new(kind: &'static str) -> Self {
        SameAsCheck { kind, types: vec![] }
    }

    pub fn include<'a, 's>(&mut self, mut ts: TypeSystem<'a, 's>, ty: Tr<IType>) -> bool {
        if self.types.is_empty() {
            self.types.push((ty, 1));
            return true;
        }

        for (prev, count) in self.types.iter_mut() {
            let ok = ts.unify(ty.span, &ty, prev);
            if ok {
                *count += 1;
                return true;
            }
        }

        self.types.push((ty, 1));
        false
    }

    pub fn finalize<'t, 's>(
        &mut self,
        module: key::Module,
        tfmt: TyFmtState<'t, 's>,
        sources: &ast::Sources,
    ) -> Tr<IType> {
        let err = sources.error("type mismatch").m(module);

        self.types.sort_by(|l, r| r.1.cmp(&l.1));

        let elem = match self.kind {
            "list" => "element",
            "match expressions" => "branch",
            "match patterns" => "pattern",
            _ => "element",
        };

        match self.types.as_slice() {
            [] => panic!("no types were checked"),
            [_] => return self.types.remove(0).0,
            [(x, xc), (y, yc)] if *xc == *yc => {
                err.eline(
                    x.span,
                    format!("this {elem} is of type `{}`", tfmt.clone().fmt(&**x)),
                )
                .eline(
                    y.span,
                    format!("but this {elem} is of type `{}`", tfmt.fmt(&**y)),
                )
                .emit();

                return IType::poison().tr(x.span);
            }
            [(exp, expc), types @ ..] => {
                for (got, _) in types {
                    err.clone()
                        .eline(got.span, self.kind)
                        .text(format!("{}      {}", "got".purple(), got))
                        .text(format!("{} {}", "expected".purple(), exp))
                        .iline(
                            exp.span,
                            format!("type set by this and {expc} other {elem}(s)"),
                        )
                        .emit();
                }

                return IType::poison().tr(exp.span);
            }
        }
    }
}
