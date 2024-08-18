use super::{Callable, Expr, FuncLower, IType, TypeAnnotation};
use crate::prelude::*;
use ast::{Entity, Mod, NFunc};
use lumina_parser as parser;
use lumina_typesystem::{RecordVar, Var};
use lumina_util::{Highlighting, Identifier, ParamFmt};
use std::fmt;
use tracing::trace;

#[derive(Clone, Debug)]
pub enum Pattern<'s> {
    Any,
    Int([parser::pat::Bound; 2], Var),
    Bind(key::Bind, Box<Self>),
    Constructor(M<key::Sum>, key::SumVariant, Vec<Tr<Self>>),
    Record(
        RecordVar,
        Option<Tr<IType>>,
        Vec<(Tr<&'s str>, key::Bind, Tr<Self>)>,
    ),
    Tuple(Vec<Tr<Self>>),
    // List(Vec<Tr<Self>>),
    Cons(Box<[Tr<Self>; 2]>, Var),
    Nil(Var),
    Bool(bool),
    String(Vec<StringPattern<'s>>),
    Poison,
}

#[derive(Clone, Debug)]
pub enum StringPattern<'s> {
    Literal(&'s str),
    Extractor(Extractor<'s>),
    Wildcard(key::Bind),
}

#[derive(Clone, Debug)]
pub struct Extractor<'s> {
    pub call: Callable<'s>,
    pub tanot: TypeAnnotation<'s>,
    pub params: Vec<Tr<Expr<'s>>>,
    pub bind: Option<Tr<&'s str>>,
}

impl<'t, 'a, 's> FuncLower<'t, 'a, 's> {
    pub fn pat(&mut self, pat: Tr<&parser::Pattern<'s>>) -> Tr<Pattern<'s>> {
        trace!("lowering pattern {pat}");

        match pat.value {
            parser::Pattern::Name(identifier, params) => {
                let is_str = params.iter().any(|pat| {
                    matches!(
                        &pat.value,
                        parser::Pattern::String(..) | parser::Pattern::Extractor(..)
                    )
                }) && identifier.is_name();

                if is_str {
                    let bind = self.bindings.declare(identifier.as_name().unwrap());
                    let init = StringPattern::Wildcard(bind);
                    self.pat_strings(init, params)
                } else {
                    self.pat_name(pat.span, identifier, params)
                }
            }
            parser::Pattern::String(str, params) => {
                self.pat_strings(StringPattern::Literal(*str), params)
            }
            parser::Pattern::Extractor(expr, bind, params) => {
                match self.extractor((**expr).as_ref(), *bind) {
                    None => Pattern::Poison,
                    Some(spat) => self.pat_strings(spat, params),
                }
            }
            parser::Pattern::Fields(init, fields) => self.pat_record(pat.span, init, fields),
            parser::Pattern::List(elems) => self.pat_list(pat.span, elems),
            parser::Pattern::Tuple(elems) => {
                let params = self.pats(elems);
                Pattern::Tuple(params)
            }
            parser::Pattern::Int(bound) => {
                let var = self.type_info.inference_mut().unwrap().int(pat.span);
                Pattern::Int(*bound, var)
            }
            parser::Pattern::Float(_) => todo!(),
            parser::Pattern::Operators { .. } => panic!("ET: unexpected operator"),
            parser::Pattern::Poison => dbg!(Pattern::Poison),
        }
        .tr(pat.span)
    }

    fn extractor(
        &mut self,
        expr: Tr<&parser::Expr<'s>>,
        bind: Option<Tr<&'s str>>,
    ) -> Option<StringPattern<'s>> {
        let expr = self.expr(expr);
        match expr.value {
            Expr::Call(call, tanot, params) => {
                let extractor = Extractor { call, tanot, params, bind };
                Some(StringPattern::Extractor(extractor))
            }
            Expr::Poison => None,
            _ => {
                self.emit_unexpected_expression(expr.span);
                None
            }
        }
    }

    fn emit_unexpected_expression(&self, span: Span) {
        self.ast
            .sources
            .error("syntax error")
            .m(self.module)
            .eline(span, "unexpected expression")
            .text("note: only extractors are allowed in patterns")
            .emit()
    }

    fn pats(&mut self, pats: &[Tr<parser::Pattern<'s>>]) -> Vec<Tr<Pattern<'s>>> {
        pats.iter().map(|p| self.pat(p.as_ref())).collect()
    }

    fn pat_strings(
        &mut self,
        init: StringPattern<'s>,
        params: &[Tr<parser::Pattern<'s>>],
    ) -> Pattern<'s> {
        let mut pats = vec![init];

        for pat in params {
            match &pat.value {
                parser::Pattern::String(str, params) => {
                    if !params.is_empty() {
                        self.emit_unexpected_expression(params[0].span);
                    }

                    pats.push(StringPattern::Literal(*str))
                }
                parser::Pattern::Extractor(expr, bind, params) => {
                    if !params.is_empty() {
                        self.emit_unexpected_expression(params[0].span);
                    }

                    if let Some(extractor) = self.extractor((**expr).as_ref(), *bind) {
                        pats.push(extractor);
                    }
                }
                parser::Pattern::Name(name, params) if name.is_name() && params.is_empty() => {
                    let bind = self.bindings.declare(name.as_name().unwrap());
                    pats.push(StringPattern::Wildcard(bind));
                }
                _ => {
                    self.ast
                        .sources
                        .error("invalid string pattern")
                        .m(self.module)
                        .eline(pat.span, "")
                        .emit();

                    return Pattern::Poison;
                }
            }
        }

        Pattern::String(pats)
    }

    fn pat_record(
        &mut self,
        span: Span,
        init: &parser::CurlyInit<'s>,
        fields: &parser::Fields<'s, parser::Pattern<'s>>,
    ) -> Pattern<'s> {
        let inf = self.type_info.inference_mut().unwrap();
        let var = inf.record(span);

        match init {
            parser::CurlyInit::Modify(_) => panic!("modify is not allowed in this context"),
            parser::CurlyInit::Construct(ty) => {
                let ty = self.to_type_lower().ty_spanned(ty.as_ref());

                let fields = self.pat_record_fields(fields);
                Pattern::Record(var, Some(ty), fields)
            }
            parser::CurlyInit::None => {
                let fields = self.pat_record_fields(fields);
                Pattern::Record(var, None, fields)
            }
        }
    }

    fn pat_list(&mut self, span: Span, elems: &[Tr<parser::Pattern<'s>>]) -> Pattern<'s> {
        let inf = self.type_info.inference_mut().unwrap();
        let ivar = inf.var(elems.get(0).map(|p| p.span).unwrap_or(span));

        if elems.is_empty() {
            return Pattern::Nil(ivar);
        }

        let mut pats = self.pats(&elems[..elems.len() - 1]);

        let xs = match elems.last() {
            Some(Tr { value: parser::Pattern::Operators { init, ops }, .. }) => {
                match ops.as_slice() {
                    [(":", rhs)] => match &rhs.value {
                        parser::Pattern::Name(xs, p) if p.is_empty() && xs.is_name() => {
                            let lhs = self.pat((**init).as_ref());
                            pats.push(lhs);

                            let xs = xs.as_name().unwrap();
                            let xs_bind = self.bindings.declare(xs);

                            Pattern::Bind(xs_bind.into(), Box::new(Pattern::Any)).tr(rhs.span)
                        }
                        _ => panic!("ET: invalid right side of cons operator"),
                    },
                    _ => {
                        self.ast
                            .sources
                            .error("syntax error")
                            .m(self.module)
                            .eline(span, "only the `:` operator is allowed in patterns")
                            .emit();

                        Pattern::Nil(ivar).tr(span)
                    }
                }
            }
            _ => {
                let last = self.pat(elems[elems.len() - 1].as_ref());
                let span = last.span;
                pats.push(last);

                Pattern::Nil(ivar).tr(span)
            }
        };

        // TODO: I added this `rev()` because it feels correct. However; It might not be.
        pats.into_iter()
            .rev()
            .fold(xs, |next, elem| {
                let span = elem.span;
                Pattern::Cons(Box::new([elem, next]), ivar).tr(span)
            })
            .value
    }

    fn pat_name(
        &mut self,
        span: Span,
        ident: &Identifier<'s>,
        params: &[Tr<parser::Pattern<'s>>],
    ) -> Pattern<'s> {
        let path = ident.as_slice();

        let is_valid_wildcard = || {
            path.len() == 1 && path[0].chars().next().unwrap().is_lowercase() && params.is_empty()
        };

        match path {
            ["true"] => self.pat_forbid_params(params, Pattern::Bool(true)),
            ["false"] => self.pat_forbid_params(params, Pattern::Bool(false)),
            ["_"] => self.pat_forbid_params(params, Pattern::Any),
            path => match self.ast.lookups.resolve_func(self.module, path) {
                Ok(Mod { key: Entity::Func(NFunc::SumVar(type_, var)), module, .. }) => {
                    let params = self.pats(params);
                    Pattern::Constructor(module.m(type_), var, params)
                }

                _ if is_valid_wildcard() => {
                    let bind = self.bindings.declare(path[0]);
                    Pattern::Bind(bind, Box::new(Pattern::Any))
                }

                Ok(entity) => {
                    let name = path.last().unwrap();
                    self.ast.sources.emit_wrong_entity(
                        self.module,
                        span,
                        name,
                        "variant",
                        entity.key,
                    );

                    Pattern::Poison
                }
                Err(err) => {
                    self.ast
                        .sources
                        .emit_lookup_err(span, self.module, "variant", err);

                    Pattern::Poison
                }
            },
        }
    }

    fn pat_forbid_params(
        &mut self,
        params: &[Tr<parser::Pattern<'s>>],
        constr: Pattern<'s>,
    ) -> Pattern<'s> {
        if let Some(p) = params.first() {
            self.ast
                .sources
                .error("unexpected pattern")
                .eline(p.span, "")
                .emit();

            Pattern::Poison
        } else {
            constr
        }
    }

    fn pat_record_fields(
        &mut self,
        fields: &parser::Fields<'s, parser::Pattern<'s>>,
    ) -> Vec<(Tr<&'s str>, key::Bind, Tr<Pattern<'s>>)> {
        fields
            .iter()
            .filter_map(|field| match field {
                parser::Field::Punned(names) => {
                    let last = names[names.len()-1];
                    let value = parser::Pattern::Name(Identifier::from_raw(*last), vec![]);
                    self.pat_handle_assignment(names, None, (&value).tr(last.span))
                },
                parser::Field::Value(v) => {
                    #[rustfmt::skip]
                    warn!("failing in HIR instead of parser due to outdated parser implementation of records");
                    self.ast.sources
                        .error("syntax error")
                        .m(self.module)
                        .eline(v.span, "expected field name")
                        .emit();

                    None
                },
                parser::Field::Assigned {
                    field_path,
                    bind,
                    value,
                } => self.pat_handle_assignment(field_path, *bind, value.as_ref()),
            })
            .collect()
    }

    fn pat_handle_assignment(
        &mut self,
        field_path: &[Tr<&'s str>],
        bind: Option<Tr<&'s str>>,
        value: Tr<&parser::Pattern<'s>>,
    ) -> Option<(Tr<&'s str>, key::Bind, Tr<Pattern<'s>>)> {
        match field_path {
            [only] => {
                let value = self.pat(value);
                let bind = self.bindings.declare(*bind.unwrap_or(*only));
                Some((*only, bind, value))
            }
            fields => {
                let binds = fields[..fields.len() - 1]
                    .iter()
                    .map(|_| self.bindings.declare_nameless())
                    .collect::<Vec<_>>();

                let init = self.pat(value);

                let this = fields.iter().copied().skip(1).zip(&binds).rev().fold(
                    init,
                    |next, (name, bind)| {
                        let inf = self.type_info.inference_mut().unwrap();
                        let var = inf.record(name.span);
                        Pattern::Record(var, None, vec![(name, *bind, next)]).tr(name.span)
                    },
                );

                Some((fields[0], binds[0], this))
            }
        }
    }
}

#[cfg(test)]
impl<'s> From<u128> for Pattern<'s> {
    fn from(value: u128) -> Self {
        Pattern::from(value..value)
    }
}

#[cfg(test)]
impl<'s> From<std::ops::Range<u128>> for Pattern<'s> {
    fn from(value: std::ops::Range<u128>) -> Self {
        Pattern::Int(
            [
                parser::pat::Bound::Pos(value.start),
                parser::pat::Bound::Pos(value.end),
            ],
            Var::from_u32(0),
        )
    }
}

#[cfg(test)]
impl<'s, T: Into<Pattern<'s>>, const N: usize> From<[T; N]> for Pattern<'s> {
    fn from(value: [T; N]) -> Self {
        Pattern::Tuple(value.into_iter().map(|p| Tr::null(p.into())).collect())
    }
}

impl<'s> fmt::Display for Pattern<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let oc = '{'.symbol();
        let cc = '}'.symbol();

        match self {
            Pattern::Any => write!(f, "_"),
            Pattern::Int([start, end], _) => write!(f, "{start}..{end}"),
            Pattern::Bind(bind, pat) => match &**pat {
                Pattern::Any => write!(f, "{bind}"),
                _ => write!(f, "{pat}@{bind}"),
            },
            Pattern::Constructor(sum, var, params) => {
                ParamFmt::new(&format!("{sum}:{var}"), params).fmt(f)
            }
            Pattern::Record(_, None, fields) => {
                write!(f, "{oc} {} {cc}", fmt_fields(fields))
            }
            Pattern::Record(_, Some(ty), fields) => {
                write!(
                    f,
                    "{oc} {:?} {} {} {cc}",
                    ty,
                    '|'.symbol(),
                    fmt_fields(fields),
                )
            }
            Pattern::Tuple(elems) => write!(f, "({})", elems.iter().format(", ")),
            Pattern::Cons(elems, _) => write!(f, "Cons {} {}", elems[0], elems[1]),
            Pattern::Nil(_) => "Nil".fmt(f),
            Pattern::Bool(b) => b.fmt(f),
            Pattern::String(str) => write!(f, "{str:?}"),
            Pattern::Poison => write!(f, "<poison>"),
        }
    }
}

fn fmt_fields<'s>(fields: &[(Tr<&'s str>, key::Bind, Tr<Pattern<'s>>)]) -> String {
    fields
        .iter()
        .format_with(", ", |(name, bind, value), f| {
            f(&format_args!("{name}@{bind} {} {value}", '='.symbol()))
        })
        .to_string()
}
