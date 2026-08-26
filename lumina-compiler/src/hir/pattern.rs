use super::{Bind, ExprId, FuncLower, PatternId, TypeKey, Typed, symbols};
use crate::hir::func::{FieldAssignment, MergedFields};
use crate::hir::ty::{LoweringKind as _, TypeLower};
use crate::{InterResolved, emit_err_item_not_found, errors, key};
use key::EntityList;
use lumina_parser as par;
use lumina_typesystem as ts;
use lumina_typesystem::{ArrayLen, IntSize, Prim, TaggedGeneric, Var};
use lumina_util::{Span, Spanned, Tr};
use rvsdg::EntityIter;
use tracing::info;

#[derive(Clone, Debug)]
pub enum Pattern<'s> {
    Any,
    Int([par::pat::Bound; 2], Var),
    Char(&'s str),
    Variant(TypeKey, key::Variant, EntityList<PatternId>),
    Record(Vec<(Tr<&'s str>, PatternId)>),
    Tuple(EntityList<PatternId>),
    List(EntityList<PatternId>, Option<PatternId>),
    Array(EntityList<PatternId>),
    GenericArray(EntityList<PatternId>, Tr<TaggedGeneric>),
    Bool(bool),
    Float(f64),
    Poison,
    BindTo(Bind, PatternId),
    String(Vec<StringPattern<'s>>),
}

#[derive(Clone, Debug)]
pub enum StringPattern<'s> {
    Literal(&'s str),
    CharLiteral(&'s str),
    Extractor(Extractor),
    CharWildcard(Bind),
    Wildcard(Bind),
    Poison,
}

#[derive(Clone, Debug)]
pub struct Extractor {
    pub call: ExprId,
    pub bind: Option<Bind>,
}

impl<'v, 's> FuncLower<'v, 's> {
    pub fn pat(&mut self, pat: Tr<&par::Pattern<'s>>) -> PatternId {
        info!("pattern: {pat}");

        let pat_id = match pat.value {
            par::Pattern::Name(identifier, params) => {
                if let [name] = identifier.as_slice() {
                    match (*name, params.is_empty()) {
                        ("true", true) => {
                            let ty = self.tctx.define(pat.span, |env| env.prim(Prim::Bool));
                            return self.add_typed_pat(Pattern::Bool(true), ty, pat.span);
                        }
                        ("false", true) => {
                            let ty = self.tctx.define(pat.span, |env| env.prim(Prim::Bool));
                            return self.add_typed_pat(Pattern::Bool(false), ty, pat.span);
                        }
                        _ => {}
                    }
                }

                let path = identifier.as_slice();

                match self.find(symbols::Namespace::Functions, path) {
                    InterResolved::Item(module, symbols::Item::Variant(ty, var)) => {
                        let params = self.pats(params);

                        let ty = TypeKey { origin: module, key: ty };

                        let apath = par::AnnotatedPath::without(identifier.clone()).tr(pat.span);

                        let fvar =
                            self.tctx
                                .lowering_instantiation()
                                .variant(ty, var, apath.as_ref());

                        // let (f, return_type) = self.tctx.inst_variant(pat.span, ty, var, 1, &[]);
                        let appl = self.tctx.env.apply(pat.span, fvar);
                        for pat in params.as_slice(self.uarena.pat_pool(self.id)) {
                            let ty = self.uarena.pats[*pat].ty;
                            self.tctx.env.apply_next_parameter(appl, ty);
                        }
                        let ret = self.tctx.env.get_return_type(appl);

                        let p = Pattern::Variant(ty, var, params);
                        self.add_typed_pat(p, ret, pat.span)
                    }
                    InterResolved::Poison => self.pat_poison(pat.span),
                    InterResolved::NotFound { in_, at, exists } => {
                        if let Some(name) = identifier.as_name() {
                            if params.is_empty() {
                                return self.bind_to_any(pat.span, Some(name)).0;
                            }
                        }
                        emit_err_item_not_found(pat.span, self.files, in_, at, exists);
                        self.pat_poison(pat.span)
                    }
                    _ => {
                        if let Some(name) = identifier.as_name() {
                            if params.is_empty() {
                                return self.bind_to_any(pat.span, Some(name)).0;
                            }
                        }

                        self.pat_poison(pat.span)
                    }
                }
            }
            par::Pattern::String(raw, params) => {
                self.string_pattern(pat.span, StringPattern::Literal(raw), params)
            }
            par::Pattern::Char(raw, params) => {
                if !params.is_empty() {
                    todo!("char in string pattern");
                }

                todo!("oh right this is a problem");
                // Technically we don't really need the types during the pass. We can iterate the
                // hir::Pats afterwards and get them.
                //
                // Would mean we need to null-initialise them though which is a bit ugly...
                //
                // Hm, actually, that would assume we don't same_as too early in the match parent
                // which is ugly. Still technically possible though.

                // we won't know whether it's string or just char here since chars can be used in
                // string patterns. <- ?? why?

                // if !params.is_empty() {
                //     errors::err("invalid pattern")
                //         .line(pat.span, "char patterns cannot take parameters")
                //         .emit();
                //     self.pat_poison(pat.span)
                // } else {
                //     let size = IntSize::new(false, 8);
                //     let ty = self.tctx.define(pat.span, |env| env.int(size));
                //     self.add_typed_pat(Pattern::Char(raw), ty, pat.span)
                // }
            }
            par::Pattern::CharWildcard(name, params) => {
                let ty = self
                    .tctx
                    .define(pat.span, |env| env.int(IntSize::new(false, 8)));
                let bind = self.scopes.declare(*name, ty);
                self.string_pattern(pat.span, StringPattern::CharWildcard(bind), params)
            }
            par::Pattern::Extractor(..) => {
                todo!("extractor");
            }
            par::Pattern::Fields(init, fields) => self.record_pattern(pat.span, init, fields),
            par::Pattern::List(elems, list_length) => {
                self.list_pattern(pat.span, elems, list_length)
            }
            par::Pattern::ListDotDot(elem) => {
                todo!();
            }
            par::Pattern::Tuple(elems) => {
                let (elems, elems_types) = self.pats_and_typeof(elems);

                let ty = self.tctx.define(pat.span, |env| env.tuple(elems_types));
                self.add_typed_pat(Pattern::Tuple(elems), ty, pat.span)
            }
            par::Pattern::Int(bounds) => {
                let ty = self.tctx.define(pat.span, |env| env.numeric());
                let p = Pattern::Int(*bounds, ty);
                self.add_typed_pat(p, ty, pat.span)
            }
            par::Pattern::Float(n) => {
                let ty = self.tctx.define(pat.span, |env| env.prim(Prim::Float));
                self.add_typed_pat(Pattern::Float(*n), ty, pat.span)
            }
            par::Pattern::Operators { init, ops } => self.operator_pattern(pat.span, init, ops),
            par::Pattern::Poison => self.pat_poison(pat.span),
        };

        pat_id
    }

    fn pat_and_typeof(&mut self, pat: Tr<&par::Pattern<'s>>) -> Typed<PatternId> {
        let pat_id = self.pat(pat);
        let ty = self.uarena.pats[pat_id].ty;
        let span = self.uarena.pats[pat_id].span;
        Typed::new(pat_id, ty, span)
    }

    fn pats(&mut self, patterns: &[Tr<par::Pattern<'s>>]) -> EntityList<PatternId> {
        let mut list = EntityList::new();
        for pat in patterns {
            let pat = self.pat(pat.as_ref());
            list.push(pat, self.uarena.pat_pool_mut(self.id));
        }
        list
    }

    pub fn pats_and_typeof(
        &mut self,
        pats: &[Tr<par::Pattern<'s>>],
    ) -> (EntityList<PatternId>, EntityList<Var>) {
        let pats = self.pats(pats);
        let types = self.type_of_pats(pats.clone());
        (pats, types)
    }

    pub fn type_of_pats(&mut self, pats: EntityList<PatternId>) -> EntityList<Var> {
        EntityList::from_iter(
            pats.as_slice(self.uarena.pat_pool(self.id))
                .iter()
                .map(|pat| self.uarena.pats[*pat].ty),
            self.tctx.var_pool_mut(),
        )
    }

    pub fn record_pattern(
        &mut self,
        span: Span,
        init: &par::CurlyInit<'s>,
        fields: &[par::Field<'s, par::Pattern<'s>>],
    ) -> PatternId {
        match init {
            par::CurlyInit::Modify(modified) => {
                errors::err("syntax error")
                    .line(modified.span, "can't modify a record in patterns")
                    .emit();

                self.pat_poison(span)
            }
            par::CurlyInit::Construct(ty) => {
                let merged = MergedFields::new(fields);
                let ty = self.tctx.ty(ty.as_ref());
                self.destruct_record(span, ty, merged)
            }
            par::CurlyInit::None => match self.hack_resolve_ambigious_constructor(fields) {
                Some(tkey) => {
                    assert_eq!(fields.len(), 1);
                    #[cfg(debug_assertions)]
                    self.ctx().in_origin(self.project(), tkey.origin, |unit| {
                        unit.header.type_signatures[tkey.key].generics.is_empty();
                    });
                    let ty = self
                        .tctx
                        .define(span, |env| env.defined(tkey, EntityList::new()));
                    self.destruct_record(span, ty, MergedFields::new(&[]))
                }
                None => {
                    let merged = MergedFields::new(fields);
                    let ty = self.tctx.define(span, |env| env.unknown());
                    self.destruct_record(span, ty, merged)
                }
            },
        }
    }

    fn destruct_record(
        &mut self,
        span: Span,
        ty: Var,
        fields: MergedFields<'_, 's, par::Pattern<'s>>,
    ) -> PatternId {
        let fields = fields
            .into_iter()
            .map(|(field, assigned)| {
                let ty = self
                    .tctx
                    .define(field.span, |env| env.add_field(ty, *field));

                let rhs = match assigned {
                    FieldAssignment::Fields(fields) => self.destruct_record(field.span, ty, fields),
                    FieldAssignment::Tail(bind_to, tail_pat) => match tail_pat {
                        Some(pat) => {
                            self.bind_to(bind_to.map(|v| *v), field.span, ty, |this, _| {
                                this.pat(pat.as_ref())
                            })
                            .0
                        }
                        None => {
                            let bind_to = bind_to.map(|v| *v).unwrap_or(*field);
                            self.bind_to(Some(bind_to), span, ty, |this: &mut Self, _| {
                                this.uarena.pats.push(Typed::new(Pattern::Any, ty, span))
                            })
                            .0
                        }
                    },
                };

                self.tctx.env.assign(ty, self.uarena.pats[rhs].ty);

                (field, rhs)
            })
            .collect();

        self.add_typed_pat(Pattern::Record(fields), ty, span)
    }

    pub fn bind_to_any(&mut self, span: Span, name: Option<&'s str>) -> (PatternId, Bind) {
        let ty = self.tctx.define(span, |env| env.unknown());
        self.bind_to(name, span, ty, |this: &mut Self, _| {
            this.uarena.pats.push(Typed::new(Pattern::Any, ty, span))
        })
    }

    pub fn bind_to(
        &mut self,
        name: Option<&'s str>,
        span: Span,
        ty: Var,
        pat: impl FnOnce(&mut Self, Bind) -> PatternId,
    ) -> (PatternId, Bind) {
        let bind = match name {
            Some(name) => self.scopes.declare(name, ty),
            None => self.scopes.declare_nameless(ty),
        };

        let src = pat(self, bind);

        let pat = self
            .uarena
            .pats
            .push(Typed::new(Pattern::BindTo(bind, src), ty, span));

        (pat, bind)
    }

    fn list_pattern(
        &mut self,
        span: Span,
        elems: &[Tr<par::Pattern<'s>>],
        list_length: &par::ListLength<'s>,
    ) -> PatternId {
        let given_len = elems.len();

        let (elems, xs) = match elems.split_last() {
            Some((Tr { value: par::Pattern::ListDotDot(xs), .. }, elems)) => {
                let elems = self.pats(elems);
                let xs = self.pat((**xs).as_ref());
                (elems, Some(xs))
            }
            _ => {
                let elems = self.pats(elems);
                (elems, None)
            }
        };

        let elem_ty = {
            let (sameas, list_ty, elem_ty) = self.tctx.env.list_sameas(span);

            for pat in elems.as_slice(self.uarena.pat_pool(self.id)) {
                let ty = self.uarena.pats[*pat].ty;
                self.tctx.env.add_sameas_member(sameas, ty);
            }

            if let Some(pat) = xs {
                let ty = self.uarena.pats[pat].ty;
                self.tctx.env.assign(ty, list_ty);
            }

            elem_ty
        };

        match list_length {
            par::ListLength::None => {
                let ty = self.tctx.define(span, |env| env.list(elem_ty));
                self.add_typed_pat(Pattern::List(elems, None), ty, span)
            }
            par::ListLength::Exact(len) => {
                let len = self
                    .tctx
                    .define(span, |env| env.const_int(len.value as i128));
                let ty = self.tctx.define(span, |env| env.array(len, elem_ty));
                self.add_typed_pat(Pattern::Array(elems), ty, span)
            }
            par::ListLength::Name(name) if **name == "_" => {
                let len = self
                    .tctx
                    .define(span, |env| env.const_int(given_len as i128));
                let ty = self.tctx.define(span, |env| env.array(len, elem_ty));
                self.add_typed_pat(Pattern::Array(elems), ty, span)
            }
            par::ListLength::Name(name) => {
                if let Some(generic) = ts::find_generic(&self.tctx.fenv, self.tctx.scope, **name) {
                    let len = self.tctx.define(span, |env| env.generic(generic));
                    let ty = self.tctx.define(span, |env| env.array(elem_ty, len));

                    self.add_typed_pat(
                        Pattern::GenericArray(elems, generic.tr(name.span)),
                        ty,
                        span,
                    )
                } else {
                    errors::err("invalid pattern")
                        .line(name.span, format!("no generic named `{name}`"))
                        .emit();
                    self.pat_poison(span)
                }
            }
        }
    }

    //     ```rs
    // match something
    // | "func" #identifier  -> 0
    // | "this" some_char "that" -> 1
    // //       ^^^^^^^^^ here its a char instead of a string
    // | "this" some_char -> 1
    // //       ^^^^^^^^^ here its a string instead of a char
    // | some_string -> 2
    // //^^^^^^^^^^^ but here its a string
    // | ..some_string -> 3
    // //  ^^^^^^^^^^^ we could do this... but that's still inconsistent
    // | "this" (some_char) that -> 4
    // //        ^^^^^^^^^ we could do this? that at least looks really good even though its a bit random
    // | "this" (some_char) (other_char) -> 4
    // //        ^^^^^^^^^ it also makes this possible which is really nice
    //
    // // But wouldn't this still be ambigious?
    // | (Nothing) -> 5
    // | (a, b) -> 6
    // | (some_char) -> 7
    //
    // // This could work instead?
    // | .some_char -> 8
    //
    // // this could work?
    // | "this" .some_char .other_char xs -> 9
    // ```

    fn string_pattern(
        &mut self,
        span: Span,
        fst: StringPattern<'s>,
        params: &[Tr<par::Pattern<'s>>],
    ) -> PatternId {
        let Some(ty) = self.string_type(span) else {
            return self.pat_poison(span);
        };

        let string_pats = std::iter::once(fst)
            .chain(
                params
                    .iter()
                    .map(|pat| self.string_pattern_kind(pat.as_ref())),
            )
            .collect();

        self.add_typed_pat(Pattern::String(string_pats), ty, span)
    }

    fn string_pattern_kind(&mut self, pat: Tr<&par::Pattern<'s>>) -> StringPattern<'s> {
        match &*pat {
            par::Pattern::CharWildcard(name, params) => {
                assert!(params.is_empty());
                let ty = self
                    .tctx
                    .define(pat.span, |env| env.int(IntSize::new(false, 8)));
                let bind = self.scopes.declare(name, ty);
                StringPattern::CharWildcard(bind)
            }
            par::Pattern::Name(name, params) => {
                assert!(params.is_empty());
                match name.as_name() {
                    None => StringPattern::Poison,
                    Some(name) => {
                        let Some(ty) = self.string_type(pat.span) else {
                            return StringPattern::Poison;
                        };

                        let bind = self.scopes.declare(name, ty);
                        StringPattern::Wildcard(bind)
                    }
                }
            }
            par::Pattern::String(lit, params) => {
                assert!(params.is_empty());
                StringPattern::Literal(lit)
            }
            par::Pattern::Char(lit, params) => {
                assert!(params.is_empty());
                StringPattern::CharLiteral(lit)
            }
            par::Pattern::Extractor(func, bind, params) => {
                assert!(params.is_empty());
                let call = self.expr((**func).as_ref());
                let ty = todo!();
                let bind = bind.map(|name| self.scopes.declare(*name, ty));
                StringPattern::Extractor(Extractor { call, bind })
            }
            _ => {
                panic!("ET: invalid string pattern");
            }
        }
    }

    fn operator_pattern(
        &mut self,
        span: Span,
        init: &Tr<par::Pattern<'s>>,
        ops: &[(&'s str, Tr<par::Pattern<'s>>)],
    ) -> PatternId {
        dbg!(&init, &ops);
        todo!("do we also want `@`? Or is that *just* for string patterns");
        // No we need `@` for `a @ <pat>` and we also are desugaring `a` to that
    }
}
