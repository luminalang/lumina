use super::{InferenceEnv, LoweringKind, Type, TypeKey, TypeLower};
use crate::errors;
use crate::project::symbols;
use crate::{Context, Map, TranslationUnit};
use crate::{
    hir::{LoweringArena, LoweringFunc},
    key,
    key::EntityList,
    project::{FuncDef, TypeDef},
};
use itertools::Itertools;
use lumina_parser as parser;
use lumina_typesystem::Prim::Self_;
use lumina_typesystem::{self as ts, GenericTag, TaggedGeneric, Var};
use lumina_util::{Span, Tr};
use tracing::info;

impl TypeLower<InferenceEnv> {
    // TODO: Rename to `annotate`?
    pub fn lowering_instantiation<'e, 's>(&mut self) -> ExplicitAnnotationLowering<'_, 'e, 's> {
        ExplicitAnnotationLowering {
            types: vec![],
            tagged_explicit: vec![],
            in_construction: ts::Annotation::new(),

            tlower: self,
        }
    }

    pub fn instantiation(
        &mut self,
        item_origin: symbols::Origin,
        anot: ts::Annotation<Var>,
    ) -> Instantiation<'_> {
        Instantiation {
            ctx: self.ctx.clone(),
            project: self.project,
            env: &mut self.env.env,
            anot,
            item_origin,
        }
    }
}

impl InferenceEnv {
    pub fn instantiate(&mut self) -> ImplicitAnnotationLowering<'_> {
        ImplicitAnnotationLowering::new(&mut self.env)
    }
}

/// When there isn't an explicit type annotation to lower and verify
///
/// (only borrows InferenceEnv)
pub struct ImplicitAnnotationLowering<'a> {
    env: &'a mut ts::Environment<TypeKey>,
    in_construction: ts::Annotation<Var>,
}

/// When there is an explicit type annotation to lower and verify
///
/// (borrows entire TypeLower)
pub struct ExplicitAnnotationLowering<'a, 'e, 's> {
    // Explicit unresolved type annotations
    types: Vec<(GenericTag, &'s str, Var)>,
    tagged_explicit: Vec<(GenericTag, &'e parser::ty::ForallAnnotation<'s>)>,

    in_construction: ts::Annotation<Var>,

    tlower: &'a mut TypeLower<InferenceEnv>, // env: &'e mut InferenceEnv,
}

type ExplicitUnres<'a, 's> = &'a [(usize, parser::ty::ForallAnnotation<'s>)];

impl<'a> ImplicitAnnotationLowering<'a> {
    pub fn new(env: &'a mut ts::Environment<TypeKey>) -> Self {
        Self { env, in_construction: ts::Annotation::new() }
    }

    pub fn finish(&mut self) -> ts::Annotation<Var> {
        std::mem::take(&mut self.in_construction)
    }

    /// Instantiate the generics and associated constraints of a Forall
    pub fn forall(&mut self, tag: GenericTag, forall: &ts::Forall<TypeKey>) -> &mut Self {
        self.keys(tag, forall.names.keys());

        for (generic, cons) in forall.constraints.iter() {
            let var = self
                .in_construction
                .get(TaggedGeneric::new(tag, generic), &self.env.var_pool)
                .unwrap();

            self.env.push_constraints(var, cons.clone());
        }

        self
    }

    /// Instantiate from a set of `key::Generic` without any constraints
    pub fn keys(&mut self, tag: GenericTag, keys: impl Iterator<Item = key::Generic>) -> &mut Self {
        for expected in keys {
            let key = self.in_construction.push_unknown(tag, &mut self.env);
            assert_eq!(key, expected);
        }
        self
    }

    pub fn unknown_self_if(&mut self, has_self: bool) -> &mut Self {
        if has_self {
            self.in_construction.self_ = Some(self.env.unknown());
        }
        self
    }

    pub fn self_recursion<'s>(&mut self, fenv: &ts::Forall<TypeKey>) -> &mut Self {
        todo!();
    }
}

impl<'a, 'e, 's> ExplicitAnnotationLowering<'a, 'e, 's> {
    pub fn finish(&mut self) -> ts::Annotation<Var> {
        std::mem::take(&mut self.in_construction)
    }

    pub fn self_recursion(&mut self, apath: Tr<&'a parser::AnnotatedPath<'s>>) -> &mut Self {
        let len = apath.path.as_slice().len();

        self.resolve(&apath.for_segments, &[(len - 1, GenericTag::Func)]);

        assert!(
            !self.tlower.fenv.contains_key(&GenericTag::Trait),
            "self-recursion on a trait method should not need edge-case"
        );

        if let Some(forall) = self.tlower.fenv.get(&GenericTag::Impl).cloned() {
            self.unify(apath.span, GenericTag::Impl, &forall.names);
            self.constraints(GenericTag::Impl, &forall.constraints);
        }

        let forall = self.tlower.fenv[&GenericTag::Func].clone();
        self.unify(apath.span, GenericTag::Func, &forall.names);
        self.constraints(GenericTag::Func, &forall.constraints);

        self
    }

    pub fn lambda(
        &mut self,
        span: Span,
        lambda: key::Lambda,
        explicit: Option<&parser::ty::ForallAnnotation<'s>>,
    ) -> &mut Self {
        let tag = GenericTag::Lambda(lambda);

        if let Some(anot) = explicit {
            for (name, ty) in &anot.assignments {
                let ty = self.tlower.ty(ty.as_ref());
                self.types.push((tag, name, ty));
            }
        }

        // Map the parent function generics to themselves.
        for generic in self.tlower.fenv[&GenericTag::Func].names.keys() {
            let tagged = TaggedGeneric::new(GenericTag::Func, generic);
            let ty = self.tlower.define(span, |env| env.generic(tagged));
            self.in_construction
                .push(GenericTag::Func, ty, &mut self.tlower.env.env.var_pool);
        }

        let lforall = self.tlower.fenv[&tag].clone();
        self.unify(span, tag, &lforall.names);
        self.constraints(tag, &lforall.constraints);

        self
    }

    pub fn builtin(
        &mut self,
        span: Span,
        path: &[&str],
        explicit: &'a [(usize, parser::ty::ForallAnnotation<'s>)],
    ) -> Option<(EntityList<Var>, Var)> {
        let size_t = self.tlower.default_int_size.unsigned();

        // `size_of` and `align_of` just explicit annotations
        self.resolve(explicit, &[(path.len(), GenericTag::Func)]);

        match path {
            ["deref"] => {
                let inner = self.tlower.define(span, |env| env.unknown());
                let ptr = self.tlower.define(span, |env| env.pointer(inner));
                let params = EntityList::from_slice(&[ptr], self.tlower.var_pool_mut());
                Some((params, inner))
            }
            ["offset"] => {
                let inner = self.tlower.define(span, |env| env.unknown());
                let ptr = self.tlower.define(span, |env| env.pointer(inner));
                let size_t = self.tlower.define(span, |env| env.int(size_t));
                let params = EntityList::from_slice(&[ptr, size_t], self.tlower.var_pool_mut());
                Some((params, ptr))
            }
            // ["offsetu"] => Some(2),
            ["alloca"] => {
                let inner = self.tlower.define(span, |env| env.unknown());
                let ptr = self.tlower.define(span, |env| env.pointer(inner));
                Some((EntityList::new(), ptr))
            }
            ["bool", "or"] | ["bool", "and"] => {
                let bool = self.tlower.define(span, |env| env.prim(ts::Prim::Bool));
                let params = EntityList::from_slice(&[bool, bool], self.tlower.var_pool_mut());
                Some((params, bool))
            }
            ["unreachable"] => {
                let any = self.tlower.define(span, |env| env.unknown());
                Some((EntityList::new(), any))
            }
            ["array_len"] => {
                let any = self.tlower.define(span, |env| env.unknown());
                let size_t = self.tlower.define(span, |env| env.int(size_t));
                let params = EntityList::from_slice(&[any], self.tlower.var_pool_mut());
                Some((params, size_t))
            }
            ["array_get"] => {
                let inner = self.tlower.define(span, |env| env.unknown());
                let any = self.tlower.define(span, |env| env.unknown());
                let size_t = self.tlower.define(span, |env| env.int(size_t));
                let params = EntityList::from_slice(&[size_t, any], self.tlower.var_pool_mut());
                Some((params, inner))
            }
            ["iabs"] => {
                let n = self.tlower.define(span, |env| env.numeric());
                let params = EntityList::from_slice(&[n], self.tlower.var_pool_mut());
                Some((params, n))
            }
            ["lt"] | ["gt"] | ["eq"] => {
                let any = self.tlower.define(span, |env| env.unknown());
                let params = EntityList::from_slice(&[any, any], self.tlower.var_pool_mut());
                let bool = self.tlower.define(span, |env| env.prim(ts::Prim::Bool));
                Some((params, bool))
            }
            ["div"] | ["mul"] | ["minus"] | ["plus"] => {
                let n = self.tlower.define(span, |env| env.numeric());
                let params = EntityList::from_slice(&[n, n], self.tlower.var_pool_mut());
                Some((params, n))
            }
            ["mul_checked"] | ["plus_checked"] | ["minus_checked"] => {
                let n = self.tlower.define(span, |env| env.numeric());
                let bool = self.tlower.define(span, |env| env.prim(ts::Prim::Bool));
                let params = EntityList::from_slice(&[n, n], self.tlower.var_pool_mut());
                let elems = EntityList::from_slice(&[n, bool], self.tlower.var_pool_mut());
                let ret = self.tlower.define(span, |env| env.tuple(elems));
                Some((params, ret))
            }
            ["write"] => {
                let inner = self.tlower.define(span, |env| env.unknown());
                let ptr = self.tlower.define(span, |env| env.pointer(inner));
                let params = EntityList::from_slice(&[ptr, inner], self.tlower.var_pool_mut());
                let unit = self.tlower.define(span, |env| env.tuple(EntityList::new()));
                Some((params, unit))
            }
            ["size_of"] => {
                let Some(_) = self.get(GenericTag::Func, "t") else {
                    errors::err("invalid type annotation")
                        .line(
                            span,
                            "`size_of` builtin requires `t` generic in explicit type annotation",
                        )
                        .emit();

                    let poison = self.tlower.define(span, |env| env.error());
                    return Some((EntityList::new(), poison));
                };

                let size_t = self.tlower.define(span, |env| env.int(size_t));

                Some((EntityList::new(), size_t))
            }
            ["align_of"] => {
                let Some(_) = self.get(GenericTag::Func, "t") else {
                    errors::err("invalid type annotation")
                        .line(
                            span,
                            "`align_of` builtin requires `t` generic in explicit type annotation",
                        )
                        .emit();

                    let poison = self.tlower.define(span, |env| env.error());
                    return Some((EntityList::new(), poison));
                };

                let size_t = self.tlower.define(span, |env| env.int(size_t));

                Some((EntityList::new(), size_t))
            }
            ["val_to_ref"] => {
                let any = self.tlower.define(span, |env| env.unknown());
                let ptr = self.tlower.define(span, |env| env.pointer(any));
                let params = EntityList::from_slice(&[any], self.tlower.var_pool_mut());
                Some((params, ptr))
            }
            _ => {
                errors::err("function not found")
                    .line(
                        span,
                        format!("no builtin function named {}", path.iter().format(":")),
                    )
                    .emit();

                None
            }
        }
    }

    pub fn variant(
        mut self,
        tkey: TypeKey,
        var: key::Variant,
        apath: Tr<&'a parser::AnnotatedPath<'s>>,
    ) -> Var {
        let len = apath.path.as_slice().len();

        if len > 1 {
            self.resolve(
                &apath.for_segments,
                &[(len - 1, GenericTag::Type), (len - 2, GenericTag::Func)],
            )
        } else {
            self.resolve(&apath.for_segments, &[(len - 1, GenericTag::Type)]);
        }

        self.tlower
            .ctx
            .clone()
            .in_origin(self.tlower.project, tkey.origin, |unit| {
                let tydef = &unit.header.type_signatures[tkey.key];

                self.unify(apath.span, GenericTag::Type, &tydef.generics);

                let (params, tparams) = self
                    .tlower
                    .instantiation(tkey.origin, self.in_construction)
                    .variant(tkey.key, var);

                let ret = self
                    .tlower
                    .define(apath.span, |env| env.defined(tkey, tparams));

                self.tlower.define(apath.span, |env| {
                    env.function(ts::CallableKind::FnPointer, params, ret)
                })
            })
    }

    pub fn operator(mut self, span: Span, origin: symbols::Origin, func: key::Func) -> Var {
        self.tlower
            .ctx
            .clone()
            .in_origin(self.tlower.project, origin, |unit| {
                let fdef = unit.header.function_signatures[func].as_ref().unwrap();

                if let Some((trait_, _)) = fdef.method_of {
                    let forall = &unit.header.type_signatures[trait_].generics;
                    self.unify(span, GenericTag::Trait, forall);
                }

                self.unify(span, GenericTag::Func, &fdef.sig.forall.names);

                let (params, ret) = self
                    .tlower
                    .instantiation(origin, self.in_construction)
                    .foreign_func(fdef);

                self.tlower.define(span, |env| {
                    env.function(ts::CallableKind::FnPointer, params, ret)
                })
            })
    }

    pub fn func_or_method(
        mut self,
        origin: symbols::Origin,
        func: key::Func,
        apath: Tr<&'a parser::AnnotatedPath<'s>>,
    ) -> Var {
        let len = apath.path.as_slice().len();

        if len > 1 {
            self.resolve(
                &apath.for_segments,
                &[(len - 1, GenericTag::Trait), (len - 2, GenericTag::Func)],
            )
        } else {
            self.resolve(&apath.for_segments, &[(len - 1, GenericTag::Func)]);
        }

        // From here on the process is the same as for operators
        self.operator(apath.span, origin, func)
    }

    pub fn val(
        self,
        origin: symbols::Origin,
        val: key::Val,
        apath: Tr<&'a parser::AnnotatedPath<'s>>,
    ) -> Var {
        if !apath.for_segments.is_empty() {
            errors::err("invalid type annotation")
                .line(
                    apath.span,
                    "global values can not have generics or be type annotated",
                )
                .emit();
        }

        self.tlower
            .ctx
            .clone()
            .in_origin(self.tlower.project, origin, |unit| {
                let func = &unit.header.values[val];
                let fdef = &unit.header.function_signatures[func.initialiser]
                    .as_ref()
                    .unwrap();

                self.tlower
                    .instantiation(origin, ts::Annotation::new())
                    .ty(&fdef.sig.ret)
            })
    }

    pub fn type_(&mut self, len: usize, explicit: ExplicitUnres<'a, 's>) {
        self.resolve(explicit, &[(len - 1, GenericTag::Type)])
    }

    fn resolve(
        &mut self,
        anots: &'a [(usize, parser::ty::ForallAnnotation<'s>)],
        tag_map: &[(usize, GenericTag)],
    ) {
        for (i, anot) in anots {
            match tag_map.iter().find(|(mapped, _)| i == mapped) {
                Some((_, tag)) => {
                    for (name, ty) in &anot.assignments {
                        let ty = self.tlower.ty(ty.as_ref());
                        self.types.push((*tag, name, ty));
                    }
                }
                None => {
                    let span = Span::from_elems(&anot.assignments, |(name, value)| {
                        name.span.extend(value.span)
                    });

                    errors::err("unexpected type annotation")
                        .line(span, "only items can be type annotated")
                        .emit();

                    for (_, ty) in &anot.assignments {
                        // Lower it anyway to create errors on invalid types but ignore the result
                        let ty = self.tlower.ty(ty.as_ref());
                        self.tlower.env.env.poison(ty);
                    }
                }
            }
        }
    }

    pub fn with_self(&mut self, var: Var) -> &mut Self {
        self.in_construction.self_ = Some(var);
        self
    }

    fn get(&mut self, tag: GenericTag, name: &str) -> Option<Var> {
        self.types
            .iter()
            .find_map(|(tag_, name_, var)| (tag == *tag_ && name == *name_).then_some(*var))
    }

    fn get_or_spawn(&mut self, span: Span, tag: GenericTag, name: &str) -> Var {
        self.get(tag, name)
            .unwrap_or_else(|| self.tlower.env.define(span, |env| env.unknown()))
    }

    /// Unify the lowered form of explicitly annotated types with the annotation to be returned by `finish`
    fn unify(
        &mut self,
        span: Span,
        tag: GenericTag,
        forall: &Map<key::Generic, String>,
    ) -> &mut Self {
        // TODO: We still aren't attaching constraints anywhere

        if tag == GenericTag::Trait {
            let self_ = self.get_or_spawn(span, tag, "self");
            self.in_construction.self_ = Some(self_);
        }

        for (generic, name) in forall {
            let var = self.get_or_spawn(span, tag, name);
            assert_eq!(
                generic,
                self.in_construction
                    .push(tag, var, self.tlower.var_pool_mut())
            );
        }

        for (tag_, anot) in &self.tagged_explicit {
            if *tag_ == tag {
                let unknowns = anot
                    .assignments
                    .iter()
                    .filter_map(|(name, _)| {
                        let isnt_valid_self = tag == GenericTag::Trait && **name != "self";
                        let isnt_valid_generic = forall.iter().all(|(_, n)| n != **name);
                        (isnt_valid_self && isnt_valid_generic).then_some(*name)
                    })
                    .collect::<Vec<_>>();

                if !unknowns.is_empty() {
                    let mut err = errors::err("invalid type annotation").line(
                        unknowns[0].span,
                        format!(
                            "generic(s) {} not found",
                            lumina_util::list_names_gramatically_correct(unknowns.iter())
                        ),
                    );
                    if !forall.is_empty() {
                        err = err.text(format!(
                            "these generics are defined for this item: {}",
                            lumina_util::list_names_gramatically_correct(forall.values())
                        ));
                    }
                    err.emit();
                }
            }
        }

        self
    }

    fn constraints(&mut self, tag: GenericTag, cons: &Map<key::Generic, ts::Constraints<TypeKey>>) {
        for (generic, cons) in cons.iter() {
            let var = self
                .in_construction
                .get(
                    TaggedGeneric::new(tag, generic),
                    &self.tlower.env.env.var_pool,
                )
                .unwrap();

            self.tlower.env.env.push_constraints(var, cons.clone());
        }
    }
}

impl InferenceEnv {
    pub fn instantiation(
        &mut self,
        ctx: Context<TranslationUnit>,
        project: key::Project,
        in_: symbols::Origin,
        anot: ts::Annotation<Var>,
    ) -> Instantiation<'_> {
        Instantiation { ctx, project, env: &mut self.env, anot, item_origin: in_ }
    }
}

pub struct Instantiation<'a> {
    ctx: Context<TranslationUnit>,
    project: key::Project,
    env: &'a mut ts::Environment<TypeKey>,
    anot: ts::Annotation<Var>,
    item_origin: symbols::Origin,
}

impl<'a> Instantiation<'a> {
    pub fn new(
        ctx: Context<TranslationUnit>,
        project: key::Project,
        item_origin: symbols::Origin,
        env: &'a mut ts::Environment<TypeKey>,
        anot: ts::Annotation<Var>,
    ) -> Self {
        Self { ctx, project, env, anot, item_origin }
    }

    pub fn ty<T: ts::InstableType<TypeKey>>(&mut self, ty: T) -> Var {
        self.env.instantiate(&self.anot, ty, &mut |ident| {
            self.ctx
                .inst_type_key(self.project, self.item_origin, *ident)
        })
    }

    pub fn tys<T: ts::InstableType<TypeKey>>(
        &mut self,
        tys: impl IntoIterator<Item = T>,
    ) -> EntityList<Var> {
        self.env.instantiate_types(&self.anot, tys, &mut |ident| {
            self.ctx
                .inst_type_key(self.project, self.item_origin, *ident)
        })
    }

    pub fn entities(&mut self, types: EntityList<Var>) -> EntityList<Var> {
        self.env
            .instantiate_entities(&self.anot, types, &mut |ident| {
                self.ctx
                    .inst_type_key(self.project, self.item_origin, *ident)
            })
            .0
    }

    pub fn foreign_func(&mut self, fdef: &FuncDef) -> (EntityList<Var>, Var) {
        info!(
            "Instantiating function {}:{}",
            self.ctx.name_of_origin(self.project, self.item_origin),
            fdef.name
        );

        let params = self.tys(&fdef.sig.params);
        let ret = self.ty(&fdef.sig.ret);

        (params, ret)
    }

    pub fn variant(
        &mut self,
        key: key::Type,
        variant: key::Variant,
    ) -> (EntityList<Var>, EntityList<Var>) {
        let oname = self.ctx.name_of_origin(self.project, self.item_origin);

        self.ctx
            .clone()
            .in_origin(self.project, self.item_origin, |unit| {
                let tname = &unit.header.type_signatures[key].name;
                let (vtypes, vname) = &unit.header.variant(key, variant);
                info!("Instantiating variant type {oname}:{tname}:{vname}");

                let params = self.tys(*vtypes);
                (params, self.anot.item.clone())
            })
    }

    pub fn self_recursion<'s>(
        &mut self,
        uarena: &LoweringArena<'s>,
        func: key::Func,
    ) -> (EntityList<Var>, Var) {
        match &uarena.funcs[func] {
            LoweringFunc::Lowering { params, ret, .. } => {
                let params = self.entities(params.clone());
                let ret = self.ty(ret);
                (params, ret)
            }
            _ => panic!("attempted self-recursion on non-lowering function"),
        }
    }
}
