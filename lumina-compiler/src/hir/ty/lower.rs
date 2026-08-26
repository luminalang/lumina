use super::{
    Context, InferenceEnv, InterResolved, StaticEnv, TranslationUnit, Type, TypeKey, errors,
    symbols, symbols::Namespace,
};
use crate::key;
use crate::project::TypeSig;
use key::{EntityList, Map};
use lumina_parser as parser;
use lumina_typesystem::{
    self as ts, ArrayLen, ForallEnv, GenericTag, IntSize, Prim, TaggedGeneric, Var,
};
use lumina_util::{Identifier, Span, Spanned as _, Tr};
use std::collections::HashMap;
use std::fmt;
use tracing::trace;

pub struct TypeLower<Env> {
    pub fenv: ForallEnv<TypeKey>,
    pub scope: GenericTag,
    pub(crate) declare_generics: bool,
    pub default_int_size: IntSize,
    pub project: key::Project,
    pub file: key::File,
    pub env: Env,
    pub generic_spans: HashMap<TaggedGeneric, Span>,
    pub ctx: Context<TranslationUnit>,
}

impl TypeLower<InferenceEnv> {
    pub fn var_pool_mut(&mut self) -> &mut key::ListPool<Var> {
        &mut self.env.env.var_pool
    }

    pub fn var_pool(&mut self) -> &mut key::ListPool<Var> {
        &mut self.env.env.var_pool
    }

    pub fn define<F>(&mut self, span: Span, f: F) -> Var
    where
        F: FnOnce(&mut ts::Environment<TypeKey>) -> Var,
    {
        self.env.define(span, f)
    }

    pub fn as_static<T, F>(&mut self, f: F, invalid_inference_reason_given: &'static str) -> T
    where
        F: FnOnce(&mut TypeLower<StaticEnv>) -> T,
    {
        let mut v = None;
        take_mut::take(self, |this| {
            let old = this.env;

            let mut new = TypeLower {
                project: this.project,
                file: this.file,
                ctx: this.ctx.clone(),
                fenv: this.fenv,
                scope: this.scope,
                declare_generics: this.declare_generics,
                default_int_size: this.default_int_size,
                generic_spans: this.generic_spans,
                env: StaticEnv { self_: None, invalid_inference_reason_given },
            };

            v = Some(f(&mut new));

            new.cast(old).1
        });

        v.unwrap()
    }

    pub fn set_closure_to_fnptr(&mut self, var: Var) {
        self.env.env.set_closure_to_fnptr(var);
    }

    pub fn reverse_forall_into_type_environment(&mut self) -> ts::Annotation<Var> {
        let mut annotation = ts::Annotation::new();
        for (tag, forall) in self.fenv.iter() {
            for generic in forall.names.keys() {
                let generic = TaggedGeneric::new(*tag, generic);
                let span = self.generic_spans[&generic];
                let ty = self.env.define(span, |env| env.generic(generic));
                assert_eq!(
                    generic.key,
                    annotation.push(*tag, ty, &mut self.env.env.var_pool)
                );
            }
        }
        annotation
    }
}

impl TypeLower<StaticEnv> {
    // Reason we don't do something similar to function forall is because those are also *lowered*.
    // Not just cloned from the trait definition.
    pub fn as_member_of_trait(mut self, sig: &TypeSig) -> Self {
        let scope = std::mem::replace(&mut self.scope, GenericTag::Trait);

        let tforall = self.fenv.entry(GenericTag::Trait).or_default();

        for (key, name) in &sig.generics {
            assert_eq!(key, tforall.names.push(name.clone()));
            assert_eq!(key, tforall.constraints.push(vec![]));
        }

        self.scope = scope;
        self
    }

    /// NOTE: Assumes `self.scope` is set
    pub fn lower_and_include_when_constraints<'s>(&mut self, when: &parser::when::Constraints<'s>) {
        self.fenv.entry(self.scope).or_default();

        // Declare all generic types, we do this on all generics before we lower any constraints such
        // that the generics are included in each when bindings scope.
        for (span, name, _) in when.generics.iter() {
            match ts::find_generic(&self.fenv, self.scope, *name) {
                Some(generic) if generic.tag == self.scope => {} // already exists in this item
                Some(generic) => {
                    errors::err("invalid when binding")
                        .line(
                            *span,
                            format!(
                                "`{name}` is already declared in the enclosing {}",
                                match generic.tag {
                                    GenericTag::Trait => "trait",
                                    GenericTag::Func => "function",
                                    GenericTag::Impl => "implementation",
                                    _ => unreachable!(),
                                }
                            ),
                        )
                        .emit();

                    continue;
                }
                None => {
                    let generic = ts::declare(&mut self.fenv, self.scope, *name);
                    self.generic_spans.insert(generic, *span);
                }
            }
        }

        for (span, name, types) in when.generics.iter() {
            let generic = ts::find_generic(&self.fenv, self.scope, *name).unwrap();
            if generic.tag != self.scope {
                continue; // already given an error in the other for loop
            }

            for ty in types {
                match self.ty(ty.as_ref()) {
                    Type::Defined(ident, type_params) => {
                        let con = (ident, type_params);
                        ts::add_constraint(&mut self.fenv, generic, con);
                    }
                    _ => {
                        errors::err("invalid when binding")
                            .line(*span, "type must be a trait")
                            .emit();
                    }
                }
            }
        }
    }

    pub fn lower_and_include_lambda_when_constraints<'a, 's: 'a>(
        &mut self,
        lambdas: Option<impl Iterator<Item = &'a parser::when::Constraints<'s>> + Clone>,
    ) {
        if let Some(lambdas) = lambdas {
            for (i, when) in lambdas.enumerate() {
                let lambda = key::Lambda(i as u32);
                let tag = GenericTag::Lambda(lambda);

                self.fenv.entry(tag).or_default();

                let scope = std::mem::replace(&mut self.scope, tag);
                self.lower_and_include_when_constraints(when);
                self.scope = scope;
            }
        }
    }
}

impl<Env> TypeLower<Env> {
    pub fn new(
        ctx: Context<TranslationUnit>,
        project: key::Project,
        file: key::File,
        env: Env,
        tag: GenericTag,
    ) -> Self {
        Self {
            project,
            file,
            ctx,
            fenv: ForallEnv::new(),
            scope: tag,
            declare_generics: false,
            default_int_size: IntSize::new(true, 64),
            generic_spans: HashMap::new(),
            env,
        }
    }

    pub fn cast<NEnv>(self, env: NEnv) -> (Env, TypeLower<NEnv>) {
        (
            self.env,
            TypeLower {
                ctx: self.ctx.clone(),
                file: self.file,
                project: self.project,
                fenv: self.fenv,
                scope: self.scope,
                declare_generics: self.declare_generics,
                default_int_size: self.default_int_size,
                generic_spans: self.generic_spans,
                env,
            },
        )
    }

    pub fn take_forall(&mut self, tag: GenericTag) -> ts::Forall<TypeKey> {
        let forall = self.fenv.get_mut(&tag).unwrap();
        std::mem::take(forall)
    }

    pub fn lambda(&self) -> Option<key::Lambda> {
        match self.scope {
            GenericTag::Lambda(lambda) => Some(lambda),
            GenericTag::Trait | GenericTag::Impl | GenericTag::Type | GenericTag::Func => None,
        }
    }

    pub fn declare_forall<'s, I>(&mut self, tag: GenericTag, cons: I)
    where
        I: Iterator<Item = Tr<&'s str>>,
    {
        let previous = std::mem::replace(&mut self.scope, tag);
        for name in cons {
            let generic = ts::declare(&mut self.fenv, self.scope, *name);
            self.generic_spans.insert(generic, name.span);
        }
        self.scope = previous;
    }

    fn try_prim(&mut self, name: &str) -> Option<Prim> {
        match name {
            "bool" => Some(Prim::Bool),
            "int" => Some(Prim::Int(self.default_int_size)),
            "uint" => Some(Prim::Int(self.default_int_size.unsigned())),
            "float" => Some(Prim::Float),
            name if name.starts_with('u') => name[1..]
                .parse::<u8>()
                .ok()
                .map(|n| Prim::Int(IntSize::new(false, n))),
            name if name.starts_with('i') => name[1..]
                .parse::<u8>()
                .ok()
                .map(|n| Prim::Int(IntSize::new(true, n))),
            _ => None,
        }
    }
}

enum NamedType {
    Self_,
    Underscore,
    Prim(Prim),
    Generic(TaggedGeneric),
    Defined(TypeKey),
    Poison,
}

impl<'s, Env> TypeLower<Env>
where
    Self: ResolvingTypeContext<'s>,
{
    fn lower_defined(
        &mut self,
        ty: Tr<&parser::Type<'s>>,
        ident: &Identifier<'s>,
        params: &[Tr<parser::Type<'s>>],
    ) -> <Self as LoweringKind<'s>>::Ty {
        let (resolved, expected_plen) = self.defined(ty.span, ident);
        let (plen, params) = self.tys(params);

        let var = self.type_from_named(ty.span, resolved, params);

        if expected_plen.is_some_and(|exp| plen != exp) {
            check_param_count(*ty, plen.tr(ty.span), expected_plen.unwrap());
            return self.poison(ty.span);
        }

        var
    }

    pub fn typing(
        &mut self,
        typing: &parser::func::Typing<'s>,
        spans: &mut Vec<Span>,
    ) -> (
        <Self as LoweringKind<'s>>::Tys,
        <Self as LoweringKind<'s>>::Ty,
    ) {
        // HACK: Our syntax for types is partially ambigious. `List a` could either be `[List, a]` or `[List a]`
        //
        // So in those cases where the former is allowed, we wait for name resolution to decide what to
        // do with the next types.
        if let Some((ptype, mut remaining)) = typing.ptypes.split_first() {
            if let parser::Type::Defined(ident, params) = &ptype.value {
                let (named, expected_plen) = self.defined(ptype.span, &ident.path);

                let first = match named {
                    NamedType::Defined(_) if expected_plen != Some(0) && params.is_empty() => {
                        let (plen, params) = self.tys(remaining);
                        check_param_count(&**ptype, plen.tr(ptype.span), expected_plen.unwrap());
                        remaining = &[];
                        let ty = self.type_from_named(ptype.span, named, params);
                        self.handle_span(&ty, ptype.span);
                        ty
                    }
                    _ => {
                        let (plen, params) = self.tys(params);
                        check_param_count(&**ptype, plen.tr(ptype.span), expected_plen.unwrap());
                        let ty = self.type_from_named(ptype.span, named, params);
                        self.handle_span(&ty, ptype.span);
                        ty
                    }
                };

                let mut params = self.alloc(1 + remaining.len());
                spans.push(ptype.span);
                self.push(first, &mut params);

                for t in remaining {
                    spans.push(t.span);
                    let ty = self.ty(t.as_ref());
                    self.handle_span(&ty, t.span);
                    self.push(ty, &mut params);
                }

                spans.push(typing.returns.span);
                let ret = self.ty(typing.returns.as_ref());
                self.handle_span(&ret, typing.returns.span);

                return (params, ret);
            }
        }

        let mut params = self.alloc(typing.ptypes.len());
        for t in &typing.ptypes {
            spans.push(t.span);
            let ty = self.ty(t.as_ref());
            self.handle_span(&ty, t.span);
            self.push(ty, &mut params);
        }

        spans.push(typing.returns.span);
        let ret = self.ty(typing.returns.as_ref());
        self.handle_span(&ret, typing.returns.span);

        (params, ret)
    }

    pub fn mandatory_typing(
        &mut self,
        header: &parser::func::Header<'s>,
        err_reason: &'static str,
    ) -> Option<(
        <Self as LoweringKind<'s>>::Tys,
        <Self as LoweringKind<'s>>::Ty,
        Vec<Span>,
    )> {
        match header.typing.as_ref() {
            None => {
                errors::err("inference error")
                    .line(header.name.span, err_reason)
                    .emit();

                None
            }
            Some(typing) => {
                let mut spans = vec![];
                let typing = self.typing(typing, &mut spans);
                Some((typing.0, typing.1, spans))
            }
        }
    }

    fn defined(&mut self, span: Span, ident: &Identifier<'s>) -> (NamedType, Option<usize>) {
        let segments = ident.as_slice();

        if let [name] = segments {
            if let Some(prim) = self.try_prim(name) {
                return (NamedType::Prim(prim), Some(0));
            }

            if let Some(generic) = ts::find_generic(&self.fenv, self.scope, name) {
                // TODO: HKT
                return (NamedType::Generic(generic), Some(0));
            }

            if *name == "self" {
                // TODO: HKT
                return (NamedType::Self_, Some(0));
            }

            if *name == "_" {
                return (NamedType::Underscore, Some(0));
            }
        }

        let ns = Namespace::Types;
        match self
            .ctx
            .find(self.project, None, self.file, ns, segments, true)
        {
            InterResolved::Item(origin, symbols::Item::Type((key, plen))) => {
                (NamedType::Defined(TypeKey { key, origin }), Some(plen))
            }

            InterResolved::NotFound { .. } => {
                if segments.len() == 1 && segments[0].len() == 1 {
                    if let Some(generic) = self.declare_generic(span, segments[0]) {
                        // TODO: HKT
                        return (NamedType::Generic(generic), Some(0));
                    }
                }

                errors::err("invalid type")
                    .line(span, format!("type `{ident}` not found"))
                    .emit();

                (NamedType::Poison, None)
            }
            InterResolved::Poison => (NamedType::Poison, None),
            _ => {
                errors::err("invalid type")
                    .line(span, format!("`type {ident}` not found"))
                    .text("NOTE: There is another non-type item with that name")
                    .emit();

                (NamedType::Poison, None)
            }
        }
    }
}

trait ResolvingTypeContext<'s>: LoweringKind<'s> {
    fn type_from_named(&mut self, span: Span, named: NamedType, params: Self::Tys) -> Self::Ty;
    fn poison(&mut self, span: Span) -> Self::Ty;
}

impl<'s> ResolvingTypeContext<'s> for TypeLower<InferenceEnv> {
    fn type_from_named(&mut self, span: Span, named: NamedType, params: Self::Tys) -> Self::Ty {
        match named {
            NamedType::Defined(tkey) => self.env.define(span, |env| env.defined(tkey, params)),
            NamedType::Self_ => match self.env.self_ {
                Some(var) => var,
                None => self.env.define(span, |env| env.prim(Prim::Self_)),
            },
            NamedType::Underscore => self.env.define(span, |env| env.unknown()),
            NamedType::Prim(prim) => self.env.define(span, |env| env.prim(prim)),
            NamedType::Generic(generic) => self.env.define(span, |env| env.generic(generic)),
            NamedType::Poison => self.env.define(span, |env| env.error()),
        }
    }

    fn poison(&mut self, span: Span) -> Self::Ty {
        self.env.define(span, |env| env.error())
    }
}

impl<'s> ResolvingTypeContext<'s> for TypeLower<StaticEnv> {
    fn type_from_named(&mut self, span: Span, named: NamedType, params: Self::Tys) -> Self::Ty {
        match named {
            NamedType::Self_ => self
                .env
                .self_
                .clone()
                .unwrap_or_else(|| Type::Prim(Prim::Self_)),
            NamedType::Underscore => {
                errors::err("invalid type")
                    .line(
                        span,
                        format!(
                            "inference is not allowed in {}",
                            self.env.invalid_inference_reason_given
                        ),
                    )
                    .emit();

                Type::Error
            }
            NamedType::Prim(prim) => Type::Prim(prim),
            NamedType::Generic(generic) => Type::Generic(generic),
            NamedType::Defined(tkey) => Type::Defined(tkey, params.into()),
            NamedType::Poison => Type::Error,
        }
    }

    fn poison(&mut self, _: Span) -> Self::Ty {
        Type::Error
    }
}

impl<'s, Env> TypeLower<Env>
where
    Self: LoweringKind<'s>,
{
    fn listlike<T>(
        &mut self,
        span: Span,
        inner: &[Tr<parser::Type<'s>>],
        len: parser::ListLength<'s>,
        array: impl FnOnce(&mut Self, [<Self as LoweringKind<'s>>::Ty; 2]) -> T,
        dynamic: impl FnOnce(&mut Self, Option<TypeKey>, <Self as LoweringKind<'s>>::Tys) -> T,
    ) -> Option<T> {
        if inner.len() != 1 {
            errors::err("invalid type").line(span, "").emit();
            None
        } else {
            match len {
                parser::ListLength::Name(name) => {
                    let inner = self.ty(inner[0].as_ref());

                    let Some(generic) = ts::find_generic(&self.fenv, self.scope, *name)
                        .or_else(|| self.declare_generic(name.span, *name))
                    else {
                        errors::err("invalid type")
                            .line(name.span, format!("no generic named `{name}`"))
                            .emit();

                        return None;
                    };

                    let len = self.array_len(name.span, ArrayLen::Generic(generic));
                    let ty = array(self, [inner, len]);
                    Some(ty)
                }
                parser::ListLength::Exact(len) => {
                    let inner = self.ty(inner[0].as_ref());
                    let len = self.array_len(len.span, ArrayLen::Int(*len as i64));
                    let ty = array(self, [inner, len]);
                    Some(ty)
                }
                parser::ListLength::None => {
                    let key = self.ctx.default_list_type(span, self.project);
                    let (_, params) = self.tys(inner);
                    let ty = dynamic(self, key, params);
                    Some(ty)
                }
            }
        }
    }
}

/// Description of how to lower a kind of type.
///
/// Enables shared code between constructing static and inferred types.
pub trait LoweringKind<'s> {
    type Ty;
    type Tys: Default;

    fn array_len(&mut self, span: Span, len: ArrayLen) -> Self::Ty;
    fn ty(&mut self, ty: Tr<&parser::Type<'s>>) -> Self::Ty;
    fn handle_span(&mut self, ty: &Self::Ty, span: Span);

    fn declare_generic(&mut self, span: Span, name: &'s str) -> Option<TaggedGeneric>;

    fn alloc(&mut self, size_hint: usize) -> Self::Tys;
    fn push(&mut self, ty: Self::Ty, buf: &mut Self::Tys);

    fn tys(&mut self, tys: &[Tr<parser::Type<'s>>]) -> (usize, Self::Tys) {
        let mut buf = self.alloc(tys.len());
        for t in tys {
            let ty = self.ty(t.as_ref());
            self.handle_span(&ty, t.span);
            self.push(ty, &mut buf);
        }
        (tys.len(), buf)
    }
}

impl<'s> LoweringKind<'s> for TypeLower<InferenceEnv> {
    type Ty = Var;
    type Tys = EntityList<Var>;

    fn declare_generic(&mut self, span: Span, name: &'s str) -> Option<TaggedGeneric> {
        self.env.env.in_signature().then(|| {
            let generic = ts::declare(&mut self.fenv, self.scope, name);
            self.generic_spans.insert(generic, span);
            generic
        })
    }

    fn array_len(&mut self, span: Span, len: ArrayLen) -> Self::Ty {
        match len {
            ArrayLen::Int(n) => self.env.define(span, |env| env.const_int(n as i128)),
            ArrayLen::Generic(generic) => self.env.define(span, |env| env.generic(generic)),
        }
    }

    fn ty(&mut self, ty: Tr<&parser::Type<'s>>) -> Self::Ty {
        trace!("lowering type {ty}");

        let var = match *ty {
            parser::Type::Defined(ident, params) => self.lower_defined(ty, &ident.path, params),
            parser::Type::Pointer(to) => {
                let to = self.ty((**to).as_ref());
                self.env.define(ty.span, |env| env.pointer(to))
            }
            parser::Type::FnPointer(ptypes, returns) => {
                let (_, params) = self.tys(ptypes);
                let ret = self.ty((**returns).as_ref());
                self.env.define(ty.span, |env| {
                    env.function(ts::CallableKind::FnPointer, params, ret)
                })
            }
            parser::Type::Closure(ptypes, returns) => {
                let (_, params) = self.tys(ptypes);
                let ret = self.ty((**returns).as_ref());
                self.env.define(ty.span, |env| {
                    env.function(ts::CallableKind::Closure, params, ret)
                })
            }
            parser::Type::Tuple(elems) => {
                let (_, elems) = self.tys(elems);
                self.env.define(ty.span, |env| env.tuple(elems))
            }
            parser::Type::Poison => self.env.define(ty.span, |env| env.error()),
            parser::Type::List(elems, len) => self
                .listlike(
                    ty.span,
                    elems,
                    len.clone(),
                    |this, [of, len]| this.env.define(ty.span, |env| env.array(of, len)),
                    |this, default_list, params| {
                        default_list
                            .map(|tkey| this.env.define(ty.span, |env| env.defined(tkey, params)))
                            .unwrap_or_else(|| this.env.define(ty.span, |env| env.error()))
                    },
                )
                .unwrap_or_else(|| self.env.define(ty.span, |env| env.error())),
        };

        var
    }

    fn handle_span(&mut self, ty: &Self::Ty, span: Span) {
        self.env.spans[*ty] = span;
    }

    fn alloc(&mut self, _size_hint: usize) -> Self::Tys {
        key::EntityList::new()
    }

    fn push(&mut self, ty: Self::Ty, buf: &mut Self::Tys) {
        buf.push(ty, self.var_pool());
    }
}

impl<'s> LoweringKind<'s> for TypeLower<StaticEnv> {
    type Ty = Type;
    type Tys = Vec<Type>;

    fn declare_generic(&mut self, span: Span, name: &'s str) -> Option<TaggedGeneric> {
        if self.scope == GenericTag::Type {
            return None;
        }

        let generic = ts::declare(&mut self.fenv, self.scope, name);
        self.generic_spans.insert(generic, span);

        Some(generic)
    }

    fn array_len(&mut self, _: Span, len: ArrayLen) -> Self::Ty {
        match len {
            ArrayLen::Int(n) => Type::Const(ts::ConstType::Int(n as i128)),
            ArrayLen::Generic(generic) => Type::Generic(generic),
        }
    }

    fn ty(&mut self, ty: Tr<&parser::Type<'s>>) -> Self::Ty {
        trace!("lowering type {ty}");

        match *ty {
            parser::Type::Defined(ident, params) => self.lower_defined(ty, &ident.path, params),
            parser::Type::Pointer(to) => {
                let to = self.ty((**to).as_ref());
                Type::Pointer(Box::new(to))
            }
            parser::Type::FnPointer(ptypes, returns) => {
                let (_, params) = self.tys(ptypes);
                let ret = self.ty((**returns).as_ref());
                Type::Function {
                    kind: ts::CallableKind::FnPointer,
                    params,
                    ret: Box::new(ret),
                }
            }
            parser::Type::Closure(ptypes, returns) => {
                let (_, params) = self.tys(ptypes);
                let ret = self.ty((**returns).as_ref());
                Type::Function { kind: ts::CallableKind::Closure, params, ret: Box::new(ret) }
            }
            parser::Type::Tuple(elems) => {
                let (_, elems) = self.tys(elems);
                Type::Tuple(elems)
            }
            parser::Type::Poison => Type::default_unit_type(),
            parser::Type::List(elems, len) => self
                .listlike(
                    ty.span,
                    elems,
                    len.clone(),
                    |_, [of, len]| Type::Array { of: Box::new(of), len: Box::new(len) },
                    |_, default_list, params| {
                        default_list
                            .map(|tkey| Type::Defined(tkey, params.into()))
                            .unwrap_or(Type::Error)
                    },
                )
                .unwrap_or_else(|| Type::default_unit_type()),
        }
    }

    fn handle_span(&mut self, _: &Self::Ty, _: Span) {}

    fn alloc(&mut self, size_hint: usize) -> Self::Tys {
        Vec::with_capacity(size_hint)
    }

    fn push(&mut self, ty: Self::Ty, buf: &mut Self::Tys) {
        buf.push(ty);
    }
}

fn check_param_count(header: impl fmt::Display, got: Tr<usize>, exp: usize) -> bool {
    let err = |span, msg| {
        errors::err("invalid type").line(span, msg).emit();
        false
    };

    if *got < exp {
        err(
            got.span,
            format!("missing {} type parameter(s) for `{header}`", exp - *got),
        )
    } else if *got > *got {
        err(got.span, format!("excess parameter for `{header}`"))
    } else {
        true
    }
}
