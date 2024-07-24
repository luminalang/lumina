//! Desugar let-binds into match
//! Desugar do..then into let-bind
//! Desugar if into match
//! Desugar record modification
//! Resolve operator precedence (including pipes)
//! Fold `Seal` into nested expressions
//! Seperate out lambdas into an adjecent buffer
//! Turn where-bindings into lambdas
//! Resolve identifiers (except for field names and dot-calls)
//! Desugar expression passing into lambdas
//! Store lists as cons lists in patterns
//! Desugar pattern parameters into match
//! Prepare type inference by spawning type variables
//! Substitute generic names for generic keys, thus resolving which `Forall` they belong to

use crate::prelude::*;
use crate::ProjectInfo;
use ast::{Mod, AST};
use derive_new::new;
use lumina_parser as parser;
use lumina_typesystem::{
    Constraint, Forall, GenericData, GenericKind, IType, ImplIndex, Prim, TEnv, Type,
};
use lumina_util::Highlighting;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use tracing::info_span;

mod expr;
use expr::ExprLower;
pub use expr::{Callable, Expr, Literal};
mod pat;
pub use pat::Pattern;
mod scope;
mod ty;
pub use ty::TypeAnnotation;
use ty::{SelfHandler, Ty, TypeEnvInfo};

pub struct HIR<'s> {
    pub funcs: ModMap<key::Func, FuncDefKind<'s>>,

    pub records: ModMap<key::Record, (Tr<&'s str>, Forall<'s, Type>)>,
    pub field_types: ModMap<key::Record, Map<key::RecordField, Tr<Type>>>,

    pub sums: ModMap<key::Sum, (Tr<&'s str>, Forall<'s, Type>)>,
    pub variant_types: ModMap<key::Sum, Map<key::SumVariant, Vec<Tr<Type>>>>,

    pub traits: ModMap<key::Trait, (Tr<&'s str>, Forall<'s, Type>)>,
    pub assoc: ModMap<key::Trait, Map<key::AssociatedType, Option<Tr<Type>>>>,

    pub impls: ModMap<key::Impl, Forall<'s, Type>>,
    pub impltors: ModMap<key::Impl, Tr<Type>>,
    pub iassoc: ModMap<key::Impl, Map<key::AssociatedType, Tr<Type>>>,
    pub itraits: ModMap<key::Impl, (M<key::Trait>, Vec<Type>)>,

    pub langitems: Map<key::Module, LangItems<'s>>,

    // Certain parts of the AST will be kept for the next pass
    pub fnames: ModMap<key::Record, Map<key::RecordField, Tr<&'s str>>>,
    pub vnames: ModMap<key::Sum, Map<key::SumVariant, Tr<&'s str>>>,
    pub func_names: ModMap<key::Func, Tr<&'s str>>,
    pub val_initializers: ModMap<key::Val, M<key::Func>>,
    pub sources: ast::Sources,
    pub lookups: ast::Lookups<'s>, // Still needed because of type-dependent lookups
    pub methods: ModMap<key::Trait, Map<key::Method, key::Func>>,
    pub imethods: ModMap<key::Impl, Map<key::Method, key::Func>>,
    pub assoc_names: ModMap<key::Trait, Map<key::AssociatedType, Tr<&'s str>>>,
    pub iassoc_names: ModMap<key::Impl, Map<key::AssociatedType, Tr<&'s str>>>,
}

type LangItems<'s> = HashMap<&'s str, M<key::TypeKind>>;

pub fn run<'a, 's>(
    info: ProjectInfo,
    ast: AST<'s>,
) -> (HIR<'s>, ModMap<key::Func, TEnv<'s>>, ImplIndex) {
    let mut tenvs = ast.entities.fheaders.secondary();

    let mut funcs = ast.entities.fheaders.secondary_inner_capacity();
    let mut records = ast.entities.records.secondary_inner_capacity();
    let mut field_types = ast.entities.field_types.secondary_inner_capacity();
    let mut variant_types = ast.entities.variant_types.secondary_inner_capacity();
    let mut sums = ast.entities.sums.secondary_inner_capacity();
    let mut traits = ast.entities.traits.secondary_inner_capacity();
    let mut assoc = ast.entities.associated_types.secondary_inner_capacity();
    let mut impls = ast.entities.impls.secondary_inner_capacity();
    let mut impltors = ast.entities.impls.secondary_inner_capacity();
    let mut iassoc = ast.entities.associated_types.secondary_inner_capacity();
    let mut itraits = ast.entities.impls.secondary_inner_capacity();
    let mut langitems = Map::with_capacity(ast.entities.langitems.len());

    // Lower type declarations in all modules
    for module in ast.sources.modules() {
        let litems = lower_langitems(&ast, module, ast.entities.get_langitems(module));

        ast.entities.sums.iter_module(module).for_each(|sum| {
            let (variants, forall) = lower_sum(&ast, &litems, sum, &info);
            let header = &ast.entities.sums[sum].header;
            sums.push(module, (header.name.tr(header.span), forall));
            variant_types.push_as(sum, variants);
        });

        ast.entities.records.iter_module(module).for_each(|record| {
            let (fields, forall) = lower_record(&ast, &litems, record, &info);
            let header = &ast.entities.records[record].header;
            records.push(module, (header.name.tr(header.span), forall));
            field_types.push_as(record, fields);
        });

        ast.entities.traits.iter_module(module).for_each(|trait_| {
            let (tassoc, forall) = lower_trait(&ast, &litems, trait_, &info);
            let header = &ast.entities.traits[trait_].header;
            traits.push_as(trait_, (header.name.tr(header.span), forall));
            assoc.push_as(trait_, tassoc);
        });

        langitems.push_as(module, litems);
    }

    // Lowering an impl requires that the trait it implements is already lowered.
    for module in ast.sources.modules() {
        for impl_ in ast.entities.impls.iter_module(module) {
            let langitems = &langitems[module];
            let parts = lower_impl(&ast, langitems, impl_, &info);
            impls.push_as(impl_, parts.2);
            impltors.push_as(impl_, parts.3);
            iassoc.push_as(impl_, parts.4);
            itraits.push_as(impl_, (parts.0, parts.1));
        }
    }

    // Lowering functions includes methods, which requires that the impl header is already lowered.
    for module in ast.sources.modules() {
        let langitems = &langitems[module];
        ast.entities.fheaders.iter_module(module).for_each(|func| {
            let (fdef, tenv) = lower_func(&ast, &traits, &impls, &impltors, &langitems, func, info);
            funcs.push_as(func, fdef);
            tenvs.push_as(func, tenv);
        });
    }

    // Build an index of the implementations
    let mut iquery = ImplIndex::new();
    for module in ast.sources.modules() {
        for ikey in impls.iter_module(module) {
            let impltor = &impltors[ikey];
            let trait_ = itraits[ikey].0;
            iquery.insert(trait_, &impltor, ikey);
        }
    }

    let func_names = ast.entities.fheaders.map(|(_, header)| header.name);

    let assoc_names = ast
        .entities
        .associated_types
        .map(|(_, assoc)| assoc.values().map(|a| a.name.tr(a.span)).collect());

    let iassoc_names = ast
        .entities
        .impls
        .map(|(_, i)| i.associations.values().map(|a| a.name.tr(a.span)).collect());

    (
        HIR {
            fnames: ast.entities.field_names,
            val_initializers: ast.entities.vals,
            func_names,
            assoc_names,
            iassoc_names,
            sources: ast.sources,
            lookups: ast.lookups,
            methods: ast.entities.methods,
            imethods: ast.entities.imethods,
            funcs,
            records,
            field_types,
            sums,
            vnames: ast.entities.variant_names,
            variant_types,
            traits,
            assoc,
            impls,
            impltors,
            iassoc,
            itraits,
            langitems,
        },
        tenvs,
        iquery,
    )
}

pub fn from_langs(ty: &str, langs: &LangItems, mlangs: &LangItems) -> Option<M<key::TypeKind>> {
    langs.get(ty).or_else(|| mlangs.get(ty)).copied()
}

pub fn list_from_langs(
    langs: &LangItems,
    mlangs: &LangItems,
    pinfo: &ProjectInfo,
) -> M<key::TypeKind> {
    from_langs("list", langs, mlangs).unwrap_or(pinfo.global_list_default)
}

pub type RecordFields = Map<key::RecordField, Tr<Type>>;
pub type SumVariants = Map<key::SumVariant, Vec<Tr<Type>>>;

fn lower_langitems<'a, 's>(
    ast: &'a AST<'s>,
    module: key::Module,
    items: &[(Tr<&'s str>, parser::Type<'s>)],
) -> LangItems<'s> {
    let err_bad_type = |span| {
        ast.sources
            .error("identifier not found")
            .m(module)
            .eline(span, "lang item type is not valid")
            .emit();
        None
    };

    items
        .iter()
        .filter_map(|(name, ty)| match ty {
            parser::Type::Defined(apath, params) if params.is_empty() => {
                match ast.lookups.resolve_type(module, apath.path.as_slice()) {
                    Ok(Mod { key: ast::Entity::Type(type_), module, .. }) => {
                        Some((**name, module.m(type_)))
                    }
                    _ => err_bad_type(name.span),
                }
            }
            _ => err_bad_type(name.span),
        })
        .collect()
}

fn lower_func<'a, 's>(
    ast: &'a AST<'s>,
    tforalls: &ModMap<key::Trait, (Tr<&'s str>, Forall<'s, Type>)>,
    iforalls: &ModMap<key::Impl, Forall<'s, Type>>,
    impltors: &ModMap<key::Impl, Tr<Type>>,
    langitems: &LangItems<'s>,
    func: M<key::Func>,
    pinfo: ProjectInfo,
) -> (FuncDefKind<'s>, TEnv<'s>) {
    let module = func.module;
    let header = &ast.entities.fheaders[func];
    let attributes = &ast.entities.fattributes[func];
    let no_mangle = attributes.no_mangle;

    let _span = info_span!(
        "lowering func",
        module = ast.sources.name_of_module(module),
        entity = header.name.to_string(),
        key = func.to_string()
    );
    let _handle = _span.enter();

    let flangitems = lower_langitems(ast, module, &attributes.shared.lang_items);

    let list = list_from_langs(&flangitems, &langitems, &pinfo);
    let mut tinfo = TypeEnvInfo::new(true, list);

    let string = from_langs("string", &flangitems, langitems)
        .unwrap_or_else(|| pinfo.string.map(key::TypeKind::Record));

    match &ast.entities.fbodies[func] {
        ast::FuncBody::Extern { link_name } => {
            let typing = lower_extern_func(module, ast, header, list);
            let kind = FuncDefKind::Extern { link_name: link_name.clone(), typing };
            (kind, TEnv::new())
        }
        ast::FuncBody::Val(body, _) | ast::FuncBody::Func(body) => {
            let to_kind = FuncDefKind::Defined;
            let mut tinfo = tinfo.inference(TEnv::new());
            ExprLower::new(module, ast, &mut tinfo, &body.where_binds)
                .lower_func(&header, &body, to_kind, no_mangle)
        }
        ast::FuncBody::TraitMethod(Some(body), tr) => {
            let to_kind = |k| FuncDefKind::TraitDefaultMethod(*tr, k);
            let mut tinfo = tinfo.inference(TEnv::new());
            tinfo.enter_type_or_impl_or_method(tforalls[*tr].1.clone(), GenericKind::Parent);
            tinfo.self_handler = SelfHandler::Direct;
            ExprLower::new(module, ast, &mut tinfo, &body.where_binds)
                .lower_func(&header, &body, to_kind, no_mangle)
        }
        ast::FuncBody::ImplMethod(body, imp) => {
            let self_ = impltors[*imp].i();
            let to_kind = |f| FuncDefKind::ImplMethod(*imp, f);
            let mut env = TEnv::new();
            env.set_self(self_);
            let mut tinfo = tinfo.inference(env);
            tinfo.enter_type_or_impl_or_method(iforalls[*imp].clone(), GenericKind::Parent);
            tinfo.self_handler = SelfHandler::Direct;
            ExprLower::new(module, ast, &mut tinfo, &body.where_binds)
                .lower_func(&header, &body, to_kind, no_mangle)
        }
        ast::FuncBody::TraitMethod(None, trait_) => {
            tinfo.self_handler = SelfHandler::Direct;
            let tforall = tforalls[*trait_].clone().1;
            tinfo.enter_type_or_impl_or_method(tforall, GenericKind::Parent);
            tinfo.enter_type_or_impl_or_method(Forall::new(), GenericKind::Entity);
            let typing = lower_func_typing(ast, module, header, &mut tinfo);
            let forall = tinfo.leave_type_or_impl_or_method();
            let kind = FuncDefKind::TraitHeader(*trait_, forall, typing);
            (kind, TEnv::new())
        }
    }
}

fn lower_extern_func<'s>(
    module: key::Module,
    ast: &AST<'s>,
    header: &parser::func::Header<'s>,
    list: M<key::TypeKind>,
) -> Typing<Type> {
    let mut tinfo = TypeEnvInfo::new(false, list);
    let typing = lower_func_typing::<Type>(ast, module, header, &mut tinfo);
    info!("typing lowered to: {typing}");
    typing
}

fn tydef_type_env<'s, K: Into<key::TypeKind>>(
    kind: M<K>,
    generics: &Map<key::Generic, &'s str>,
    list: M<key::TypeKind>,
) -> TypeEnvInfo<'s> {
    let mut tinfo = TypeEnvInfo::new(false, list);
    tinfo.self_handler = SelfHandler::Substituted(kind.map(Into::into));
    let forall = generics
        .values()
        .map(|&name| GenericData::new(name))
        .collect::<Forall<'s, _>>();

    tinfo.enter_type_or_impl_or_method(forall, GenericKind::Entity);

    tinfo
}

type LoweredType<'s, K, V> = (Map<K, V>, Forall<'s, Type>);

fn lower_sum<'s>(
    ast: &AST<'s>,
    lang: &LangItems,
    sum: M<key::Sum>,
    pinfo: &ProjectInfo,
) -> LoweredType<'s, key::SumVariant, Vec<Tr<Type>>> {
    let ty = &ast.entities.sums[sum];

    let _span = info_span!(
        "lowering sum",
        entity = ty.header.name.to_string(),
        key = sum.to_string()
    );

    let _handle = _span.enter();

    let tlangs = lower_langitems(ast, sum.module, &ty.attributes.shared.lang_items);

    let list = list_from_langs(&tlangs, lang, pinfo);
    let mut tinfo = tydef_type_env(sum, &ty.header.type_params, list);

    let mut tlower = ty::TypeLower::new(sum.module, ast, &mut tinfo);

    let variants = &ast.entities.variant_types[sum];
    (
        variants.values().map(|ty| tlower.tys_spanned(ty)).collect(),
        tinfo.leave_type_or_impl_or_method(),
    )
}

fn lower_record<'s>(
    ast: &AST<'s>,
    lang: &LangItems,
    rec: M<key::Record>,
    pinfo: &ProjectInfo,
) -> LoweredType<'s, key::RecordField, Tr<Type>> {
    let ty = &ast.entities.records[rec];

    let _span = info_span!(
        "lowering record",
        entity = ty.header.name.to_string(),
        key = rec.to_string()
    );
    let _handle = _span.enter();

    let tlangs = lower_langitems(ast, rec.module, &ty.attributes.shared.lang_items);

    let list = list_from_langs(&tlangs, lang, pinfo);
    let mut tinfo = tydef_type_env(rec, &ty.header.type_params, list);

    let mut tlower = ty::TypeLower::new(rec.module, ast, &mut tinfo);

    let fields = &ast.entities.field_types[rec];
    (
        fields
            .values()
            .map(|ty| tlower.ty_spanned(ty.as_ref()))
            .collect(),
        tinfo.leave_type_or_impl_or_method(),
    )
}

fn lower_trait<'s>(
    ast: &AST<'s>,
    lang: &LangItems,
    trait_: M<key::Trait>,
    pinfo: &ProjectInfo,
) -> (Map<key::AssociatedType, Option<Tr<Type>>>, Forall<'s, Type>) {
    let module = trait_.module;
    let ty = &ast.entities.traits[trait_];

    let _span = info_span!(
        "lowering trait",
        entity = ty.header.name.to_string(),
        key = trait_.to_string()
    );
    let _handle = _span.enter();

    let tlangs = lower_langitems(ast, module, &ty.attributes.shared.lang_items);

    let list = list_from_langs(&tlangs, lang, pinfo);
    let mut tinfo = TypeEnvInfo::new(false, list);
    let forall = ty
        .header
        .type_params
        .values()
        .map(|&name| GenericData::new(name))
        .collect();
    tinfo.enter_type_or_impl_or_method(forall, GenericKind::Parent);
    tinfo.self_handler = SelfHandler::Direct;

    tinfo.declare_generics = true;

    tinfo.declare_generics = false;
    let mut tlower = ty::TypeLower::new(module, ast, &mut tinfo);

    let associations = ast.entities.associated_types[trait_]
        .values()
        .map(|assoc| match &assoc.type_ {
            None => None,
            Some(ty) => Some(tlower.ty_spanned(ty.as_ref())),
        })
        .collect();

    (associations, tinfo.leave_type_or_impl_or_method())
}

fn check_generic_param_validity<'s, T>(
    ast: &AST<'s>,
    span: Span,
    module: key::Module,
    forall: &mut Forall<'s, T>,
) {
    for gdata in forall.values_mut() {
        if gdata.params == usize::MAX {
            ast.sources
                .warning("unused type parameter")
                .m(module)
                .eline(span, format!("`{}` is not used", gdata.name))
                .emit();
        } else if gdata.params != 0 {
            ast.sources
                .error("syntax error")
                .m(module)
                .eline(span, "higher kinded types are not yet supported")
                .emit();
        }

        gdata.params = 0;
    }
}

// TODO: we ended up needing more of the ones containing `FuncDef` than we thought. It's probably a
// good idea to split out those tags into a separate enum instead.
#[derive(Debug)]
pub enum FuncDefKind<'s> {
    Extern {
        link_name: String,
        typing: Typing<Type>,
    },
    Defined(FuncDef<'s>),
    TraitDefaultMethod(M<key::Trait>, FuncDef<'s>),
    ImplMethod(M<key::Impl>, FuncDef<'s>),
    InheritedDefault(M<key::Trait>, key::Func),
    TraitHeader(M<key::Trait>, Forall<'s, Type>, Typing<Type>),
}

impl<'s> FuncDefKind<'s> {
    #[track_caller]
    pub fn as_defined(&self) -> &FuncDef<'s> {
        match self {
            FuncDefKind::TraitDefaultMethod(_, f)
            | FuncDefKind::Defined(f)
            | FuncDefKind::ImplMethod(_, f) => f,
            def => panic!("not a defined function: {def:?}"),
        }
    }
}

#[derive(new, Clone, Debug)]
pub struct FuncDef<'s> {
    pub forall: RefCell<Forall<'s, IType>>,
    pub typing: Typing<IType>,
    pub list: M<key::TypeKind>,
    pub params: Vec<Tr<Pattern<'s>>>,
    pub expr: Tr<Expr<'s>>,

    pub no_mangle: bool,

    #[new(default)]
    pub lambdas: Lambdas<'s>,
}

#[derive(Clone, Default, Debug)]
pub struct Lambdas<'s> {
    pub foralls: RefCell<Map<key::Lambda, Forall<'s, IType>>>,
    pub typings: Map<key::Lambda, Typing<IType>>,
    pub params: Map<key::Lambda, Vec<Tr<Pattern<'s>>>>,
    pub bodies: Map<key::Lambda, Tr<Expr<'s>>>,
    pub captures: Map<key::Lambda, Vec<key::Bind>>,
}

impl<'s> Lambdas<'s> {
    /// for where-bindings we create lambdas ahead of time so that the
    /// indices don't get screwed up on nested lambdas
    pub fn create_placeholder(
        &mut self,
        span: Span,
        forall: Forall<'s, IType>,
        typing: Typing<IType>,
    ) -> key::Lambda {
        let lkey = self.foralls.get_mut().push(forall);
        self.typings.push_as(lkey, typing);
        self.params.push_as(lkey, vec![]);
        self.bodies.push_as(lkey, Expr::Poison.tr(span));
        self.captures.push_as(lkey, vec![]);
        lkey
    }

    pub fn complete_lambda(
        &mut self,
        key: key::Lambda,
        expr: Tr<Expr<'s>>,
        patterns: Vec<Tr<Pattern<'s>>>,
        captures: Vec<key::Bind>,
    ) {
        self.bodies[key] = expr;
        self.params[key] = patterns;
        self.captures[key] = captures;
    }

    pub fn is_empty(&self) -> bool {
        self.captures.is_empty()
    }

    pub fn keys(&self) -> impl Iterator<Item = key::Lambda> + 'static {
        self.typings.keys()
    }

    pub fn next_key(&mut self) -> key::Lambda {
        self.typings.next_key()
    }
}

#[derive(Clone, Constructor, Debug)]
pub struct Typing<T> {
    pub params: Map<key::Param, Tr<T>>,
    pub returns: Tr<T>,
}

impl<'s> Typing<IType> {
    pub fn inferred(
        span: impl Fn(usize) -> Span,
        ret_span: Span,
        params: usize,
        vars: &mut TEnv<'s>,
        is_from_lambda: Option<key::Lambda>,
    ) -> Self {
        Self {
            params: (0..params)
                .map(|i| IType::Var(vars.var(span(i))).tr(span(i)))
                .collect(),
            returns: IType::Var(vars.var(ret_span)).tr(ret_span),
        }
    }
}

impl<'t, 'a, 's> ExprLower<'t, 'a, 's> {
    fn lower_func(
        mut self,
        header: &parser::func::Header<'s>,
        body: &parser::func::Body<'s>,
        to_kind: impl FnOnce(FuncDef<'s>) -> FuncDefKind<'s>,
        no_mangle: bool,
    ) -> (FuncDefKind<'s>, TEnv<'s>) {
        let forall = generics_from_con(&header.when);
        self.type_info.enter_function(forall);

        include_constraints(
            (self.ast, self.module, self.type_info),
            &header.when,
            |tinfo, key, cons| tinfo.iforalls.last_mut().unwrap().0[key].trait_constraints = cons,
        );

        let typing = lower_func_typing(self.ast, self.module, &header, self.type_info);
        info!("typing lowered to: {typing}");

        self.type_info.declare_generics = false;

        let (params, expr) = self.lower_func_body(header, body);

        let forall = self.type_info.leave_function();
        info!("forall lowered to: {}", forall.keys().format(" "));

        let list = self.type_info.list;

        let mut func = FuncDef::new(RefCell::new(forall), typing, list, params, expr, no_mangle);
        func.lambdas = self.lambdas;

        info!(
            "func lowered to:\n {} {} {} {func}",
            "fn".keyword(),
            header.name,
            "as".keyword(),
        );

        let env = self.type_info.take_inference().unwrap();

        (to_kind(func), env)
    }

    fn lower_func_body(
        &mut self,
        header: &parser::func::Header<'s>,
        body: &parser::func::Body<'s>,
    ) -> (Vec<Tr<Pattern<'s>>>, Tr<Expr<'s>>) {
        // lower the where binding typings ahead of time so that each where binding is associated with a lambda
        for (i, fdecl) in body.where_binds.iter().enumerate() {
            let lkey = key::Lambda(i as u32);

            self.type_info.enter_lambda(lkey, Forall::new());

            let typing =
                lower_func_typing(self.ast, self.module, &fdecl.header, &mut self.type_info);
            info!("where-binding typing lowered to: {typing}");

            let forall = self.type_info.leave_function();

            let _lambda = self
                .lambdas
                .create_placeholder(fdecl.header.name.span, forall, typing);
        }

        // lower the function body
        let params = self.patterns(&header.params);
        info!("patterns lowered to: {}", params.iter().format(" "));
        let expr = self.expr(body.expr.as_ref());
        info!("expr lowered to: {expr}");

        // lower the where-bind bodies
        for (fdecl, lkey) in body.where_binds.iter().zip(self.lambdas.keys()) {
            let Some(lbody) = fdecl.body.as_ref() else {
                self.ast
                    .sources
                    .error("syntax error")
                    .m(self.module)
                    .eline(
                        fdecl.header.name.span,
                        "this where-binding is missing an expression",
                    )
                    .emit();

                continue;
            };

            self.bindings.enter();
            let patterns = self.patterns(&fdecl.header.params);
            let expr = self.expr(lbody.expr.as_ref());
            let captures = self.bindings.leave();
            self.lambdas.complete_lambda(lkey, expr, patterns, captures);
        }

        (params, expr)
    }
}

fn lower_func_typing<'a, 's, T: Ty>(
    ast: &'a AST<'s>,
    module: key::Module,
    header: &parser::func::Header<'s>,
    tinfo: &mut TypeEnvInfo<'s>,
) -> Typing<T> {
    let mut tlower = ty::TypeLower::new(module, ast, tinfo);

    let (params, returns) = match header.typing.as_ref() {
        Some(typing) => (
            typing
                .ptypes
                .iter()
                .map(|ty| tlower.ty_spanned(ty.as_ref()))
                .collect(),
            tlower.ty_spanned(typing.returns.as_ref()),
        ),
        None => {
            let vars = tinfo.inference_mut().unwrap();
            (
                header
                    .params
                    .iter()
                    .map(|p| T::var(vars.var(p.span)).tr(p.span))
                    .collect(),
                T::var(vars.var(header.name.span)).tr(header.name.span),
            )
        }
    };

    Typing { params, returns }
}

fn lower_impl<'a, 's>(
    ast: &'a AST<'s>,
    langitems: &LangItems<'s>,
    impl_: M<key::Impl>,
    pinfo: &ProjectInfo,
) -> (
    M<key::Trait>,
    Vec<Type>,
    Forall<'s, Type>,
    Tr<Type>,
    Map<key::AssociatedType, Tr<Type>>,
) {
    let module = impl_.module;
    let imp = &ast.entities.impls[impl_];

    let _span = info_span!(
        "lowering impl",
        module = ast.sources.name_of_module(module),
        entity = ast.sources.get_span(module, imp.header.span),
        key = impl_.to_string()
    );

    let list = list_from_langs(langitems, &HashMap::new(), pinfo);
    let mut tinfo = TypeEnvInfo::new(true, list);

    let impl_forall = generics_from_con(&imp.header.when);
    tinfo.enter_type_or_impl_or_method(impl_forall, GenericKind::Parent);

    let mut tlower = ty::TypeLower::new(module, ast, &mut tinfo);

    let impltor = tlower.ty_spanned(imp.header.impltor.as_ref());
    let trait_ = tlower.ty_spanned(imp.header.trait_.as_ref());

    let (trkey, trparams) = match trait_.value {
        Type::Defined(M { module, value: key::TypeKind::Trait(trkey) }, params) => {
            (module.m(trkey), params)
        }
        _ => panic!("ET: not a trait"),
    };

    tinfo.self_handler = SelfHandler::Direct;

    include_constraints(
        (ast, module, &mut tinfo),
        &imp.header.when,
        |tinfo, key, cons| tinfo.cforalls[0].0[key].trait_constraints = cons,
    );

    let mut tlower = ty::TypeLower::new(module, ast, &mut tinfo);

    tlower.type_info.declare_generics = false;
    let associations = ast.entities.associated_types[trkey]
        .values()
        .map(|astassoc| {
            match imp
                .associations
                .values()
                .find(|assoc| assoc.name == astassoc.name)
            {
                Some(parser::r#impl::Association { type_: Some(ty), .. }) => {
                    tlower.ty_spanned(ty.as_ref())
                }

                // try a default
                _ => todo!(
                    "get default, map the type parameters, or append to missing assoc and poison"
                ),
            }
        })
        .collect();

    let forall = tinfo.leave_type_or_impl_or_method();

    (trkey, trparams, forall, impltor, associations)
}

impl ast::Sources {
    pub fn emit_wrong_entity(
        &self,
        module: key::Module,
        span: Span,
        name: &str,
        kind: &str,
        found: ast::Entity,
    ) {
        self.error("identifier not found")
            .m(module)
            .eline(span, format!("no {kind} named {name}"))
            .text(format!(
                "there is however a `{}` with that name",
                found.describe()
            ))
            .emit()
    }
}

fn generics_from_con<'s, T>(when: &parser::when::Constraints<'s>) -> Forall<'s, T> {
    when.generics
        .iter()
        .map(|(_, generic, _)| GenericData::new(*generic))
        .collect()
}

fn include_constraints<'s, T: ty::Ty>(
    (ast, module, tinfo): (&AST<'s>, key::Module, &mut TypeEnvInfo<'s>),
    when: &parser::when::Constraints<'s>,
    mut attach: impl FnMut(&mut TypeEnvInfo<'s>, key::Generic, Vec<Constraint<T>>),
) {
    let generic_iter = (0..when.generics.len()).map(|i| key::Generic(i as u32));

    for key in generic_iter {
        let (_, _, constraints) = &when.generics[key.0 as usize];
        let cons = constraints
            .iter()
            .filter_map(|ty| {
                let con_ty: T = ty::TypeLower::new(module, ast, tinfo).ty(ty.as_ref());

                match con_ty.as_trait() {
                    Ok((trait_, params)) => Some(Constraint { span: ty.span, trait_, params }),
                    Err(_) => {
                        ast.sources
                            .error("invalid constraint")
                            .m(module)
                            .eline(ty.span, "must be a trait")
                            .emit();

                        None
                    }
                }
            })
            .collect();

        attach(tinfo, key, cons)
    }
}

impl<'s> fmt::Display for FuncDef<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let forall = self.forall.borrow();
        if !forall.is_empty() {
            writeln!(f, "{}", "when".keyword())?;
            for (key, gdata) in forall.iter() {
                writeln!(
                    f,
                    "  {key} {} {:?}",
                    "can".keyword(),
                    gdata.trait_constraints
                )?;
            }
        }
        write!(f, "{} {}\n  {}", &self.typing, '='.symbol(), self.expr)?;

        if self.lambdas.is_empty() {
            return Ok(());
        } else {
            write!(f, "\n {}\n{}", "where".keyword(), &self.lambdas)
        }
    }
}

impl<'s> fmt::Display for Lambdas<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for key in self.typings.keys() {
            write!(
                f,
                "  {} {key} {} {} {} {}",
                "fn".keyword(),
                "as".keyword(),
                &self.typings[key],
                '='.keyword(),
                &self.bodies[key]
            )?;
        }

        Ok(())
    }
}

impl<'s, T: fmt::Display> fmt::Display for Typing<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} -> {}",
            self.params.values().format(", "),
            self.returns
        )
    }
}
