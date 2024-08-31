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
use crate::{ProjectInfo, Target};
use ast::{Mod, AST};
use derive_new::new;
use lumina_parser as parser;
use lumina_typesystem::{
    Constraint, Downgrade, Forall, Generic, GenericKind, IType, ImplIndex, Inference, Static, TEnv,
    Transformer, Ty, Type,
};
use lumina_util::{Highlighting, Identifier};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use tracing::info_span;

mod expr;
pub use expr::{Callable, Expr, Literal};
mod pat;
pub use pat::{Pattern, StringPattern};
mod scope;
use scope::Bindings;
mod ty;
pub use ty::TypeAnnotation;
use ty::{SelfHandler, TypeEnvInfo};

pub struct HIR<'s> {
    pub funcs: ModMap<key::Func, FuncDefKind<'s>>,

    pub records: ModMap<key::Record, (Tr<&'s str>, Forall<'s, Static>)>,
    pub field_types: ModMap<key::Record, Map<key::RecordField, Tr<Type>>>,

    pub sums: ModMap<key::Sum, (Tr<&'s str>, Forall<'s, Static>)>,
    pub variant_types: ModMap<key::Sum, Map<key::SumVariant, Vec<Tr<Type>>>>,

    pub traits: ModMap<key::Trait, (Tr<&'s str>, Forall<'s, Static>)>,
    pub assoc: ModMap<key::Trait, Map<key::AssociatedType, Option<Tr<Type>>>>,

    pub impls: ModMap<key::Impl, Forall<'s, Static>>,
    pub impltors: ModMap<key::Impl, Tr<Type>>,
    pub iassoc: ModMap<key::Impl, Vec<(Tr<&'s str>, Tr<Type>)>>,
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
}

type LangItems<'s> = HashMap<&'s str, M<key::TypeKind>>;

pub fn run<'a, 's>(
    info: ProjectInfo,
    target: Target,
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

    let info = Info { ast: &ast, langitems: &HashMap::new(), pinfo: &info, target };

    // Lower type declarations in all modules
    for module in ast.sources.modules() {
        let langs = lower_langitems(&ast, module, ast.entities.get_langitems(module));

        let info = Info { langitems: &langs, ..info };

        ast.entities.sums.iter_module(module).for_each(|sum| {
            let (variants, forall) = lower_sum(info, sum);
            let header = &ast.entities.sums[sum].header;
            sums.push(module, (header.name.tr(header.span), forall));
            variant_types.push_as(sum, variants);
        });

        ast.entities.records.iter_module(module).for_each(|record| {
            let (fields, forall) = lower_record(info, record);
            let header = &ast.entities.records[record].header;
            records.push(module, (header.name.tr(header.span), forall));
            field_types.push_as(record, fields);
        });

        ast.entities.traits.iter_module(module).for_each(|trait_| {
            let (tassoc, forall) = lower_trait(info, trait_);
            let header = &ast.entities.traits[trait_].header;
            traits.push_as(trait_, (header.name.tr(header.span), forall));
            assoc.push_as(trait_, tassoc);
        });

        langitems.push_as(module, langs);
    }

    // Lowering an impl requires that the trait it implements is already lowered.
    for module in ast.sources.modules() {
        let info = Info { langitems: &langitems[module], ..info };

        for impl_ in ast.entities.impls.iter_module(module) {
            let parts = lower_impl(info, impl_);
            let Ok(trait_) = parts.0.value.as_trait() else {
                todo!();
            };
            impls.push_as(impl_, parts.1);
            impltors.push_as(impl_, parts.2);
            iassoc.push_as(impl_, parts.3);
            itraits.push_as(impl_, trait_);
        }
    }

    // Lowering functions includes methods, which requires that the impl header is already lowered.
    for module in ast.sources.modules() {
        let info = Info { langitems: &langitems[module], ..info };

        ast.entities.fheaders.iter_module(module).for_each(|func| {
            let (fdef, tenv) = lower_func(info, &traits, &impls, &impltors, func);
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

    (
        HIR {
            fnames: ast.entities.field_names,
            val_initializers: ast.entities.vals,
            func_names,
            assoc_names,
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

#[derive(new)]
pub struct FuncLower<'t, 'a, 's> {
    pub module: key::Module,
    pub ast: &'a ast::AST<'s>,

    pub type_info: &'t mut TypeEnvInfo<'s>,
    where_binds: &'a [parser::func::Declaration<'s>],

    pub target: Target,

    #[new(default)]
    pub bindings: Bindings<'s>,
    #[new(default)]
    pub lambdas: Lambdas<'s>,
}

fn lower_func<'a, 's>(
    Info { ast, target, langitems, pinfo, .. }: Info<'a, 's>,
    tforalls: &ModMap<key::Trait, (Tr<&'s str>, Forall<'s, Static>)>,
    iforalls: &ModMap<key::Impl, Forall<'s, Static>>,
    impltors: &ModMap<key::Impl, Tr<Type>>,
    func: M<key::Func>,
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
    let string = pinfo.string.map(key::TypeKind::Record);

    let mut tinfo = TypeEnvInfo::new(true, string, list);

    match &ast.entities.fbodies[func] {
        ast::FuncBody::Extern { link_name } => {
            let typing = ty::TypeLower::new(module, ast, target.int_size(), &mut tinfo)
                .typing_or_emit_and_poison(header, "extern functions");
            let kind = FuncDefKind::Extern { link_name: link_name.clone(), typing };
            (kind, TEnv::new())
        }
        ast::FuncBody::Val(body, _) | ast::FuncBody::Func(body) => {
            let mut tinfo = tinfo.inference(TEnv::new());
            let (fdef, env) = FuncLower::new(module, ast, &mut tinfo, &body.where_binds, target)
                .lower_func(&header, &body, no_mangle);
            (FuncDefKind::Defined(fdef), env)
        }
        ast::FuncBody::TraitMethod(Some(body), tr) => {
            let mut tinfo = tinfo.inference(TEnv::new());
            tinfo.enter_type_or_impl_or_method(tforalls[*tr].1.clone(), GenericKind::Parent);
            tinfo.self_handler = SelfHandler::Direct;
            let (fdef, env) = FuncLower::new(module, ast, &mut tinfo, &body.where_binds, target)
                .lower_func(&header, &body, no_mangle);

            let kind = disallow_inference_in_trait_default(module, ast, *tr, fdef);
            (kind, env)
        }
        ast::FuncBody::ImplMethod(body, imp) => {
            let self_ = Downgrade.transform(&impltors[*imp]);
            let mut env = TEnv::new();
            env.self_ = Some(self_);
            let mut tinfo = tinfo.inference(env);
            tinfo.enter_type_or_impl_or_method(iforalls[*imp].clone(), GenericKind::Parent);
            tinfo.self_handler = SelfHandler::Direct;
            let (fdef, env) = FuncLower::new(module, ast, &mut tinfo, &body.where_binds, target)
                .lower_func(&header, &body, no_mangle);
            (FuncDefKind::ImplMethod(*imp, fdef), env)
        }
        ast::FuncBody::TraitMethod(None, trait_) => {
            tinfo.self_handler = SelfHandler::Direct;
            let tforall = tforalls[*trait_].clone().1;
            tinfo.enter_type_or_impl_or_method(tforall, GenericKind::Parent);
            tinfo.enter_type_or_impl_or_method(Forall::new(0), GenericKind::Entity);

            let typing = ty::TypeLower::new(module, ast, target.int_size(), &mut tinfo)
                .typing_or_emit_and_poison(header, "trait methods");

            let forall = tinfo.leave_type_or_impl_or_method();
            let kind = FuncDefKind::TraitHeader(*trait_, forall, typing);
            (kind, TEnv::new())
        }
    }
}

fn tydef_type_env<'s, K: Into<key::TypeKind>>(
    kind: M<K>,
    generics: &Map<key::Generic, &'s str>,
    list: M<key::TypeKind>,
    string: M<key::TypeKind>,
) -> TypeEnvInfo<'s> {
    let mut tinfo = TypeEnvInfo::new(false, string, list);
    tinfo.self_handler = SelfHandler::Substituted(kind.map(Into::into));
    let forall = Forall::from_names(generics.values().copied());

    tinfo.enter_type_or_impl_or_method(forall, GenericKind::Entity);

    tinfo
}

/// Information used during the lower of any item
#[derive(Clone, Copy)]
struct Info<'a, 's> {
    ast: &'a AST<'s>,
    langitems: &'a LangItems<'s>,
    pinfo: &'a ProjectInfo,
    target: Target,
}

type LoweredType<'s, K, V> = (Map<K, V>, Forall<'s, Static>);

fn lower_sum<'a, 's>(
    Info { ast, langitems: lang, pinfo, target, .. }: Info<'a, 's>,
    sum: M<key::Sum>,
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
    let string = pinfo.string.map(key::TypeKind::Record);

    let mut tinfo = tydef_type_env(sum, &ty.header.type_params, list, string);

    let mut tlower = ty::TypeLower::new(sum.module, ast, target.int_size(), &mut tinfo);

    let variants = &ast.entities.variant_types[sum];
    (
        variants.values().map(|ty| tlower.tys_spanned(ty)).collect(),
        tinfo.leave_type_or_impl_or_method(),
    )
}

fn lower_record<'a, 's>(
    Info { ast, langitems: lang, pinfo, target, .. }: Info<'a, 's>,
    rec: M<key::Record>,
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
    let string = pinfo.string.map(key::TypeKind::Record);

    let mut tinfo = tydef_type_env(rec, &ty.header.type_params, list, string);

    let mut tlower = ty::TypeLower::new(rec.module, ast, target.int_size(), &mut tinfo);

    let fields = &ast.entities.field_types[rec];
    (
        fields
            .values()
            .map(|ty| tlower.ty_spanned(ty.as_ref()))
            .collect(),
        tinfo.leave_type_or_impl_or_method(),
    )
}

fn lower_trait<'a, 's>(
    Info { ast, pinfo, langitems, target, .. }: Info<'a, 's>,
    trait_: M<key::Trait>,
) -> (
    Map<key::AssociatedType, Option<Tr<Type>>>,
    Forall<'s, Static>,
) {
    let module = trait_.module;
    let ty = &ast.entities.traits[trait_];

    let _span = info_span!(
        "lowering trait",
        entity = ty.header.name.to_string(),
        key = trait_.to_string()
    );
    let _handle = _span.enter();

    let tlangs = lower_langitems(ast, module, &ty.attributes.shared.lang_items);

    let list = list_from_langs(&tlangs, langitems, pinfo);
    let string = pinfo.string.map(key::TypeKind::Record);
    let mut tinfo = TypeEnvInfo::new(false, string, list);
    let forall = Forall::from_names(ty.header.type_params.values().copied());
    tinfo.enter_type_or_impl_or_method(forall, GenericKind::Parent);
    tinfo.self_handler = SelfHandler::Direct;

    tinfo.declare_generics = true;

    tinfo.declare_generics = false;
    let mut tlower = ty::TypeLower::new(module, ast, target.int_size(), &mut tinfo);

    let associations = ast.entities.associated_types[trait_]
        .values()
        .map(|assoc| match &assoc.type_ {
            None => None,
            Some(ty) => Some(tlower.ty_spanned(ty.as_ref())),
        })
        .collect();

    (associations, tinfo.leave_type_or_impl_or_method())
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
    TraitDefaultMethod(M<key::Trait>, Forall<'s, Static>, Typing<Type>, FuncDef<'s>),
    ImplMethod(M<key::Impl>, FuncDef<'s>),
    InheritedDefault(M<key::Trait>, key::Func),
    TraitHeader(M<key::Trait>, Forall<'s, Static>, Typing<Type>),
}

impl<'s> FuncDefKind<'s> {
    #[track_caller]
    pub fn as_defined(&self) -> &FuncDef<'s> {
        match self {
            FuncDefKind::TraitDefaultMethod(_, _, _, f)
            | FuncDefKind::Defined(f)
            | FuncDefKind::ImplMethod(_, f) => f,
            def => panic!("not a defined function: {def:?}"),
        }
    }
}

#[derive(new, Clone, Debug)]
pub struct FuncDef<'s> {
    pub forall: RefCell<Forall<'s, Inference>>,
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
    pub foralls: RefCell<Map<key::Lambda, Forall<'s, Inference>>>,
    pub typings: Map<key::Lambda, Typing<IType>>,
    pub params: Map<key::Lambda, Vec<Tr<Pattern<'s>>>>,
    pub bodies: Map<key::Lambda, Tr<Expr<'s>>>,
    pub captures: Map<key::Lambda, Vec<key::Bind>>,
    // If this lambda references other lambdas, it'll need to inherit it's captures even though
    // they aren't directly references. (Since they will be references in the next lower)
    pub capture_lambdas: Map<key::Lambda, Vec<key::Lambda>>,
}

impl<'s> Lambdas<'s> {
    /// for where-bindings we create lambdas ahead of time so that the
    /// indices don't get screwed up on nested lambdas
    pub fn create_placeholder(
        &mut self,
        span: Span,
        forall: Forall<'s, Inference>,
        typing: Typing<IType>,
    ) -> key::Lambda {
        let lkey = self.foralls.get_mut().push(forall);
        self.typings.push_as(lkey, typing);
        self.params.push_as(lkey, vec![]);
        self.bodies.push_as(lkey, Expr::Poison.tr(span));
        self.captures.push_as(lkey, vec![]);
        self.capture_lambdas.push_as(lkey, vec![]);
        lkey
    }

    pub fn complete_lambda(
        &mut self,
        key: key::Lambda,
        expr: Tr<Expr<'s>>,
        patterns: Vec<Tr<Pattern<'s>>>,
        captures: Vec<key::Bind>,
        capture_lambdas: Vec<key::Lambda>,
    ) {
        self.bodies[key] = expr;
        self.params[key] = patterns;
        self.captures[key] = captures;
        self.capture_lambdas[key] = capture_lambdas;
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

impl<T> Typing<T> {
    pub fn map<F, U>(&self, mut f: impl FnMut(Tr<&T>) -> U) -> (F, Tr<U>)
    where
        F: FromIterator<Tr<U>>,
    {
        let ptypes = self
            .params
            .values()
            .map(|ty| f(ty.as_ref()).tr(ty.span))
            .collect();
        let returns = f(self.returns.as_ref()).tr(self.returns.span);
        (ptypes, returns)
    }
}

impl<'s, T: ty::FromVar> Typing<Ty<T>> {
    pub fn inferred<U>(
        params: &[Tr<U>],
        ret_span: Span,
        vars: &mut TEnv<'s>,
        lifted: bool,
    ) -> Self {
        Self {
            params: params
                .iter()
                .map(|tr| {
                    let var = vars.var(tr.span);
                    if lifted {
                        vars.enable_lift_to_generic(var);
                    }
                    T::var(var).tr(tr.span)
                })
                .collect(),
            returns: {
                let var = vars.var(ret_span);
                if lifted {
                    vars.enable_lift_to_generic(var);
                }
                T::var(var).tr(ret_span)
            },
        }
    }

    pub fn poisoned<U>(params: &[Tr<U>], ret_span: Span) -> Self {
        Self {
            params: params.iter().map(|tr| Ty::poison().tr(tr.span)).collect(),
            returns: Ty::poison().tr(ret_span),
        }
    }
}

impl<'t, 'a, 's> FuncLower<'t, 'a, 's> {
    fn lower_func(
        mut self,
        header: &parser::func::Header<'s>,
        body: &parser::func::Body<'s>,
        no_mangle: bool,
    ) -> (FuncDef<'s>, TEnv<'s>) {
        let forall = generics_from_con(&header.when);
        self.type_info.enter_function(forall);

        include_constraints(
            (self.ast, self.module, self.type_info),
            self.target.int_size(),
            &header.when,
            |tinfo, key, cons| tinfo.iforalls.last_mut().unwrap().0[key].trait_constraints = cons,
        );

        let mut typing = self.to_type_lower().typing_or_inferred(header, true);
        info!("typing lowered to: {typing}");

        self.type_info.declare_generics = false;

        let (mut params, expr) = self.lower_func_body(header, body);

        let error_text = || {
            format!(
                "{} patterns, function expects {} parameters",
                params.len().numeric(),
                typing.params.len().numeric(),
            )
        };

        match params.len().cmp(&typing.params.len()) {
            Ordering::Equal => {}
            Ordering::Less => {
                let span = header.name.span.extend_by_params(&params);
                self.ast
                    .sources
                    .error("missing parameter pattern(s)")
                    .m(self.module)
                    .eline(span, error_text())
                    .emit();

                while params.len() < typing.params.len() {
                    params.push(Pattern::Poison.tr(span));
                }
            }
            Ordering::Greater => {
                let span = Span::from_params(typing.params.values(), |v| v.span);
                self.ast
                    .sources
                    .error("missing parameter type(s)")
                    .m(self.module)
                    .eline(span, error_text())
                    .emit();

                while params.len() > typing.params.len() {
                    typing.params.push(Ty::poison().tr(span));
                }
            }
        }

        let forall = self.type_info.leave_function();
        info!("forall lowered to: {}", forall);

        let list = self.type_info.list;

        let mut func = FuncDef::new(RefCell::new(forall), typing, list, params, expr, no_mangle);
        func.lambdas = self.lambdas;

        // Copy all captures used by child lambda into the captures of the parent lambda for nesting
        for lambda in func.lambdas.keys() {
            for child in std::mem::take(&mut func.lambdas.capture_lambdas[lambda]) {
                if lambda == child {
                    continue;
                }

                let [lcap, ccap] = func.lambdas.captures.get_many_mut([lambda, child]).unwrap();
                for bind in ccap {
                    if !lcap.contains(bind) {
                        lcap.push(*bind);
                    }
                }
            }
        }

        info!(
            "func lowered to:\n {} {} {} {func}",
            "fn".keyword(),
            header.name,
            "as".keyword(),
        );

        let env = self.type_info.take_inference().unwrap();

        (func, env)
    }

    fn lower_func_body(
        &mut self,
        header: &parser::func::Header<'s>,
        body: &parser::func::Body<'s>,
    ) -> (Vec<Tr<Pattern<'s>>>, Tr<Expr<'s>>) {
        // lower the where binding typings ahead of time so that each where binding is associated with a lambda
        for (i, fdecl) in body.where_binds.iter().enumerate() {
            let lkey = key::Lambda(i as u32);

            self.type_info.enter_lambda(lkey, Forall::new(0));

            let typing = self
                .to_type_lower()
                .typing_or_inferred(&fdecl.header, false);
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
            let (captures, lcaptures) = self.bindings.leave();
            self.lambdas
                .complete_lambda(lkey, expr, patterns, captures, lcaptures);
        }

        (params, expr)
    }

    fn to_type_lower(&mut self) -> ty::TypeLower<'_, 'a, 's> {
        let int_size = self.target.int_size();
        ty::TypeLower::new(self.module, self.ast, int_size, self.type_info)
    }

    fn resolve_callable(
        &mut self,
        span: Span,
        ident: &Identifier<'s>,
    ) -> Option<(Callable<'s>, ToAnnotate)> {
        if let Some(name) = ident.as_name() {
            if let Some(bind) = self.bindings.resolve(name) {
                return Some((Callable::Binding(bind), ToAnnotate::None));
            }

            if let Some(lkey) = self.where_binds.find(|decl| *decl.header.name == name) {
                self.bindings.reference_lambda(lkey);
                return Some((Callable::Lambda(lkey), ToAnnotate::Some(None)));
            }
        }

        let path = ident.as_slice();

        match self.ast.lookups.resolve_func(self.module, path) {
            Ok(entity) => match entity.key {
                ast::Entity::Func(nfunc) => {
                    let type_ = match nfunc {
                        ast::NFunc::SumVar(key, _) => {
                            Some(entity.module.m(key::TypeKind::Sum(key)))
                        }
                        _ => None,
                    };
                    let callable = Callable::Func(entity.map(|_| nfunc));
                    Some((callable, ToAnnotate::Some(type_)))
                }
                ast::Entity::Member(type_, name) => {
                    let module = entity.module;
                    let ty = self.ast.entities.header_of_ty(module.m(type_));
                    let tname = ty.header.name;

                    let nfunc = match type_ {
                        key::TypeKind::Trait(trait_) => {
                            let methods = &self.ast.entities.methods[module.m(trait_)];
                            let fheaders = &self.ast.entities.fheaders;

                            let Some(method) =
                                methods.find(|fkey| *fheaders[module.m(*fkey)].name == name)
                            else {
                                self.ast
                                    .sources
                                    .emit_member_not_found(module, span, "trait", tname, name);
                                return None;
                            };

                            ast::NFunc::Method(trait_, method)
                        }
                        key::TypeKind::Sum(sum) => {
                            let variants = &self.ast.entities.variant_names[module.m(sum)];
                            match variants.find(|n| **n == name) {
                                None => {
                                    self.ast
                                        .sources
                                        .emit_member_not_found(module, span, "type", tname, name);
                                    return None;
                                }
                                Some(var) => ast::NFunc::SumVar(sum, var),
                            }
                        }
                        key::TypeKind::Record(key) => {
                            let is_valid_field = self.ast.entities.field_names[module.m(key)]
                                .values()
                                .any(|n| **n == name);

                            let message = if is_valid_field {
                                format!("if you're trying to use the field named `{name}` from `{tname}`, then use `.{name}`")
                            } else {
                                format!("tried to use the type `{tname}` as a module")
                            };
                            self.ast
                                .sources
                                .emit_identifier_not_found(module, span, message);

                            return None;
                        }
                    };

                    let to_anot = ToAnnotate::Some(Some(entity.module.m(type_)));
                    Some((Callable::Func(entity.map(|_| nfunc)), to_anot))
                }
                _ => {
                    let name = path[path.len() - 1];
                    self.ast.sources.emit_wrong_entity(
                        self.module,
                        span,
                        name,
                        "function",
                        entity.key,
                    );

                    None
                }
            },
            Err(err) => {
                self.ast
                    .sources
                    .emit_lookup_err(span, self.module, "function or binding", err);

                None
            }
        }
    }
}

fn lower_impl<'a, 's>(
    Info { ast, langitems, pinfo, target, .. }: Info<'a, 's>,
    impl_: M<key::Impl>,
) -> (
    Tr<Type>,
    Forall<'s, Static>,
    Tr<Type>,
    Vec<(Tr<&'s str>, Tr<Ty<Static>>)>,
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
    let string = pinfo.string.map(key::TypeKind::Record);

    let mut tinfo = TypeEnvInfo::new(true, string, list);

    let impl_forall = generics_from_con(&imp.header.when);
    tinfo.enter_type_or_impl_or_method(impl_forall, GenericKind::Parent);

    let mut tlower = ty::TypeLower::new(module, ast, target.int_size(), &mut tinfo);

    let impltor = tlower.ty_spanned(imp.header.impltor.as_ref());
    let trait_ = tlower.ty_spanned(imp.header.trait_.as_ref());

    tinfo.self_handler = SelfHandler::Direct;

    include_constraints(
        (ast, module, &mut tinfo),
        target.int_size(),
        &imp.header.when,
        |tinfo, key, cons| tinfo.cforalls[0].0[key].trait_constraints = cons,
    );

    let mut tlower = ty::TypeLower::new(module, ast, target.int_size(), &mut tinfo);

    tlower.type_info.declare_generics = false;
    let associations = imp
        .associations
        .iter()
        .filter_map(|(_, assoc)| {
            assoc
                .type_
                .as_ref()
                .map(|ty| tlower.ty_spanned(ty.as_ref()))
                .map(|ty| (assoc.name.tr(assoc.span), ty))
        })
        .collect();

    let forall = tinfo.leave_type_or_impl_or_method();

    (trait_, forall, impltor, associations)
}

enum ToAnnotate {
    Some(Option<M<key::TypeKind>>),
    None,
}

impl ast::Sources {
    fn emit_identifier_not_found<'s>(
        &self,
        module: key::Module,
        span: Span,
        str: impl Into<String>,
    ) {
        self.error("identifier not found")
            .m(module)
            .eline(span, str)
            .emit();
    }

    fn emit_member_not_found<'s>(
        &self,
        module: key::Module,
        span: Span,
        kind: &str,
        tname: &str,
        name: &str,
    ) {
        self.emit_identifier_not_found(
            module,
            span,
            format!("the {kind} {tname} does not have a member named {name}"),
        )
    }

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
    Forall::from_names(when.generics.iter().map(|(_, name, _)| *name))
}

fn include_constraints<'s, T: ty::FromVar>(
    (ast, module, tinfo): (&AST<'s>, key::Module, &mut TypeEnvInfo<'s>),
    default_int_size: u8,
    when: &parser::when::Constraints<'s>,
    mut attach: impl FnMut(&mut TypeEnvInfo<'s>, key::Generic, Vec<Constraint<T>>),
) {
    let generic_iter = (0..when.generics.len()).map(|i| key::Generic(i as u32));

    for key in generic_iter {
        let (_, _, constraints) = &when.generics[key.0 as usize];
        let cons = constraints
            .iter()
            .filter_map(|ty| {
                let con_ty: Ty<T> =
                    ty::TypeLower::new(module, ast, default_int_size, tinfo).ty(ty.as_ref());

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

fn disallow_inference_in_trait_default<'s>(
    module: key::Module,
    ast: &AST<'s>,
    trait_: M<key::Trait>,
    fdef: FuncDef<'s>,
) -> FuncDefKind<'s> {
    let forall = fdef.forall.borrow();

    let mut span = Span::null();
    let mut transformer = Staticify {
        on_err: move || {
            ast.sources
                .error("invalid inference")
                .m(module)
                .eline(span, "type signatures for trait methods are mandatory")
                .emit()
        },
    };

    let (ptypes, ret) = fdef.typing.map(|ty| {
        span = ty.span;
        transformer.transform(*ty)
    });

    let sforall = forall.map(
        |sp, ty| {
            span = sp;
            transformer.transform(ty)
        },
        |_, name| name,
    );
    drop(forall);

    FuncDefKind::TraitDefaultMethod(trait_, sforall, Typing::new(ptypes, ret), fdef)
}

struct Staticify<F: FnMut()> {
    on_err: F,
}

impl<F: FnMut()> Transformer<Inference> for Staticify<F> {
    type Output = Static;

    fn special(&mut self, _: &Inference) -> Ty<Self::Output> {
        (self.on_err)();
        Ty::poison()
    }

    fn generic(&mut self, generic: Generic) -> Ty<Self::Output> {
        Ty::Generic(generic)
    }
}

impl<'s> fmt::Display for FuncDef<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let forall = self.forall.borrow();
        if !forall.generics.is_empty() {
            writeln!(f, "{}", "when".keyword())?;
            for (key, gdata) in forall.generics.iter() {
                writeln!(
                    f,
                    "  {key} {} {}",
                    "can".keyword(),
                    gdata.trait_constraints.iter().format(" + "),
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
            writeln!(
                f,
                "  {} {key}[{}] {} {}{} {} {}",
                "fn".keyword(),
                self.captures[key].iter().format(", "),
                "as".keyword(),
                self.foralls
                    .try_borrow()
                    .map(|forall| forall[key].to_string())
                    .unwrap_or_else(|_| String::from("<borrowed>")),
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
