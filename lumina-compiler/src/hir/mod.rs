use crate::ast;
use crate::hir::ty::{InferenceEnv, LoweringKind as _, StaticEnv, TypeLower};
use crate::project::symbols;
use crate::project::{Files, ProjectNode};
use crate::project::{FuncSig, HeaderFile, ImplDef, TypeDef, TypeSig, ValDef};
use crate::{Context, TranslationUnit, ast::AST, errors, prelude::*};
pub use expr::Expr;
use lumina_key::{EntityList, Map, entity_impl};
use lumina_parser as parser;
use lumina_typesystem::GenericTag;
use lumina_typesystem::{self as ts, TaggedGeneric, Var};
use lumina_util::Identifier;
pub use pattern::Pattern;
use tracing::info_span;

mod expr;
mod func;
mod r#impl;
mod lit;
mod nodes;
mod operator;
mod pattern;
mod record;
mod ty;

const DEFAULT_OPERATOR_PRECEDENCE: u32 = 1000;

pub use lit::Literal;
pub use ty::{Type, TypeKey};

pub struct HIR {
    headerfile: HeaderFile,
}

pub fn lower_project_to_hir<'s>(
    ctx: Context<TranslationUnit>,
    node: &ProjectNode,
    files: &Files,
    ast: &AST<'s>,
) {
    let mut type_signatures = Map::with_capacity(ast.types.len());
    let mut typedefs = Map::with_capacity(ast.types.len());

    let _span = info_span!("Lowering project to HIR", project = node.config.name);
    let _span = _span.enter();

    for (file, attr) in ast.attributes.iter() {
        files.switch_errors_file(*file);

        for (name, ty) in attr.lang_items.iter() {
            langitem_lower(ctx.clone(), node.key, *file, *name, ty);
        }
    }

    // Lower type declarations
    for typedef in ast.types.values() {
        let file = ast.items[typedef.item].file;

        let _span = info_span!("Lowering ", r#type = typedef.header.name);
        let _span = _span.enter();
        files.switch_errors_file(file);

        let self_ = hack_define_self(typedefs.next_key(), &typedef.header);
        let tag = match &typedef.body {
            ast::TypeBody::Trait { .. } => GenericTag::Trait,
            _ => GenericTag::Type,
        };
        let env = StaticEnv::new("type declarations").with_self(self_);
        let tctx = TypeLower::new(ctx.clone(), node.key, file, env, tag);

        let sig = tydef_sig_lower(file, &typedef.decl);
        let kind = tydef_kind_lower(tctx, &typedef.decl);

        type_signatures.push(sig);
        typedefs.push(kind);
    }

    let mut uarena = LoweringArena::new(ast.functions.len() + ast.vals.len());

    // Copy mapping of values to initialiser functions
    for (val, initialiser) in ast.vals.iter() {
        ctx.in_project_mut(node.key, |unit| {
            let val_id = unit
                .header
                .values
                .push(ValDef { initialiser: *initialiser });

            assert_eq!(val, val_id);
        });
    }

    // Lower function signatures of public functions
    //
    // This is done before lowering any function bodies as it makes dotpipe name resolution easier.
    // All public functions need to be statically type annotated so it's fine to do this.
    for (func, funcdef) in ast.functions.iter() {
        let item = &ast.items[funcdef.item];

        files.switch_errors_file(item.file);

        if let ast::ItemKind::Func(func) = item.kind {
            prepass_function_signature(ctx.clone(), ast, node, &mut uarena, func);
        }
    }

    // Lower method type signatures in traits
    for (type_, typedef) in ast.types.iter() {
        let TypeDef::Trait { functions } = &typedefs[type_] else {
            continue;
        };

        let file = ast.items[typedef.item].file;
        files.switch_errors_file(file);

        for (method, func) in functions.iter() {
            let funcdef = &ast.functions[*func].decl;

            let _span = info_span!("Lowering ", r#type = *funcdef.header.name);
            let _span = _span.enter();

            let env = StaticEnv::new("method").with_self(Type::Prim(ts::Prim::Self_));
            let tctx = TypeLower::new(ctx.clone(), node.key, file, env, GenericTag::Func)
                .as_member_of_trait(&type_signatures[type_]);

            let method_of = (type_, method);

            trait_method_lower(ast, tctx, *func, funcdef, &mut uarena, method_of);
        }
    }
    // Update the final headerfile with the type declarations
    ctx.in_project_mut(node.key, |unit| {
        unit.header.type_signatures = type_signatures;
        unit.header.typedefs = typedefs;
    });

    // Lower implementation headers
    let mut impl_error_sets = ast
        .impls
        .iter()
        .map(|(impl_, impldef)| {
            let file = ast.items[impldef.item].file;

            let _span = info_span!(
                "Lowering ",
                impl_ = impldef.header.span.get_line(files.source(file)).0
            );
            let _span = _span.enter();
            files.switch_errors_file(file);

            let tag = GenericTag::Impl;
            let env = StaticEnv::new("methods");
            let tctx = TypeLower::new(ctx.clone(), node.key, file, env, tag);

            let (ierrors, lowering_impl) = r#impl::lower_impl_header(node, tctx, ast, impl_);

            uarena.park_impl(impl_, lowering_impl);

            ierrors
        })
        .collect::<Map<key::Impl, r#impl::Errors>>();

    // Lower function definitions (including methods)
    for func_id in ast.functions.keys() {
        let mut flower = func::FuncLower::new(ctx.clone(), files, ast, node, &mut uarena, func_id);

        if let Some(impl_) = ast.method_member_mapping[func_id] {
            let impldef = &ast.impls[impl_];
            let file = ast.items[impldef.item].file;
            files.switch_errors_file(file);

            flower.lower_impl_method(impl_, &mut impl_error_sets[impl_]);
        } else {
            flower.ensure_function_is_lowered(true);
        }
    }

    // Report implementation errors
    for (impl_, ierrors) in impl_error_sets {
        let item = ast.impls[impl_].item;
        let file = ast.items[item].file;
        files.switch_errors_file(file);
        ierrors.report(ctx.clone(), node.key, ast, impl_);
    }

    ctx.in_project(node.key, |unit| {
        info!(
            "Finished lowering {} to HIR:\n{}",
            node.config.name, &unit.header
        );
    });
}

// Allocations for a unit in the process of lowering to HIR
#[derive(Default)]
pub struct LoweringArena<'s> {
    // Arenas are shared across items
    exprs: Map<ExprId, Typed<Expr<'s>>>,
    pats: Map<PatternId, Typed<Pattern<'s>>>,

    // Function-specific lowering information
    funcs: Map<key::Func, LoweringFunc<'s>>,

    // Implementation headers and implementation method bodies are lowered in separate passes.
    //
    // Here we 'park' data inbetween those steps.
    impls: Map<key::Impl, LoweringImpl>,
}

pub struct LoweringImpl {
    generic_spans: HashMap<TaggedGeneric, Span>,
}

pub(crate) struct Typed<T> {
    pub span: Span,
    pub ty: Var,
    pub entity: T,
}

impl<T> Typed<T> {
    pub fn new(entity: T, var: Var, span: Span) -> Typed<T> {
        Typed { span, ty: var, entity }
    }
}

macro_rules! as_lowering {
    ($func:expr, $from:literal, $fields:tt => $expr:expr) => {
        match $func {
            LoweringFunc::Lowering $fields => $expr,
            _ => panic!("`{}` called for non-lowering function", $from),
        }
    };
}

impl<'s> LoweringArena<'s> {
    pub fn new(func_cap: usize) -> Self {
        Self {
            exprs: Map::new(),
            pats: Map::new(),
            funcs: (0..func_cap).map(|_| LoweringFunc::Pending).collect(),
            impls: Map::new(),
        }
    }

    #[track_caller]
    pub fn lambda_mut(&mut self, f: key::Func, lambda: key::Lambda) -> &mut LoweringLambda<'s> {
        as_lowering!(&mut self.funcs[f], "lambda_mut", { lambdas, .. } => lambdas[lambda].as_mut().unwrap())
    }

    #[track_caller]
    pub fn lambda(&self, f: key::Func, lambda: key::Lambda) -> &LoweringLambda<'s> {
        as_lowering!(&self.funcs[f], "lambda", { lambdas, .. } => lambdas[lambda].as_ref().unwrap() )
    }

    pub fn reserve_lambda_key(&mut self, f: key::Func) -> key::Lambda {
        as_lowering!(&mut self.funcs[f], "next_lambda_key", { lambdas, .. } => lambdas.push(None))
    }

    pub fn park_impl(&mut self, impl_: key::Impl, lowering_impl: LoweringImpl) {
        assert_eq!(
            impl_,
            self.impls.push(lowering_impl),
            "implementations lowered out of order"
        );
    }

    pub fn set_preprocessed(
        &mut self,
        f: key::Func,
        generic_spans: HashMap<TaggedGeneric, Span>,
        signature_spans: Vec<Span>,
    ) {
        self.funcs[f] = LoweringFunc::Prepassed { generic_spans, signature_spans };
    }

    pub fn take_preprocessed(
        &mut self,
        f: key::Func,
    ) -> Option<(HashMap<TaggedGeneric, Span>, Vec<Span>)> {
        match &mut self.funcs[f] {
            LoweringFunc::Prepassed { generic_spans, signature_spans } => {
                let gspans = std::mem::take(generic_spans);
                let sspans = std::mem::take(signature_spans);
                self.funcs[f] = LoweringFunc::Pending;
                Some((gspans, sspans))
            }
            _ => None,
        }
    }

    pub fn set_lambda(
        &mut self,
        f: key::Func,
        l: key::Lambda,
        parked_scope: Option<(Vec<(&'s str, Bind)>, Vec<Bind>)>,
        params: Option<EntityList<Var>>,
        ret: Var,
    ) {
        as_lowering!(&mut self.funcs[f], "finish_lambda", { lambdas, .. } => {
            assert!(lambdas[l].is_none());
            lambdas[l] = Some(LoweringLambda::new(parked_scope, params, ret, None));
        });
    }

    pub fn is_trait_method(&self, f: key::Func) -> bool {
        matches!(&self.funcs[f], LoweringFunc::Method)
    }

    pub fn expr_pool(&self, f: key::Func) -> &key::ListPool<ExprId> {
        as_lowering!(&self.funcs[f], "expr_pool", { expr_pool, .. } => expr_pool)
    }
    pub fn expr_pool_mut(&mut self, f: key::Func) -> &mut key::ListPool<ExprId> {
        as_lowering!(&mut self.funcs[f], "expr_pool", { expr_pool, .. } => expr_pool)
    }

    pub fn pat_pool(&self, f: key::Func) -> &key::ListPool<PatternId> {
        match &self.funcs[f] {
            LoweringFunc::SemiLowering { pat_pool } | LoweringFunc::Lowering { pat_pool, .. } => {
                pat_pool
            }
            _ => panic!("`pat_pool` called on non-lowering function"),
        }
    }
    pub fn pat_pool_mut(&mut self, f: key::Func) -> &mut key::ListPool<PatternId> {
        match &mut self.funcs[f] {
            LoweringFunc::SemiLowering { pat_pool } | LoweringFunc::Lowering { pat_pool, .. } => {
                pat_pool
            }
            _ => panic!("`pat_pool` called on non-lowering function"),
        }
    }

    pub fn params(&self, f: key::Func, lambda: Option<key::Lambda>) -> EntityList<Var> {
        match lambda {
            Some(lambda) => self.lambda(f, lambda).params.clone().unwrap(),
            None => as_lowering!(&self.funcs[f], "params", { params, .. } => params.clone()),
        }
    }

    #[track_caller]
    pub fn ret(&self, f: key::Func, lambda: Option<key::Lambda>) -> Var {
        match lambda {
            Some(lambda) => self.lambda(f, lambda).ret,
            None => as_lowering!(&self.funcs[f], "ret", { ret, .. } => *ret),
        }
    }

    pub fn semi_init_func(&mut self, f: key::Func) {
        assert!(matches!(self.funcs[f], LoweringFunc::Pending));
        self.funcs[f] = LoweringFunc::SemiLowering { pat_pool: key::ListPool::new() };
    }

    pub fn init_func(&mut self, f: key::Func, params: EntityList<Var>, ret: Var, pre: bool) {
        let pat_pool = match std::mem::replace(&mut self.funcs[f], LoweringFunc::Pending) {
            LoweringFunc::SemiLowering { pat_pool } => pat_pool,
            LoweringFunc::Pending => key::ListPool::new(),
            _ => panic!("cannot initialize function that is already lowering"),
        };

        self.funcs[f] = LoweringFunc::Lowering {
            preprocessed: pre,
            lambdas: Map::new(),
            params,
            ret,
            expr_pool: key::ListPool::new(),
            pat_pool,
        };
    }

    pub fn finish_func(
        &mut self,
        f: key::Func,
        static_types: Map<Var, Type>,
        entrypoint: Option<ExprId>,
    ) {
        as_lowering!(&mut self.funcs[f], "finish_func", { expr_pool, pat_pool, lambdas, .. } => {
            let expr_pool = std::mem::take(expr_pool);
            let pat_pool = std::mem::take(pat_pool);

            self.funcs[f] =
                LoweringFunc::Lowered { expr_pool, pat_pool, entrypoint, static_types };
        })
    }

    pub fn finish_method(&mut self, f: key::Func) {
        match &mut self.funcs[f] {
            r @ LoweringFunc::Pending => *r = LoweringFunc::Method,
            _ => panic!("`finish_method` called on non-pending function"),
        }
    }
}

#[derive(Debug)]
pub enum LoweringFunc<'s> {
    Pending,
    // When a function is un-annotated, the type signature is derived from the pattern types.
    //
    // This means that the pattern pool needs to be initialized before the LoweringFunc::Lowering can be.
    SemiLowering {
        pat_pool: key::ListPool<PatternId>,
    },
    Lowering {
        preprocessed: bool,

        // `None` is when the lambda key is reserved, as lowering one lambda may encounter
        // additional lambdas.
        lambdas: Map<key::Lambda, Option<LoweringLambda<'s>>>,

        // Types prior to inference finalization.
        params: EntityList<Var>,
        ret: Var,

        expr_pool: key::ListPool<ExprId>,
        pat_pool: key::ListPool<PatternId>,
    },
    // This function has had its type signature lowered to allow for dotpipes and mutation recursion.
    Prepassed {
        generic_spans: HashMap<TaggedGeneric, Span>,
        signature_spans: Vec<Span>,
    },
    Lowered {
        expr_pool: key::ListPool<ExprId>,
        pat_pool: key::ListPool<PatternId>,

        static_types: Map<Var, Type>,

        // Trait methods may have `None`
        entrypoint: Option<ExprId>,
    },
    Method,
}

#[derive(new, Debug)]
pub struct LoweringLambda<'s> {
    parked_scope: Option<(Vec<(&'s str, Bind)>, Vec<Bind>)>,

    // None means that this lambda is generated from partial application.
    // With partial application we don't necesarily know the amount of parameters until
    // after type inference has completed.
    params: Option<EntityList<Var>>,
    ret: Var,

    // `None` means that the expression has not yet been lowered but will be.
    entrypoint: Option<ExprId>,

    #[new(default)]
    captures: Vec<Bind>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ExprId(u32);
entity_impl!(ExprId, "expr");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct PatternId(u32);
entity_impl!(PatternId, "pat");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Bind(u32);
entity_impl!(Bind, "b");

/// Complete lower of a function from AST.
pub struct FuncLower<'a, 's> {
    files: &'a Files,
    ast: &'a AST<'s>,
    node: &'a ProjectNode,
    uarena: &'a mut LoweringArena<'s>,
    tctx: TypeLower<InferenceEnv>,
    id: key::Func,
    lambda_instantiations: Vec<(key::Lambda, Var)>,

    where_binding_names: Map<key::Lambda, Tr<&'s str>>,
    scopes: func::Scopes<'s>,
}

fn tydef_sig_lower<'s>(file: key::File, tydef: &ast::TypeDeclaration<'s>) -> TypeSig {
    let generics = tydef
        .header
        .type_params
        .values()
        .map(|name| name.to_string())
        .collect();

    TypeSig { generics, name: tydef.header.name.to_string(), file }
}

fn tydef_kind_lower<'s>(
    mut tctx: TypeLower<StaticEnv>,
    tydef: &ast::TypeDeclaration<'s>,
) -> TypeDef {
    tctx.fenv.entry(tctx.scope).or_default();
    tctx.declare_forall(
        tctx.scope,
        // TODO: Attach spans to generic names instead
        tydef
            .header
            .type_params
            .values()
            .map(|name| (*name).tr(tydef.header.span)),
    );

    match &tydef.body {
        ast::TypeBody::Sum(sum) => TypeDef::Sum {
            vtypes: sum
                .variants
                .values()
                .map(|(_, _, types)| tctx.tys(types).1)
                .collect(),
            vnames: sum
                .variants
                .values()
                .map(|(_, name, _)| name.to_string())
                .collect(),
        },
        ast::TypeBody::Record(record) => TypeDef::Struct {
            ftypes: record
                .fields
                .values()
                .map(|(span, _, ty)| tctx.ty(ty.tr(*span)))
                .collect(),
            fnames: record
                .fields
                .values()
                .map(|(_, name, _)| name.to_string())
                .collect(),
        },
        ast::TypeBody::Trait { methods, associations } => {
            TypeDef::Trait { functions: methods.clone() }
        }
        ast::TypeBody::Alias(dst) => TypeDef::Alias { for_: tctx.ty(dst.as_ref()) },
    }
}

fn trait_method_lower<'s>(
    ast: &AST<'s>,
    mut tctx: TypeLower<StaticEnv>,
    func: key::Func,
    decl: &parser::func::Declaration<'s>,
    uarena: &mut LoweringArena<'s>,
    method_of: (key::Type, key::Method),
) {
    {
        let (when, lambdas) = ast.functions[func].when_bindings();
        tctx.lower_and_include_when_constraints(when);
        tctx.lower_and_include_when_constraints(when);
        tctx.lower_and_include_lambda_when_constraints(lambdas);
    }

    let emsg = || "methods in trait declaration must have explicit type annotation";
    let Some((params, ret, _)) = tctx.mandatory_typing(&decl.header, emsg()) else {
        // We leave it as `None` in the HeaderFile
        return;
    };

    let sig = FuncSig { forall: tctx.take_forall(GenericTag::Func), params, ret };

    uarena.finish_method(func);

    let prec = ast.functions[func]
        .attr
        .precedence
        .unwrap_or(DEFAULT_OPERATOR_PRECEDENCE);

    tctx.ctx.in_project_mut(tctx.project, |unit| {
        let has_body = decl.body.is_some();
        let name = (*decl.header.name).to_string();
        unit.header
            .set_func(tctx.file, func, name, sig, Some(method_of), prec, has_body);
    });
}

// Even private functions may need to be prepassed to enable circular recursion.
fn can_prepass<'s>(ast: &AST<'s>, func: key::Func) -> bool {
    fn can_prepass_tys<'s>(tys: &[Tr<parser::Type<'s>>]) -> bool {
        tys.iter().all(|ty| can_prepass_ty(&**ty))
    }

    fn can_prepass_ty<'s>(ty: &parser::Type<'s>) -> bool {
        match ty {
            parser::Type::Closure(params, ret) | parser::Type::FnPointer(params, ret) => {
                can_prepass_tys(params) && can_prepass_ty(ret)
            }
            parser::Type::Pointer(inner) => can_prepass_ty(inner),
            parser::Type::Defined(apath, elems) => {
                apath.path.as_slice() != &["_"] && can_prepass_tys(&**elems)
            }
            parser::Type::Tuple(elems) | parser::Type::List(elems, _) => can_prepass_tys(elems),
            parser::Type::Poison => true,
        }
    }

    let funcdef = &ast.functions[func];
    funcdef
        .header
        .typing
        .as_ref()
        .map(|typing| can_prepass_tys(&typing.ptypes) && can_prepass_ty(&*typing.returns))
        .unwrap_or(false)
}

fn prepass_function_signature<'s>(
    ctx: Context<TranslationUnit>,
    ast: &AST<'s>,
    node: &ProjectNode,
    uarena: &mut LoweringArena<'s>,
    func: key::Func,
) {
    let funcdef = &ast.functions[func];
    let item = &ast.items[funcdef.item];

    if item.attr.public || can_prepass(ast, func) {
        if let ast::ItemKind::Func(_) = item.kind {
            let tag = GenericTag::Func;
            let env = StaticEnv {
                self_: None,
                invalid_inference_reason_given: "public functions",
            };
            let mut tctx = TypeLower::new(ctx.clone(), node.key, item.file, env, tag);

            let (when, _) = funcdef.when_bindings();

            if let Some(impl_) = ast.method_member_mapping[func].as_ref() {
                todo!("are we meant to also prepass impl methods? Surely not right?");
                // let impl_when = &ast.impls[*impl_].header.when;
                // todo!();
            }

            tctx.lower_and_include_when_constraints(when);

            let error = "public functions must have a type signature";
            let Some((params, ret, sspans)) = tctx.mandatory_typing(&funcdef.header, error) else {
                return;
            };

            let dominant = params.last().cloned();

            let sig = FuncSig {
                forall: std::mem::take(tctx.fenv.get_mut(&tag).unwrap()),
                params,
                ret,
            };

            let prec = funcdef
                .attr
                .precedence
                .unwrap_or(DEFAULT_OPERATOR_PRECEDENCE);

            ctx.in_project_mut(node.key, |unit| {
                let symbol = funcdef.header.name.to_string();
                let has_body = funcdef.body.is_some();
                unit.header
                    .set_func(item.file, func, symbol, sig, None, prec, has_body);
            });

            let name = *funcdef.header.name;
            let origin = symbols::Origin::Intra;
            if !matches!(item.kind, ast::ItemKind::Val(_)) {
                update_dotcall_lookup(&ctx, node, dominant.as_ref(), origin, func, name);
            }

            uarena.set_preprocessed(func, tctx.generic_spans, sspans);
        };
    };
}

fn update_dotcall_lookup(
    ctx: &Context<TranslationUnit>,
    node: &ProjectNode,
    receiver: Option<&Type>,
    origin: symbols::Origin,
    func: key::Func,
    fname: impl Into<String>,
) {
    let Some(receiver) = receiver else {
        return;
    };

    let root = ts::KnownTypeRoot::from_known(receiver, |ident| {
        matches!(ident.origin, symbols::Origin::Intra).then(|| ident.key)
    });

    let is_candidate = match root {
        Some(root @ ts::KnownTypeRoot::Defined(_)) => Some(root),
        None => None,
        // allow dotcall insertion on primitive types in standard libraries
        Some(root) => (node.config.name == "std").then(|| root),
    };

    if let Some(root) = is_candidate {
        ctx.in_project_mut(node.key, |unit| {
            unit.header
                .symbols
                .insert_dotcall_lookup(root, fname, (origin, func))
        });
    }
}

fn hack_define_self<'s>(key: key::Type, header: &parser::ty::Header<'s>) -> Type {
    Type::Defined(
        TypeKey { origin: symbols::Origin::Intra, key },
        header
            .type_params
            .keys()
            .map(|g| Type::Generic(TaggedGeneric::new(GenericTag::Type, g)))
            .collect(),
    )
}

fn langitem_lower<'s>(
    ctx: Context<TranslationUnit>,
    project: key::Project,
    file: key::File,
    name: Tr<&str>,
    ty: &parser::Type<'s>,
) {
    let env = StaticEnv::new("lang item");
    let tag = GenericTag::Type;
    let mut tctx = TypeLower::new(ctx.clone(), project, file, env, tag);

    let Type::Defined(TypeKey { origin: symbols::Origin::Intra, key }, _) =
        tctx.ty((ty).tr(name.span))
    else {
        errors::err("invalid attribute")
            .line(name.span, "lang item must be defined type")
            .emit();

        return;
    };

    match *name {
        "Listable" => {
            ctx.in_project_mut(project, |unit| unit.langitems.default_listable = Some(key))
        }
        "String" => ctx.in_project_mut(project, |unit| unit.langitems.default_string = Some(key)),
        _ => {
            errors::err("invalid attribute")
                .line(name.span, format!("no lang item named {name}"))
                .emit();
        }
    }
}
