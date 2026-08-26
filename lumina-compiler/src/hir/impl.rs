use super::{
    FuncLower, ImplDef, LoweringImpl, ProjectNode, StaticEnv, Type, TypeKey, TypeLower, ast,
    errors, key, symbols, update_dotcall_lookup,
};
use crate::{Context, TranslationUnit, hir::ty::LoweringKind as _};
use itertools::Itertools;
use lumina_key::{Map, SecondaryMap};
use lumina_typesystem::{self as ts, GenericTag, TaggedGeneric, Var};
use lumina_util::{Span, Spanned as _};
use owo_colors::OwoColorize;
use std::collections::HashMap;
use tracing::{info, trace};

pub struct Errors {
    applications: SecondaryMap<ts::Application, Option<key::Method>>,

    pub impl_: key::Impl,
    pub spans: SecondaryMap<Var, Span>,

    unknown_methods: Vec<key::Method>,
    missing_methods: Vec<key::Method>,
    trait_check_errors: Vec<(ts::Error<TypeKey>, ts::ForallEnv<TypeKey>)>,
    unknown_trait: Option<Type>,
}

impl Errors {
    fn new(impl_: key::Impl) -> Self {
        Self {
            applications: Default::default(),
            spans: SecondaryMap::with_default(Span::null()),
            impl_,
            unknown_methods: Default::default(),
            missing_methods: Default::default(),
            trait_check_errors: Default::default(),
            unknown_trait: None,
        }
    }
}

pub fn lower_impl_header<'s>(
    node: &ProjectNode,
    mut tctx: TypeLower<StaticEnv>,
    ast: &ast::AST<'s>,
    impl_: key::Impl,
) -> (Errors, LoweringImpl) {
    tctx.fenv.entry(ts::GenericTag::Impl).or_default();

    let impldef = &ast.impls[impl_].decl;

    tctx.scope = GenericTag::Impl;
    tctx.lower_and_include_when_constraints(&impldef.header.when);

    // Lower `self` in the scope of the `GenericTag::Impl`
    let self_ = tctx.ty(impldef.header.impltor.as_ref());

    tctx.scope = GenericTag::Func;

    let project = tctx.project;
    let ctx = tctx.ctx.clone();

    let mut ierrs = Errors::new(impl_);

    let mut lowering_impl = LoweringImpl { generic_spans: HashMap::new() };

    match tctx.ty(impldef.header.trait_.as_ref()) {
        Type::Defined(trait_, trait_params) => {
            let impldef = {
                let unit_guard = ctx.get_origin(project, trait_.origin);
                let trait_origin_unit = unit_guard.as_ref().unwrap();
                let trait_methods = trait_origin_unit.header.as_trait(trait_.key).unwrap();

                let get_match = |trait_func, impl_method, impl_func| {
                    let iname = *ast.functions[impldef.methods[impl_method]].header.name;

                    let tfunc = &trait_origin_unit.header.function_signatures[trait_func]
                        .as_ref()
                        .unwrap();

                    (iname == tfunc.name).then_some(impl_func)
                };

                let method_has_default = |trait_func| {
                    trait_origin_unit.header.function_signatures[trait_func]
                        .as_ref()
                        .unwrap()
                        .has_body
                };

                let methods = trait_methods
                    .iter()
                    .map(|(trait_method, trait_func)| {
                        let impl_func =
                            impldef.methods.iter().find_map(|(impl_method, impl_func)| {
                                get_match(*trait_func, impl_method, *impl_func)
                            });

                        if impl_func.is_none() && !method_has_default(*trait_func) {
                            ierrs.missing_methods.push(trait_method);
                        }

                        impl_func
                    })
                    .collect::<Map<key::Method, Option<key::Func>>>();

                ierrs.unknown_methods = impldef
                    .methods
                    .iter()
                    .filter_map(|(impl_method, impl_func)| {
                        trait_methods
                            .values()
                            .all(|trait_func| {
                                get_match(*trait_func, impl_method, *impl_func).is_none()
                            })
                            .then_some(impl_method)
                    })
                    .collect();

                // If the trait is public and `self` is a receiver, add this method to dotpipe lookup
                let trait_is_public = match trait_.origin {
                    symbols::Origin::Intra => ast.items[ast.types[trait_.key].item].attr.public,
                    symbols::Origin::Inter(_) => true,
                };

                drop(unit_guard);

                if trait_is_public {
                    for (trait_method, im) in methods.iter() {
                        let Some(method) = im else {
                            continue;
                        };

                        if let Some(func) = ctx.in_origin(project, trait_.origin, |unit| {
                            let trait_func = unit.header.method(trait_.key, trait_method);
                            unit.header
                                .func(trait_func)
                                .sig
                                .params
                                .get(0)
                                .and_then(|ty| {
                                    (*ty == Type::Prim(ts::Prim::Self_)).then_some(trait_func)
                                })
                        }) {
                            let fname = *ast.functions[*method].header.name;
                            let origin = trait_.origin;
                            update_dotcall_lookup(&ctx, node, Some(&self_), origin, func, fname);
                        }
                    }
                }

                let forall = std::mem::take(tctx.fenv.get_mut(&GenericTag::Impl).unwrap());

                lowering_impl.generic_spans = std::mem::take(&mut tctx.generic_spans);

                ImplDef {
                    forall,
                    trait_,
                    methods,
                    self_,
                    trait_params: trait_params.values().cloned().collect(),
                }
            };

            ctx.in_project_mut(project, |unit| {
                unit.header.implementations.push(impldef);
            });
        }
        type_ => ierrs.unknown_trait = Some(type_),
    }

    (ierrs, lowering_impl)
}

impl<'a, 's> FuncLower<'a, 's> {
    pub fn apply_trait_template(&mut self, func: key::Func) -> Option<ts::Application> {
        self.ast.method_member_mapping[self.id].and_then(|impl_| {
            self.ctx().in_project(self.tctx.project, |unit| {
                let impldef = &unit.header.implementations[impl_];

                impldef
                    .methods
                    .iter()
                    .find(|(_, func)| Some(self.id) == **func)
                    .map(|(trait_method, _)| {
                        let trait_func =
                            self.ctx()
                                .in_origin(self.project(), impldef.trait_.origin, |unit| {
                                    unit.header.as_trait(impldef.trait_.key).unwrap()[trait_method]
                                });

                        let mname = self.ast.functions[func].header.name;
                        let origin = impldef.trait_.origin;

                        let apath = &lumina_parser::AnnotatedPath::without(
                            lumina_util::Identifier::new(*mname),
                        )
                        .tr(mname.span);

                        let self_ = self
                            .tctx
                            .define(mname.span, |env| env.prim(ts::Prim::Self_));

                        let mut inst = self.tctx.lowering_instantiation();
                        inst.with_self(self_);

                        let fvar = inst.func_or_method(origin, trait_func, apath.as_ref());

                        self.tctx.env.apply(mname.span, fvar)
                    })
            })
        })
    }
}

impl Errors {
    pub fn trait_check_errors(
        &mut self,
        errors: Vec<(ts::Error<TypeKey>, ts::ForallEnv<TypeKey>)>,
        method: key::Method,
        cappl: Option<ts::Application>,
    ) {
        assert!(self.trait_check_errors.is_empty());
        if let Some(cappl) = cappl {
            self.applications[cappl] = Some(method);
        }
        self.trait_check_errors = errors;
    }

    pub fn report<'s>(
        self,
        ctx: Context<TranslationUnit>,
        project: key::Project,
        ast: &ast::AST<'s>,
        impl_: key::Impl,
    ) {
        let mut failures =
            self.unknown_methods.len() + self.missing_methods.len() + self.trait_check_errors.len();

        if self.unknown_trait.is_some() {
            failures += 1;
        }

        if failures == 0 {
            return;
        }

        let mut err = errors::err("invalid implementation");

        info!("reporting errors of {impl_}", impl_ = impl_.to_string());

        let impl_unit_guard = ctx.get_unit(project);
        let impl_header_file = &impl_unit_guard.as_ref().unwrap().header;
        let impldef = &impl_header_file.implementations[impl_];

        let trait_unit_guard = ctx.get_origin(project, impldef.trait_.origin);
        let trait_header_file = &trait_unit_guard.as_ref().unwrap().header;
        let trait_name = &trait_header_file.type_signatures[impldef.trait_.key].name;
        let trait_methods = trait_header_file.as_trait(impldef.trait_.key).unwrap();

        err = err.line(ast.impls[impl_].header.span, "");

        let ident_formatter = |ident: &TypeKey| {
            let unstable_key = ctx.resolve_origin(project, ident.origin);
            ctx.in_project(unstable_key, |unit| {
                unit.header.type_signatures[ident.key].name.to_string()
            })
        };

        if let Some(ty) = self.unknown_trait {
            let generic_formatter = |generic: TaggedGeneric| {
                assert_eq!(generic.tag, GenericTag::Impl);
                impldef.forall.names[generic.key].clone()
            };
            let fmt = ts::KnownTypeFormatter::new(&ident_formatter, &generic_formatter, &ty);
            err = err.text(format!("`{fmt}` is not a trait"));
        }

        match (
            self.unknown_methods.as_slice(),
            self.missing_methods.as_slice(),
            self.trait_check_errors.as_slice(),
        ) {
            (unknown, missing, type_errors) => {
                if !missing.is_empty() {
                    err = err.text("missing methods");
                    for &missing in missing {
                        let func = trait_methods[missing];
                        let func = trait_header_file.func(func);
                        err = err.text(format!("  {func}"));
                    }
                }

                if !unknown.is_empty() {
                    if !missing.is_empty() {
                        err = err.text("");
                    }
                    err = err.text(format!("methods not part of `{}`", trait_name));
                    for &unknown in unknown {
                        let func = ast.impls[impl_].methods[unknown];
                        let func = impl_header_file.func(func);
                        err = err.text(format!("  {func}"));
                    }
                }

                if !type_errors.is_empty() {
                    if !missing.is_empty() && !unknown.is_empty() {
                        err = err.text("");
                    }

                    for (i, (error, fenv)) in type_errors.iter().enumerate() {
                        let generic_formatter =
                            |generic: TaggedGeneric| fenv[&generic.tag].names[generic.key].clone();
                        let fmt =
                            ts::KnownTypeFormatter::new(&ident_formatter, &generic_formatter, &());

                        if i != 0 {
                            err = err.text("");
                        }

                        match error {
                            ts::Error::Mismatch { expected, given, .. } => {
                                err = err.text(format!(
                                    "{}      {}",
                                    "got".purple(),
                                    fmt.fork(given)
                                ));
                                err = err.text(format!(
                                    "{} {}",
                                    "expected".purple(),
                                    fmt.fork(expected)
                                ));
                            }
                            ts::Error::UnknownFunction(_) => unreachable!(),
                            ts::Error::NonFunctionApplication(_, type_) => {
                                err = err.text(format!(
                                    "cannot give parameters to non-function {}",
                                    fmt.fork(type_)
                                ));
                            }
                            ts::Error::FunctionWrongParameterCount { expected, given, .. } => {
                                err = err.text(format!(
                                    "wrong number of parameters: got {}, expected {}",
                                    given, expected
                                ));
                            }
                            ts::Error::DoesNotHaveFields(_, record, fields) => {
                                err = err.text(format!(
                                    "{} does not have fields {}",
                                    fmt.fork(record),
                                    fields.iter().format(", ")
                                ));
                            }
                            ts::Error::NoListType(var) => {
                                let span = self.spans[*var];
                                err = err.line(span, "no list lang item available");
                            }
                            ts::Error::UnresolvedFunction { .. }
                            | ts::Error::CircularInference { .. } => unreachable!(),
                        }
                    }
                }
            }
        }

        err.emit();
    }
}
