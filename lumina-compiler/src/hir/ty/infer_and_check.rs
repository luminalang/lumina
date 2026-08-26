use super::{
    InferenceEnv, Type, TypeKey, TypeLower,
    inst::{ImplicitAnnotationLowering, Instantiation},
};
use crate::key;
use crate::{
    Context, Files, InterResolved, ProjectNode, TranslationUnit, ast::AST, errors,
    hir::LoweringArena, hir::func, project::TypeDef, symbols, symbols::Namespace,
};
use itertools::Itertools;
use lumina_key::Map;
use lumina_typesystem as ts;
use lumina_typesystem::{
    Application, ForallEnv, GenericTag, KnownTypeFormatter, ResolverError, TaggedGeneric, Var,
};
use lumina_util::Span;
use std::collections::HashMap;
use tracing::info;

// TODO: Switch to using configurable color management
use owo_colors::OwoColorize;

impl TypeLower<InferenceEnv> {
    pub fn type_check_and_finalize<'s>(
        &mut self,
        ast: &AST<'s>,
        node: &ProjectNode,
        files: &Files,
        uarena: &mut LoweringArena<'s>,
        impl_check: Option<Application>,
        func: key::Func,
    ) -> (
        Map<Var, Type>,
        Vec<(ts::Error<TypeKey>, ForallEnv<TypeKey>)>,
    ) {
        let resolver = TypeCheckPassHandler {
            ast,
            node,
            files,
            uarena,
            ctx: self.ctx.clone(),
            fenv: &mut self.fenv,
            spans: &self.env.spans,
            project: self.project,
            file: self.file,
            func,
        };

        let (listables, default_listable) = self.ctx.collect_listables(self.project);

        let mut inference = ts::InferenceUnifier::new(
            &mut self.env.env,
            resolver,
            self.default_int_size,
            listables,
            default_listable,
        );
        inference.infer();

        let vars = ts::Finalizer::new(&mut self.env.env).finalize_all();
        let errors = ts::Checker::new(&vars, &self.env.env).type_check();

        info!(
            "Finalized Static Types\n  {}",
            vars.iter()
                .map(|(var, type_)| format!("{var} => {type_}"))
                .format("\n  ")
        );

        // For implementations, we want to collect type errors against the expected trait method
        // signature. That way we can better present the errors as a single "bad implementation" error.
        let mut impl_method_error_buffer = vec![];

        for err in errors {
            let ident_formatter = |ident: &TypeKey| {
                let unstable_key = self.ctx.resolve_origin(self.project, ident.origin);
                self.ctx.in_project(unstable_key, |unit| {
                    match unit.header.type_signatures.get(ident.key) {
                        Some(sig) => sig.name.to_string(),
                        None => format!("{}·{unstable_key}·{}", ident.origin, ident.key),
                    }
                })
            };
            let generic_formatter =
                |generic: TaggedGeneric| self.fenv[&generic.tag].names[generic.key].clone();
            let fmt = KnownTypeFormatter::new(&ident_formatter, &generic_formatter, &());

            match err {
                ts::Error::NonFunctionApplication(cappl, _) if impl_check == Some(cappl) => {
                    impl_method_error_buffer.push((err, self.fenv.clone()))
                }

                ts::Error::NonFunctionApplication(appl, type_) => {
                    let ret = self.env.env.get_return_type(appl);

                    errors::err("type mismatch")
                        .line(
                            self.env.spans[ret],
                            format!(
                                "cannot give parameters to non-function `{}`",
                                fmt.fork(&type_)
                            ),
                        )
                        .emit();
                }
                ts::Error::Mismatch { expected, given, kind, var } => {
                    errors::err("type mismatch")
                        .line(
                            self.env.spans[var],
                            match kind {
                                ts::MismatchKind::Parameter(application, i) => {
                                    let func = self.env.env.get_appl_function(application);
                                    let fname = errors::get_str(self.env.spans[func]);

                                    let descr = lumina_util::gramatically_correct_numbered(i + 1);

                                    if fname.chars().all(|c| !b"(\\".contains(&(c as u8))) {
                                        format!("{descr} parameter to `{fname}`",)
                                    } else {
                                        format!("{descr} parameter")
                                    }
                                }
                                ts::MismatchKind::SameasExpr(_) => {
                                    "branch is of a different type than the rest of the branches"
                                        .into()
                                }
                                ts::MismatchKind::SameasList(_) => {
                                    "element is of a different type than the rest of the list"
                                        .into()
                                }
                                ts::MismatchKind::Assignment => "".into(),
                            },
                        )
                        .text(format!("{}      {}", "got".purple(), fmt.fork(&given)))
                        .text(format!("{} {}", "expected".purple(), fmt.fork(&expected)))
                        .emit();
                }
                ts::Error::FunctionWrongParameterCount { expected, given, appl } => {
                    let ret = self.env.env.get_return_type(appl);

                    errors::err("type mismatch")
                        .line(self.env.spans[ret], "wrong number of parameters")
                        .text(format!("{}      {}", "got".purple(), given))
                        .text(format!("{} {}", "expected".purple(), expected))
                        .emit();
                }
                ts::Error::DoesNotHaveFields(var, record, fields) => {
                    errors::err("type mismatch")
                        .line(
                            self.env.spans[var],
                            format!(
                                "`{}` does not have fields {}",
                                fmt.fork(&record),
                                fields.iter().format(", ")
                            ),
                        )
                        .emit();
                }

                ts::Error::UnknownFunction(func) => {
                    errors::err("inference error")
                        .line(
                            self.env.spans[func],
                            "cannot infer type signature of partial application",
                        )
                        .emit();
                }
                ts::Error::NoListType(var) => {
                    errors::err("inference error")
                        .line(self.env.spans[var], "no list lang item available")
                        .emit();
                }
                ts::Error::UnresolvedFunction { func, dominant } => {
                    let dominant = fmt.fork(&vars[dominant]);
                    errors::err("function not found")
                        .line(
                            self.env.spans[func],
                            format!("function not in scope and isn't defined for {dominant}"),
                        )
                        .emit();
                }
                ts::Error::CircularInference { func } => {
                    let span = self.env.spans[func];
                    errors::err("inference error")
                        .line(span, "pipe results in circular inference")
                        .emit();
                }
            }
        }

        (vars, impl_method_error_buffer)
    }
}

struct TypeCheckPassHandler<'a, 's> {
    files: &'a Files,
    ast: &'a AST<'s>,
    ctx: Context<TranslationUnit>,
    node: &'a ProjectNode,
    uarena: &'a mut LoweringArena<'s>,
    fenv: &'a mut ts::ForallEnv<TypeKey>,
    spans: &'a key::SecondaryMap<ts::Var, Span>,
    project: key::Project,
    file: key::File,
    func: key::Func,
}

impl<'a, 's> ts::Resolver<TypeKey> for TypeCheckPassHandler<'a, 's> {
    type ResolvedMetadata = symbols::Origin;

    fn implicitly_declare_generic(&mut self, tag: GenericTag) -> TaggedGeneric {
        ts::implicitly_declare(self.fenv, tag)
    }

    fn type_of_field(
        &self,
        ident: &TypeKey,
        field: key::Field,
    ) -> Result<(symbols::Origin, Type), ResolverError<TypeKey>> {
        self.ctx.in_origin(self.project, ident.origin, |unit| {
            match &unit.header.typedefs[ident.key] {
                TypeDef::Struct { ftypes, .. } => match ftypes.get(field) {
                    Some(ty) => Ok((symbols::Origin::Intra, ty.clone())),
                    None => Err(ResolverError::FieldNotFound(*ident, field)),
                },
                _ => Err(ResolverError::RecordNotFound(*ident)),
            }
        })
    }

    fn guess_by_fields<'b, I>(&self, var: ts::Var, fields: I) -> Option<TypeKey>
    where
        I: Iterator<Item = &'b str> + Clone,
    {
        let mut possibilities = HashMap::new();

        for field in fields.clone() {
            self.ctx.in_project(self.project, |unit| {
                match unit
                    .header
                    .symbols
                    .find(self.file, symbols::Namespace::Fields, &[field])
                {
                    symbols::Resolve::Item(symbols::WithOrigin {
                        key: symbols::Item::Field(ty, _),
                        origin,
                        ..
                    }) => {
                        let tkey = TypeKey { origin, key: ty };
                        *possibilities.entry(tkey).or_insert(0) += 1;
                    }
                    // we intentionally ignore invalid fields as type system will uncover those later
                    _ => {}
                }
            });
        }

        let mut winner = None;
        let mut tie = None;
        for (&tkey, &count) in &possibilities {
            match winner.as_mut() {
                None => winner = Some((tkey, count)),
                Some(winner) => {
                    if winner.1 == count {
                        tie = Some((tkey, count));
                    } else if winner.1 < count {
                        *winner = (tkey, count);
                        tie = None;
                    }
                }
            }
        }

        let span = self.spans[var];

        if let Some(tie) = tie {
            let winner = winner.unwrap();

            let tied_name = self.ctx.name_of_type(self.project, tie.0);
            let winner_name = self.ctx.name_of_type(self.project, winner.0);

            errors::err("inference error")
                .line(
                    span,
                    format!("fields could belong to either type {winner_name} or {tied_name}"),
                )
                .emit();

            None
        } else if let Some(winner) = winner {
            Some(winner.0)
        } else {
            errors::err("inference error")
                .line(
                    span,
                    format!(
                        "no record with the field(s) {} in scope",
                        fields.format(", ")
                    ),
                )
                .emit();

            None
        }
    }

    fn get_field(&self, ident: &TypeKey, name: &str) -> Option<key::Field> {
        let project = self.ctx.resolve_origin(self.project, ident.origin);
        self.ctx
            .in_project(project, |unit| match &unit.header.typedefs[ident.key] {
                TypeDef::Struct { fnames, .. } => fnames
                    .iter()
                    .find_map(|(field, n)| (n == name).then_some(field)),
                _ => None,
            })
    }

    fn for_each_type_parameter<F>(&self, ident: &TypeKey, mut f: F)
    where
        F: FnMut(key::Generic),
    {
        self.ctx
            .clone()
            .in_origin(self.project, ident.origin, |unit| {
                for (generic, _) in &unit.header.type_signatures[ident.key].generics {
                    f(generic);
                }
            });
    }

    // PROCEED WITH CAUTION
    //
    // The type system needs to call into the compilers name resolution in order to resolve which
    // function to call by the receiver type (due to dotpipes). This occurs in the middle of type
    // inference according to our micropass inference design.
    //
    // So far that seems completely normal. However, if the function is *not* found by the receiver
    // type, we need to fallback to ordinary name resolution. As the dotpipe would still be valid when
    // invoking a function that is in the current file's scope.
    //
    // But with normal name resolution we may resolve a function which does *not* have a type
    // signature. If this happens, in order to continue type inference, we need to recursively lower
    // that function to HIR first before continuing inference with the current one.
    //
    // This is why we pass so much context into the `TypeCheckPassHandler`. Inside of this method,
    // invoked by one of the inference passes in the middle of type inference, we will completely
    // switch context and lower the child function before later continuing on type inference.
    fn receiver_lookup(
        &mut self,
        env: &mut ts::Environment<TypeKey>,
        root: ts::KnownTypeRoot<TypeKey>,
        name: &str,
    ) -> ts::ReceiverLookupResult {
        let (origin, root) = match root {
            ts::KnownTypeRoot::Defined(tkey) => (tkey.origin, ts::KnownTypeRoot::Defined(tkey.key)),
            _ => {
                let Some(std) = self
                    .ctx
                    .in_project(self.project, |unit| unit.header.stdlib())
                else {
                    return ts::ReceiverLookupResult::NotFound;
                };

                (std, root.map(|_| unreachable!()))
            }
        };

        let dotcall_discovery = match root {
            ts::KnownTypeRoot::Defined(_) => self
                .ctx
                .in_origin(self.project, origin, |unit| {
                    unit.header.symbols.dotcall_lookup(root, name)
                })
                .map(|(forigin, func)| {
                    let origin = self.ctx.map_origin(self.project, origin, forigin);
                    self.get_func(env, origin, func)
                }),
            _ => self
                .ctx
                .in_project(self.project, |unit| unit.header.stdlib())
                .and_then(|std| {
                    self.ctx
                        .in_origin(self.project, std, |unit| {
                            unit.header.symbols.dotcall_lookup(root, name)
                        })
                        .map(|(forigin, func)| {
                            let origin = self.ctx.map_origin(self.project, origin, forigin);
                            self.get_func(env, origin, func)
                        })
                }),
        };

        if let Some(discovery) = dotcall_discovery {
            return discovery;
        }

        // Fallback to normal name resolution
        let ns = Namespace::Functions;
        match self
            .ctx
            .find(self.project, None, self.file, ns, &[name], true)
        {
            InterResolved::Item(origin, symbols::Item::Func(func)) => {
                if let symbols::Origin::Intra = origin {
                    let result = func::FuncLower::new(
                        self.ctx.clone(),
                        self.files,
                        self.ast,
                        self.node,
                        self.uarena,
                        func,
                    )
                    .ensure_function_is_lowered(false);

                    if let func::LowerResult::CircularInference = result {
                        return ts::ReceiverLookupResult::CircularInference;
                    }
                }

                self.get_func(env, origin, func)
            }
            InterResolved::Item(origin, symbols::Item::Variant(type_, variant)) => {
                self.ctx.in_origin(self.project, origin, |unit| {
                    let sig = &unit.header.type_signatures[type_];
                    let generics = sig.generics.keys();

                    let anot = ImplicitAnnotationLowering::new(env)
                        .keys(GenericTag::Type, generics)
                        .finish();

                    let (params, tparams) =
                        Instantiation::new(self.ctx.clone(), self.project, origin, env, anot)
                            .variant(type_, variant);

                    let type_key = TypeKey { key: type_, origin };

                    let type_ = env.defined(type_key, tparams);

                    ts::ReceiverLookupResult::Func { params, ret: type_ }
                })
            }
            InterResolved::Builtin(_) => {
                todo!("dotpipe into builtin");
                // let plen = super::params_of_builtin(&builtin)?;

                // let params = self.t

                // Some((symbols::Origin::Intra, None, ts::Forall::new(), params, ret))
            }
            _ => ts::ReceiverLookupResult::NotFound,
        }
    }

    fn map_type_key(&self, in_: symbols::Origin, ident: &TypeKey) -> TypeKey {
        self.ctx.inst_type_key(self.project, in_, *ident)
    }
}

impl<'a, 's> TypeCheckPassHandler<'a, 's> {
    fn get_func(
        &mut self,
        env: &mut ts::Environment<TypeKey>,
        origin: symbols::Origin,
        func: key::Func,
    ) -> ts::ReceiverLookupResult {
        if symbols::Origin::Intra == origin && func == self.func {
            let mut anot_lower = ImplicitAnnotationLowering::new(env);

            if let Some(forall) = self.fenv.get(&GenericTag::Trait) {
                anot_lower.forall(GenericTag::Trait, forall);
            }

            let anot = anot_lower
                .forall(GenericTag::Func, &self.fenv[&GenericTag::Func])
                .unknown_self_if(self.fenv.contains_key(&GenericTag::Trait))
                .finish();

            let (params, ret) =
                Instantiation::new(self.ctx.clone(), self.project, origin, env, anot)
                    .self_recursion(self.uarena, func);

            return ts::ReceiverLookupResult::Func { params, ret };
        }

        self.ctx.in_origin(self.project, origin, |unit| {
            let func = &unit.header.func(func);

            let mut anot_lower = ImplicitAnnotationLowering::new(env);

            if let Some((trait_, _)) = func.method_of {
                let keys = unit.header.type_signatures[trait_].generics.keys();
                anot_lower.keys(GenericTag::Trait, keys);
            }

            let anot = anot_lower
                .forall(GenericTag::Func, &func.sig.forall)
                .unknown_self_if(func.method_of.is_some())
                .finish();

            let (params, ret) =
                Instantiation::new(self.ctx.clone(), self.project, origin, env, anot)
                    .foreign_func(func);

            ts::ReceiverLookupResult::Func { params, ret }
        })
    }
}
