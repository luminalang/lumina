use crate::hir::TypeKey;
use crate::{Context, TranslationUnit, hir, key, project};
use lumina_typesystem as ts;
use std::io;

use serde::Serialize;
use std::collections::HashMap;

pub fn json(ctx: Context<TranslationUnit>, buf: impl io::Write) -> serde_json::Result<()> {
    let export = Export::from_compiled(ctx);
    serde_json::ser::to_writer_pretty(buf, &export)
}

impl Export {
    fn from_compiled(ctx: Context<TranslationUnit>) -> Self {
        Export {
            project_names: ctx
                .project_nodes
                .projects()
                .map(|project| {
                    let config = &ctx.project_nodes.get(project).unwrap().config;
                    (
                        Dependency {
                            name: config.name.to_string(),
                            version: config.version.to_string(),
                        },
                        project.0 as usize,
                    )
                })
                .collect(),
            projects: ctx
                .project_nodes
                .projects()
                .map(|project| Project::from_compiled(ctx.clone(), project))
                .collect(),
        }
    }
}

impl Project {
    fn from_compiled(ctx: Context<TranslationUnit>, project: key::Project) -> Self {
        let root = ctx.project_nodes.root;

        let config = &ctx.project_nodes.get(project).unwrap().config;
        let name = config.name.to_string();
        let version = config.version.to_string();

        Project {
            authors: config.authors.clone(),
            dependencies: ctx
                .get_node(project)
                .config
                .dependencies
                .iter()
                .map(|dep| Dependency {
                    name: dep.name.to_string(),
                    version: dep.version.to_string(),
                })
                .collect(),
            id: project.0 as usize,
            indirect: ctx
                .get_node(root)
                .config
                .dependencies
                .iter()
                .any(|dep| dep.name == name && dep.version == version && !dep.indirect),
            name,
            version,

            items: Items {
                implementations: ctx.in_project(project, |unit| {
                    unit.header
                        .implementations
                        .iter()
                        .map(|(_, impldef)| {
                            Implementation::from_compiled(ctx.clone(), project, impldef)
                        })
                        .collect()
                }),
                funcs: ctx.in_project(project, |unit| {
                    unit.header
                        .function_signatures
                        .iter()
                        .map(|(key, func)| {
                            let func = func
                                .as_ref()
                                .unwrap_or_else(|| panic!("function {key} is unlowered"));
                            FunctionDef::from_compiled(ctx.clone(), project, &unit.header, func)
                        })
                        .collect()
                }),
                types: ctx.in_project(project, |unit| {
                    unit.header
                        .type_signatures
                        .iter()
                        .map(|(key, sig)| {
                            TypeDef::from_compiled(
                                ctx.clone(),
                                project,
                                sig,
                                unit.header.typedefs[key].clone(),
                            )
                        })
                        .collect()
                }),
            },
        }
    }
}

struct TypeSimplification<'a> {
    ctx: Context<TranslationUnit>,
    fenv: &'a ts::ForallEnv<TypeKey>,
    current: key::Project,
}

impl<'a> TypeSimplification<'a> {
    fn type_key(&self, tkey: TypeKey) -> (ProjectId, TypeId) {
        let project = match tkey.origin {
            project::symbols::Origin::Intra => self.current,
            project::symbols::Origin::Inter(external) => {
                self.ctx.get_node(self.current).ext_as_unstable(external)
            }
        };

        (project.0 as ProjectId, tkey.key.0 as TypeId)
    }

    fn simplify_type(&self, ty: &hir::Type) -> Type {
        match ty {
            hir::Type::Error => Type::Unlinked("_".into()),
            hir::Type::Defined(tkey, params) => {
                let (project, ty) = self.type_key(*tkey);
                let params = self.simplify_types(params.as_values_slice());

                Type::Linked(project, ty, params)
            }
            hir::Type::List(of) => {
                let of = self.simplify_type(of);
                Type::List(Box::new(of))
            }
            hir::Type::Array { of, len } => {
                let of = self.simplify_type(of);
                let len = self.simplify_type(len);
                Type::Array { of: Box::new(of), len: Box::new(len) }
            }
            hir::Type::Tuple(elems) => {
                let elems = self.simplify_types(elems);
                Type::Tuple(elems)
            }
            hir::Type::Generic(generic) => Type::Unlinked(
                self.fenv
                    .get(&generic.tag)
                    .and_then(|forall| forall.names.get(generic.key))
                    .cloned()
                    .unwrap_or_else(|| generic.key.to_string()),
            ),
            hir::Type::Prim(prim) => Type::Unlinked(prim.to_string()),
            hir::Type::Pointer(of) => {
                let of = self.simplify_type(of);
                Type::Pointer(Box::new(of))
            }
            hir::Type::Function { kind, params, ret } => Type::Function {
                is_fn_ptr: *kind == ts::CallableKind::FnPointer,
                params: self.simplify_types(params),
                ret: Box::new(self.simplify_type(ret)),
            },
            hir::Type::Const(const_) => Type::Unlinked(const_.to_string()),
        }
    }

    fn simplify_types(&self, tys: &[hir::Type]) -> Vec<Type> {
        tys.iter().map(|ty| self.simplify_type(ty)).collect()
    }
}

impl TypeDef {
    fn from_compiled(
        ctx: Context<TranslationUnit>,
        project: key::Project,
        sig: &project::TypeSig,
        def: project::TypeDef,
    ) -> Self {
        let tag = match &def {
            project::TypeDef::Trait { .. } => ts::GenericTag::Trait,
            _ => ts::GenericTag::Type,
        };
        let fenv = HashMap::from([(tag, ts::Forall::from_names(&sig.generics))]);
        let simp = TypeSimplification { ctx, fenv: &fenv, current: project };

        Self {
            name: sig.name.clone(),
            generics: sig.generics.values().cloned().collect(),
            def: match def {
                project::TypeDef::Sum { vtypes, vnames } => TypeKind::Variant(
                    vnames
                        .iter()
                        .map(|(variant, name)| {
                            (name.clone(), simp.simplify_types(&vtypes[variant]))
                        })
                        .collect(),
                ),
                project::TypeDef::Struct { ftypes, fnames } => TypeKind::Record(
                    fnames
                        .iter()
                        .map(|(field, name)| (name.clone(), simp.simplify_type(&ftypes[field])))
                        .collect(),
                ),
                project::TypeDef::Trait { functions } => {
                    TypeKind::Trait(functions.values().map(|func| func.0 as FuncId).collect())
                }
                project::TypeDef::Alias { for_ } => TypeKind::Alias(simp.simplify_type(&for_)),
            },
        }
    }
}

impl FunctionDef {
    fn from_compiled(
        ctx: Context<TranslationUnit>,
        project: key::Project,
        header: &project::HeaderFile,
        func: &project::FuncDef,
    ) -> Self {
        let mut fenv = HashMap::from([(ts::GenericTag::Func, func.sig.forall.clone())]);

        if let Some((trait_, _)) = func.method_of {
            let trait_sig = &header.type_signatures[trait_];
            fenv.insert(
                ts::GenericTag::Trait,
                ts::Forall::from_names(&trait_sig.generics),
            );
        }

        let simp = TypeSimplification { ctx, fenv: &fenv, current: project };

        Self {
            name: func.name.clone(),
            parameters: simp.simplify_types(&func.sig.params),
            ret: simp.simplify_type(&func.sig.ret),
        }
    }
}

impl Implementation {
    fn from_compiled(
        ctx: Context<TranslationUnit>,
        project: key::Project,
        impldef: &project::ImplDef,
    ) -> Self {
        let fenv = HashMap::from([(ts::GenericTag::Impl, impldef.forall.clone())]);
        let simp = TypeSimplification { ctx, fenv: &fenv, current: project };
        let (trait_project, trait_) = simp.type_key(impldef.trait_);

        Self {
            trait_: (trait_project, trait_),
            trait_params: simp.simplify_types(&impldef.trait_params),
            self_: simp.simplify_type(&impldef.self_),
            methods: impldef
                .methods
                .values()
                .filter_map(|func| func.map(|func| func.0 as FuncId))
                .collect(),
        }
    }
}

#[derive(Serialize)]
struct Export {
    projects: Vec<Project>,
    project_names: Vec<(Dependency, ProjectId)>,
}

#[derive(Serialize)]
struct Project {
    name: String,
    version: String,
    authors: Vec<String>,
    dependencies: Vec<Dependency>,
    id: ProjectId,

    items: Items,

    indirect: bool,
}

#[derive(Serialize, PartialEq, Eq, Hash)]
struct Dependency {
    name: String,
    version: String,
}

#[derive(Serialize)]
struct Items {
    implementations: Vec<Implementation>,

    funcs: Vec<FunctionDef>,
    types: Vec<TypeDef>,
}

#[derive(Serialize)]
struct Implementation {
    trait_: (ProjectId, TypeId),
    trait_params: Vec<Type>,
    self_: Type,
    methods: Vec<FuncId>,
}

#[derive(Serialize)]
struct TypeDef {
    name: String,
    generics: Vec<String>,
    def: TypeKind,
}

#[derive(Serialize)]
enum TypeKind {
    Variant(Vec<(String, Vec<Type>)>),
    Record(Vec<(String, Type)>),
    Trait(Vec<FuncId>),
    Alias(Type),
}

#[derive(Serialize)]
struct FunctionDef {
    name: String,
    parameters: Vec<Type>,
    ret: Type,
    // method_of: Option<(TypeIndex, MethodIndex)>,
}

#[derive(Serialize)]
enum Type {
    Linked(ProjectId, TypeId, Vec<Type>),
    Unlinked(String),
    Tuple(Vec<Type>),
    List(Box<Type>),
    Array {
        of: Box<Self>,
        len: Box<Self>,
    },
    Function {
        is_fn_ptr: bool,
        params: Vec<Self>,
        ret: Box<Self>,
    },
    Pointer(Box<Self>),
}

type ProjectId = usize;
type FuncId = usize;
type TypeId = usize;
