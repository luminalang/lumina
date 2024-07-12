//! Type checking/inference
//! Complete record field name resolution
//! Verify constriants
//! Resolve & Desugar dot-pipe calls

use crate::prelude::*;
use crate::{ProjectInfo, TRAIT_OBJECT_DATA_FIELD, VTABLE_FIELD};
use lumina_typesystem::{
    ConcreteInst, Container, Forall, Generic, GenericData, GenericKind, IType, TEnv, TypeSystem,
};
use lumina_util::Highlighting;
use std::collections::VecDeque;
use std::fmt;
use std::ops::Not;
use tracing::info_span;

mod expr;
mod func;
use func::InstInfo;
pub use func::{FunctionStatus, Local};
use lumina_typesystem::{ImplIndex, Type};
mod pat;
mod tcheck;

mod tyfmt;

mod lower;
pub use lower::{
    BranchOf, CallTypes, ConcreteTyping, DecTree, DecTreeBranch, Expr, Function, ListConstr,
    PatPoint, PointToBindTranslationTable, Range,
};

type SelfPositions = Map<key::Method, key::Param>;

pub struct MIR {
    pub funcs: ModMap<key::Func, FunctionStatus>,
    pub read_only_table: ModMap<key::ReadOnly, (ReadOnlyBytes, Type)>,

    pub module_names: Map<key::Module, String>,

    // Certain parts of the HIR will be kept for the next pass
    pub methods: ModMap<key::Trait, Map<key::Method, key::Func>>,
    pub imethods: ModMap<key::Impl, Map<key::Method, Option<M<key::Func>>>>,
    pub field_types: ModMap<key::Record, Map<key::RecordField, Tr<Type>>>,
    pub variant_types: ModMap<key::Sum, Map<key::SumVariant, Vec<Tr<Type>>>>,
    pub impls: ModMap<key::Impl, Forall<'static, Type>>,
    pub impltors: ModMap<key::Impl, Tr<Type>>,
    pub itraits: ModMap<key::Impl, (M<key::Trait>, Vec<Type>)>,
    pub func_names: ModMap<key::Func, String>,
    pub val_initializers: ModMap<key::Val, M<key::Func>>,
    pub trait_objects: ModMap<key::Trait, Option<SelfPositions>>,
}

pub struct ReadOnlyBytes(pub Box<[u8]>);

pub fn run<'a, 'h, 's>(
    pinfo: ProjectInfo,
    mut hir: hir::HIR<'s>,
    mut tenvs: ModMap<key::Func, TEnv<'s>>,
    iquery: &mut ImplIndex,
) -> (MIR, bool) {
    let mut funcs = hir.funcs.secondary_with(|(_, _)| FunctionStatus::Pending);
    let mut rotable = hir.sources.modules().collect();

    let fields = hir.lookups.to_field_lookup();

    // Verify members of implementations, as well as statically finishing the method typings
    let mut imethods = hir.impls.secondary();
    for impl_ in hir.impls.iter() {
        let (assoc, methods) = verify_impl_headers(&hir, &mut tenvs, impl_);
        if !assoc.is_empty() {
            unimplemented!("associated types");
        }
        imethods.push(impl_.module, methods);
    }

    // Check and lower the functions
    for func in hir.funcs.iter() {
        Verify::start_at(
            pinfo,
            &hir,
            &fields,
            &mut tenvs,
            &iquery,
            &mut funcs,
            &mut rotable,
            func,
        );
    }

    let trait_objects = hir.methods.map(|(tr, methods)| {
        methods
            .values()
            .map(|func| match &funcs[tr.module.m(*func)] {
                FunctionStatus::Done(func) => {
                    func.typing.params.find(|ty| matches!(ty, Type::Self_))
                }
                _ => None,
            })
            .collect()
    });

    // Rename generics for 'static so we can drop source code
    let mut impls = hir.impls.secondary();
    hir.impls.iter().for_each(|imp| {
        let foralls = rename_forall(&hir.impls[imp]);
        impls[imp.module].push(foralls);
    });

    let has_failed = hir.sources.has_failed();

    (
        MIR {
            funcs,
            read_only_table: rotable,

            module_names: hir
                .sources
                .modules()
                .map(|m| hir.sources.name_of_module(m))
                .collect(),
            trait_objects,
            impls,
            impltors: hir.impltors,
            itraits: hir.itraits,
            imethods,
            methods: hir.methods,
            field_types: hir.field_types,
            variant_types: hir.variant_types,
            val_initializers: hir.val_initializers,
            func_names: hir.func_names.map(|(_, name)| name.value.to_string()),
        },
        has_failed,
    )
}

#[derive(new)]
pub struct Verify<'a, 's> {
    hir: &'a hir::HIR<'s>,
    iquery: &'a ImplIndex,

    tenvs: &'a mut ModMap<key::Func, TEnv<'s>>,

    rsolver: RSolver<'a, 's>,
    items: LangItems,

    funcs: &'a mut ModMap<key::Func, FunctionStatus>,
    read_only_table: &'a mut ModMap<key::ReadOnly, (ReadOnlyBytes, Type)>,

    current: Current,
    pforall: &'a Forall<'s, Type>,
    fdef: &'a hir::FuncDef<'s>,
}

// The information that gets swapped each time we context-switch between functions
#[derive(Clone)]
pub struct Current {
    pub lambda: Option<key::Lambda>,
    pub fkey: M<key::Func>,
    pub binds: HashMap<key::Bind, Tr<IType>>,
    pub insts: HashMap<Option<key::Lambda>, VecDeque<Tr<InstInfo>>>,
    pub type_dependent_lookup: VecDeque<M<ast::NFunc>>,
    pub casts: VecDeque<Tr<IType>>,
}

#[derive(new, Clone, Copy)]
struct RSolver<'a, 's> {
    field_lookup: &'a HashMap<&'s str, Vec<M<key::Record>>>,
    records: &'a ModMap<key::Record, (Tr<&'s str>, Forall<'s, Type>)>,
    ftypes: &'a ModMap<key::Record, Map<key::RecordField, Tr<Type>>>,
    fnames: &'a ModMap<key::Record, Map<key::RecordField, Tr<&'s str>>>,
}

impl<'a, 's> RSolver<'a, 's> {
    pub fn as_typesystem<'t>(&'t mut self, env: &'t mut TEnv<'s>) -> TypeSystem<'t, 's> {
        TypeSystem::new(
            env,
            self.records,
            self.ftypes,
            self.fnames,
            self.field_lookup,
        )
    }
}

impl Current {
    pub fn new(fkey: M<key::Func>, lambdas: impl Iterator<Item = key::Lambda>) -> Self {
        let mut insts = HashMap::new();

        insts.insert(None, VecDeque::new());
        for lambda in lambdas {
            insts.insert(Some(lambda), VecDeque::new());
        }

        Self {
            lambda: None,
            fkey,
            binds: HashMap::new(),
            type_dependent_lookup: VecDeque::new(),
            casts: VecDeque::new(),
            insts,
        }
    }

    fn push_inst(&mut self, span: Span, finst: InstInfo) {
        self.insts
            .get_mut(&self.lambda)
            .unwrap()
            .push_back(Tr::new(span, finst));
    }

    #[track_caller]
    pub(crate) fn pop_inst(&mut self, span: Span) -> InstInfo {
        let inst = self.pop_inst_without_assertion();
        assert_eq!(inst.span, span);
        inst.value
    }

    #[track_caller]
    pub(crate) fn pop_inst_without_assertion(&mut self) -> Tr<InstInfo> {
        self.insts
            .get_mut(&self.lambda)
            .unwrap()
            .pop_front()
            .unwrap()
    }
}

#[derive(new, Clone, Copy)]
struct LangItems {
    list_default: Option<M<key::TypeKind>>,
    listable: M<key::Trait>,
    reflect_type: M<key::Sum>,
    string: M<key::TypeKind>,
}

fn verify_impl_headers<'s>(
    hir: &hir::HIR<'s>,
    tenvs: &mut ModMap<key::Func, TEnv<'s>>,
    impl_: M<key::Impl>,
) -> (
    Map<key::AssociatedType, Tr<Type>>,
    Map<key::Method, Option<M<key::Func>>>,
) {
    let imod = impl_.module;
    let mname = hir.sources.name_of_module(imod);

    let (trkey, trparams) = &hir.itraits[impl_];
    let tmod = trkey.module;

    let (trname, tforall) = &hir.traits[*trkey];
    let trmethodmap = &hir.methods[*trkey];
    let imethodmap = &hir.imethods[impl_];

    let impltor = hir.impltors[impl_].clone();

    let _span = info_span!(
        "verifying impl",
        module = mname,
        entity = format!(
            "impl {} for {}",
            hir.sources.get_span(imod, impltor.span),
            *trname
        )
    );
    let _handle = _span.enter();

    let mut missing_methods: Vec<key::Method> = vec![];
    let mut failed_methods: Vec<(M<key::Func>, M<key::Func>, CmpResult)> = vec![];

    let mut inst = ConcreteInst {
        generics: Map::new(),
        pgenerics: trparams.iter().cloned().collect(),
        self_: Some(impltor.value),
    };

    let resolved_methods = trmethodmap
        .iter()
        .map(|(method, tfkey)| {
            let tfkey = tmod.m(*tfkey);
            let mname = &hir.func_names[tfkey];

            match imethodmap.find(|func| hir.func_names[imod.m(*func)] == *mname) {
                None => {
                    match &hir.funcs[tfkey] {
                        hir::FuncDefKind::Defined(_) => unreachable!(),
                        hir::FuncDefKind::TraitHeader(..) => {
                            missing_methods.push(method);
                            None
                        }
                        // use the default if it exists
                        hir::FuncDefKind::TraitDefaultMethod(..) => Some(tfkey),
                        _ => unreachable!("method in trait declaration has an invalid header"),
                    }
                }
                Some(matching) => {
                    let ifkey = imod.m(imethodmap[matching]);
                    let to_generic = |k| Type::Generic(Generic::new(k, GenericKind::Entity));
                    inst.generics = match &hir.funcs[tfkey] {
                        hir::FuncDefKind::Defined(fdef) => {
                            fdef.forall.borrow().keys().map(to_generic).collect()
                        }
                        hir::FuncDefKind::TraitHeader(_, forall, _) => {
                            forall.keys().map(to_generic).collect()
                        }
                        _ => unreachable!(),
                    };
                    let result = verify_impl_method(hir, tenvs, &mut inst, tfkey, ifkey);
                    if !result.is_ok() {
                        failed_methods.push((tfkey, ifkey, result));
                    }
                    Some(ifkey)
                }
            }
        })
        .collect::<Map<key::Method, _>>();
    inst.generics = Map::new();

    let mut missing_associations: Vec<key::AssociatedType> = vec![];

    let resolved_associations = hir.assoc_names[*trkey]
        .iter()
        .map(
            |(assoc, &aname)| match hir.iassoc_names[impl_].find(|name| aname == *name) {
                None => match hir.assoc[*trkey][assoc].as_ref() {
                    Some(ty) => ty.clone(),
                    None => {
                        missing_associations.push(assoc);
                        Type::poison().tr(hir.impltors[impl_].span)
                    }
                },
                Some(ty) => {
                    todo!("verify the assocs");
                }
            },
        )
        .collect::<Map<key::AssociatedType, _>>();

    let error = hir.sources.error("bad implementation");

    if missing_methods.len() + missing_associations.len() + failed_methods.len() != 0 {
        let mut error = error.m(imod).eline(hir.impltors[impl_].span, "");

        fn tmethod_to_fstring<'s>(
            hir: &hir::HIR<'s>,
            pforall: &Forall<'s, Type>,
            func: M<key::Func>,
            env: &TEnv<'s>,
        ) -> String {
            let forall = match &hir.funcs[func] {
                hir::FuncDefKind::TraitDefaultMethod(_, fdef)
                | hir::FuncDefKind::Defined(fdef)
                | hir::FuncDefKind::ImplMethod(_, fdef) => fdef
                    .forall
                    .borrow()
                    .values()
                    .map(|gdata| gdata.name)
                    .collect(),
                hir::FuncDefKind::InheritedDefault(_, _) => todo!(),
                hir::FuncDefKind::TraitHeader(_, forall, _) => {
                    forall.values().map(|gdata| gdata.name).collect()
                }
                hir::FuncDefKind::Extern { .. } => Map::new(),
            };
            let pforall = pforall.values().map(|gdata| gdata.name).collect();
            let tfmt = tyfmt::TyFmtState::new(hir, env, forall, pforall);
            let fname = hir.func_names[func];
            match &hir.funcs[func] {
                hir::FuncDefKind::Defined(fdef)
                | hir::FuncDefKind::ImplMethod(_, fdef)
                | hir::FuncDefKind::TraitDefaultMethod(_, fdef) => {
                    tfmt.function(fname, &fdef.typing)
                }
                hir::FuncDefKind::TraitHeader(_, _, typing) => tfmt.function(fname, typing),
                other => unreachable!("{other:?}"),
            }
        }

        for method in missing_methods {
            let func = tmod.m(trmethodmap[method]);
            let env = TEnv::new();
            let fstring = tmethod_to_fstring(hir, tforall, func, &env);
            error = error.text(format!("{}  {}", "missing".symbol(), fstring));
            error = error.text("");
        }

        for assoc in missing_associations {
            let name = hir.assoc_names[*trkey][assoc];
            error = error.text(format!(
                "{}  {} {name}",
                "missing".symbol(),
                "type".keyword()
            ));
            error = error.text("");
        }

        for (tfunc, ifunc, result) in failed_methods {
            let env = &tenvs[ifunc];
            let got = tmethod_to_fstring(hir, tforall, ifunc, env);
            let env = TEnv::new();
            // the forall we actually use during instantiation just assumes the same order.
            //
            // but it doesn't care about names.
            // eh. let's use the `got` forall and on missing provide a placeholder.
            let forall = hir.funcs[ifunc]
                .as_defined()
                .forall
                .borrow()
                .values()
                .map(|gdata| gdata.name)
                .collect();
            let tforall = tforall.values().map(|gdata| gdata.name).collect();
            let exp = tyfmt::TyFmtState::new(hir, &env, forall, tforall)
                .function(hir.func_names[tfunc], &result.instantiated);
            error = error.text(format!("{}      {}", "got".symbol(), got));
            error = error.text(format!("{} {}", "expected".symbol(), exp));
            error = error.text("");
        }

        error.emit();
    }

    (resolved_associations, resolved_methods)
}

fn verify_impl_method<'s>(
    hir: &hir::HIR<'s>,
    tenvs: &mut ModMap<key::Func, TEnv<'s>>,
    inst: &mut ConcreteInst,
    tfunc: M<key::Func>,
    ifunc: M<key::Func>,
) -> CmpResult {
    let trmtyping = match &hir.funcs[tfunc] {
        hir::FuncDefKind::Defined(func) => {
            todo!();
            // todo!("for the instantiation to work, we'd have to first upgrade *then* downgrade. Which sucks");
            // BUT: we also do want to force teh annotation to be there at a
            // declaration level.
            //
            // SO; maybe we should create a `Typing<Type>` and then *also*
            // downgrade to a `Typing<IType>` in the `FuncDef`? ye that makes
            // sense.
        }
        hir::FuncDefKind::TraitHeader(_, _, typing) => typing,
        _ => unreachable!(),
    };
    let trmtyping = hir::Typing {
        params: trmtyping
            .params
            .values()
            .map(|ty| inst.apply(ty).tr(ty.span))
            .collect(),
        returns: inst.apply(&*trmtyping.returns).tr(trmtyping.returns.span),
    };

    let idef = hir.funcs[ifunc].as_defined();

    ImplComparison {
        env: &mut tenvs[ifunc],
        self_: inst.self_.as_ref().unwrap().i(),
    }
    .typing(&idef.typing, trmtyping)
}

struct ImplComparison<'a, 's> {
    self_: IType,
    env: &'a mut TEnv<'s>,
}

#[derive(Debug)]
struct CmpResult {
    badlen: bool,
    params: Vec<key::Param>,
    returns: bool,
    instantiated: hir::Typing<Type>,
}

impl CmpResult {
    fn is_ok(&self) -> bool {
        !self.badlen && !self.returns && self.params.is_empty()
    }
}

impl<'a, 's> ImplComparison<'a, 's> {
    fn typing(&mut self, got: &hir::Typing<IType>, exp: hir::Typing<Type>) -> CmpResult {
        CmpResult {
            badlen: got.params.len() != exp.params.len(),
            params: got
                .params
                .iter()
                .zip(exp.params.values())
                .filter_map(|((p, got), exp)| self.cmp(got.span, got, exp).not().then_some(p))
                .collect(),
            returns: self.cmp(got.returns.span, &got.returns, &exp.returns).not(),
            instantiated: exp,
        }
    }

    fn cmp(&mut self, span: Span, got: &IType, exp: &Type) -> bool {
        let exp = exp.i();
        self.cmpi(span, got, &exp)
    }

    fn cmpi(&mut self, span: Span, got: &IType, exp: &IType) -> bool {
        trace!("impl-comparing {got} & {exp}");

        match (got, exp) {
            (IType::Prim(got), IType::Prim(exp)) => got == exp,
            (IType::Generic(ggeneric), IType::Generic(egeneric)) => ggeneric == egeneric,
            (IType::Defined(gkind, gparams), IType::Defined(ekind, eparams)) if gkind == ekind => {
                self.cmpis(span, gparams, eparams)
            }
            (IType::Container(gcon), IType::Container(econ)) => match (gcon, econ) {
                (Container::Func(gkind, gparams, gret), Container::Func(ekind, eparams, eret)) => {
                    gkind == ekind
                        && self.cmpis(span, gparams, eparams)
                        && self.cmpi(span, gret, eret)
                }
                (Container::Tuple(gelems), Container::Tuple(eelems)) => {
                    self.cmpis(span, gelems, eelems)
                }
                (Container::Pointer(g), Container::Pointer(e)) => self.cmpi(span, g, e),
                _ => false,
            },
            (IType::Self_, IType::Self_) => true,
            (IType::Self_, _) => {
                let self_ = self.self_.clone();
                self.cmpi(span, &self_, exp)
            }
            (IType::Var(var), ty) => {
                let asgn = self.env.get(*var).clone();
                match asgn.value {
                    None => {
                        self.env.assign(*var, ty.clone().tr(span));
                        true
                    }
                    Some(ty) => {
                        let ty = ty.cloned();
                        self.cmpi(span, got, &ty)
                    }
                }
            }
            (ty, IType::Var(var)) => {
                let asgn = self.env.get(*var).clone();
                match asgn.value {
                    None => {
                        self.env.assign(*var, ty.clone().tr(span));
                        true
                    }
                    Some(ty) => {
                        let ty = ty.cloned();
                        self.cmpi(span, &ty, exp)
                    }
                }
            }
            (IType::InferringRecord(_), _) => unreachable!(),
            other => {
                trace!("the {other:?} comparison yielded false");
                false
            }
        }
    }

    fn cmpis(&mut self, span: Span, got: &[IType], exp: &[IType]) -> bool {
        got.iter().zip(exp).all(|(g, e)| self.cmpi(span, g, e))
    }
}

fn rename_forall<T: Clone>(forall: &Forall<'_, T>) -> Forall<'static, T> {
    forall
        .values()
        .map(|gdata| GenericData { name: "_", ..gdata.clone() })
        .collect()
}

impl fmt::Display for ReadOnlyBytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match std::str::from_utf8(&self.0) {
            Ok(str) => write!(f, "{str}"),
            Err(_) => write!(f, "{:?}", &self.0),
        }
    }
}
