//! Type checking/inference
//! Complete record field name resolution
//! Verify constriants
//! Resolve & Desugar dot-pipe calls

use crate::prelude::*;
use crate::{ProjectInfo, Target};
use lumina_typesystem::{
    Downgrade, Forall, GenericKind, GenericMapper, IType, Inference, Static, TEnv, Transformer, Ty,
    TypeSystem, Var,
};
use lumina_util::Highlighting;
use std::collections::VecDeque;
use std::fmt;
use std::ops::Not;
use tracing::info_span;

mod expr;
mod func;
pub use func::FunctionStatus;
use func::InstInfo;
use lumina_typesystem::{ImplIndex, Type};
mod patc;
mod tcheck;

mod tyfmt;

mod lower;
pub use lower::{pat, CallTypes, Callable, ConcreteTyping, Expr, Function};
pub type DecTree = pat::DecTree<key::DecisionTreeTail>;
pub type Branching<K> = pat::Branching<K, key::DecisionTreeTail>;

type SelfPositions = Map<key::Method, key::Param>;

pub struct MIR {
    pub funcs: ModMap<key::Func, FunctionStatus>,
    pub read_only_table: ModMap<key::ReadOnly, (ReadOnlyBytes, Type)>,

    // Certain parts of the HIR will be kept for the next pass
    pub methods: ModMap<key::Trait, Map<key::Method, key::Func>>,
    pub imethods: ModMap<key::Impl, Map<key::Method, Option<M<key::Func>>>>,
    pub field_types: ModMap<key::Record, Map<key::RecordField, Tr<Type>>>,
    pub variant_types: ModMap<key::Sum, Map<key::SumVariant, Vec<Tr<Type>>>>,
    pub impls: ModMap<key::Impl, Forall<'static, Static>>,
    pub impltors: ModMap<key::Impl, Tr<Type>>,
    pub itraits: ModMap<key::Impl, (M<key::Trait>, Vec<Type>)>,
    pub val_initializers: ModMap<key::Val, M<key::Func>>,
    pub trait_objects: ModMap<key::Trait, Option<SelfPositions>>,

    pub module_names: Map<key::Module, String>,
    pub func_names: ModMap<key::Func, String>,
    pub record_names: ModMap<key::Record, String>,
    pub sum_names: ModMap<key::Sum, String>,
    pub trait_names: ModMap<key::Trait, String>,
    pub field_names: ModMap<key::Record, Map<key::RecordField, String>>,
    pub variant_names: ModMap<key::Sum, Map<key::SumVariant, String>>,
}

impl MIR {
    pub fn name_of_type<K: Into<key::TypeKind> + Copy>(&self, key: M<K>) -> &str {
        match key.map(K::into).value {
            key::TypeKind::Sum(k) => &self.sum_names[key.module.m(k)],
            key::TypeKind::Record(k) => &self.record_names[key.module.m(k)],
            key::TypeKind::Trait(k) => &self.trait_names[key.module.m(k)],
        }
    }

    pub fn name_of_method(&self, key: M<key::Trait>, method: key::Method) -> &str {
        let fkey = self.methods[key][method];
        &self.func_names[key.module.m(fkey)]
    }
}

pub struct ReadOnlyBytes(pub Box<[u8]>);

pub fn run<'a, 'h, 's>(
    pinfo: ProjectInfo,
    target: Target,
    hir: hir::HIR<'s>,
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
            target,
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
                FunctionStatus::Done(func) => func
                    .typing
                    .params
                    .find(|ty| matches!(ty, Type::Simple("self"))),
                _ => None,
            })
            .collect()
    });

    // Rename generics for 'static so we can drop source code
    let mut impls = hir.impls.secondary();
    hir.impls.iter().for_each(|imp| {
        let foralls = hir.impls[imp].rename_to_keys();
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
            func_names: hir.func_names.map(|(_, name)| name.value.to_string()),
            record_names: hir.records.map(|(name, _)| name.value.to_string()),
            sum_names: hir.sums.map(|(name, _)| name.value.to_string()),
            trait_names: hir.traits.map(|(name, _)| name.value.to_string()),
            field_names: hir
                .fnames
                .map(|(_, fields)| fields.values().map(|name| name.to_string()).collect()),
            variant_names: hir
                .vnames
                .map(|(_, variants)| variants.values().map(|name| name.to_string()).collect()),

            trait_objects,
            impls,
            impltors: hir.impltors,
            itraits: hir.itraits,
            imethods,
            methods: hir.methods,
            field_types: hir.field_types,
            variant_types: hir.variant_types,
            val_initializers: hir.val_initializers,
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

    target: Target,

    current: Current,
    pforall: &'a Forall<'s, Static>,
    fdef: &'a hir::FuncDef<'s>,
}

// The information that gets swapped each time we context-switch between functions
#[derive(Clone)]
pub struct Current {
    pub lambda: Option<key::Lambda>,
    pub fkey: M<key::Func>,
    pub binds: HashMap<key::Bind, Tr<IType>>,

    // We traverse the HIR two times. First imutably to perform all type unification than a second
    // time which actually performs the transformation when all types are statically known.
    //
    // These ring buffers are pushed to when traversing the first time and then popped from when
    // traversing the second time. Allowing us to save some information generating on the first pass to the other.
    pub insts: HashMap<Option<key::Lambda>, VecDeque<Tr<Option<InstInfo>>>>,
    pub type_dependent_lookup: VecDeque<M<ast::NFunc>>,
    pub casts_and_matches: VecDeque<Tr<IType>>,
}

#[derive(new, Clone, Copy)]
struct RSolver<'a, 's> {
    field_lookup: &'a Map<key::Module, HashMap<&'s str, Vec<M<key::Record>>>>,
    records: &'a ModMap<key::Record, (Tr<&'s str>, Forall<'s, Static>)>,
    ftypes: &'a ModMap<key::Record, Map<key::RecordField, Tr<Type>>>,
    fnames: &'a ModMap<key::Record, Map<key::RecordField, Tr<&'s str>>>,
}

impl<'a, 's> RSolver<'a, 's> {
    pub fn as_typesystem<'t>(
        &'t mut self,
        module: key::Module,
        env: &'t mut TEnv<'s>,
    ) -> TypeSystem<'t, 's> {
        TypeSystem::new(
            env,
            self.records,
            self.ftypes,
            self.fnames,
            &self.field_lookup[module],
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
            casts_and_matches: VecDeque::new(),
            insts,
        }
    }

    fn push_inst(&mut self, span: Span, finst: Option<InstInfo>) {
        self.insts
            .get_mut(&self.lambda)
            .unwrap()
            .push_back(Tr::new(span, finst));
    }

    #[track_caller]
    pub(crate) fn pop_inst(&mut self, span: Span) -> Option<InstInfo> {
        let inst = self.pop_inst_without_assertion();
        assert_eq!(inst.span, span);
        inst.value
    }

    #[track_caller]
    pub(crate) fn pop_inst_without_assertion(&mut self) -> Tr<Option<InstInfo>> {
        self.insts
            .get_mut(&self.lambda)
            .unwrap()
            .pop_front()
            .unwrap()
    }
}

#[derive(new, Clone, Copy)]
struct LangItems {
    list_default: M<key::TypeKind>,
    pinfo: ProjectInfo,
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

    let (trname, _) = &hir.traits[*trkey];
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

    let mut missing_methods: Vec<(key::Method, &Forall<'s, Static>, hir::Typing<Type>)> = vec![];
    let mut failed_methods: Vec<(M<key::Func>, M<key::Func>, CmpResult)> = vec![];
    let mut unknown_methods: Vec<M<key::Func>> = vec![];

    for method in imethodmap.values() {
        let fkey = imod.m(*method);
        let name = *hir.func_names[fkey];
        if trmethodmap
            .values()
            .all(|trmethod| name != *hir.func_names[tmod.m(*trmethod)])
        {
            unknown_methods.push(fkey);
        }
    }

    let mut tinst = GenericMapper::from_types(GenericKind::Parent, trparams.iter().cloned());
    tinst.self_ = Some(impltor.value);

    let resolved_methods = trmethodmap
        .iter()
        .map(|(method, tfkey)| {
            let tfkey = tmod.m(*tfkey);
            let mname = &hir.func_names[tfkey];

            let (tmforall, trmtyping) = match &hir.funcs[tfkey] {
                hir::FuncDefKind::TraitDefaultMethod(_, forall, typing, _)
                | hir::FuncDefKind::TraitHeader(_, forall, typing) => (forall, typing),
                _ => unreachable!(),
            };

            tinst.replicate_entity_forall(tmforall);

            let trmtyping = {
                let (params, returns) = trmtyping.map(|ty| (&tinst).transform(*ty));
                hir::Typing { params, returns }
            };

            match imethodmap.find(|func| hir.func_names[imod.m(*func)] == *mname) {
                None => {
                    match &hir.funcs[tfkey] {
                        hir::FuncDefKind::Defined(_) => unreachable!(),
                        hir::FuncDefKind::TraitHeader(..) => {
                            missing_methods.push((method, tmforall, trmtyping));
                            None
                        }
                        // use the default if it exists
                        hir::FuncDefKind::TraitDefaultMethod(..) => Some(tfkey),
                        _ => unreachable!("method in trait declaration has an invalid header"),
                    }
                }
                Some(matching) => {
                    let ifkey = imod.m(imethodmap[matching]);
                    let idef = hir.funcs[ifkey].as_defined();

                    let result = ImplComparison {
                        env: &mut tenvs[ifkey],
                        self_: Downgrade.transform(&tinst.self_.as_ref().unwrap()),
                    }
                    .typing(&idef.typing, trmtyping);

                    if !result.is_ok() {
                        failed_methods.push((tfkey, ifkey, result));
                    }

                    Some(ifkey)
                }
            }
        })
        .collect::<Map<key::Method, _>>();

    tinst.clear_assignments_of_kind(GenericKind::Entity);

    let mut missing_associations: Vec<key::AssociatedType> = vec![];

    let resolved_associations = hir.assoc_names[*trkey]
        .iter()
        .map(
            |(assoc, &aname)| match hir.iassoc[impl_].iter().find(|(name, _)| aname == *name) {
                Some((_, ty)) => ty.clone(),
                // Use the default (if it exists)
                None => match hir.assoc[*trkey][assoc].as_ref() {
                    Some(ty) => (&tinst).transform(&**ty).tr(ty.span),
                    None => {
                        missing_associations.push(assoc);
                        Type::poison().tr(hir.impltors[impl_].span)
                    }
                },
            },
        )
        .collect::<Map<key::AssociatedType, _>>();

    let error = hir.sources.error("bad implementation");

    if missing_methods.len()
        + missing_associations.len()
        + failed_methods.len()
        + unknown_methods.len()
        != 0
    {
        let mut error = error.m(imod).eline(hir.impltors[impl_].span, "");

        let iforall: Map<_, _> = hir.impls[impl_].names().collect();

        for (i, &fkey) in unknown_methods.iter().enumerate() {
            let env = TEnv::new();
            let fdef = &hir.funcs[fkey].as_defined();
            let fname = *hir.func_names[fkey];
            let forall = fdef.forall.borrow().names().collect();
            let fstring = tyfmt::TyFmtState::new(hir, &env, forall, iforall.clone())
                .function(fname, &fdef.typing);
            error = error.text(format!("{}  {}", "unknown".symbol(), fstring));

            if i == unknown_methods.len() - 1 {
                error = error.text("");
            }
        }

        for (i, (method, forall, typing)) in missing_methods.iter().enumerate() {
            let env = TEnv::new();
            let fname = hir.func_names[tmod.m(trmethodmap[*method])];
            let forall = forall.names().collect();
            let fstring =
                tyfmt::TyFmtState::new(hir, &env, forall, iforall.clone()).function(fname, &typing);
            error = error.text(format!("{}  {}", "missing".symbol(), fstring));

            if i == missing_methods.len() - 1 {
                error = error.text("");
            }
        }

        for (i, assoc) in missing_associations.iter().enumerate() {
            let name = hir.assoc_names[*trkey][*assoc];
            error = error.text(format!(
                "{}  {} {name}",
                "missing".symbol(),
                "type".keyword()
            ));

            if i == missing_associations.len() - 1 {
                error = error.text("");
            }
        }

        for (i, (tfunc, ifunc, result)) in failed_methods.iter().enumerate() {
            let env = &tenvs[*ifunc];
            let fdef = hir.funcs[*ifunc].as_defined();
            let imforall: Map<_, _> = fdef.forall.borrow().names().collect();
            let fname = hir.func_names[*ifunc];

            let got = tyfmt::TyFmtState::new(hir, &env, imforall.clone(), iforall.clone())
                .function(fname, &fdef.typing);

            let exp = tyfmt::TyFmtState::new(hir, &TEnv::new(), imforall, iforall.clone())
                .function(hir.func_names[*tfunc], &result.instantiated);

            error = error.text(format!("{}      {}", "got".symbol(), got));
            error = error.text(format!("{} {}", "expected".symbol(), exp));

            if i == failed_methods.len() - 1 {
                error = error.text("");
            }
        }

        error.emit();
    }

    (resolved_associations, resolved_methods)
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
        let exp = Downgrade.transform(&exp);
        self.cmpi(span, got, &exp)
    }

    fn cmpi(&mut self, span: Span, got: &IType, exp: &IType) -> bool {
        trace!("impl-comparing {got} & {exp}");

        match (got, exp) {
            (Ty::Int(gsize), Ty::Int(esize)) => gsize == esize,
            (Ty::Generic(ggeneric), Ty::Generic(egeneric)) => ggeneric == egeneric,
            (Ty::Simple(g), Ty::Simple(e)) => g == e,
            (Ty::Container(g, gparams), Ty::Container(e, eparams)) if g == e => {
                self.cmpis(span, gparams, eparams)
            }
            (Ty::Special(Inference::Var(var)), ty) => self.assign_or_check(false, span, *var, ty),
            (ty, Ty::Special(Inference::Var(var))) => self.assign_or_check(true, span, *var, ty),
            (Ty::Simple("self"), ty) => {
                let self_ = self.self_.clone();
                self.cmpi(span, &self_, ty)
            }
            other => {
                trace!("the {other:?} comparison yielded false");
                false
            }
        }
    }

    fn assign_or_check(&mut self, flip: bool, span: Span, var: Var, ty: &IType) -> bool {
        let asgn = self.env.get(var);
        match asgn.value {
            None => {
                self.env.assign(var, ty.clone().tr(span));
                true
            }
            Some(var_ty) => {
                let var_ty = var_ty.cloned();
                if flip {
                    self.cmpi(span, &ty, &var_ty)
                } else {
                    self.cmpi(span, &var_ty, &ty)
                }
            }
        }
    }

    fn cmpis(&mut self, span: Span, got: &[IType], exp: &[IType]) -> bool {
        got.iter().zip(exp).all(|(g, e)| self.cmpi(span, g, e))
    }
}

impl fmt::Display for ReadOnlyBytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match std::str::from_utf8(&self.0) {
            Ok(str) => write!(f, "{str}"),
            Err(_) => write!(f, "{:?}", &self.0),
        }
    }
}
