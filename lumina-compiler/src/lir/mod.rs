//! Monomorphization
//! Closure lowering to vtables
//! Dynamic Object lowering to vtables
//! Pattern decision trees lowering to expressions
//! Flatten to SSA+CFG with Basic Blocks

use crate::prelude::*;
use crate::{ProjectInfo, Target, LIST_CONCAT, LIST_NIL, LIST_SINGLETON};
use key::{entity_impl, keys};
use lumina_typesystem::{Generic, GenericKind, GenericMapper, ImplIndex, IntSize, Static, Type};
use lumina_util::Highlighting;
use std::fmt;
use tracing::info_span;

const UNIT: MonoTypeKey = MonoTypeKey(0);

#[macro_export]
macro_rules! to_morphization {
    ($lir:expr, $mir:expr, $tmap:expr) => {
        crate::lir::mono::Monomorphization::new(
            &mut $lir.types,
            &$mir.field_types,
            &$mir.variant_types,
            &$mir.methods,
            &$mir.funcs,
            &$mir.trait_objects,
            $tmap,
        )
    };
}

mod mono;
mod reflect;
mod ssa;
pub use mono::{
    fmt as ty_fmt, BitOffset, MonoFormatter, MonoType, MonoTypeKey, MonomorphisedRecord, Records,
    TypeMap,
};
pub use ssa::{Block, Blocks, ControlFlow, Entry, Value, V};
mod closure;
mod expr;
mod pat;

keys! { MonoFunc . "mfunc" }

pub struct Output {
    pub functions: Map<MonoFunc, Function>,
    pub extern_funcs: HashMap<M<key::Func>, ExternFunction>,
    pub val_initializers: HashMap<M<key::Val>, MonoFunc>,
    pub val_types: ModMap<key::Val, MonoType>,

    pub read_only_table: ModMap<key::ReadOnly, (mir::ReadOnlyBytes, MonoType)>,

    pub types: Records,

    pub func_names: ModMap<key::Func, String>,
    pub module_names: Map<key::Module, String>,

    pub main: MonoFunc,
    pub sys_init: MonoFunc,

    pub alloc: MonoFunc,
    pub dealloc: MonoFunc,
}

#[derive(new)]
struct LIR {
    #[new(default)]
    mono_resolve: HashMap<MonoTypesKey, MonoFunc>,
    #[new(default)]
    functions: Map<MonoFunc, Function>,
    types: mono::MonomorphisedTypes,
    #[new(default)]
    vtables: HashMap<VTableHash, M<key::Val>>,

    read_only_table: ModMap<key::ReadOnly, (mir::ReadOnlyBytes, MonoType)>,

    target: Target,

    vals: ModMap<key::Val, MonoType>,
    #[new(default)]
    val_initialisers: HashMap<M<key::Val>, MonoFunc>,

    #[new(default)]
    alloc: Option<MonoFunc>,
    #[new(default)]
    dealloc: Option<MonoFunc>,
}

#[derive(new, PartialEq, Eq, Hash)]
pub struct MonoTypesKey {
    origin: FuncOrigin,
    generics: Vec<(Generic, MonoType)>,
    self_: Option<MonoType>,
}

#[derive(PartialEq, Eq, Hash)]
pub enum VTableHash {
    Lambda(MonoFunc),
    Object {
        weak_impltor: Type,
        trait_: M<key::Trait>,
        weak_trait_params: Vec<Type>,
    },
    PartialApplication(MonoFunc, Vec<MonoType>),
}

struct FuncLower<'a> {
    lir: &'a mut LIR,
    mir: &'a mir::MIR,

    iquery: &'a ImplIndex,

    info: ProjectInfo,

    current: Current,
}

struct Current {
    origin: FuncOrigin,
    mfkey: MonoFunc,
    tmap: TypeMap,
    bindmap: HashMap<key::Bind, ssa::Value>,
    has_captures: bool,
}

pub struct Function {
    pub symbol: String,
    pub blocks: ssa::Blocks,
    pub returns: MonoType,
}

pub struct ExternFunction {
    pub symbol: String,
    pub params: Vec<MonoType>,
    pub returns: MonoType,
}

impl Function {
    fn placeholder() -> Self {
        Self {
            symbol: String::new(),
            blocks: ssa::Blocks::placeholder(),
            returns: MonoType::Unreachable,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, new)]
struct MonoTyping {
    origin: FuncOrigin,
    params: Map<key::Param, MonoType>,
    returns: MonoType,
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum FuncOrigin {
    Defined(M<key::Func>),
    Method(M<key::Impl>, key::Method),
    Lambda(Box<FuncOrigin>, key::Lambda),
}

impl FuncOrigin {
    fn name(&self, mir: &mir::MIR) -> String {
        match self {
            FuncOrigin::Defined(key) => mir.func_names[*key].clone(),
            FuncOrigin::Method(impl_, method) => {
                mir.func_names[impl_.module.m(mir.imethods[*impl_][*method]).unwrap()].clone()
            }
            FuncOrigin::Lambda(parent, lkey) => format!("{}:{lkey}", parent.name(mir)),
        }
    }
}

pub fn run<'s>(info: ProjectInfo, target: Target, iquery: &ImplIndex, mut mir: mir::MIR) -> Output {
    info!("starting LIR lower");

    let mainfunc = &mir.funcs[info.main].as_done();

    let mut mono = mono::MonomorphisedTypes::new(info.closure);

    fn to_morphization<'a>(
        mir: &'a mir::MIR,
        mono: &'a mut mono::MonomorphisedTypes,
        tmap: &'a mut TypeMap,
    ) -> mono::Monomorphization<'a> {
        mono::Monomorphization::new(
            mono,
            &mir.field_types,
            &mir.variant_types,
            &mir.methods,
            &mir.funcs,
            &mir.trait_objects,
            tmap,
        )
    }

    let mut tmap = TypeMap::new();
    let mut monomorphization = to_morphization(&mir, &mut mono, &mut tmap);

    // the main functions generics are substituted for unit when used as entrypoint
    monomorphization.substitute_generics_for_unit_type(&mainfunc.typing.forall);

    let returns = monomorphization.apply(&mainfunc.typing.returns);

    let vals = mir.val_initializers.map(|(_, func)| {
        let typing = &mir.funcs[*func].as_done().typing;
        assert!(
            typing.params.is_empty(),
            "static vals cannot take parameters in their initialiser"
        );
        assert!(
            typing.forall.generics.is_empty(),
            "static vals cannot be generic"
        );
        monomorphization.apply(&typing.returns)
    });

    // Move ReadOnly from MIR to LIR so that we can define more of them
    // without borrowing the rest of MIR mutably. This isn't a great workaround.
    let mut read_only_table = mir.read_only_table.secondary_inner_capacity();
    for module in mir.read_only_table.modules() {
        let this = &mut mir.read_only_table[module];
        for (key, (bytes, ty)) in std::mem::take(this).into_iter() {
            let mut monomorphization = to_morphization(&mir, &mut mono, &mut tmap);
            let mono_ty = monomorphization.apply(&ty);
            read_only_table[module].push_as(key, (bytes, mono_ty));
        }
    }

    assert!(
        mainfunc.typing.params.is_empty(),
        "main function can not take parameters"
    );

    let mut lir = LIR::new(mono, read_only_table, target, vals);

    // fn alloc size as int -> *u8 =
    // fn dealloc ptr size as *u8, int -> () =
    let [alloc, dealloc] =
        [info.allocator.0, info.allocator.1].map(|func| lir.static_func(&mir, iquery, info, func));
    lir.alloc = Some(alloc);
    lir.dealloc = Some(dealloc);

    // for example:
    //
    // fn _lumina_sys_init argc argv as i32, **u8 -> () =
    let sys_init = lir.static_func(&mir, iquery, info, info.sys_init);

    let main = {
        let typing = MonoTyping::new(FuncOrigin::Defined(info.main), Map::new(), returns);
        lir.func(&mir, iquery, info, tmap, typing, None)
    };
    lir.functions[main].symbol = String::from("_lumina_main");

    for val in mir.val_initializers.iter() {
        let func = mir.val_initializers[val];
        let mfunc = lir.static_func(&mir, iquery, info, func);
        let previous = lir.val_initialisers.insert(val, mfunc);
        assert_eq!(previous, None);
    }

    // TODO: This is kinda hacky and inefficient.
    //
    // We should probably seperate out externs entirely instead of treating them like semi-functions.
    let extern_funcs = mir
        .funcs
        .iter()
        .filter_map(|func| match &mir.funcs[func] {
            mir::FunctionStatus::Extern { link_name, typing } => {
                let mut tmap = TypeMap::new();
                let mut monomorphization = to_morphization(&mir, &mut lir.types, &mut tmap);
                let params = monomorphization.applys(&typing.params);
                let returns = monomorphization.apply(&typing.returns);
                Some((
                    func,
                    ExternFunction { symbol: link_name.clone(), params, returns },
                ))
            }
            _ => None,
        })
        .collect();

    Output {
        functions: lir.functions,
        extern_funcs,
        val_initializers: lir.val_initialisers,
        val_types: lir.vals,
        read_only_table: lir.read_only_table,
        func_names: mir.func_names,
        module_names: mir.module_names,
        types: lir.types.into_records(),
        alloc,
        dealloc,
        main,
        sys_init,
    }
}

impl LIR {
    fn static_func(
        &mut self,
        mir: &mir::MIR,
        iquery: &ImplIndex,
        info: ProjectInfo,
        key: M<key::Func>,
    ) -> MonoFunc {
        let fdef = mir.funcs[key].as_done();
        let mut tmap = TypeMap::new();
        let mut morph = to_morphization!(self, mir, &mut tmap);
        let typing = morph.apply_typing(FuncOrigin::Defined(key), &fdef.typing);
        self.func(mir, iquery, info, tmap, typing, None)
    }

    fn func<'h, 's>(
        &mut self,
        mir: &mir::MIR,
        iquery: &ImplIndex,
        info: ProjectInfo,
        mut tmap: TypeMap,
        mut typing: MonoTyping,
        captures: Option<Map<key::Capture, (key::Bind, MonoType)>>,
    ) -> MonoFunc {
        let key = MonoTypesKey::new(typing.origin, tmap.generics, tmap.self_);

        match self.mono_resolve.get(&key) {
            Some(key) => *key,
            None => {
                typing.origin = key.origin;
                tmap.generics = key.generics;
                tmap.self_ = key.self_;

                let _span = info_span!(
                    "lowering function",
                    module = &mir.module_names[typing.origin.module()],
                    entity = typing.origin.name(mir),
                );
                let _handle = _span.enter();

                let has_captures = captures.is_some();
                let entryblock = ssa::Block::entry();

                // Add an implicit capture record as the first parameter if this is a lambda
                let mut ssa = if let Some(captures) = captures.as_ref() {
                    info!(
                        "setting up capture tuple [{}]",
                        captures
                            .iter()
                            .map(|(src, (dst, _))| format!("{src}->{dst}"))
                            .format(" ")
                    );

                    let mut ssa = ssa::Blocks::new(typing.params.len() as u32 + 1);
                    let mut morph = mono::Monomorphization::new(
                        &mut self.types,
                        &mir.field_types,
                        &mir.variant_types,
                        &mir.methods,
                        &mir.funcs,
                        &mir.trait_objects,
                        &mut tmap,
                    );

                    let capture_record = morph.create_capture_record(&captures);

                    info!("adding implicit capture record param");
                    ssa.add_block_param(entryblock, MonoType::Monomorphised(capture_record));

                    ssa
                } else {
                    ssa::Blocks::new(typing.params.len() as u32)
                };

                info!("adding block parameters for {}", self.types.fmt(&typing));
                for ty in typing.params.values() {
                    ssa.add_block_param(entryblock, ty.clone());
                }

                let mut bindmap = HashMap::new();

                // Add captures to the bindmap if this is a lambda
                if let Some(captures) = captures {
                    for (i, (bind, ty)) in captures {
                        let cap_param = ssa.get_block_param(entryblock, 0);
                        let field = key::RecordField(i.0);
                        let mk = ssa.type_of(cap_param).as_key();
                        let v = ssa.field(cap_param.value(), mk, field, ty);
                        bindmap.insert(bind, v);
                    }
                }

                // Reserve the key in case of recursion
                let origin = typing.origin.clone();
                let returns = typing.returns.clone();

                let symbol = func_symbol(mir, self.functions.next_key(), &origin);
                let mfkey = self.push_function(symbol, ssa, returns);

                let key = MonoTypesKey::new(
                    typing.origin.clone(),
                    tmap.generics.clone(),
                    tmap.self_.clone(),
                );
                self.mono_resolve.insert(key, mfkey);

                let lower = FuncLower {
                    lir: self,
                    mir,
                    iquery,
                    info,
                    current: Current { origin, mfkey, tmap, bindmap, has_captures },
                };

                lower.run();

                mfkey
            }
        }
    }

    fn push_function(
        &mut self,
        symbol: String,
        blocks: ssa::Blocks,
        returns: MonoType,
    ) -> MonoFunc {
        let func = Function { symbol, blocks, returns };
        let key = self.functions.push(func);
        info!("reserving {key} as {}", &self.functions[key].symbol);
        key
    }
}

impl FuncOrigin {
    fn module(&self) -> key::Module {
        match self {
            FuncOrigin::Defined(key) => key.module,
            FuncOrigin::Method(key, _) => key.module,
            FuncOrigin::Lambda(orig, _) => orig.module(),
        }
    }

    fn get_root_fdef<'m>(&self, mir: &'m mir::MIR) -> &'m mir::Function {
        match self {
            FuncOrigin::Defined(f) => mir.funcs[*f].as_done(),
            FuncOrigin::Method(ikey, m) => {
                let f = mir.imethods[*ikey][*m].unwrap();
                mir.funcs[f].as_done()
            }
            FuncOrigin::Lambda(origin, _) => origin.get_root_fdef(mir),
        }
    }
}

impl<'a> FuncLower<'a> {
    fn ssa(&mut self) -> &mut ssa::Blocks {
        &mut self.lir.functions[self.current.mfkey].blocks
    }

    fn expr_of_origin(&mut self, f: FuncOrigin) -> &'a mir::Expr {
        match f {
            FuncOrigin::Defined(func) => &self.mir.funcs[func].as_done().expr,
            FuncOrigin::Method(imp, method) => {
                let fkey = self.mir.imethods[imp][method].unwrap();
                &self.mir.funcs[fkey].as_done().expr
            }
            FuncOrigin::Lambda(new, lkey) => match *new {
                FuncOrigin::Defined(func) => &self.mir.funcs[func].as_done().lambdas[lkey].expr,
                FuncOrigin::Method(imp, method) => {
                    let fkey = self.mir.imethods[imp][method].unwrap();
                    &self.mir.funcs[fkey].as_done().lambdas[lkey].expr
                }
                _ => unreachable!("{new}"),
            },
        }
    }

    pub fn run(mut self) {
        let origin = self.current.origin.clone();

        let expr = self.expr_of_origin(origin.clone());

        self.expr_to_flow(&expr);

        let func_slot = &mut self.lir.functions[self.current.mfkey];
        info!(
            "resulting blocks for {}:\n{}",
            &func_slot.symbol,
            self.lir.types.fmt(&func_slot.blocks)
        );
    }

    fn params_to_values(&mut self, params: &[mir::Expr]) -> Vec<ssa::Value> {
        params.iter().map(|p| self.expr_to_value(p)).collect()
    }

    fn morphise_inst(&mut self, inst: &GenericMapper<Static>) -> TypeMap {
        let mut morph = to_morphization!(self.lir, self.mir, &mut self.current.tmap);

        let mut tmap = TypeMap::new();

        for (generic, hty) in inst.generics.iter() {
            let ty = morph.apply(hty);
            let weak = morph.apply_weak(hty);
            tmap.push(*generic, weak, ty);
        }

        if let Some(ty) = inst.self_.as_ref() {
            panic!("morphise_inst used when tmap from implementation query takes priority (self = {ty})");
        }

        tmap
    }

    fn elems_to_tuple(&mut self, elems: Vec<Value>, payload: Option<u32>) -> Value {
        let ty = payload
            .map(|largest| MonoType::SumDataCast { largest })
            .unwrap_or_else(|| {
                let types = elems.iter().map(|v| self.type_of_value(*v)).collect();
                self.lir.types.get_or_make_tuple(types).into()
            });

        self.ssa().construct(elems, ty)
    }

    // Relies on the layout of `std:prelude:List`
    //
    // type List a = Slice (Slice a) | Concat self self | Singleton a | Nil
    fn values_to_cons_list(&mut self, values: Vec<Value>, inner: Type) -> Value {
        let list_type: MonoType = to_morphization!(self.lir, self.mir, &mut self.current.tmap)
            .defined(self.info.global_list_default, &[inner])
            .into();

        let [nil_tag, singleton_tag, concat_tag] = [LIST_NIL, LIST_SINGLETON, LIST_CONCAT]
            .map(|n| Value::Int(n.0 as i128, mono::TAG_SIZE));

        let payload_size = self.lir.types.types.size_of(&list_type) - mono::TAG_SIZE.bits() as u32;

        let empty_tuple = self.elems_to_tuple(vec![], Some(payload_size));

        // TODO: we technically don't need a `Nil` at the end since we're using
        // singleton. But; perhaps it's still a good idea?
        let init = self
            .ssa()
            .construct(vec![nil_tag, empty_tuple], list_type.clone());

        values.into_iter().rev().fold(init, |next, v| {
            let singleton_payload = self.elems_to_tuple(vec![v], Some(payload_size));

            let this = self
                .ssa()
                .construct(vec![singleton_tag, singleton_payload], list_type.clone());

            let concat_payload = self.elems_to_tuple(vec![this, next], Some(payload_size));
            self.ssa()
                .construct(vec![concat_tag, concat_payload], list_type.clone())
        })
    }

    fn string_type(&mut self) -> MonoType {
        let string_type = Type::defined(self.info.string, vec![]);
        to_morphization!(self.lir, self.mir, &mut self.current.tmap).apply(&string_type)
    }

    fn string_from_raw_parts_mfunc(&mut self) -> (MonoFunc, MonoType) {
        let string_type = self.string_type();

        let tmap = TypeMap::new();
        let typing = MonoTyping::new(
            FuncOrigin::Defined(self.info.string_from_raw_parts),
            [
                MonoType::u8_pointer(),
                MonoType::Int(IntSize::new(false, self.lir.target.int_size())),
            ]
            .into_iter()
            .collect(),
            string_type.clone(),
        );

        let func = self
            .lir
            .func(self.mir, self.iquery, self.info, tmap, typing, None);
        (func, string_type)
    }

    fn string_to_value(&mut self, str: &[u8]) -> Value {
        let (mfunc, string) = self.string_from_raw_parts_mfunc();

        let ro = self.string_to_readonly(str.to_vec());
        let len = Value::Int(
            str.len() as i128,
            IntSize::new(false, self.lir.target.int_size()),
        );

        self.ssa()
            .call(mfunc, vec![Value::ReadOnly(ro), len], string)
    }

    fn string_to_readonly(&mut self, str: Vec<u8>) -> M<key::ReadOnly> {
        let module = self.current.origin.module();
        let ro = self.lir.read_only_table[module].push((
            mir::ReadOnlyBytes(str.into_boxed_slice()),
            MonoType::u8_pointer(),
        ));
        module.m(ro)
    }

    // Instantiates and lowers the lambda as a function.
    // Monomorphises the captures as a tuple.
    // Constructs the capture tuple in the SSA
    fn morphise_lambda(
        &mut self,
        lambda: key::Lambda,
        inst: &GenericMapper<Static>,
    ) -> (MonoFunc, ssa::Value, MonoTypeKey, MonoType) {
        let (func, captures) = self.get_lambda_origin(self.current.origin.clone(), lambda);

        let mut tmap = self.morphise_inst(inst);

        let mut morph = to_morphization!(self.lir, self.mir, &mut tmap);

        // Add the generics that are still in scope from the parent
        for ((g, mono), (_, ty)) in self
            .current
            .tmap
            .generics
            .iter()
            .zip(&self.current.tmap.weak.generics)
        {
            if !matches!(g.kind, GenericKind::Lambda(_)) {
                morph.tmap.push(*g, ty.clone(), mono.clone());
            }
        }
        if let Some(self_) = self.current.tmap.self_.clone() {
            morph
                .tmap
                .set_self(self.current.tmap.weak.self_.clone().unwrap(), self_);
        }

        let fdef = self.current.origin.get_root_fdef(self.mir);
        let typing = morph.apply_typing(func, &fdef.lambdas[lambda].typing);

        // Gather the captures
        let mut catypes = vec![];
        let ca = captures
            .iter()
            .map(|bind| {
                let value = self.current.bindmap[bind];
                let ty = self.type_of_value(value);
                catypes.push(ty.clone());
                (*bind, ty)
            })
            .collect();

        let capture_tuple_ty = self.lir.types.get_or_make_tuple(catypes);

        let ca = Some(ca);

        let return_ty = typing.returns.clone();

        let mfunc = self
            .lir
            .func(self.mir, self.iquery, self.info, tmap, typing, ca);

        let captures = {
            let values = captures
                .iter()
                .map(|bind| self.current.bindmap[bind].into())
                .collect();
            self.ssa()
                .construct(values, MonoType::Monomorphised(capture_tuple_ty))
        };

        (mfunc, captures, capture_tuple_ty, return_ty)
    }

    fn get_lambda_origin(
        &self,
        origin: FuncOrigin,
        lambda: key::Lambda,
    ) -> (FuncOrigin, &'a [key::Bind]) {
        match &origin {
            FuncOrigin::Defined(f) => {
                let origin = FuncOrigin::Lambda(Box::new(origin.clone()), lambda);
                let func = self.mir.funcs[*f].as_done();
                let captures = &func.lcaptures[lambda];
                (origin, captures)
            }
            FuncOrigin::Method(imp, m) => {
                let origin = FuncOrigin::Lambda(Box::new(origin.clone()), lambda);
                let fkey = self.mir.imethods[*imp][*m].unwrap();
                let func = self.mir.funcs[imp.module.m(*fkey)].as_done();
                let captures = &func.lcaptures[lambda];
                (origin, captures)
            }
            FuncOrigin::Lambda(origin, _) => self.get_lambda_origin((**origin).clone(), lambda),
        }
    }

    fn type_of_value(&mut self, value: ssa::Value) -> MonoType {
        match value {
            ssa::Value::ReadOnly(ro) => self.lir.read_only_table[ro].1.clone(),
            ssa::Value::V(v) => self.ssa().type_of(v).clone(),
            ssa::Value::Int(_, intsize) => MonoType::Int(intsize),
            ssa::Value::Float(_) => MonoType::Float,
            ssa::Value::FuncPtr(ptr) => {
                let func = &self.lir.functions[ptr];
                func.blocks.as_fnpointer(ssa::Block::entry())
            }
        }
    }

    fn resolve_nfunc(
        &mut self,
        func: M<ast::NFunc>,
        inst: &GenericMapper<Static>,
    ) -> ResolvedNFunc {
        match func.value {
            ast::NFunc::Key(key) => {
                let key = func.module.m(key);
                match &self.mir.funcs[key] {
                    mir::FunctionStatus::Extern { typing, .. } => {
                        let mut map = TypeMap::new();
                        let ret =
                            to_morphization!(self.lir, self.mir, &mut map).apply(&typing.returns);
                        ResolvedNFunc::Extern(key, ret)
                    }
                    _ => {
                        let func = FuncOrigin::Defined(key);
                        let tmap = self.morphise_inst(inst);
                        let (mfunc, ret) = self.call_to_mfunc(func, tmap);
                        ResolvedNFunc::Static(mfunc, ret)
                    }
                }
            }
            ast::NFunc::Val(val) => {
                let key = func.module.m(val);
                let ty: MonoType = self.lir.vals[key].clone();

                ResolvedNFunc::Val(key, ty)
            }
            ast::NFunc::Method(key, method) => {
                let trait_ = func.module.m(key);

                let mut morph = to_morphization!(self.lir, self.mir, &mut self.current.tmap);

                let self_ = inst.self_.as_ref().unwrap();

                let weak_impltor = morph.apply_weak(self_);
                let impltor = morph.apply(self_);

                let trtp = inst
                    .types(GenericKind::Parent)
                    .map(|ty| morph.apply_weak(ty))
                    .collect::<Vec<_>>();

                let (ikey, itmap) = self.find_implementation(trait_, &trtp, weak_impltor, impltor);

                let forigin = FuncOrigin::Method(ikey, method);

                let (mfunc, ret) = self.call_to_mfunc(forigin, itmap);

                ResolvedNFunc::Static(mfunc, ret)
            }
            ast::NFunc::SumVar(sum, var) => {
                let sum = func.map(|_| sum);

                let mut tmap = self.morphise_inst(inst);
                let type_params = tmap.weak.to_types(GenericKind::Entity);

                let mut morph = to_morphization!(self.lir, self.mir, &mut tmap);
                let ty = morph.sum(sum, &type_params);

                let tag = Value::Int(var.0 as i128, mono::TAG_SIZE);

                let (tag_size, payload_size) = self.lir.types.types.as_sum_type(ty).unwrap();
                assert_eq!(tag_size, mono::TAG_SIZE.bits() as u32);

                ResolvedNFunc::Sum { tag, ty, payload_size }
            }
        }
    }

    pub fn ty_symbol(&self, ty: &MonoType) -> String {
        match ty {
            MonoType::Array(inner, times) => format!("[{}; {times}]", self.ty_symbol(&inner)),
            MonoType::Pointer(inner) => format!("*{}", self.ty_symbol(inner)),
            MonoType::FnPointer(params, ret) => format!(
                "fnptr({} -> {})",
                params.iter().map(|ty| self.ty_symbol(ty)).format(", "),
                self.ty_symbol(&ret)
            ),
            MonoType::Unreachable => format!("!"),
            MonoType::Monomorphised(mk) => match self.lir.types.types[*mk].original {
                Some(key) => format!("{}>{mk}", self.mir.name_of_type(key)),
                None => mk.to_string(),
            },
            _ => MonoFormatter { types: &self.lir.types.types, v: ty }.to_string(),
        }
    }
}

fn func_symbol(mir: &mir::MIR, key: MonoFunc, origin: &FuncOrigin) -> String {
    if let FuncOrigin::Defined(key) = origin {
        if mir.funcs[*key].as_done().no_mangle {
            return mir.func_names[*key].clone();
        }
    }

    let module = origin.module();
    let mname = &mir.module_names[module];
    let fname = origin.name(mir);
    format!("{module}:{mname}:{fname}:{key}")
}

#[derive(Debug)]
enum ResolvedNFunc {
    Extern(M<key::Func>, MonoType),
    Static(MonoFunc, MonoType),
    Sum {
        tag: Value,
        payload_size: u32,
        ty: MonoTypeKey,
    },
    Val(M<key::Val>, MonoType),
}

struct Closure {
    vtableptr: V,
    object: MonoTypeKey,
    vtable: MonoTypeKey,
    vtable_init: MonoFunc,
    vtable_val: M<key::Val>,
}

impl fmt::Display for FuncOrigin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FuncOrigin::Defined(func) => func.fmt(f),
            FuncOrigin::Method(ikey, method) => write!(f, "{ikey}:{method}"),
            FuncOrigin::Lambda(parent, lambda) => write!(f, "{parent}:{lambda}"),
        }
    }
}

impl fmt::Display for Output {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (val, func) in &self.val_initializers {
            writeln!(f, "{} {val} {} {func}\n", "val".keyword(), '='.symbol())?;
        }

        self.functions
            .values()
            .format_with("\n\n", |func, f| {
                f(&format_args!("{}", mono::fmt(&self.types, func)))
            })
            .fmt(f)
    }
}
