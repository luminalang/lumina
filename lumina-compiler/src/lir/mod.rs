//! Monomorphization
//! Closure lowering to vtables
//! Dynamic Object lowering to vtables
//! Pattern decision trees lowering to expressions
//! Flatten to SSA+CFG with Basic Blocks

use crate::prelude::*;
use crate::{ProjectInfo, CLOSURE_CAPTURES};
use key::{entity_impl, keys};
use lumina_typesystem::{Bitsize, ConcreteInst, Generic, GenericKind, ImplIndex, Type};
use lumina_util::Highlighting;
use std::fmt;
use tracing::info_span;

const UNIT: MonoTypeKey = MonoTypeKey(0);

#[macro_export]
macro_rules! to_morphization {
    ($this:expr, $tmap:expr) => {
        crate::lir::mono::Monomorphization::new(
            &mut $this.lir.types,
            &$this.mir.field_types,
            &$this.mir.variant_types,
            &$this.mir.methods,
            &$this.mir.funcs,
            &$this.mir.trait_objects,
            $tmap,
        )
    };
}

mod mono;
mod ssa;
pub use mono::{
    fmt as ty_fmt, BitOffset, MonoFormatter, MonoType, MonoTypeKey, MonomorphisedRecord, Records,
    TypeMap,
};
pub use ssa::{Block, BlockParam, Blocks, ControlFlow, Entry, Value, V};
mod closure;
mod expr;
mod pat;

keys! { MonoFunc . "mfunc" }

pub struct Output {
    pub functions: Map<MonoFunc, Function>,
    pub extern_funcs: HashMap<M<key::Func>, ExternFunction>,
    pub val_initializers: HashMap<M<key::Val>, MonoFunc>,
    pub val_types: ModMap<key::Val, MonoType>,

    pub read_only_table: ModMap<key::ReadOnly, (mir::ReadOnlyBytes, Type)>,

    pub types: Records,

    pub func_names: ModMap<key::Func, String>,
    pub module_names: Map<key::Module, String>,

    pub main: MonoFunc,

    pub alloc: MonoFunc,
    pub dealloc: MonoFunc,
}

#[derive(new)]
struct LIR {
    #[new(default)]
    mono_resolve: HashMap<MonoTyping, MonoFunc>,
    #[new(default)]
    functions: Map<MonoFunc, Function>,
    types: mono::MonomorphisedTypes,
    #[new(default)]
    vtables: HashMap<MonoTypeKey, M<key::Val>>,

    vals: ModMap<key::Val, MonoType>,
    #[new(default)]
    val_initialisers: HashMap<M<key::Val>, MonoFunc>,

    #[new(default)]
    alloc: Option<MonoFunc>,
    #[new(default)]
    dealloc: Option<MonoFunc>,
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

pub fn run<'s>(info: ProjectInfo, iquery: &ImplIndex, mir: mir::MIR) -> Output {
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
    };

    let mut tmap = TypeMap::new();
    let mut monomorphization = to_morphization(&mir, &mut mono, &mut tmap);

    let mut vals = mir.val_initializers.secondary_inner_capacity();
    for val in mir.val_initializers.iter() {
        let func = mir.val_initializers[val];
        let typing = &mir.funcs[func].as_done().typing;
        assert!(
            typing.params.is_empty(),
            "static vals cannot take parameters in their initialiser"
        );
        assert!(typing.forall.is_empty(), "static vals cannot be generic");
        let ret = monomorphization.apply(&typing.returns);
        vals.push_as(val, ret);
    }

    // the main functions generics are substituted for unit when used as entrypoint
    monomorphization.substitute_generics_for_unit_type(&mainfunc.typing.forall);

    let returns = monomorphization.apply(&mainfunc.typing.returns);

    assert!(
        mainfunc.typing.params.is_empty(),
        "main function can not take parameters"
    );

    let mut lir = LIR::new(mono, vals);

    // fn alloc size as int -> *u8 =
    // fn dealloc ptr size as *u8, int -> () =
    let [alloc, dealloc] = {
        let unit = MonoType::Monomorphised(lir.types.get_or_make_tuple(vec![]));

        let mut allocparams = Map::new();
        allocparams.push(MonoType::Int(Bitsize(64)));

        let mut deallocparams = Map::new();
        deallocparams.push(MonoType::u8_pointer());
        deallocparams.push(MonoType::Int(Bitsize(64)));

        [
            (info.allocator.0, allocparams, MonoType::u8_pointer()),
            (info.allocator.1, deallocparams, unit),
        ]
        .map(|(func, ptypes, ret)| {
            let typing = MonoTyping::new(FuncOrigin::Defined(func), ptypes, ret);
            lir.func(&mir, iquery, info, TypeMap::new(), typing, None)
        })
    };
    lir.alloc = Some(alloc);
    lir.dealloc = Some(dealloc);

    let main = {
        let typing = MonoTyping::new(FuncOrigin::Defined(info.main), Map::new(), returns);
        lir.func(&mir, iquery, info, tmap, typing, None)
    };

    for val in mir.val_initializers.iter() {
        let func = mir.val_initializers[val];
        let returns = lir.vals[val].clone();
        let typing = MonoTyping::new(FuncOrigin::Defined(func), Map::new(), returns);
        let mfunc = lir.func(&mir, iquery, info, TypeMap::new(), typing, None);
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
        read_only_table: mir.read_only_table,
        func_names: mir.func_names,
        module_names: mir.module_names,
        types: lir.types.into_records(),
        alloc,
        dealloc,
        main,
    }
}

impl LIR {
    fn func<'h, 's>(
        &mut self,
        mir: &mir::MIR,
        iquery: &ImplIndex,
        info: ProjectInfo,
        mut tmap: TypeMap,
        typing: MonoTyping,
        captures: Option<Map<key::Capture, (key::Bind, MonoType)>>,
    ) -> MonoFunc {
        match self.mono_resolve.get(&typing) {
            Some(key) => *key,
            None => {
                let _span = info_span!(
                    "lowering function",
                    module = &mir.module_names[typing.origin.module()],
                    entity = typing.origin.name(mir),
                );
                let _handle = _span.enter();

                let has_captures = captures.is_some();

                // Add an implicit capture record as the first parameter if this is a lambda
                let mut ssa = if let Some(captures) = captures.as_ref() {
                    info!(
                        "setting up capture tuple [{}]",
                        captures
                            .iter()
                            .map(|(src, (dst, _))| format!("{src}->{dst}"))
                            .format(" ")
                    );

                    let mut ssa = ssa::Blocks::new(Map::new());
                    let mut morph = mono::Monomorphization::new(
                        &mut self.types,
                        &mir.field_types,
                        &mir.variant_types,
                        &mir.methods,
                        &mir.funcs,
                        &mir.trait_objects,
                        &mut tmap,
                    );
                    let entry = ssa::Block::entry();

                    let capture_record = morph.create_capture_record(&captures);

                    ssa.add_block_param(entry, MonoType::Monomorphised(capture_record));

                    for ty in typing.params.values() {
                        ssa.add_block_param(entry, ty.clone());
                    }

                    ssa
                } else {
                    info!("adding block parameters for {}", self.types.fmt(&typing));
                    ssa::Blocks::new(typing.params.clone())
                };

                let mut bindmap = HashMap::new();

                // Add captures to the bindmap if this is a lambda
                if let Some(captures) = captures {
                    for (i, (bind, ty)) in captures {
                        let cap_param = ssa::BlockParam(0);
                        let field = key::RecordField(i.0);
                        let mk = ssa.type_of_param(ssa::Block::entry(), cap_param).as_key();
                        let v = ssa.field(cap_param.into(), mk, field, ty);
                        bindmap.insert(bind, v.into());
                    }
                }

                // Reserve the key in case of recursion
                let origin = typing.origin.clone();
                let returns = typing.returns.clone();
                // HACK: If we need to generate a vtable that references this function; then the
                // typing still needs to be available.
                //
                // FUCK: the symbol also needs to be available. The hack would still work but the
                // symbol names will kinda suck.
                //
                // Perhaps we should access the ssa via `current` instead?
                // we already use a helper
                let symbol = func_symbol(mir, self.functions.next_key(), &origin);
                let mfkey = self.push_function(symbol, ssa, returns);
                self.mono_resolve.insert(typing, mfkey);

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

    fn morphise_inst(&mut self, kinds: [GenericKind; 2], inst: &ConcreteInst) -> TypeMap {
        let mut morph = to_morphization!(self, &mut self.current.tmap);

        let mut tmap = TypeMap::new();

        for (key, hty) in &inst.pgenerics {
            let ty = morph.apply(hty);
            let weak = morph.apply_weak(hty);
            let generic = Generic::new(key, kinds[0]);
            tmap.generics.push((generic, (weak, ty)));
        }

        for (key, hty) in &inst.generics {
            let ty = morph.apply(hty);
            let weak = morph.apply_weak(hty);
            let generic = Generic::new(key, kinds[1]);
            tmap.generics.push((generic, (weak, ty)));
        }

        if let Some(hty) = inst.self_.as_ref() {
            let ty = morph.apply(hty);
            let weak = morph.apply_weak(hty);

            // HACK: if finding the implementation of `self` lands us at one with generics then
            // we need to add those generics now since they weren't known during the MIR lower.
            match &weak {
                Type::Defined(_, params) | Type::List(_, params) => {
                    for (i, p) in params.iter().enumerate() {
                        let key = key::Generic(i as u32);
                        let kind = GenericKind::Parent;

                        let already_defined = tmap
                            .generics
                            .iter()
                            .any(|(gen, _)| gen.kind == kind && gen.key == key);

                        if !already_defined {
                            // TODO: Is it valid to just throw it *back* into monomorphisation?
                            //
                            // It shouldn't contain any generics so I guess so
                            let mty = morph.apply(p);
                            tmap.generics
                                .push((Generic::new(key, kind), (p.clone(), mty)));
                        }
                    }
                }
                _ => {}
            }

            tmap.self_ = Some((weak, ty));
        }

        tmap
    }

    // Rebinds `BlockParam`s so that this value can safely be used in a future block
    // We don't use PHI block parameters, any block parameter is assumed to be for the current block.
    //
    // WARNING: might cause incorrect behavior on recursion
    pub fn ensure_no_scope_escape(&mut self, value: ssa::Value) -> ssa::Value {
        match value {
            ssa::Value::BlockParam(bparam) => {
                let block = self.ssa().block();
                let ty = self.ssa().type_of_param(block, bparam).clone();
                self.ssa().copy(value, ty).into()
            }
            _ => value,
        }
    }

    // Instantiates and lowers the lambda as a function.
    // Monomorphises the captures as a tuple.
    // Constructs the capture tuple in the SSA
    fn morphise_lambda(
        &mut self,
        lambda: key::Lambda,
        inst: &ConcreteInst,
    ) -> (MonoFunc, ssa::Value, MonoTypeKey, MonoType) {
        let (func, captures) = self.get_lambda_origin(self.current.origin.clone(), lambda);

        let kinds = [GenericKind::Entity, GenericKind::Lambda(lambda)];
        let mut tmap = self.morphise_inst(kinds, inst);

        let mut morph = to_morphization!(self, &mut tmap);

        // Add the generics that are still in scope from the parent
        let in_scope = self
            .current
            .tmap
            .generics
            .iter()
            .filter(|(g, _)| !matches!(g.kind, GenericKind::Lambda(..)))
            .cloned();

        // TODO: we probably need to add `self` as well?
        morph.tmap.generics.extend(in_scope);

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

        (mfunc, captures.into(), capture_tuple_ty, return_ty)
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
            ssa::Value::BlockParam(param) => {
                let block = self.ssa().block();
                self.ssa().type_of_param(block, param).clone()
            }
            ssa::Value::ReadOnly(ro) => {
                let ty = &self.mir.read_only_table[ro].1;
                to_morphization!(self, &mut self.current.tmap).apply(&ty)
            }
            ssa::Value::V(v) => self.ssa().type_of(v).clone(),
            ssa::Value::Int(_, bitsize) => MonoType::Int(bitsize),
            ssa::Value::UInt(_, bitsize) => MonoType::UInt(bitsize),
            ssa::Value::Float(_) => MonoType::Float,
            ssa::Value::FuncPtr(ptr) => {
                let func = &self.lir.functions[ptr];
                func.blocks.as_fnpointer(ssa::Block::entry())
            }
        }
    }

    fn resolve_nfunc(&mut self, func: M<ast::NFunc>, inst: &ConcreteInst) -> ResolvedNFunc {
        match func.value {
            ast::NFunc::Key(key) => {
                let key = func.module.m(key);
                match &self.mir.funcs[key] {
                    mir::FunctionStatus::Extern { typing, .. } => {
                        let mut map = TypeMap::new();
                        let ret = to_morphization!(self, &mut map).apply(&typing.returns);
                        ResolvedNFunc::Extern(key, ret)
                    }
                    _ => {
                        let func = FuncOrigin::Defined(key);
                        let tmap =
                            self.morphise_inst([GenericKind::Parent, GenericKind::Entity], inst);
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

                let morph = to_morphization!(self, &mut self.current.tmap);

                let self_ = inst.self_.as_ref().unwrap();

                let weak_impltor = morph.apply_weak(self_);

                let trtp = inst
                    .pgenerics
                    .values()
                    .map(|ty| morph.apply_weak(ty))
                    .collect::<Vec<_>>();

                let ikey = self.find_implementation(trait_, &trtp, &weak_impltor);

                let forigin = FuncOrigin::Method(ikey, method);

                let tmap = self.morphise_inst([GenericKind::Parent, GenericKind::Entity], inst);
                let (mfunc, ret) = self.call_to_mfunc(forigin, tmap);

                ResolvedNFunc::Static(mfunc, ret)
            }
            ast::NFunc::SumVar(sum, var) => {
                let sum = func.map(|_| sum);

                let ptypes = inst.generics.values().cloned().collect::<Vec<_>>();

                let mut morph = to_morphization!(self, &mut self.current.tmap);
                let ty = morph.sum(sum, &ptypes);

                let tag = Value::UInt(var.0 as u128, mono::TAG_SIZE);

                let size = self.lir.types.types.size_of_defined(ty);
                let payload_size = size - mono::TAG_SIZE.0 as u32;

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
                Some(key) => format!("{key}>{mk}"),
                None => mk.to_string(),
            },
            _ => MonoFormatter { types: &self.lir.types.types, v: ty }.to_string(),
        }
    }
}

fn func_symbol(mir: &mir::MIR, key: MonoFunc, origin: &FuncOrigin) -> String {
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
