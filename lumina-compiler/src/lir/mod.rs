//! Monomorphization
//! Closure lowering to vtables
//! Dynamic Object lowering to vtables
//! Pattern decision trees lowering to expressions
//! Flatten to SSA+CFG with Basic Blocks

use crate::prelude::*;
use crate::{ProjectInfo, Target};
use either::Either;
use lumina_collections::map_key_impl;
use lumina_typesystem::{Generic, GenericKind, GenericMapper, ImplIndex, IntSize, Static, Type};
use lumina_util::Highlighting;
use std::fmt;
use tracing::info_span;
mod debug;
pub use debug::Debugger;

pub const UNIT: MonoTypeKey = MonoTypeKey(0);

#[macro_export]
macro_rules! to_morphization {
    ($lir:expr, $mir:expr, $tmap:expr) => {
        crate::lir::mono::Monomorphization::new(
            &mut $lir.mono,
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
mod dyn_dispatch;
mod expr;
mod pat;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct MonoFunc(u32);
map_key_impl!(MonoFunc(u32), "mfunc");

pub struct Output {
    pub functions: Map<MonoFunc, Function>,
    pub extern_funcs: HashMap<M<key::Func>, ExternFunction>,
    pub val_initializers: HashMap<M<key::Val>, MonoFunc>,
    pub val_types: MMap<key::Val, MonoType>,

    pub read_only_table: MMap<key::ReadOnly, (mir::ReadOnlyBytes, MonoType)>,

    pub types: Records,

    pub func_names: MMap<key::Func, String>,
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
    extern_funcs: HashMap<M<key::Func>, ExternFunction>,
    mono: mono::MonomorphisedTypes,

    #[new(default)]
    memo_trait_objects: HashMap<M<key::Impl>, Either<M<key::Val>, MonoFunc>>,
    #[new(default)]
    memo_closures: HashMap<(MonoFunc, Vec<MonoType>), MonoFunc>,

    read_only_table: MMap<key::ReadOnly, (mir::ReadOnlyBytes, MonoType)>,

    target: Target,

    vals: MMap<key::Val, MonoType>,
    #[new(default)]
    val_initialisers: HashMap<M<key::Val>, MonoFunc>,

    #[new(default)]
    alloc: Option<MonoFunc>,
    #[new(default)]
    dealloc: Option<MonoFunc>,

    #[new(default)]
    stringable: Option<Stringable>,
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
    ClosurePartialApplication {
        in_: MonoFunc,
        target: V,
        params: Vec<MonoType>,
    },
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
    captures: Option<usize>,
}

pub struct Function {
    pub symbol: String,
    pub blocks: ssa::Blocks,
    pub returns: MonoType,
}

impl Function {
    pub fn as_fnpointer(&self) -> MonoType {
        let params = self.blocks.param_types(Block::entry()).cloned().collect();
        let ret = self.returns.clone();
        MonoType::FnPointer(params, Box::new(ret))
    }
}

pub struct ExternFunction {
    pub symbol: String,
    pub params: Vec<MonoType>,
    pub returns: MonoType,
}

#[derive(Clone, PartialEq, Eq, Hash, new)]
struct MonoTyping {
    origin: FuncOrigin,
    params: Map<key::Param, MonoType>,
    returns: MonoType,
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum FuncOrigin {
    SumConstructorWrapper(M<key::Sum>, key::Variant),
    Defined(M<key::Func>),
    Method(M<key::Impl>, key::Method),
    Lambda(Box<FuncOrigin>, key::Lambda),
}

impl FuncOrigin {
    fn name(&self, mir: &mir::MIR) -> String {
        match self {
            FuncOrigin::SumConstructorWrapper(sum, var) => mir.variant_names[*sum][*var].clone(),
            FuncOrigin::Defined(key) => mir.func_names[*key].clone(),
            FuncOrigin::Method(impl_, method) => {
                mir.func_names[mir.imethods[*impl_][*method].unwrap().inside(impl_.0)].clone()
            }
            FuncOrigin::Lambda(parent, lkey) => format!("{}:{lkey}", parent.name(mir)),
        }
    }
}

enum Callable {
    Extern(M<key::Func>),
    Static(MonoFunc),
    LiftedLambda(MonoFunc, Vec<Value>),
    Val(M<key::Val>),
    Sum {
        tmap: TypeMap,
        var: key::Variant,
        payload_size: u32,
        ty: MonoTypeKey,
    },
    Local(Value),
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

    // TODO: This is kinda hacky and inefficient.
    //
    // We should probably seperate out externs entirely instead of treating them like semi-functions.
    let extern_funcs = mir
        .funcs
        .iter()
        .filter_map(|func| match &mir.funcs[func] {
            mir::FunctionStatus::Extern { link_name, typing } => {
                let mut tmap = TypeMap::new();
                let mut monomorphization = to_morphization(&mir, &mut mono, &mut tmap);
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

    let mut tmap = TypeMap::new();
    let mut monomorphization = to_morphization(&mir, &mut mono, &mut tmap);

    // the main functions generics are substituted for unit when used as entrypoint
    monomorphization.substitute_generics_for_unit_type(&mainfunc.typing.forall);

    let returns = monomorphization.apply(&mainfunc.typing.returns);

    let vals = mir.val_initializers.map(|_, func| {
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
    let mut read_only_table = mir.read_only_table.secondary();
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

    let mut lir = LIR::new(extern_funcs, mono, read_only_table, target, vals);

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

    #[cfg(debug_assertions)]
    Debugger::new(&lir, &mir).run();

    Output {
        functions: lir.functions,
        extern_funcs: lir.extern_funcs,
        val_initializers: lir.val_initialisers,
        val_types: lir.vals,
        read_only_table: lir.read_only_table,
        func_names: mir.func_names,
        module_names: mir.module_names,
        types: lir.mono.into_records(),
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

                let entryblock = ssa::Block::entry();
                let mut bindmap = HashMap::new();

                // Add an implicit capture record as the first parameter if this is a lambda
                let mut ssa = if let Some(captures) = captures.as_ref() {
                    info!(
                        "setting up captures [{}]",
                        captures
                            .iter()
                            .map(|(src, (dst, _))| format!("{src}->{dst}"))
                            .format(" ")
                    );

                    let mut ssa =
                        ssa::Blocks::new(typing.params.len() as u32 + captures.len() as u32);

                    for (bind, ty) in captures.values() {
                        let v = ssa.add_block_param(entryblock, ty.clone());
                        bindmap.insert(*bind, v.value());
                    }

                    ssa
                } else {
                    ssa::Blocks::new(typing.params.len() as u32)
                };

                info!("adding block parameters for {}", self.mono.fmt(&typing));
                for ty in typing.params.values() {
                    ssa.add_block_param(entryblock, ty.clone());
                }

                let capture_count = captures.as_ref().map(|elems| elems.len());

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
                    current: Current { origin, mfkey, tmap, bindmap, captures: capture_count },
                };

                lower.run();

                mfkey
            }
        }
    }

    fn type_of_value(&self, mfkey: MonoFunc, value: ssa::Value) -> MonoType {
        match value {
            ssa::Value::ReadOnly(ro) => MonoType::pointer(self.read_only_table[ro].1.clone()),
            ssa::Value::V(v) => self.functions[mfkey].blocks.type_of(v).clone(),
            ssa::Value::Int(_, intsize) => MonoType::Int(intsize),
            ssa::Value::Float(_) => MonoType::Float,
            ssa::Value::FuncPtr(ptr) => {
                let func = &self.functions[ptr];
                func.as_fnpointer()
            }
            ssa::Value::ExternFuncPtr(ptr) => {
                let func = &self.extern_funcs[&ptr];
                MonoType::FnPointer(func.params.clone(), Box::new(func.returns.clone()))
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
        self.functions.push(func)
    }

    fn mfunc_to_fnpointer_type(&self, mfunc: MonoFunc) -> MonoType {
        let ptypes = self.functions[mfunc].blocks.func_params();
        let ret = self.functions[mfunc].returns.clone();
        MonoType::FnPointer(ptypes, Box::new(ret))
    }
}

impl FuncOrigin {
    fn module(&self) -> key::Module {
        match self {
            FuncOrigin::Method(M(module, _), _)
            | FuncOrigin::SumConstructorWrapper(M(module, _), _)
            | FuncOrigin::Defined(M(module, _)) => *module,
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
            FuncOrigin::SumConstructorWrapper(_, _) => {
                panic!("sum constructor have no mir function definition")
            }
        }
    }
}

impl<'a> FuncLower<'a> {
    fn ssa(&mut self) -> &mut ssa::Blocks {
        &mut self.lir.functions[self.current.mfkey].blocks
    }

    fn types(&self) -> &Records {
        &self.lir.mono.types
    }

    fn type_of_value(&self, v: Value) -> MonoType {
        self.lir.type_of_value(self.current.mfkey, v)
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
            FuncOrigin::SumConstructorWrapper(_, _) => {
                panic!("sum constructor have no mir function definition")
            }
        }
    }

    pub fn run(mut self) {
        let origin = self.current.origin.clone();

        let expr = self.expr_of_origin(origin.clone());

        self.expr_to_flow(&expr);

        let func_slot = &mut self.lir.functions[self.current.mfkey];
        info!(
            "resulting lir function for {}:\n{}",
            &func_slot.symbol,
            self.lir.mono.fmt(&*func_slot)
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
                self.lir.mono.get_or_make_tuple(types).into()
            });

        self.ssa().construct(elems, ty)
    }

    fn monofunc_wrapper_for_sumvar(
        &mut self,
        ty: MonoTypeKey,
        mut tmap: TypeMap,
        var: key::Variant,
    ) -> MonoFunc {
        let data = &self.lir.mono.types[ty];
        let Some(M(module, key::TypeKind::Sum(key))) = data.original else {
            panic!();
        };
        let sum = key.inside(module);

        let origin = FuncOrigin::SumConstructorWrapper(sum, var);

        // Use the sum-type monomorphic key in the hashing for mono_resolve
        let ty = MonoType::Monomorphised(ty);
        let key = MonoTypesKey::new(origin.clone(), vec![], Some(ty.clone()));

        match self.lir.mono_resolve.get(&key) {
            Some(mfunc) => *mfunc,
            None => {
                let mfunc = self.lir.functions.next_key();

                let mut morph = to_morphization!(self.lir, self.mir, &mut tmap);
                let raw_params = &self.mir.variant_types[sum][var];

                let types: Vec<_> = raw_params.iter().map(|ty| morph.apply(ty)).collect();

                let mut ssa = Blocks::new(raw_params.len() as u32);
                let block_params = types
                    .iter()
                    .cloned()
                    .map(|ty| ssa.add_block_param(ssa::Block::entry(), ty))
                    .map(V::value)
                    .collect::<Vec<_>>();

                let params_tuple = self.lir.mono.get_or_make_tuple(types).into();

                ssa.construct(block_params, params_tuple);

                let function = Function {
                    symbol: func_symbol(self.mir, mfunc, &origin),
                    blocks: ssa,
                    returns: ty,
                };

                self.lir.functions.push_as(mfunc, function);
                self.lir.mono_resolve.insert(key, mfunc);

                mfunc
            }
        }
    }

    fn stringable(&mut self) -> Stringable {
        match self.lir.stringable {
            Some(str) => str,
            None => {
                let weakstring = Type::string(self.info.string, vec![]);
                let type_ = self.string_type();

                let (ikey, tmap) =
                    self.find_implementation(self.info.stringable, &[], weakstring, type_.into());

                let [split_at, split_while, split_first, equals, from_raw_parts] = [0, 1, 2, 3, 4]
                    .map(key::Method)
                    .map(|method| FuncOrigin::Method(ikey, method))
                    .map(|origin| self.call_to_mfunc(origin, tmap.clone()));

                let str = Stringable {
                    split_at,
                    split_while,
                    split_first,
                    equals,
                    from_raw_parts,
                    type_,
                };

                self.lir.stringable = Some(str);

                str
            }
        }
    }

    // Relies on the layout of `std:prelude:List`
    //
    // type List a = Slice (Slice a) | Concat self self | Singleton a | Nil
    // fn values_to_cons_list(&mut self, values: Vec<Value>, inner: Type) -> Value {
    //     let list_type: MonoType = to_morphization!(self.lir, self.mir, &mut self.current.tmap)
    //         .defined(self.info.global_list_default, &[inner])
    //         .into();

    //     let [nil_tag, singleton_tag, concat_tag] = [LIST_NIL, LIST_SINGLETON, LIST_CONCAT]
    //         .map(|n| Value::Int(n.0 as i128, mono::TAG_SIZE));

    //     let payload_size = self.lir.types.types.size_of(&list_type) - mono::TAG_SIZE.bits() as u32;

    //     let empty_tuple = self.elems_to_tuple(vec![], Some(payload_size));

    //     // TODO: we technically don't need a `Nil` at the end since we're using
    //     // singleton. But; perhaps it's still a good idea?
    //     let init = self
    //         .ssa()
    //         .construct(vec![nil_tag, empty_tuple], list_type.clone());

    //     values.into_iter().rev().fold(init, |next, v| {
    //         let singleton_payload = self.elems_to_tuple(vec![v], Some(payload_size));

    //         let this = self
    //             .ssa()
    //             .construct(vec![singleton_tag, singleton_payload], list_type.clone());

    //         let concat_payload = self.elems_to_tuple(vec![this, next], Some(payload_size));
    //         self.ssa()
    //             .construct(vec![concat_tag, concat_payload], list_type.clone())
    //     })
    // }

    fn string_from_ro(&mut self, ro: M<key::ReadOnly>) -> (Value, Value) {
        let slen = self.lir.read_only_table[ro].0 .0.len();
        let (slen_arg, _) = self.uint(slen as i128);
        let ptr = Value::ReadOnly(ro);
        (self.string_from_raw_parts(ptr, slen_arg), slen_arg)
    }

    fn string_from_raw_parts(&mut self, ptr: Value, len: Value) -> Value {
        let stringable = self.stringable();
        let f = stringable.from_raw_parts;
        self.ssa().call(f, vec![ptr, len], stringable.type_.into())
    }

    fn string_split_at(&mut self, str: Value, at: Value) -> [Value; 2] {
        let stringable = self.stringable();

        let string = stringable.type_;
        let str_tuple = self.lir.mono.get_or_make_tuple(vec![string.into(); 2]);

        let split_at = stringable.split_at;

        let splitted = self.ssa().call(split_at, vec![str, at], str_tuple.into());

        [key::Field(0), key::Field(1)]
            .map(|f| self.ssa().field(splitted, str_tuple, f, string.into()))
    }

    fn string_split_while(&mut self, str: Value, f: Value) -> [Value; 2] {
        let stringable = self.stringable();

        let string = stringable.type_;
        let str_tuple = self.lir.mono.get_or_make_tuple(vec![string.into(); 2]);

        let splitted = self
            .ssa()
            .call(stringable.split_while, vec![str, f], str_tuple.into());

        [key::Field(0), key::Field(1)]
            .map(|f| self.ssa().field(splitted, str_tuple, f, string.into()))
    }

    fn string_split_first(&mut self, str: Value) -> [Value; 2] {
        let stringable = self.stringable();

        let tuple = self
            .lir
            .mono
            .get_or_make_tuple(vec![MonoType::byte(), stringable.type_.into()]);

        let splitted = self
            .ssa()
            .call(stringable.split_first, vec![str], tuple.into());

        let [x, xs] = [key::Field(0), key::Field(1)];

        [
            self.ssa().field(splitted, tuple, x, MonoType::byte()),
            self.ssa()
                .field(splitted, tuple, xs, stringable.type_.into()),
        ]
    }

    fn string_equals(&mut self, strs: [Value; 2]) -> Value {
        let stringable = self.stringable();

        self.ssa()
            .call(stringable.equals, strs.into(), MonoType::bool())
    }

    fn string_type(&mut self) -> MonoTypeKey {
        to_morphization!(self.lir, self.mir, &mut self.current.tmap).record(self.info.string, &[])
    }

    fn uint(&self, n: i128) -> (Value, IntSize) {
        let size = IntSize::new(false, self.lir.target.int_size());
        (Value::Int(n, size), size)
    }

    // Instantiates and lowers the lambda as a function.
    // Monomorphises the captures as a tuple.
    // Constructs the capture tuple in the SSA
    fn morphise_lambda(
        &mut self,
        lambda: key::Lambda,
        inst: &GenericMapper<Static>,
    ) -> (MonoFunc, Vec<ssa::Value>) {
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
        let ca = captures
            .iter()
            .map(|bind| {
                let value = self.current.bindmap[bind];
                let ty = self.type_of_value(value);
                (*bind, ty)
            })
            .collect();

        let ca = Some(ca);

        let mfunc = self
            .lir
            .func(self.mir, self.iquery, self.info, tmap, typing, ca);

        let captures = captures
            .iter()
            .map(|bind| self.current.bindmap[bind].into())
            .collect();

        (mfunc, captures)
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
                let func = self.mir.funcs[fkey.inside(imp.0)].as_done();
                let captures = &func.lcaptures[lambda];
                (origin, captures)
            }
            FuncOrigin::Lambda(origin, _) => self.get_lambda_origin((**origin).clone(), lambda),
            FuncOrigin::SumConstructorWrapper(_, _) => unreachable!(),
        }
    }

    fn lower_callable(&mut self, callable: &mir::Callable) -> Callable {
        match callable {
            mir::Callable::Func(M(module, key), mapper) => match key {
                ast::NFunc::Key(key) => {
                    let key = key.inside(*module);
                    match &self.mir.funcs[key] {
                        mir::FunctionStatus::Extern { .. } => Callable::Extern(key),
                        _ => {
                            let func = FuncOrigin::Defined(key);
                            let tmap = self.morphise_inst(mapper);
                            let mfunc = self.call_to_mfunc(func, tmap);
                            Callable::Static(mfunc)
                        }
                    }
                }
                ast::NFunc::Val(val) => Callable::Val(val.inside(*module)),
                ast::NFunc::Method(key, method) => {
                    let trait_ = key.inside(*module);

                    let mut morph = to_morphization!(self.lir, self.mir, &mut self.current.tmap);

                    let self_ = mapper.self_.as_ref().unwrap();

                    let weak_impltor = morph.apply_weak(self_);
                    let impltor = morph.apply(self_);

                    let trtp = mapper
                        .types(GenericKind::Parent)
                        .map(|ty| morph.apply_weak(ty))
                        .collect::<Vec<_>>();

                    let (ikey, itmap) =
                        self.find_implementation(trait_, &trtp, weak_impltor, impltor);

                    let forigin = FuncOrigin::Method(ikey, *method);

                    let mfunc = self.call_to_mfunc(forigin, itmap);

                    Callable::Static(mfunc)
                }
                &ast::NFunc::SumVar(sum, var) => {
                    let sum = sum.inside(*module);

                    let mut tmap = self.morphise_inst(mapper);
                    let type_params = tmap.weak.to_types(GenericKind::Entity);

                    let mut morph = to_morphization!(self.lir, self.mir, &mut tmap);
                    let ty = morph.sum(sum, &type_params);

                    let (tag_size, payload_size) = self.lir.mono.types.as_sum_type(ty).unwrap();
                    assert_eq!(tag_size, mono::TAG_SIZE.bits() as u32);

                    Callable::Sum { var, ty, tmap, payload_size }
                }
            },
            mir::Callable::Lambda(lambda, mapper) => {
                let (mfunc, captures) = self.morphise_lambda(*lambda, mapper);
                Callable::LiftedLambda(mfunc, captures)
            }
            mir::Callable::Binding(bind) => {
                let v = self.bind_to_value(*bind);
                Callable::Local(v)
            }
            mir::Callable::Param(param) => {
                let v = self.param_to_value(*param);
                Callable::Local(v)
            }
        }
    }

    pub fn ty_symbol(&self, ty: &MonoType) -> String {
        match ty {
            MonoType::Pointer(inner) => format!("*{}", self.ty_symbol(inner)),
            MonoType::FnPointer(params, ret) => format!(
                "fnptr({} -> {})",
                params.iter().map(|ty| self.ty_symbol(ty)).format(", "),
                self.ty_symbol(&ret)
            ),
            MonoType::Unreachable => format!("!"),
            MonoType::Monomorphised(mk) => match self.lir.mono.types[*mk].original {
                Some(key) => format!("{}>{mk}", self.mir.name_of_type(key)),
                None => mk.to_string(),
            },
            _ => MonoFormatter { types: &self.lir.mono.types, v: ty }.to_string(),
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

// Convenience access to the mono functions for the `string` langitem's implementation of `Stringable`
#[derive(Clone, Copy)]
struct Stringable {
    split_at: MonoFunc,       // self, int -> (self, self)
    split_while: MonoFunc,    // self, fn(u8 -> bool) -> (self, self)
    split_first: MonoFunc,    // self -> (u8, self)
    equals: MonoFunc,         // self, self -> bool
    from_raw_parts: MonoFunc, // *u8, uint -> self

    type_: MonoTypeKey,
}

impl fmt::Display for FuncOrigin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FuncOrigin::SumConstructorWrapper(sum, var) => write!(f, "{sum}:{var}"),
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
