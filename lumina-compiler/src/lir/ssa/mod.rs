use super::{
    mono::MonoFormatter, Function, MonoFunc, MonoType, MonoTypeKey, TRAP_UNREACHABLE, UNIT,
};
use crate::{MAYBE_JUST, MAYBE_NONE};
use derive_more::{Add, AddAssign, From};
use derive_new::new;
use itertools::Itertools;
use key::{Map, M};
use lumina_collections::{map_key_impl, KeysIter};
use lumina_key as key;
use lumina_typesystem::IntSize;
use lumina_util::{Highlighting, ParamFmt};
use owo_colors::OwoColorize;
use std::fmt;
use tracing::{info, trace};

mod opts;
mod rewrite;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Block(pub u32);
map_key_impl!(Block(u32), "block");

#[derive(Clone, Copy, PartialEq, Eq, Hash, Add, AddAssign)]
pub struct V(pub u32);
map_key_impl!(V(u32), "v");

#[derive(Clone, Debug)]
pub struct BasicBlock {
    start: V,
    predecessors: u16,
    // parameters: u32,
    // flow: ControlFlow,
}

impl BasicBlock {
    pub fn new() -> BasicBlock {
        Self { start: V(u32::MAX), predecessors: 0 }
    }
}

pub struct SSA {
    current: Block,
    blocks: Map<Block, BasicBlock>,
    ventries: Map<V, Entry>,
    vtypes: Map<V, MonoType>,
}

/// Information retrieved with the `SSA::block_info` method.
#[derive(Debug)]
pub struct BlockInfo<'a> {
    pub start: V,
    pub end: V,
    pub binds: u32,
    pub params: u32,
    pub tail: &'a Entry,
}

impl<'a> BlockInfo<'a> {
    pub fn is_param(&self, v: V) -> Option<usize> {
        (v.0 >= self.start.0 && v.0 < self.start.0 + self.params)
            .then(|| (v.0 - self.start.0) as usize)
    }
}

impl SSA {
    pub fn new() -> Self {
        let mut entry = BasicBlock::new();
        entry.predecessors += 1;

        SSA {
            current: Block::entry(),
            blocks: [entry].into(),
            ventries: Map::new(),
            vtypes: Map::new(),
        }
    }

    pub fn add_block_param(&mut self, block: Block, ty: MonoType) -> V {
        self.in_block(block, |this| {
            if this.blocks[block].start == V(u32::MAX) {
                this.blocks[block].start = this.ventries.next_key();
            }

            let i = this.block_params(block).count() as u32;
            let entry = Entry::BlockParam(block, i);
            let Value::V(v) = this.assign(entry, ty) else {
                unreachable!();
            };
            v
        })
    }

    pub fn as_block_start(&self, v: V) -> Option<(Block, BlockInfo)> {
        self.blocks
            .find(|bdata| bdata.start == v)
            .map(|block| (block, self.block_info(block)))
    }

    pub fn blocks(&self) -> KeysIter<Block> {
        self.blocks.keys()
    }

    pub fn block_info(&self, block: Block) -> BlockInfo<'_> {
        let start = self.blocks[block].start;

        let mut iter = KeysIter::range(start, usize::MAX);
        let mut params = 0;
        let mut binds = 0;

        loop {
            let v = iter.next().expect("no tail for block");

            match &self.ventries[v] {
                Entry::BlockParam(..) => {
                    params += 1;
                }
                entry if entry.is_terminator() => {
                    break BlockInfo { binds, params, tail: entry, start, end: v }
                }
                _ => binds += 1,
            }
        }
    }

    /// Checks whether this blocks parmaeters are only used within the block
    pub fn parameters_are_local(&self, block: Block) -> bool {
        let start = self.blocks[block].start;
        self.block_params(block)
            .all(|p| self.usage_count(start, p) == 0)
    }

    pub fn usage_count(&self, from: V, target: V) -> usize {
        KeysIter::range(from, usize::MAX)
            .take_while(|&v| self.ventries.has(v))
            .map(|v| {
                let mut count = 0;
                rewrite::for_entry(&self.ventries[v], &mut |v| {
                    if v == target {
                        count += 1;
                    }
                });
                count
            })
            .sum()
    }

    pub fn get_block_param(&self, block: Block, i: u32) -> V {
        let start = self.blocks[block].start;
        assert_ne!(start, V(u32::MAX));
        let v = V(start.0 + i);
        debug_assert!(
            matches!(&self.ventries[v], Entry::BlockParam(b, bi) if *b == block && *bi == i)
        );
        v
    }

    pub fn block_params(&self, block: Block) -> impl Iterator<Item = V> + 'static {
        let start = self.blocks[block].start;
        let mut params = 0;

        assert_ne!(
            start,
            V(u32::MAX),
            "attempted to iterate block params of non-setup block"
        );

        for v in KeysIter::range(start, usize::MAX) {
            if !self.ventries.has(v) {
                break;
            }

            match &self.ventries[v] {
                Entry::BlockParam(b, _) if *b == block => params += 1,
                _ => break,
            }
        }

        KeysIter::range(start, params)
    }

    pub fn param_types(&self, block: Block) -> impl Iterator<Item = &MonoType> {
        let vs = self.block_params(block);
        vs.map(|v| &self.vtypes[v])
    }

    pub fn func_param_types(&self) -> impl Iterator<Item = &MonoType> {
        self.param_types(Block::entry())
    }

    pub fn iterv(&self) -> impl Iterator<Item = V> + 'static {
        self.ventries.keys()
    }

    pub fn predecessors(&self, block: Block) -> u16 {
        self.blocks[block].predecessors
    }

    pub fn new_block(&mut self) -> Block {
        debug_assert_ne!(self.blocks.len(), 0);
        self.blocks.push(BasicBlock::new())
    }

    /// Switch block for added instructions
    pub fn switch_to_block(&mut self, block: Block) {
        self.current = block;
    }

    fn assert_not_double_tail(&self) {
        let block = self.current;
        let start = self.blocks[block].start;

        for v in KeysIter::range(start, usize::MAX) {
            if !self.ventries.has(v) {
                break;
            }

            let entry = &self.ventries[v];
            if entry.is_terminator() {
                panic!("double tail: {v} = {entry}");
            }
        }
    }

    fn assign(&mut self, entry: Entry, ty: MonoType) -> Value {
        let block = self.current;

        if self.blocks[block].start == V(u32::MAX) {
            self.blocks[block].start = self.ventries.next_key();
        }

        trace!(
            "assign {} {} {entry} {} {}",
            self.ventries.next_key(),
            '='.symbol(),
            ':'.symbol(),
            format!("// {block}").dimmed()
        );

        match &entry {
            Entry::Select { on_true, on_false, .. } => {
                self.blocks[on_true.id].predecessors += 1;
                self.blocks[on_false.id].predecessors += 1;
            }
            Entry::JmpTable(_, blocks) => {
                for block in blocks {
                    self.blocks[*block].predecessors += 1;
                }
            }
            Entry::JmpBlock(block) => self.blocks[block.id].predecessors += 1,
            _ => {}
        }

        #[cfg(debug_assertions)]
        if entry.is_terminator() {
            self.assert_not_double_tail();
        }

        let v = self.ventries.push(entry);
        assert_eq!(self.vtypes.push(ty), v);

        v.value()
    }

    pub fn type_of(&self, v: V) -> &MonoType {
        &self.vtypes[v]
    }
    pub fn entry_of(&self, v: V) -> &Entry {
        &self.ventries[v]
    }

    /// Perform a change to a block without switching to it
    pub fn in_block<T>(&mut self, block: Block, perform: impl FnOnce(&mut Self) -> T) -> T {
        let previous = std::mem::replace(&mut self.current, block);
        let out = perform(self);
        self.current = previous;
        out
    }

    /// Get the current block
    pub fn block(&self) -> Block {
        self.current
    }

    pub fn write(&mut self, ptr: Value, value: Value) -> Value {
        let entry = Entry::WritePtr { ptr, value };
        let ty = MonoType::Monomorphised(UNIT);
        self.assign(entry, ty)
    }

    pub fn transmute(&mut self, v: Value, to: MonoType) -> Value {
        let entry = Entry::Transmute(v);
        self.assign(entry, to)
    }

    pub fn size_of(&mut self, ty: MonoType, to: IntSize) -> Value {
        let entry = Entry::SizeOf(ty);
        self.assign(entry, MonoType::Int(to))
    }

    pub fn align_of(&mut self, ty: MonoType, to: IntSize) -> Value {
        let entry = Entry::AlignOf(ty);
        self.assign(entry, MonoType::Int(to))
    }

    pub fn call<C: Callable>(&mut self, call: C, params: Vec<Value>, ret: MonoType) -> Value {
        let entry = C::construct(call, params);
        self.assign(entry, ret)
    }

    pub fn call_extern(&mut self, key: M<key::Func>, params: Vec<Value>, ret: MonoType) -> Value {
        let entry = Entry::CallExtern(key, params);
        self.assign(entry, ret)
    }

    pub fn construct(&mut self, params: Vec<Value>, ty: MonoType) -> Value {
        let entry = Entry::Construct(params);
        self.assign(entry, ty)
    }

    pub fn replicate(&mut self, value: Value, times: u64, ty: MonoType) -> Value {
        let entry = Entry::Replicate(value, times);
        self.assign(entry, ty)
    }

    pub fn variant(&mut self, var: key::Variant, params: Vec<Value>, ty: MonoTypeKey) -> Value {
        let entry = Entry::Variant(var, params);
        self.assign(entry, MonoType::Monomorphised(ty))
    }

    pub fn tag_of(&mut self, sum: Value, tagsize: IntSize) -> Value {
        let entry = Entry::TagFromSum { of: sum };
        self.assign(entry, MonoType::Int(tagsize))
    }

    pub fn reduce(&mut self, value: Value, ty: MonoType) -> Value {
        let entry = Entry::Reduce(value);
        self.assign(entry, ty)
    }
    pub fn extend(&mut self, value: Value, from_signed: bool, ty: MonoType) -> Value {
        let entry = if from_signed {
            Entry::ExtendSigned(value)
        } else {
            Entry::ExtendUnsigned(value)
        };
        self.assign(entry, ty)
    }

    pub fn int_to_float(&mut self, value: Value, intsize: IntSize) -> Value {
        let entry = Entry::IntToFloat(value, intsize);
        self.assign(entry, MonoType::Float)
    }
    pub fn float_to_int(&mut self, value: Value, intsize: IntSize) -> Value {
        let entry = Entry::FloatToInt(value, intsize);
        self.assign(entry, MonoType::Int(intsize))
    }

    pub fn cmp(&mut self, v: [Value; 2], ord: std::cmp::Ordering, bitsize: IntSize) -> Value {
        let entry = Entry::IntCmpInclusive(v, ord, bitsize);
        let ty = MonoType::bool();
        self.assign(entry, ty)
    }
    pub fn not(&mut self, v: Value) -> Value {
        let entry = Entry::BitNot(v);
        let ty = MonoType::bool();
        self.assign(entry, ty)
    }
    pub fn eq(&mut self, v: [Value; 2], bitsize: IntSize) -> Value {
        self.cmp(v, std::cmp::Ordering::Equal, bitsize)
    }
    pub fn lti(&mut self, v: [Value; 2], bitsize: IntSize) -> Value {
        self.cmp(v, std::cmp::Ordering::Less, bitsize)
    }
    pub fn gti(&mut self, v: [Value; 2], bitsize: IntSize) -> Value {
        self.cmp(v, std::cmp::Ordering::Greater, bitsize)
    }

    // return type overloaded numeric operations
    pub fn add(&mut self, v: Value, by: Value, ty: MonoType) -> Value {
        let entry = Entry::BinOp(BinOp::Add, [v, by]);
        self.assign(entry, ty)
    }
    pub fn sub(&mut self, v: Value, by: Value, ty: MonoType) -> Value {
        let entry = Entry::BinOp(BinOp::Sub, [v, by]);
        self.assign(entry, ty)
    }
    pub fn mul(&mut self, v: Value, by: Value, ty: MonoType) -> Value {
        let entry = Entry::BinOp(BinOp::Mul, [v, by]);
        self.assign(entry, ty)
    }
    pub fn div(&mut self, v: Value, by: Value, ty: MonoType) -> Value {
        let entry = Entry::BinOp(BinOp::Div, [v, by]);
        self.assign(entry, ty)
    }
    pub fn abs(&mut self, v: Value, ty: MonoType) -> Value {
        let entry = Entry::IntAbs(v);
        self.assign(entry, ty)
    }

    pub fn field(&mut self, of: Value, key: MonoTypeKey, field: key::Field, ty: MonoType) -> Value {
        let entry = Entry::Field { of, key, field };
        self.assign(entry, ty)
    }
    pub fn indice(&mut self, of: Value, indice: Value, ty: MonoType) -> Value {
        let entry = Entry::Indice { of, indice };
        self.assign(entry, ty)
    }
    pub fn cast_payload(&mut self, of: Value, tuple: MonoType) -> Value {
        let entry = Entry::CastFromSum { of };
        self.assign(entry, tuple)
    }

    pub fn cmps<const N: usize>(
        &mut self,
        on: Value,
        cmps: [std::cmp::Ordering; N],
        values: [Value; N],
        intsize: IntSize,
        ty: MonoType,
    ) -> Value {
        if N == 1 {
            self.cmp([on, values[0]], cmps[0], intsize)
        } else {
            let mut iter = values.into_iter().zip(cmps);
            let (right, ord) = iter.next().unwrap();

            let init = self.cmp([on, right], ord, intsize);

            iter.fold(init, |left, (right, ord)| {
                let v = self.cmp([on, right], ord, intsize);
                self.bit_and([left, v], ty.clone())
            })
        }
    }

    pub fn alloc(&mut self, objty: MonoType) -> Value {
        let entry = Entry::Alloc;
        let ty = MonoType::Pointer(Box::new(objty));
        self.assign(entry, ty)
    }
    pub fn alloca(&mut self, ty: MonoType) -> Value {
        let entry = Entry::Alloca;
        let ty = MonoType::Pointer(Box::new(ty));
        self.assign(entry, ty)
    }
    pub fn dealloc(&mut self, ptr: Value, ty: MonoType) {
        let entry = Entry::Dealloc { ptr };
        self.assign(entry, ty);
    }
    pub fn deref(&mut self, value: Value, ty: MonoType) -> Value {
        let entry = Entry::Deref(value);
        self.assign(entry, ty)
    }

    pub fn val_to_ref(&mut self, val: M<key::Val>, ty: MonoType) -> Value {
        let entry = Entry::RefStaticVal(val);
        let ty = MonoType::Pointer(Box::new(ty));
        self.assign(entry, ty)
    }

    pub fn bit_and(&mut self, v: [Value; 2], ty: MonoType) -> Value {
        let entry = Entry::BinOp(BinOp::And, v);
        self.assign(entry, ty)
    }

    #[track_caller]
    pub fn jump<J: Jumpable>(&mut self, j: J, params: Vec<Value>) -> Value {
        let entry = J::construct(j, params);
        self.assign(entry, MonoType::unit())
    }

    pub fn unreachable(&mut self, ty: MonoType) -> Value {
        let and_then = self.new_block();
        let entry = Entry::Trap(cranelift_codegen::ir::TrapCode::user(TRAP_UNREACHABLE).unwrap());
        self.assign(entry, MonoType::unit());
        let v = self.add_block_param(and_then, ty.clone());
        self.switch_to_block(and_then);
        v.value()
    }

    pub fn return_(&mut self, value: Value) -> Value {
        let entry = Entry::Return(value);
        self.assign(entry, MonoType::unit())
    }

    pub fn select(&mut self, value: Value, [on_true, on_false]: [(Block, Vec<Value>); 2]) -> Value {
        let on_true = BlockJump { id: on_true.0, params: on_true.1 };
        let on_false = BlockJump { id: on_false.0, params: on_false.1 };
        let entry = Entry::Select { value, on_true, on_false };
        self.assign(entry, MonoType::unit())
    }

    pub fn jump_table(&mut self, on: Value, blocks: Vec<Block>) -> Value {
        let entry = Entry::JmpTable(on, blocks);
        self.assign(entry, MonoType::unit())
    }
}

pub trait Jumpable {
    fn construct(self, params: Vec<Value>) -> Entry;
}

impl Jumpable for MonoFunc {
    fn construct(self, params: Vec<Value>) -> Entry {
        Entry::JmpFunc(self, params)
    }
}

impl Jumpable for Block {
    fn construct(self, params: Vec<Value>) -> Entry {
        Entry::JmpBlock(BlockJump { id: self, params })
    }
}

pub trait Callable {
    fn construct(self, params: Vec<Value>) -> Entry;
}

impl Callable for MonoFunc {
    fn construct(self, params: Vec<Value>) -> Entry {
        Entry::CallStatic(self, params)
    }
}

impl Callable for Value {
    fn construct(self, params: Vec<Value>) -> Entry {
        match self {
            Value::FuncPtr(func) => Entry::CallStatic(func, params),
            v => Entry::CallValue(v, params),
        }
    }
}

impl V {
    pub fn value(self) -> Value {
        Value::V(self)
    }
}

impl Value {
    pub fn u(n: i128, bits: u8) -> Value {
        Value::Int(n, IntSize::new(false, bits))
    }
    pub fn i(n: i128, bits: u8) -> Value {
        Value::Int(n, IntSize::new(true, bits))
    }

    pub fn bool(b: bool) -> Value {
        Value::Int(b as i128, IntSize::new(false, 8))
    }

    pub fn maybe_just() -> Value {
        Value::Int(MAYBE_JUST.0 as i128, IntSize::new(false, 16))
    }

    pub fn maybe_none() -> Value {
        Value::Int(MAYBE_NONE.0 as i128, IntSize::new(true, 16))
    }

    pub fn as_fptr(&self) -> MonoFunc {
        match self {
            Value::FuncPtr(mfunc) => *mfunc,
            _ => panic!("as_fptr called on non-fptr: {self}"),
        }
    }
}

impl Block {
    pub fn entry() -> Block {
        Block(0)
    }
}

#[derive(Clone, Debug, PartialEq, new)]
pub struct BlockJump {
    pub id: Block,
    pub params: Vec<Value>,
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Entry {
    // Function Calls
    CallStatic(MonoFunc, Vec<Value>),
    CallExtern(M<key::Func>, Vec<Value>),
    CallValue(Value, Vec<Value>),

    // Control Flow
    JmpFunc(MonoFunc, Vec<Value>),
    JmpBlock(BlockJump),
    Return(Value),
    Select {
        value: Value,
        on_true: BlockJump,
        on_false: BlockJump,
    },
    JmpTable(Value, Vec<Block>),
    Trap(cranelift_codegen::ir::TrapCode),

    // Value Construction
    Construct(Vec<Value>),
    Replicate(Value, u64),
    Variant(key::Variant, Vec<Value>),
    RefStaticVal(M<key::Val>),
    BlockParam(Block, u32),

    // Value Destruction
    Field {
        of: Value,
        key: MonoTypeKey,
        field: key::Field,
    },
    CastFromSum {
        of: Value,
    },
    TagFromSum {
        of: Value,
    },
    Indice {
        of: Value,
        indice: Value,
    },

    // Binary Operators
    BinOp(BinOp, [Value; 2]),
    IntCmpInclusive([Value; 2], std::cmp::Ordering, IntSize),
    IntAbs(Value),

    Transmute(Value), // Transmute two values of equal size
    SizeOf(MonoType),
    AlignOf(MonoType),
    Reduce(Value),
    ExtendSigned(Value),
    ExtendUnsigned(Value),

    IntToFloat(Value, IntSize),
    FloatToInt(Value, IntSize),

    BitNot(Value),

    // Pointer Manipulation
    Alloc,
    Alloca,
    Dealloc {
        ptr: Value,
    },
    WritePtr {
        ptr: Value,
        value: Value,
    },
    Deref(Value),
}

#[derive(From, Clone, Copy, PartialEq)]
#[rustfmt::skip]
pub enum Value {
    #[from] ReadOnly(M<key::ReadOnly>),
    #[from] FuncPtr(MonoFunc),
    #[from] ExternFuncPtr(M<key::Func>),
    V(V),
    
    Int(i128, IntSize),

    #[from] Float(f64),
}

impl<'a, 't> fmt::Display for MonoFormatter<'a, &'t SSA> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut values = self.v.ventries.iter();

        while let Some((v, entry)) = values.next() {
            if let Some(block) = self.v.blocks.find(|bdata| bdata.start == v) {
                write!(f, "\n{block}(")?;

                let mut has_bparam = false;
                for paramv in self.v.block_params(block) {
                    has_bparam = true;

                    if paramv == v {
                        self.param(f, block, paramv, entry)
                    } else {
                        let entry = values.next().unwrap().1;
                        self.param(f, block, paramv, entry)
                    }?
                }
                writeln!(f, "): predecessors={}", self.v.blocks[block].predecessors)?;

                if has_bparam {
                    continue;
                }
            }

            if entry.is_terminator() {
                writeln!(
                    f,
                    "  {} // {v}",
                    self.fork(entry).to_string().lines().format("\n  ")
                )?;
            } else {
                writeln!(
                    f,
                    "  {v} {} {} : {}",
                    '='.symbol(),
                    self.fork(entry),
                    self.fork(&self.v.vtypes[v])
                )?;
            }
        }

        Ok(())
    }
}

impl<'a, 't> MonoFormatter<'a, &'t SSA> {
    fn param(&self, f: &mut fmt::Formatter, block: Block, v: V, entry: &Entry) -> fmt::Result {
        match entry {
            Entry::BlockParam(b, _) => assert_eq!(*b, block),
            other => panic!("{other:?}"),
        }
        let ty = self.v.type_of(v);
        if v != self.v.blocks[block].start {
            write!(f, ", ")?;
        }
        write!(f, "{v}: {}", self.fork(ty))
    }
}

impl<'a> fmt::Display for MonoFormatter<'a, &Entry> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        EntryFmt(self.funcs, self.v).fmt(f)
    }
}

struct CStyle<'a, H, P>(&'a H, &'a [P]);

impl<'a, H: fmt::Display, P: fmt::Display> fmt::Display for CStyle<'a, H, P> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            self.0,
            '('.symbol(),
            self.1.iter().format(", "),
            ')'.symbol()
        )
    }
}

struct EntryFmt<'a, 'e>(Option<&'a Map<MonoFunc, Function>>, &'e Entry);

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", EntryFmt(None, self))
    }
}

impl<'a, 'e> fmt::Display for EntryFmt<'a, 'e> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.1 {
            Entry::CallStatic(mfunc, params) => {
                let symbol = self
                    .0
                    .map(|funcs| funcs[*mfunc].symbol.clone())
                    .unwrap_or(mfunc.to_string());
                write!(f, "{} {}", "call".keyword(), CStyle(&symbol, params))
            }
            Entry::CallValue(mfunc, params) => {
                write!(f, "{} {}", "callv".keyword(), CStyle(mfunc, params))
            }
            Entry::CallExtern(key, params) => {
                write!(f, "{} {}", "callc".keyword(), CStyle(key, params))
            }
            Entry::Transmute(v) => write!(f, "{} {v}", "transmute".keyword()),
            Entry::SizeOf(v) => write!(f, "{} {v:#?}", "size-of".keyword()),
            Entry::AlignOf(v) => write!(f, "{} {v:#?}", "align-of".keyword()),
            Entry::RefStaticVal(val) => write!(f, "&{val}"),
            Entry::BlockParam(block, i) => write!(f, "{} {block}[{i}]", "bparam".keyword()),
            Entry::Deref(v) => write!(f, "{} {v}", "deref".keyword()),
            Entry::Construct(elems) => ParamFmt::new(&"construct".keyword(), elems).fmt(f),
            Entry::Replicate(elem, times) => {
                write!(f, "{} {times} {elem}", "replicate".keyword())
            }
            Entry::Variant(var, elems) => {
                write!(
                    f,
                    "{} {var} ({})",
                    "variant".keyword(),
                    elems.iter().format(" ")
                )
            }
            Entry::IntCmpInclusive([left, right], cmp, size) => {
                let kind = match cmp {
                    std::cmp::Ordering::Less => "lt",
                    std::cmp::Ordering::Equal => "eq",
                    std::cmp::Ordering::Greater => "gt",
                };
                let header = format!("{kind}.{size}");
                write!(f, "{} {} {}", header.keyword(), left, right)
            }
            Entry::BitNot(v) => write!(f, "{} {v}", "bit-not".keyword()),
            Entry::Alloc => write!(f, "{}", "alloc".keyword(),),
            Entry::Alloca => write!(f, "{}", "alloca".keyword()),
            Entry::Dealloc { ptr } => write!(f, "{} {ptr}", "dealloc".keyword()),
            Entry::Field { of, field, .. } => write!(f, "{} {of} {field}", "field".keyword(),),
            Entry::Indice { of, indice } => write!(f, "{} {of} {indice}", "indice".keyword()),
            Entry::CastFromSum { of } => {
                write!(f, "{} {of}", "cast-payload".keyword())
            }
            Entry::TagFromSum { of } => {
                write!(f, "{} {of}", "cast-tag".keyword())
            }
            Entry::BinOp(kind, [a, b]) => write!(f, "{} {a} {b}", kind.keyword()),
            Entry::IntAbs(v) => write!(f, "{} {v}", "abs".keyword()),
            Entry::Reduce(v) => write!(f, "{} {v}", "reduce".keyword()),
            Entry::ExtendUnsigned(v) => write!(f, "{} {v}", "uextend".keyword()),
            Entry::ExtendSigned(v) => write!(f, "{} {v}", "sextend".keyword()),
            Entry::WritePtr { ptr, value } => {
                write!(f, "{} {ptr} {} {value}", "write".keyword(), "<-".symbol())
            }
            Entry::IntToFloat(v, _) => {
                write!(f, "{} {v}", "int_to_float".keyword())
            }
            Entry::FloatToInt(v, intsize) => {
                write!(f, "{} {intsize} {v}", "float_to_int".keyword())
            }
            Entry::JmpFunc(mfunc, params) => {
                write!(f, "{} {}", "jump".keyword(), CStyle(mfunc, params))
            }
            Entry::JmpBlock(jump) => {
                write!(f, "{} {}", "jump".keyword(), CStyle(&jump.id, &jump.params))
            }
            Entry::Return(value) => write!(f, "{} {value}", "return".keyword()),
            Entry::Trap(code) => write!(f, "{} {code}", "trap".keyword()),
            Entry::Select { value, on_true, on_false, .. } => {
                writeln!(f, "{} {value}", "select".keyword())?;
                let mut f = |str: &str, b: &BlockJump| {
                    writeln!(
                        f,
                        "{} {} {} {}",
                        '|'.symbol(),
                        str.keyword(),
                        "->".symbol(),
                        CStyle(&b.id, &b.params),
                    )
                };
                f("true ", on_true)?;
                f("false", on_false)
            }
            Entry::JmpTable(on, blocks) => {
                writeln!(f, "{} {on}", "select".keyword())?;
                blocks.iter().enumerate().try_for_each(|(i, block)| {
                    writeln!(f, "{i} {} {} {}()", "->".symbol(), "jump".keyword(), block)
                })
            }
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Add => "add",
            BinOp::Sub => "sub",
            BinOp::Mul => "mul",
            BinOp::Div => "div",
            BinOp::And => "and",
        }
        .fmt(f)
    }
}

impl Value {
    pub fn for_values_mut(&mut self, f: &mut dyn Fn(&mut V)) {
        match self {
            Value::V(v) => f(v),
            _ => {}
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::ReadOnly(ro) => ro.fmt(f),
            Value::V(v) => v.fmt(f),
            Value::Int(n, _) => n.fmt(f),
            Value::FuncPtr(ptr) => ptr.fmt(f),
            Value::ExternFuncPtr(ptr) => ptr.fmt(f),
            Value::Float(n) => write!(f, "{n:?}"),
        }
    }
}
