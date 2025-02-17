use lumina_collections::{map_key_impl, M};
use lumina_key::Map;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct KFunc(u32);
map_key_impl!(KFunc(u32), "v");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct KBlock(pub u32);
map_key_impl!(KBlock(u32), "block");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct KBind(u32);
map_key_impl!(KBind(u32), "v");

pub struct SSA {
    pub funcs: Map<KFunc, Func>,
}

impl SSA {
    pub fn new() -> Self {
        SSA { funcs: Map::new() }
    }
}

pub struct Func {
    pub bbs: Map<KBlock, Block>,
}

pub struct Block {
    pub binds: Map<KBind, Instruction>,
    pub then: Flow,
}

pub enum Instruction {
    Call(KFunc, Vec<KBind>),
    Copy,
    Const(usize),
    Add(KBind, KBind),
    Param,
}

pub enum Flow {
    Jump(KBlock),
    Unreachable,
    Return(KBind),
}

impl Func {
    pub fn new() -> Self {
        Self {
            bbs: [Block { binds: Map::new(), then: Flow::Unreachable }].into(),
        }
    }

    pub fn call<const N: usize>(
        &mut self,
        block: KBlock,
        func: KFunc,
        params: [KBind; N],
    ) -> KBind {
        self.bbs[block]
            .binds
            .push(Instruction::Call(func, params.into()))
    }

    pub fn uconst(&mut self, block: KBlock, n: usize) -> KBind {
        self.bbs[block].binds.push(Instruction::Const(n))
    }

    pub fn add(&mut self, block: KBlock, a: KBind, b: KBind) -> KBind {
        self.bbs[block].binds.push(Instruction::Add(a, b))
    }

    pub fn param(&mut self, block: KBlock) -> KBind {
        self.bbs[block].binds.push(Instruction::Param)
    }

    pub fn ret(&mut self, block: KBlock, v: KBind) {
        self.bbs[block].then = Flow::Return(v)
    }
}
