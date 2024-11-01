//! Optimizations that are more specific to Lumina and more appropriate for a higher-level IR than
//! our backends.

use crate::lir::{Block, BlockJump, Entry, Function, MonoFunc, Value, LIR, SSA};
use lumina_collections::KeysIter;
use tracing::{error, error_span, info};

pub const ENABLE_OPTS: bool = true;

impl LIR {
    pub fn perform_optimizations(&mut self) {
        let mut opt = Optimizer { lir: self, changed: true };
        opt.optimizations();
    }
}

struct Optimizer<'a> {
    lir: &'a mut LIR,
    changed: Changed,
}

type Changed = bool;

impl<'a> Optimizer<'a> {
    fn optimizations(&mut self) {
        if !ENABLE_OPTS {
            return;
        }

        self.block_opts();
        self.func_opts();
    }

    fn block_opts(&mut self) {
        const FUEL: usize = 1000;

        for fkey in self.lir.functions.keys() {
            let _span = error_span!(
                "running block optimizations",
                entity = self.lir.functions[fkey].symbol.clone(),
            );
            let _handle = _span.enter();
            error!(
                "BEFORE OPTIMIZATIONS:\n{}",
                self.lir.mono.fmt(&self.lir.functions[fkey])
            );

            self.with_fuel(FUEL, |lir| {
                let func = &mut lir.functions[fkey];
                let changed = func
                    .ssa
                    .blocks
                    .keys()
                    .any(|block| block_opt_iter(func, fkey, block));

                if changed {
                    error!(
                        "MIDDLE OPTIMIZATION:\n{}",
                        lir.mono.fmt(&lir.functions[fkey])
                    );
                    panic!();
                }

                changed
            })
        }
    }

    fn func_opts(&mut self) {
        const FUEL: usize = 1000;

        for fkey in self.lir.functions.keys() {
            self.with_fuel(FUEL, |lir| {
                let ssa = &lir.functions[fkey].ssa;
                ssa.blocks
                    .keys()
                    .any(|block| func_opt_iter(lir, fkey, block))
            })
        }
    }

    fn with_fuel(&mut self, mut fuel: usize, mut f: impl FnMut(&'_ mut LIR) -> Changed) {
        while fuel != 0 && self.changed {
            self.changed = false;

            self.changed = f(self.lir);

            fuel -= 1;
        }
    }
}

fn block_opt_iter(func: &mut Function, fkey: MonoFunc, block: Block) -> Changed {
    let start = func.ssa.blocks[block].start;

    let mut changed = false;

    let mut entrybuf = vec![];
    let mut typebuf = vec![];

    for v in KeysIter::range(start, usize::MAX) {
        match &func.ssa.ventries[v] {
            Entry::JmpBlock(jump) => {
                if func.ssa.block_info(jump.id).binds < 3 {
                    info!("inlining the block jump {} from {block}", jump.id);

                    changed = true;

                    // TODO: we need to check whether the block parameters are used outside of the
                    // block before we can inline it.
                    //  ^ oh ye, we want to do that inside of `inline_candidate`
                    //
                    // *TECHNICALLY* we could also check whether this is the only predecessor
                    // and if so inline it anyways. However; since we only use block params for
                    // continuations then that's probably unecesarry.

                    let istart = func.ssa.blocks[jump.id].start;
                    func.ssa
                        .copy_for_inline(istart, v, &jump.params, &mut entrybuf, &mut typebuf);
                    func.ssa.insert(
                        start,
                        std::mem::take(&mut entrybuf),
                        std::mem::take(&mut typebuf),
                        true,
                    );
                    return true;
                }
            }
            // Substitute tail call to self with a jump to the entry block instead
            Entry::JmpFunc(mfunc, _) if *mfunc == fkey => {
                let Entry::CallStatic(_, params) = &mut func.ssa.ventries[v] else {
                    unreachable!()
                };
                let jump = BlockJump::new(Block::entry(), std::mem::take(params));
                func.ssa.ventries[v] = Entry::JmpBlock(jump);
            }
            Entry::Select { on_true, on_false, .. } => {
                let new_on_true = try_inline_block_rejump(&func.ssa, on_true);
                let new_on_false = try_inline_block_rejump(&func.ssa, on_false);

                let Entry::Select { on_true, on_false, .. } = &mut func.ssa.ventries[v] else {
                    unreachable!()
                };

                if let Some(jump) = new_on_true {
                    *on_true = jump;
                }
                if let Some(jump) = new_on_false {
                    *on_false = jump;
                }

                return true;
            }
            Entry::JmpTable(_, _) => {
                todo!();
            }
            _ => {}
        }
    }

    changed
}

fn try_inline_block_rejump(ssa: &SSA, ijump: &BlockJump) -> Option<BlockJump> {
    let binfo = ssa.block_info(ijump.id);
    if binfo.binds == 0 {
        if let Entry::JmpBlock(jump) = binfo.tail {
            if ssa.parameters_are_local(ijump.id) {
                // Substitute the parameters in iblock with the parameters given to the
                // jump to iblock, to be used for the jump to the block jumped to by iblock.
                let params = jump
                    .params
                    .iter()
                    .map(|p| match p {
                        Value::V(v) => {
                            if let Some(i) = binfo.is_param(*v) {
                                ijump.params[i]
                            } else {
                                *p
                            }
                        }
                        _ => *p,
                    })
                    .collect();

                return Some(BlockJump::new(jump.id, params));
            }
        }
    }

    None
}

fn func_opt_iter(lir: &mut LIR, fkey: MonoFunc, block: Block) -> Changed {
    todo!();
}

// impl SSA {}

impl Entry {
    pub fn is_terminator(&self) -> bool {
        match self {
            Entry::JmpFunc(..)
            | Entry::JmpBlock(..)
            | Entry::Return(..)
            | Entry::Select { .. }
            | Entry::JmpTable(..)
            | Entry::Trap(..) => true,
            _ => false,
        }
    }
}
