//! Optimizations that are more specific to Lumina and more appropriate for a higher-level IR than
//! our backends.

use crate::lir::{
    ssa::rewrite::for_entry_mut, ty_fmt, Block, BlockJump, Entry, Function, MonoFunc, Value, LIR,
    SSA, V,
};
use lumina_collections::KeysIter;
use smallvec::SmallVec;
use std::mem::take;
use tracing::{info, info_span, trace};

pub const ENABLE_OPTS: bool = true;

impl LIR {
    pub fn perform_optimizations(&mut self) {
        let mut opt = Optimizer { lir: self };
        opt.optimizations();
    }
}

struct Optimizer<'a> {
    lir: &'a mut LIR,
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
            let _span = info_span!(
                "running block optimizations",
                entity = self.lir.functions[fkey].symbol.clone(),
            );
            let _handle = _span.enter();
            info!(
                "BEFORE OPTIMIZATIONS:\n{}",
                self.lir
                    .mono
                    .fmt(&self.lir.functions[fkey])
                    .fns(&self.lir.functions)
            );

            let any_change = self.with_fuel(FUEL, |lir| {
                let func = &mut lir.functions[fkey];
                let changed = func
                    .ssa
                    .blocks
                    .keys()
                    .any(|block| block_opt_iter(func, fkey, block));

                if func.symbol.contains("tuple::show::mfunc96") {
                    if changed {
                        info!(
                            "MIDDLE OPTIMIZATION:\n{}",
                            ty_fmt(&lir.mono.types, &lir.functions[fkey]).fns(&lir.functions)
                        );
                    }
                }
                changed
            });

            if any_change {
                trace!(
                    "post-optimizations:\n{}",
                    self.lir.mono.fmt(&self.lir.functions[fkey])
                );
            }
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
            });
        }
    }

    fn with_fuel(&mut self, mut fuel: usize, mut f: impl FnMut(&'_ mut LIR) -> Changed) -> Changed {
        let mut changed = true;
        let mut any_change = false;

        while fuel != 0 && changed {
            changed = f(self.lir);
            any_change |= changed;

            fuel -= 1;
        }

        any_change
    }
}

fn block_opt_iter(func: &mut Function, fkey: MonoFunc, block: Block) -> Changed {
    if func.ssa.blocks[block].predecessors == 0 {
        info!("purging {block}");
        func.ssa.purge_block(block);
        return true;
    }

    let start = func.ssa.blocks[block].start;

    for v in KeysIter::range(start, usize::MAX) {
        if !func.ssa.ventries.has(v) {
            break;
        }

        match &func.ssa.ventries[v] {
            // block0():
            //  v1 = 0
            //  jump block1(v1)
            //
            // block1(v2): // predecessors = 1
            //  return v2
            // -----
            // block0():
            //  v1 = 0
            //  return v1
            //  ...
            Entry::JmpBlock(jump)
                if func.ssa.blocks[jump.id].predecessors == 1 && jump.id == Block(block.0 + 1) =>
            {
                info!("inlining {} in {}", jump.id, block);
                let jump = jump.clone();

                for vv in KeysIter::range(v, usize::MAX) {
                    if !func.ssa.ventries.has(vv) {
                        break;
                    }

                    for_entry_mut(&mut func.ssa.ventries[vv], |vv| {
                        if vv.0 > v.0 && vv.0 <= v.0 + jump.params.len() as u32 {
                            let start = v.0 + 1;
                            let i = vv.0 - start;
                            let new = jump.params[i as usize];
                            info!("at {v}, {vv} detected as parameter {i}, rewriting to {new}");
                            new
                        } else {
                            vv.value()
                        }
                    });
                }

                // Remove the jump instruction, and parameter declarations of the block we'll remove
                func.ssa.blocks.as_mut_vec().remove(jump.id.0 as usize);
                func.ssa.delete_range(v, 1 + jump.params.len());
                func.ssa.for_each_block(V(0), |b| {
                    if b.0 > block.0 {
                        b.0 -= 1;
                    }
                });

                return true;
            }
            // Substitute tail call to self with a jump to the entry block instead
            Entry::JmpFunc(mfunc, _) if *mfunc == fkey => {
                info!("substiuting call to self with jump to entry block");
                let Entry::JmpFunc(_, params) = &mut func.ssa.ventries[v] else {
                    unreachable!()
                };
                let jump = BlockJump::new(Block::entry(), take(params));
                func.ssa.ventries[v] = Entry::JmpBlock(jump);
            }
            Entry::Select { on_true, on_false, .. } => {
                let new_on_true = try_inline_block_rejump(&func.ssa, on_true);
                let new_on_false = try_inline_block_rejump(&func.ssa, on_false);

                let Entry::Select { on_true, on_false, .. } = &mut func.ssa.ventries[v] else {
                    unreachable!()
                };

                let mut changed = false;
                if let Some(jump) = new_on_true {
                    changed = true;
                    info!("{} => {}", on_true.id, jump.id);
                    func.ssa.blocks[on_true.id].predecessors -= 1;
                    *on_true = jump;
                }
                if let Some(jump) = new_on_false {
                    changed = true;
                    info!("{} => {}", on_false.id, jump.id);
                    func.ssa.blocks[on_false.id].predecessors -= 1;
                    *on_false = jump;
                }

                return changed;
            }

            // v0 = call mfunc0()
            // return v0
            //   ----
            // jump mfunc0()
            Entry::CallStatic(..)
                if func.ssa.ventries[V(v.0 + 1)] == Entry::Return(Value::V(v)) =>
            {
                info!("substituting call+ret into tail call");

                let Entry::CallStatic(mfunc, params) = &mut func.ssa.ventries[v] else {
                    unreachable!();
                };

                let params = take(params);
                func.ssa.ventries[v] = Entry::JmpFunc(*mfunc, params);
                func.ssa.delete_range(V(v.0 + 1), 1);

                return true;
            }
            Entry::JmpTable(_, blocks) => {
                let rejumps = blocks
                    .iter()
                    .map(|block| {
                        let jmp = BlockJump::new(*block, vec![]);
                        try_inline_block_rejump(&func.ssa, &jmp)
                    })
                    .collect::<SmallVec<[_; 5]>>();

                let Entry::JmpTable(_, blocks) = &mut func.ssa.ventries[v] else {
                    unreachable!();
                };

                let changed = !rejumps.is_empty();

                for (rejump, jump) in rejumps.into_iter().zip(blocks) {
                    if let Some(new) = rejump {
                        *jump = new.id;
                    }
                }

                return changed;
            }
            _ => {}
        }
    }

    false
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
    false
}

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::debuginfo::Item;
    use crate::lir::{ty_fmt, MonoType};
    use lumina_key as key;
    use lumina_key::{Map, M};
    use tracing::info;

    fn test_block_opts(name: &str, ssa: SSA) -> (String, String) {
        let mut fuel = 5;

        let item = Item::Defined(M(key::Module::from(0), key::Func::from(0)));
        let mut func = Function::new(name.into(), item, ssa, MonoType::u(8), 1);

        let mut changed = true;

        let types = Map::new();
        lumina_util::enable_highlighting(false);
        let before = format!("BEFORE:\n{}", ty_fmt(&types, &func.ssa));

        while fuel != 0 && changed {
            changed = func
                .ssa
                .blocks
                .keys()
                .any(|block| block_opt_iter(&mut func, MonoFunc(0), block));

            if changed {
                info!("MIDDLE OPTIMIZATION:\n{}", ty_fmt(&types, &func));
            }

            fuel -= 1;
        }

        let after = format!("AFTER:\n{}", ty_fmt(&types, &func.ssa));

        (before, after)
    }

    #[test]
    fn inline_block() {
        lumina_util::test_logger();

        let mut ssa = SSA::new();

        let block = [Block::entry(), ssa.new_block()];

        let v0 = ssa.add_block_param(block[0], MonoType::u(0));
        let v1 = ssa.add(v0.value(), Value::u(1, 1), MonoType::u(1));
        let _v2 = ssa.jump(block[1], vec![v1]);

        ssa.switch_to_block(block[1]);
        let v3 = ssa.add_block_param(block[1], MonoType::u(3));
        let v4 = ssa.construct(vec![v0.value(), v1, v1, v3.value()], MonoType::u(4));
        let _v5 = ssa.return_(v4);

        let (before, after) = test_block_opts("inline_block", ssa);
        insta::assert_snapshot!(format!("{before}\n{after}"));
    }

    #[test]
    fn tricky() {
        lumina_util::test_logger();

        let mut ssa = SSA::new();

        let block = [Block::entry(), ssa.new_block(), ssa.new_block()];
        ssa.select(
            Value::bool(true),
            [
                (block[1], vec![Value::u(1, 8)]),
                (block[1], vec![Value::u(2, 8)]),
            ],
        );

        ssa.switch_to_block(block[1]);
        let p = ssa.add_block_param(block[1], MonoType::u(8));
        ssa.jump(block[2], vec![]);

        ssa.switch_to_block(block[2]);
        ssa.return_(p.value());

        let (before, after) = test_block_opts("inline_block", ssa);
        insta::assert_snapshot!(format!("{before}\n{after}"));
    }
}
