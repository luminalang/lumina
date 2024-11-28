//! Optimizations that are more specific to Lumina and more appropriate for a higher-level IR than
//! our backends.

use crate::lir::{
    ssa::rewrite::{insert_buf, offset_predecessors, Rewrite},
    Block, BlockJump, Entry, Function, MonoFunc, Value, LIR, SSA, V,
};
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

            let any_change = self.with_fuel(FUEL, |lir| {
                let func = &mut lir.functions[fkey];
                let changed = func
                    .ssa
                    .blocks
                    .keys()
                    .any(|block| block_opt_iter(func, fkey, block));

                if changed {
                    trace!(
                        "mid-optimization {fkey} ({})\n{}",
                        &lir.functions[fkey].symbol,
                        lir.mono.fmt(&lir.functions[fkey])
                    );
                }

                changed
            });

            if any_change {
                info!(
                    "post-optimizations {fkey} ({}):\n{}",
                    &self.lir.functions[fkey].symbol,
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

fn block_opt_iter(func: &mut Function, _: MonoFunc, block: Block) -> Changed {
    if func.ssa.blocks[block].predecessors == 0 {
        info!("purging {block}");
        func.ssa.purge_block(block);
        return true;
    }

    let start = func.ssa.blocks[block].start;

    for v in func.ssa.ventries.range_to_end(start) {
        match &func.ssa.ventries[v] {
            // TODO: TODO: This check kinda sucks because things like jumptables which are really
            // large and heavy will still be inlined in a lot of situations, bloating the size.
            Entry::JmpBlock(jump) if fully_inlinable(&func.ssa, jump.id) && jump.id != block => {
                info!("inlining {} in {} regardless of size", jump.id, block);
                let jump = jump.clone(); // TODO: we don't have to clone
                full_block_inline(&mut func.ssa, v, &jump);
                return true;
            }
            // Substitute tail call to self with a jump to the entry block instead
            // TODO: re-add this but fix the cranelift limitation.
            // Entry::JmpFunc(mfunc, _) if *mfunc == fkey => {
            //     info!("substiuting call to self with jump to entry block");
            //     let Entry::JmpFunc(_, params) = &mut func.ssa.ventries[v] else {
            //         unreachable!()
            //     };
            //     // TODO: if we increment predecessors here, we can use that information to work
            //     // around cranelift optimizations technically not supporting jump to entrypoint.
            //     let jump = BlockJump::new(Block::entry(), take(params));
            //     func.ssa.blocks[Block::entry()].predecessors += 1;
            //     func.ssa.ventries[v] = Entry::JmpBlock(jump);
            //     return true;
            // }
            Entry::Select { on_true, on_false, .. } => {
                let new_on_true = try_inline_blockjump(&func.ssa, on_true);
                let new_on_false = try_inline_blockjump(&func.ssa, on_false);

                let Entry::Select { on_true, on_false, .. } = &mut func.ssa.ventries[v] else {
                    unreachable!()
                };

                let mut changed = false;
                if let Some(jump) = new_on_true {
                    changed = true;
                    info!("jump-inlining {} => {}", on_true.id, jump.id);
                    func.ssa.blocks[on_true.id].predecessors -= 1;
                    func.ssa.blocks[jump.id].predecessors += 1;
                    *on_true = jump;
                }
                if let Some(jump) = new_on_false {
                    changed = true;
                    info!("jump-inlining {} => {}", on_false.id, jump.id);
                    func.ssa.blocks[on_false.id].predecessors -= 1;
                    func.ssa.blocks[jump.id].predecessors += 1;
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

                let retv = V(v.0 + 1);
                func.ssa.delete_range_no_offset(retv, 1);
                let mut r = Rewrite::new(retv, Block(0));
                r.voff = -1;
                func.ssa.apply(retv, &r);

                return true;
            }
            Entry::JmpTable(_, blocks) => {
                let rejumps = blocks
                    .iter()
                    .map(|block| {
                        let jmp = BlockJump::new(*block, vec![]);
                        try_inline_blockjump(&func.ssa, &jmp)
                    })
                    .collect::<SmallVec<[_; 5]>>();

                let Entry::JmpTable(_, blocks) = &mut func.ssa.ventries[v] else {
                    unreachable!();
                };

                let mut changed = false;

                for (rejump, jump) in rejumps.into_iter().zip(blocks) {
                    if let Some(new) = rejump {
                        changed = true;
                        info!("jump-inlining {} => {} inside jump table", jump, new.id);
                        *jump = new.id;
                    }
                }

                return changed;
            }
            entry if entry.is_terminator() => break,
            _ => {}
        }
    }

    false
}

// When blocks contain binds, only single-use or scope-pure blocks are inlineable.
fn fully_inlinable(ssa: &SSA, block: Block) -> bool {
    ssa.blocks[block].predecessors == 1
        || (ssa.usages_outside_this_block(block) == 0 && ssa.block_info(block).binds < 3)
}

// Fully inline a block, assume its valid to inline.
//
// 1. copy all `V` into a local buffer
// 2. in that local one, offset `V` by the *diff* between `atv` while substituting parameters
// 3. offset all `V` occuring after the inlined data by `inlined.len()`
fn full_block_inline(ssa: &mut SSA, atv: V, jump: &BlockJump) {
    ssa.blocks[jump.id].predecessors -= 1;
    let binfo = ssa.block_info(jump.id);

    let mut r = Rewrite::new(V(atv.0 + 1), Block(0));
    r.new_block_params = Some((binfo.start, binfo.end, &jump.params));

    let (mut inlinedv, mut inlinedt) = {
        let range = (binfo.start.0 + binfo.params) as usize..=binfo.end.0 as usize;

        let mut entries = ssa.ventries.as_slice()[range.clone()].to_vec();
        let types = ssa.vtypes.as_slice()[range].to_vec();

        // Offset the new inlined to start at the current V.
        //
        // since voff is signed, this should work regardless of whether `atv` is before or after
        // `jump.id` is defined.
        r.voff = (atv.0 as i32) - (binfo.start.0 as i32 + binfo.params as i32);

        entries.iter_mut().for_each(|entry| r.entry(false, entry));

        (entries, types)
    };

    offset_predecessors(ssa, binfo.end, 1);

    // Offset all values occuring after the inline by the inline size
    // We haven't inserted the values yet. So; `V(atv+1)` still represents "all after inline".
    r.voff = inlinedv.len() as i32 - 1;
    // we don't remove the `new_block_params` because we still inline impure blocks if there's
    // a single predecessor. `apply` will check and make sure we're not corrupting the inlined block.
    ssa.apply(V(atv.0 + 1), &r);

    // Actually perform the inline
    insert_buf(atv, &mut ssa.ventries, &mut inlinedv, true);
    insert_buf(atv, &mut ssa.vtypes, &mut inlinedt, true);
}

fn try_inline_blockjump(ssa: &SSA, ijump: &BlockJump) -> Option<BlockJump> {
    let binfo = ssa.block_info(ijump.id);
    if binfo.binds == 0 {
        if let Entry::JmpBlock(jump) = binfo.tail {
            if ssa.usages_outside_this_block(ijump.id) == 0 {
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

fn func_opt_iter(_: &mut LIR, _: MonoFunc, _: Block) -> Changed {
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

    fn test_block_opts(hi: bool, name: &str, ssa: SSA) -> (String, String) {
        let mut fuel = 5;

        let item = Item::Defined(M(key::Module::from(0), key::Func::from(0)));
        let mut func = Function::new(name.into(), item, ssa, MonoType::u(8), 1);

        let mut changed = true;

        let types = Map::new();
        lumina_util::enable_highlighting(hi);
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

        let (before, after) = test_block_opts(false, "inline_block", ssa);
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

        let (before, after) = test_block_opts(false, "inline_block", ssa);
        insta::assert_snapshot!(format!("{before}\n{after}"));
    }

    #[test]
    fn practical() {
        lumina_util::test_logger();
        use lumina_typesystem::IntSize;

        let mut ssa = SSA::new();

        let block = [Block::entry(), ssa.new_block()];

        let v0 = ssa.add_block_param(block[0], MonoType::u(0));
        let _v1 = ssa.cast_payload(v0.value(), MonoType::u(1));
        let _v2 = ssa.abs(v0.value(), MonoType::u(2));
        ssa.jump(block[1], vec![]);

        ssa.switch_to_block(block[1]);
        let v3 = ssa.tag_of(v0.value(), IntSize::new(false, 16));
        ssa.return_(v3);
        // ssa.select(v3, [(block[2], vec![]), (block[3], vec![])]);

        let (before, after) = test_block_opts(false, "inline_block", ssa);
        insta::assert_snapshot!(format!("{before}\n{after}"));
    }
}
