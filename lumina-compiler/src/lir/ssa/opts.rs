//! Optimizations that are more specific to Lumina and more appropriate for a higher-level IR than
//! our backends.

use super::rewrite::Rewrite;
use crate::lir::{
    mono::Types, Block, Blocks, ControlFlow, Entry, Function, MonoFunc, Value, LIR, V,
};
use lumina_collections::{Map, MapKey};
use tracing::{info, info_span, trace};

pub const ENABLE_OPTS: bool = true;

impl LIR {
    pub fn perform_optimizations(&mut self) {
        let mut opt = Optimizer { lir: self, changed: true };
        opt.optimizations();
    }
}

struct Optimizer<'a> {
    lir: &'a mut LIR,
    changed: bool,
}

impl<'a> Optimizer<'a> {
    fn optimizations(&mut self) {
        if !ENABLE_OPTS {
            return;
        }

        self.block_opts();
        self.func_opts();
    }

    fn func_opts(&mut self) {
        for fkey in self.lir.functions.keys() {
            self.changed = true;

            let _span = info_span!(
                "running LIR optimizer",
                entity = self.lir.functions[fkey].symbol,
                key = fkey.to_string(),
            );
            let _handle = _span.enter();

            let mut fuel = 10000;

            while fuel > 0 && self.changed {
                self.changed = false;

                'block_iter: for block in self.lir.functions[fkey].blocks.blocks() {
                    let func = &mut self.lir.functions[fkey];

                    let Some(_) = func.blocks.blocks.as_slice().get(block.0 as usize) else {
                        break 'block_iter;
                    };

                    // Optimizations on the block entries
                    for v in self.lir.functions[fkey].blocks.entries(block) {
                        let entry = self.lir.functions[fkey].blocks.entry_of(v);

                        trace!("optimizing {v} = {entry}");

                        match entry {
                            Entry::CallStatic(mfunc, params) => {
                                if self.should_inline(*mfunc) {
                                    self.changed = true;

                                    info!("inlining the call to {mfunc} in {fkey}");
                                    let params = params.clone();
                                    let [fin, ftarget] =
                                        self.lir.functions.get_many_mut([fkey, *mfunc]);
                                    let types = &self.lir.mono.types;
                                    Optimizer::inline_function(
                                        types, ftarget, fin, block, v, params,
                                    );
                                    break 'block_iter;
                                }
                            }
                            _ => {}
                        }
                    }
                }
                fuel -= 1;
            }

            if self.lir.functions[fkey].symbol.contains("main::testing") {
                println!(
                    "OPTIMIZED FORM: \n{}",
                    self.lir
                        .mono
                        .fmt(&self.lir.functions[fkey])
                        .fns(&self.lir.functions)
                );
            }
        }
    }

    fn block_opts(&mut self) {
        for fkey in self.lir.functions.keys() {
            self.changed = true;

            if self.lir.functions[fkey].symbol.contains("main::testing") {
                println!(
                    "BEFORE FORM: \n{}",
                    self.lir
                        .mono
                        .fmt(&self.lir.functions[fkey])
                        .fns(&self.lir.functions)
                );
            }

            let _span = info_span!(
                "running LIR optimizer",
                entity = self.lir.functions[fkey].symbol,
                key = fkey.to_string(),
            );
            let _handle = _span.enter();

            let mut fuel = 10000;

            while fuel > 0 && self.changed {
                self.changed = false;

                'block_iter: for block in self.lir.functions[fkey].blocks.blocks() {
                    let func = &mut self.lir.functions[fkey];

                    let Some(_) = func.blocks.blocks.as_slice().get(block.0 as usize) else {
                        break 'block_iter;
                    };

                    if Optimizer::is_unused(func, block) {
                        self.changed = true;
                        info!("deleting unused block {block}");
                        Rewrite::delete_unused_block(&mut func.blocks, block);
                        break 'block_iter;
                    }

                    match Optimizer::should_inline_block(func, block) {
                        BlockInline::JumpSubst { dst } => {
                            self.changed = true;
                            Optimizer::jump_subst_block_inline(&mut func.blocks, block, dst);
                            break 'block_iter;
                        }
                        BlockInline::FlowSubst { removed } => {
                            self.changed = true;
                            Optimizer::flow_subst_block_inline(&mut func.blocks, block, removed);
                            break 'block_iter;
                        }
                        BlockInline::Impossible => {}
                    }

                    trace!("entering {block}");
                }
                fuel -= 1;
            }
        }
    }

    // inlining breaks when the inline function has unreachable branches. not sure why
    fn has_unreachable_branches(&self, f: MonoFunc) -> bool {
        self.lir.functions[f]
            .blocks
            .blocks
            .values()
            .any(|block| matches!(&block.tail, ControlFlow::Unreachable))
    }

    fn is_unused(func: &Function, block: Block) -> bool {
        block != Block::entry() && func.blocks.blocks[block].predecessors == 0
    }

    fn should_inline(&self, f: MonoFunc) -> bool {
        let func = &self.lir.functions[f];
        !func.directly_recursive
            && ((func.invocations == 1 || self.inlineable(f)) && !self.has_unreachable_branches(f))
    }

    fn should_inline_block(_: &Function, _: Block) -> BlockInline {
        // Turns out; this way of doing optimizations is *highly* unsound.
        //
        // However; I've got a completely different idea of doing LIR optimizations entirely, and
        // suspect I've fundamentally missunderstood the benefits of A-Normal.
        //
        // So; time to try something different
        BlockInline::Impossible
        // if block == Block::entry() {
        //     return BlockInline::Impossible;
        // }

        // let iblock = &func.blocks.blocks[block];

        // // If it has its own entries, we do not inline it
        // match iblock.offset {
        //     Some((start, end)) if end.0 - start.0 != iblock.parameters => {
        //         return BlockInline::Impossible
        //     }
        //     _ => {}
        // }

        // // If the block does nothing but jump to another block with
        // // parameters then we can inline it in more situations, and without shifting values.
        // if let ControlFlow::JmpBlock(dst, params) = &iblock.tail {
        //     if params.is_empty()
        //         || (func.blocks.blocks[*dst].predecessors == 1
        //             && all_predecessors_support_block_args(&func.blocks, *dst))
        //     {
        //         // TODO: we need to check for jump tables and not use JumpSubst if they exist
        //         return BlockInline::JumpSubst { dst: *dst };
        //     }
        // }

        // // Otherwise we can only inline it if all references to it
        // // are from places that can be substituted by a new Flow instead of
        // // just a different block Jump.
        // let does_not_require_block_id = func.blocks.blocks.iter().all(|(b, bdata)| {
        //     match &bdata.tail {
        //         ControlFlow::Select { on_true, on_false, .. } => {
        //             on_true.0 != block && on_false.0 != block
        //         }
        //         ControlFlow::JmpTable(_, _, blocks) => blocks.iter().all(|&b| b != block),
        //         // Don't inline self-recursive blocks
        //         ControlFlow::JmpBlock(jump, _) if b == block => b != *jump,
        //         _ => true,
        //     }
        // });

        // if does_not_require_block_id {
        //     let removed = iblock.offset;
        //     dbg!(removed);
        //     return BlockInline::FlowSubst { removed };
        // }

        // return BlockInline::Impossible;
    }

    fn inlineable(&self, f: MonoFunc) -> bool {
        self.lir.functions[f].blocks.entries_count() < 5
    }

    fn jump_subst_block_inline(ssa: &mut Blocks, iblock: Block, dst: Block) {
        info!("inlining {iblock}'s re-jump to {dst} in all blocks");

        let ControlFlow::JmpBlock(ddst, dst_params) = ssa.blocks[iblock].tail.clone() else {
            unreachable!()
        };
        assert_eq!(ddst, dst);

        let iblock_params = ssa.params(iblock);

        ssa.blocks[dst].parameters += iblock_params.clone().count() as u32;
        let start = match ssa.blocks[dst].offset.as_mut() {
            Some((start, _)) => {
                start.0 -= iblock_params.clone().count() as u32;
                Some(*start)
            }
            None => iblock_params.clone().next().inspect(|&start| {
                ssa.blocks[dst].offset = Some((start, start + iblock_params.clone().count() as u32))
            }),
        };

        let inlines = ssa
            .blocks
            .iter_mut()
            .map(|(block, bdata)| {
                if block == iblock {
                    return 0;
                }

                let subst = |b: &mut Block, params: &mut Vec<_>| {
                    if *b == iblock {
                        *b = dst;
                        if params.is_empty() {
                            params.extend(dst_params.iter().cloned());
                        } else {
                            let start = start.unwrap();
                            let r = Rewrite::new().and_block_params(start, params);
                            let dst_params: Vec<_> =
                                dst_params.iter().map(|&v| r.value(v)).collect();
                            params.extend(dst_params);
                        }
                        1
                    } else {
                        0
                    }
                };

                match &mut bdata.tail {
                    ControlFlow::JmpBlock(b, params) => subst(b, params),
                    ControlFlow::Select { on_true, on_false, .. } => {
                        subst(&mut on_true.0, &mut on_true.1)
                            + subst(&mut on_false.0, &mut on_false.1)
                    }
                    ControlFlow::JmpTable(_, _, blocks) => {
                        let mut params = vec![];
                        let c = blocks.iter_mut().map(|b| subst(b, &mut params)).sum();
                        if c > 0 {
                            assert_eq!(
                                iblock_params.clone().count(),
                                0,
                                "jump tables do not support block params"
                            );
                        }
                        c
                    }
                    _ => 0,
                }
            })
            .sum::<u16>();

        assert_eq!(inlines, ssa.blocks[iblock].predecessors);

        ssa.blocks[dst].predecessors += inlines - 1;
        ssa.blocks[iblock].predecessors -= inlines;
        ssa.blocks[iblock].offset = None;

        Rewrite::delete_unused_block(ssa, iblock);
    }

    fn flow_subst_block_inline(ssa: &mut Blocks, iblock: Block, removed: Option<(V, V)>) {
        info!("inlining {iblock}'s flow in all blocks");

        let tail = ssa.blocks[iblock].tail.clone();

        let mut inlines: i16 = 0;

        for (block, bdata) in ssa.blocks.iter_mut() {
            if block == iblock {
                continue;
            }

            match &mut bdata.tail {
                ControlFlow::JmpBlock(b, params) if *b == iblock => {
                    let params = std::mem::take(params);
                    let mut r = Rewrite::new();
                    dbg!(removed);
                    if let Some((start, _)) = removed {
                        dbg!(start, &params);
                        r = r.and_block_params(start, &params);
                    }
                    inlines += 1;
                    bdata.tail = r.flow(&tail);
                }
                _ => {}
            }
        }

        let predecessor_increment = inlines - 1;
        tail.for_each_block(|block| {
            let p = &mut ssa.blocks[block].predecessors;
            *p = p.wrapping_add_signed(predecessor_increment);
        });

        Rewrite::delete_unused_block(ssa, iblock);
    }

    fn inline_function(
        _types: &Types,
        ftarget: &Function,
        fin: &mut Function,
        inbl: Block,
        at: V,
        mut params: Vec<Value>,
    ) {
        let added_values = ftarget.blocks.ventries.len() as i32;
        let original_inbl_offset = fin.blocks.blocks[inbl].offset.unwrap();
        let cont_param_v = at + added_values as u32;

        // Update the ventries/vtypes maps to contain the inlined values
        {
            // merge the vtypes from what we want to inline into the current position
            let inlined_vtypes = ftarget.blocks.vtypes.values().cloned();
            let (after, call_bind_type) =
                split_and_insert(at, &mut fin.blocks.vtypes, inlined_vtypes);

            // continuation parameter is moved to after inlined values
            fin.blocks.vtypes.push(call_bind_type);
            fin.blocks.vtypes.as_mut_vec().extend(after);

            // modify all `v` after `at` inside the entries to be offset by the amount of values
            // that were added inbetween.
            fin.blocks
                .ventries
                .values_mut()
                .for_each(|v| *v = Rewrite::new().atv(at).byv(added_values).entry(v));

            // for all the values we copy over, offset by `at` so the inlined values start at `0`
            let inlined_ventries_with_offset = ftarget
                .blocks
                .ventries
                .values()
                .map(|entry| Rewrite::new().valways().byv(at.0 as i32).entry(&entry));
            // merge the inlined values
            let (after, _call_entry) =
                split_and_insert(at, &mut fin.blocks.ventries, inlined_ventries_with_offset);

            // continuation parameter is moved to after inlined values
            fin.blocks
                .ventries
                .push_as(cont_param_v, Entry::BlockParam(V(0)));
            fin.blocks.ventries.as_mut_vec().extend(after);
        }

        // +1 because this is before we add the continuation block
        // which will be created before the inlined entry block.
        let inline_block_offset = fin.blocks.blocks.len() as i32 + 1;

        let mut old_tail = None;

        // For each block prior to inlining
        // Offset blocks and values inside the tail
        // Offset block offsets if the block's V occurs after the inline.
        //   or: Reduce end if this is the block we're inlining
        let r = Rewrite::new().atv(at).byv(added_values);
        for block in fin.blocks.blocks() {
            let bdata = &mut fin.blocks.blocks[block];
            bdata.tail = r.flow(&bdata.tail);

            if let Some((start, end)) = bdata.offset.as_mut() {
                if inbl == block {
                    *end = at;
                    let inline_entry = Block(inline_block_offset as u32);
                    let jump = ControlFlow::JmpBlock(inline_entry, std::mem::take(&mut params));
                    old_tail = Some(std::mem::replace(&mut bdata.tail, jump));
                } else if start.0 > at.0 {
                    start.0 = start.0.wrapping_add_signed(added_values);
                    end.0 = end.0.wrapping_add_signed(added_values);
                }
            }
        }

        // Create continuation block
        let continuation_block = {
            let original_len = original_inbl_offset.1 .0 - original_inbl_offset.0 .0;
            let remaining = original_len - (at.0 - original_inbl_offset.0 .0);
            let continuation_block = fin.blocks.new_block(1);
            let bdata = &mut fin.blocks.blocks[continuation_block];
            bdata.offset = Some((cont_param_v, cont_param_v + remaining));
            bdata.tail = old_tail.unwrap();
            continuation_block
        };

        // Copy over and offset the blocks in ftarget into fin
        // Offset the blocks and values inside the tail of each block
        // Substitute any `return v` with `jump continuation(v)`
        for block in ftarget.blocks.blocks() {
            let mut blockdata = ftarget.blocks.blocks[block].clone();

            if block == Block::entry() {
                blockdata.predecessors += 1;
            }

            if let Some((start, end)) = blockdata.offset.as_mut() {
                start.0 += at.0;
                end.0 += at.0;
            }

            blockdata.tail = Rewrite::new()
                .valways()
                .balways()
                .byv(at.0 as i32)
                .byb(inline_block_offset)
                .flow(&blockdata.tail);

            match &blockdata.tail {
                ControlFlow::JmpFunc(_, _) => todo!("this is a problem"),
                &ControlFlow::Return(v) => {
                    fin.blocks.blocks[continuation_block].predecessors += 1;
                    blockdata.tail = ControlFlow::JmpBlock(continuation_block, vec![v]);
                }
                _ => {}
            }

            fin.blocks
                .blocks
                .push_as(Block(inline_block_offset as u32 + block.0), blockdata);
        }
    }
}

enum BlockInline {
    JumpSubst { dst: Block },
    FlowSubst { removed: Option<(V, V)> },
    Impossible,
}

// Inserts `added` at `k` so that `k` remains but all keys after it are returned and replaced by `added`
fn split_and_insert<K: MapKey, V: Clone + std::fmt::Debug>(
    k: K,
    map: &mut Map<K, V>,
    added: impl Iterator<Item = V>,
) -> (Vec<V>, V) {
    let map = map.as_mut_vec();
    let after = map.split_off(k.into() + 1);

    // Remove the call instruction that we're inlining
    let removed = map.pop().unwrap();

    map.extend(added);
    (after, removed)
}

// HACK: Since jump tables do not support block arguments, inlining a block with none that jumps to
// a block with some is not possible.
//
// ideally we'd set some bitflags during lower instead or something
fn all_predecessors_support_block_args(ssa: &Blocks, dst: Block) -> bool {
    for bdata in ssa.blocks.values() {
        match &bdata.tail {
            ControlFlow::JmpTable(_, _, branches) if branches.contains(&dst) => return false,
            _ => {}
        }
    }

    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lir::ssa::Blocks;
    use crate::lir::{ty_fmt, MonoType};

    #[test]
    fn jump_subst_block_inline() {
        lumina_util::enable_highlighting(false);

        let records = Map::new();

        let mut ssa = Blocks::new(1);
        let v0 = ssa.add_block_param(Block::entry(), MonoType::u(0)).value();

        let [truthy, cont] = [1, 2].map(|p| ssa.new_block(p));
        ssa.set_tail(ControlFlow::Select {
            value: Value::bool(true),
            on_true: (truthy, vec![Value::u(1, 16)]),
            on_false: (truthy, vec![Value::u(1, 16)]),
        });

        ssa.switch_to_block(truthy);
        let v1 = ssa.add_block_param(truthy, MonoType::u(16)).value();
        let two = Value::u(2, 32);
        ssa.set_tail(ControlFlow::JmpBlock(cont, vec![v1, two]));

        ssa.switch_to_block(cont);
        let v2 = ssa.add_block_param(cont, MonoType::u(16)).value();
        let v3 = ssa.add_block_param(cont, MonoType::u(32)).value();
        let v4 = ssa.construct(vec![v0, v1, v2, v3], MonoType::u(0 + 16 + 16 + 32));
        ssa.set_tail(ControlFlow::Return(v4));

        let mut comparison = format!("BEFORE OPTS:\n{}", ty_fmt(&records, &ssa));

        Optimizer::jump_subst_block_inline(&mut ssa, truthy, cont);

        comparison.push_str("\nAFTER OPTS:\n");
        comparison += &ty_fmt(&records, &ssa).to_string();

        insta::assert_snapshot!(comparison);
    }

    #[test]
    fn flow_subst_block_inline() {
        lumina_util::enable_highlighting(false);

        let records = Map::new();

        let mut ssa = Blocks::new(1);
        ssa.add_block_param(Block::entry(), MonoType::u(0));

        let [truthy, falsely, cont] = [1, 1, 2].map(|p| ssa.new_block(p));
        ssa.set_tail(ControlFlow::Select {
            value: Value::bool(true),
            on_true: (truthy, vec![Value::u(1, 16)]),
            on_false: (falsely, vec![Value::u(2, 16)]),
        });

        ssa.switch_to_block(truthy);
        let v1 = ssa.add_block_param(truthy, MonoType::u(16)).value();
        let v2 = ssa.copy(Value::u(4, 32), MonoType::u(32));
        ssa.set_tail(ControlFlow::JmpBlock(cont, vec![v1, v2]));

        ssa.switch_to_block(cont);
        let _v3 = ssa.add_block_param(cont, MonoType::u(16));
        let v4 = ssa.add_block_param(cont, MonoType::u(32));
        ssa.set_tail(ControlFlow::Return(v4.value()));

        ssa.switch_to_block(falsely);
        let v5 = ssa.add_block_param(falsely, MonoType::u(16)).value();
        let v6 = ssa.copy(Value::u(6, 32), MonoType::u(32));
        ssa.set_tail(ControlFlow::JmpBlock(cont, vec![v5, v6]));

        let mut comparison = format!("BEFORE OPTS:\n{}", ty_fmt(&records, &ssa));

        let removed = Some((V(3), V(5)));
        assert_eq!(ssa.blocks[cont].offset, removed);
        Optimizer::flow_subst_block_inline(&mut ssa, cont, removed);

        comparison.push_str("\nAFTER OPTS:\n");
        comparison += &ty_fmt(&records, &ssa).to_string();

        insta::assert_snapshot!(comparison);
    }
}
