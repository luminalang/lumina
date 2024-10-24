//! Optimizations that are more specific to Lumina and more appropriate for a higher-level IR than
//! our backends.

use crate::lir::{mono::Types, Block, ControlFlow, Entry, Function, MonoFunc, Value, LIR, V};
use lumina_collections::{Map, MapKey};
use tracing::{info, info_span};

pub const ENABLE_OPTS: bool = false;

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

        for fkey in self.lir.functions.keys() {
            self.changed = true;

            let _span = info_span!(
                "running LIR optimizer",
                entity = self.lir.functions[fkey].symbol,
                key = fkey.to_string(),
            );
            let _handle = _span.enter();

            let mut fuel = 200;

            while fuel > 0 && self.changed {
                self.changed = false;

                'block_iter: for block in self.lir.functions[fkey].blocks.blocks() {
                    let func = &mut self.lir.functions[fkey];

                    match std::mem::replace(&mut func.blocks.blocks[block].tail, ControlFlow::Empty)
                    {
                        ControlFlow::JmpFunc(mfunc, params) if mfunc == fkey => {
                            self.changed = true;

                            let flow = ControlFlow::JmpBlock(Block::entry(), params);
                            func.blocks.switch_to_block(block);
                            func.blocks.set_tail(flow);
                            // func.blocks.blocks[block].tail = flow;
                            break 'block_iter;
                        }
                        // we could perform block inlning as well however; cranelift does already
                        // do that. So; let's not bother for now.
                        c => {
                            func.blocks.blocks[block].tail = c;
                        }
                    }

                    info!("entering {block}");
                    for v in self.lir.functions[fkey].blocks.entries(block) {
                        let entry = self.lir.functions[fkey].blocks.entry_of(v);

                        info!("optimizing {v} = {entry}");

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

    fn should_inline(&self, f: MonoFunc) -> bool {
        let func = &self.lir.functions[f];
        !func.directly_recursive
            && ((func.invocations == 1 || self.inlineable(f)) && !self.has_unreachable_branches(f))
    }

    fn inlineable(&self, f: MonoFunc) -> bool {
        self.lir.functions[f].blocks.entries_count() < 5
    }

    fn inline_function(
        _types: &Types,
        ftarget: &Function,
        fin: &mut Function,
        inbl: Block,
        at: V,
        mut params: Vec<Value>,
    ) {
        let added_values = ftarget.blocks.ventries.len() as u32;
        let original_inbl_offset = fin.blocks.blocks[inbl].offset.unwrap();
        let cont_param_v = at + added_values;

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
                .for_each(|v| v.offset_v(at, added_values));

            // merge the ventries, but also modify all `v` inside the entries to be offset by our new
            // entrypoint V.
            let inlined_ventries_with_offsete = ftarget.blocks.ventries.iter().map(|(_, entry)| {
                let mut entry = entry.clone();
                let always = V(0);
                entry.offset_v(always, at.0);
                entry
            });

            let (after, _call_entry) =
                split_and_insert(at, &mut fin.blocks.ventries, inlined_ventries_with_offsete);

            fin.blocks
                .ventries
                .push_as(cont_param_v, Entry::BlockParam(V(0)));

            fin.blocks.ventries.as_mut_vec().extend(after);
        }

        // +1 because this is before we add the continuation block
        // which will be created before the inlined entry block.
        let inline_block_offset = fin.blocks.blocks.len() as u32 + 1;

        let mut old_tail = None;

        // For each block prior to inlining
        // Offset blocks and values inside the tail
        // Offset block offsets if the block's V occurs after the inline.
        //   or: Reduce end if this is the block we're inlining
        for block in fin.blocks.blocks() {
            let bdata = &mut fin.blocks.blocks[block];
            offset_flow(at, &mut bdata.tail, 0, added_values);

            if let Some((start, end)) = bdata.offset.as_mut() {
                if inbl == block {
                    *end = at;
                    let inline_entry = Block(inline_block_offset);
                    let jump = ControlFlow::JmpBlock(inline_entry, std::mem::take(&mut params));
                    old_tail = Some(std::mem::replace(&mut bdata.tail, jump));
                } else if start.0 > at.0 {
                    start.0 += added_values;
                    end.0 += added_values;
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

            let always = V(0);
            offset_flow(always, &mut blockdata.tail, inline_block_offset, at.0);

            if let &ControlFlow::Return(v) = &blockdata.tail {
                fin.blocks.blocks[continuation_block].predecessors += 1;
                blockdata.tail = ControlFlow::JmpBlock(continuation_block, vec![v]);
            }

            fin.blocks
                .blocks
                .push_as(Block(inline_block_offset + block.0), blockdata);
        }
    }
}

fn offset_flow(at: V, flow: &mut ControlFlow, boffset: u32, voffset: u32) {
    match flow {
        ControlFlow::JmpBlock(block, params) => {
            offset_values(at, params, voffset);
            block.0 += boffset;
        }
        ControlFlow::Select { on_true, on_false, value } => {
            value.for_values_mut(&mut |v| offset_value(at, v, voffset));
            on_true.0 .0 += boffset;
            on_false.0 .0 += boffset;
        }
        ControlFlow::JmpTable(v, params, blocks) => {
            v.for_values_mut(&mut |v| offset_value(at, v, voffset));
            offset_values(at, params, voffset);
            blocks.iter_mut().for_each(|block| block.0 += boffset)
        }
        ControlFlow::JmpFunc(_, params) => {
            offset_values(at, params, voffset);
        }
        ControlFlow::Return(v) => {
            v.for_values_mut(&mut |v| offset_value(at, v, voffset));
        }
        _ => {}
    }
}

fn offset_value(at: V, v: &mut V, by: u32) {
    if v.0 >= at.0 {
        v.0 += by
    }
}

fn offset_values(at: V, values: &mut [Value], by: u32) {
    values
        .iter_mut()
        .for_each(|v| v.for_values_mut(&mut |v| offset_value(at, v, by)))
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
