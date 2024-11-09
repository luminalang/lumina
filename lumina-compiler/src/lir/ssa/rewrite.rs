use super::*;
use lumina_collections::MapKey;

pub struct Rewrite<'p> {
    // Only apply to anything above this point
    atv: V,
    atb: Block,

    // Offsets in either direction
    voff: i32,
    boff: i32,

    // Start of the block and parameters to substitute bparams
    new_block_params: Option<(V, &'p [Value])>,
}

impl<'p> Rewrite<'p> {
    pub fn new() -> Self {
        Self {
            atv: V(u32::MAX),
            atb: Block(u32::MAX),
            voff: 0,
            boff: 0,
            new_block_params: None,
        }
    }

    #[inline(always)]
    pub fn valways(mut self) -> Self {
        self.atv = V(0);
        self
    }
    #[inline(always)]
    pub fn balways(mut self) -> Self {
        self.atb = Block(0);
        self
    }
    #[inline(always)]
    pub fn atv(mut self, v: V) -> Self {
        self.atv = v;
        self
    }
    #[inline(always)]
    pub fn atb(mut self, b: Block) -> Self {
        self.atb = b;
        self
    }

    #[inline(always)]
    pub fn byv(mut self, voff: i32) -> Self {
        self.voff = voff;
        self
    }
    #[inline(always)]
    pub fn byb(mut self, boff: i32) -> Self {
        self.boff = boff;
        self
    }

    pub fn and_block_params<'a>(self, start: V, params: &'a [Value]) -> Rewrite<'a> {
        Rewrite { new_block_params: Some((start, params)), ..self }
    }

    pub fn v(&self, v: V) -> Value {
        if v == V(u32::MAX) {
            panic!("underflow caused by optimization");
        }

        if let Some((start, bparams)) = self.new_block_params {
            let pend = start.0 + bparams.len() as u32;

            if v.0 >= start.0 && v.0 < pend {
                let i = v.0 - start.0;
                return bparams[i as usize];
            }
        }

        Value::V(offset_if_after(self.atv, self.voff, v))
    }

    pub fn b(&self, block: Block) -> Block {
        offset_if_after(self.atb, self.boff, block)
    }

    pub fn value(&self, v: Value) -> Value {
        match v {
            Value::V(v) => self.v(v),
            v => v,
        }
    }

    pub fn values(&self, vs: &[Value]) -> Vec<Value> {
        vs.iter().map(|&v| self.value(v)).collect()
    }

    pub fn flow(&self, flow: &ControlFlow) -> ControlFlow {
        match flow {
            ControlFlow::JmpFunc(mfunc, params) => {
                let nparams = self.values(params);
                ControlFlow::JmpFunc(*mfunc, nparams)
            }
            ControlFlow::JmpBlock(block, params) => {
                let nparams = self.values(params);
                let nblock = self.b(*block);
                ControlFlow::JmpBlock(nblock, nparams)
            }
            ControlFlow::Unreachable => ControlFlow::Unreachable,
            ControlFlow::Empty => ControlFlow::Empty,
            ControlFlow::Return(v) => {
                let nv = self.value(*v);
                ControlFlow::Return(nv)
            }
            ControlFlow::Select { value, on_true, on_false } => {
                let value = self.value(*value);
                let on_true = (self.b(on_true.0), self.values(&on_true.1));
                let on_false = (self.b(on_false.0), self.values(&on_false.1));
                ControlFlow::Select { value, on_true, on_false }
            }
            ControlFlow::JmpTable(v, values, blocks) => {
                let v = self.value(*v);
                let values = self.values(values);
                let blocks = blocks.iter().map(|b| self.b(*b)).collect();
                ControlFlow::JmpTable(v, values, blocks)
            }
        }
    }

    pub fn entry(&self, entry: &Entry) -> Entry {
        let mut entry = entry.clone();
        entry.for_values_mut(&mut |v| {
            *v = match self.v(*v) {
                Value::V(v) => v,
                _ => panic!("block params substitution not allowed for entries"),
            }
        });
        entry
    }

    pub fn drain_values(
        ssa: &mut Blocks,
        start: V,
        end: V,
    ) -> impl Iterator<Item = (MonoType, Entry)> + '_ {
        ssa.vtypes
            .as_mut_vec()
            .drain(start.0 as usize..end.0 as usize)
            .zip(
                ssa.ventries
                    .as_mut_vec()
                    .drain(start.0 as usize..end.0 as usize),
            )
    }

    pub fn delete_unused_block(ssa: &mut Blocks, block: Block) {
        let removed = ssa.blocks.as_mut_vec().remove(block.0 as usize);

        let mut r = Rewrite::new().atb(block).byb(-1);

        if let Some((start, end)) = removed.offset {
            // Remove the values that no longer exist
            Rewrite::drain_values(ssa, start, end).count();

            let removedv = -(end.0 as i32 - start.0 as i32);
            r = r.atv(start).byv(removedv);

            for (_, bdata) in ssa.blocks.iter_mut() {
                // TODO: we do this in multiple places, we should create a helper
                if let Some((s, e)) = bdata.offset.as_mut() {
                    if s.0 > start.0 {
                        s.0 = s.0.wrapping_add_signed(removedv);
                        e.0 = e.0.wrapping_add_signed(removedv);
                    }
                }
            }
        }

        // Offset values to compensate
        ssa.ventries
            .values_mut()
            .for_each(|entry| *entry = r.entry(entry));

        for (_, bdata) in ssa.blocks.iter_mut() {
            bdata.tail = r.flow(&bdata.tail);
        }
    }
}

impl Entry {
    // TODO: consider whether splitting out parameters like `Entry = EntryKind * [Value]` is a good idea
    fn for_values_mut(&mut self, f: &mut dyn Fn(&mut V)) {
        match self {
            Entry::Variant(_, params)
            | Entry::Construct(params)
            | Entry::CallExtern(_, params)
            | Entry::CallStatic(_, params) => params.iter_mut().for_each(|p| p.for_values_mut(f)),
            Entry::CallValue(v, params) => {
                v.for_values_mut(f);
                params.iter_mut().for_each(|p| p.for_values_mut(f));
            }
            Entry::ExtendUnsigned(v)
            | Entry::ExtendSigned(v)
            | Entry::Reduce(v)
            | Entry::Transmute(v)
            | Entry::IntAbs(v)
            | Entry::Indice { of: v, .. }
            | Entry::Field { of: v, .. }
            | Entry::TagFromSum { of: v }
            | Entry::CastFromSum { of: v }
            | Entry::BitNot(v)
            | Entry::Replicate(v, _)
            | Entry::Deref(v)
            | Entry::IntToFloat(v, _)
            | Entry::FloatToInt(v, _)
            | Entry::Dealloc { ptr: v }
            | Entry::Copy(v) => v.for_values_mut(f),
            Entry::RefStaticVal(_) => {}
            Entry::IntCmpInclusive(lhs, _, rhs, _)
            | Entry::IntAdd(lhs, rhs)
            | Entry::BitAnd([lhs, rhs])
            | Entry::IntSub(lhs, rhs)
            | Entry::IntMul(lhs, rhs)
            | Entry::WritePtr { ptr: lhs, value: rhs }
            | Entry::IntDiv(lhs, rhs) => {
                lhs.for_values_mut(f);
                rhs.for_values_mut(f);
            }
            Entry::BlockParam(v) => f(v),
            Entry::SizeOf(_) | Entry::AlignOf(_) | Entry::Alloc | Entry::Alloca => {}
        }
    }
}

fn offset_if_after<K: MapKey>(at: K, off: i32, k: K) -> K {
    let k = k.into();
    if k >= at.into() {
        ((k as u32).wrapping_add_signed(off) as usize).into()
    } else {
        k.into()
    }
}

#[cfg(test)]
mod tests {
    use super::{Block, ControlFlow, Rewrite, V};

    #[test]
    fn offset_values() {
        let r = Rewrite::new().atv(V(2)).byv(2).atb(Block(1)).byb(1);

        let flow = ControlFlow::Select {
            value: V(0).value(),
            on_true: (Block(0), vec![V(5).value()]),
            on_false: (Block(3), vec![V(6).value()]),
        };

        let exp = ControlFlow::Select {
            value: V(0).value(),
            on_true: (Block(0), vec![V(7).value()]),
            on_false: (Block(4), vec![V(8).value()]),
        };

        assert_eq!(r.flow(&flow), exp);
    }
}
