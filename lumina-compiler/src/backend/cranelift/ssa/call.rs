use super::*;

pub struct Call<'a> {
    pub params: Vec<Value>,
    rlayout: &'a Layout<Type>,
    ret_pointer: Option<Value>,
}

impl<'c, 'a, 'f> InstHelper<'c, 'a, 'f> {
    pub fn new_call<'t>(&mut self, mut plen: usize, rlayout: &'t Layout<Type>) -> Call<'t> {
        let mut ret_pointer = None;
        rlayout.out_pointers(&mut |kind, size_t| {
            let (size, align) = self.structs.size_and_align_of_ptr_dst(&kind);
            let slot = self.create_struct_stack_slot(size, align as u8);
            let ptr = self.ins().stack_addr(size_t, slot, 0);
            ret_pointer = Some(ptr);
        });
        let params = if let Some(ptr) = ret_pointer {
            plen += 1;
            let mut params = Vec::with_capacity(plen + 1);
            params.push(ptr);
            params
        } else {
            Vec::with_capacity(plen)
        };
        Call { params, rlayout, ret_pointer }
    }

    pub fn call_direct<'t>(&mut self, id: FuncId, call: Call<'t>) -> VLayout {
        let fref = self.declare_func_in_func(id);
        let c = self.ins().call(fref, &call.params);

        //let rptr = call.return_pointer.map(|i| call.params[i]);
        let inst_values = self.builder.inst_results(c).to_vec();
        self.layout_from_raw_values(call.ret_pointer, call.rlayout, &mut inst_values.as_slice())
        // self.map_call_results_to_vlayout(c, call.rlayout, rptr)
    }

    fn call_indirect<'t>(&mut self, ptr: Value, layout: &FuncLayout, call: Call<'t>) -> VLayout {
        let sig = self.structs.signature(&layout);
        let sigref = self.builder.import_signature(sig);

        let c = self.ins().call_indirect(sigref, ptr, &call.params);

        let inst_values = self.builder.inst_results(c).to_vec();
        self.layout_from_raw_values(call.ret_pointer, call.rlayout, &mut inst_values.as_slice())
    }
}

impl<'c, 'a, 'f> Translator<'c, 'a, 'f> {
    pub fn call_func(&mut self, mfunc: MonoFunc, params: &[lir::Value]) -> VLayout {
        let id = self.ctx.funcmap[mfunc];
        self.call_func_id(id, params)
    }

    pub fn call_extern(&mut self, key: M<key::Func>, params: &[lir::Value]) -> VLayout {
        let id = self.ctx.externmap[&key];
        self.call_func_id(id, params)
    }

    pub fn call_func_id(&mut self, id: FuncId, lparams: &[lir::Value]) -> VLayout {
        let layout = &self.ctx.flayouts[id];
        let rlayout = layout.ret.clone();
        let plen = layout.params.len();
        let mut call = self.ins().new_call(plen, &rlayout);
        self.fparams_from_funcid(false, id, lparams, &mut call.params);
        self.ins().call_direct(id, call)
    }

    pub fn call_vlayout(&mut self, vlayout: VLayout, params: &[lir::Value]) -> VLayout {
        match vlayout {
            Layout::Scalar(Scalar::FuncPointer(layout), point) => {
                let mut call = self.ins().new_call(layout.params.len(), &layout.ret);
                self.fparams_from_layout(layout.params.as_slice(), params, &mut call.params);
                self.ins().call_indirect(point, &layout, call)
            }
            _ => panic!("call to non-function"),
        }
    }

    pub fn bparams(&mut self, jump: &lir::BlockJump) -> Vec<Value> {
        let clblock = self.f.blockmap[jump.id].0;
        let mut buf = Vec::with_capacity(self.f.builder.block_params(clblock).len());

        for (i, p) in jump.params.iter().enumerate() {
            let got = self.value_to_vlayout(*p);

            let v = self.f.func.ssa.get_block_param(jump.id, i as u32);
            let ty = self.f.func.ssa.type_of(v);
            let exp = self.ctx.structs.type_to_layout(ty, Stability::F);

            let unified = self.ins().make_compatible(&exp, got);
            self.ins().layout_into_raw_values(&unified, &mut buf);
        }

        buf
    }

    pub fn fparams_from_layout(
        &mut self,
        exp: &[Layout<Type>],
        src: &[lir::Value],
        buf: &mut Vec<Value>,
    ) {
        src.iter().zip(exp).enumerate().for_each(|(i, (p, exp))| {
            let vlayout = self.value_to_vlayout(*p);
            trace!("param{i} as {vlayout:?} for {exp:?}");
            let unified = self.ins().make_compatible(&exp, vlayout);
            self.ins().layout_into_raw_values(&unified, buf);
        });
    }

    fn fparams_from_funcid(
        &mut self,
        promote_stack: bool,
        id: FuncId,
        src: &[lir::Value],
        buf: &mut Vec<Value>,
    ) {
        src.iter().enumerate().for_each(|(i, p)| {
            let vlayout = self.value_to_vlayout(*p);
            let exp = &self.ctx.flayouts[id].params[key::Param(i as u32)];
            let exp = if promote_stack {
                exp.promote_all_stack_to_heap()
            } else {
                exp.clone()
            };
            trace!("param{i} as {vlayout:?} for {exp:?}");
            let unified = self.ins().make_compatible(&exp, vlayout);
            self.ins().layout_into_raw_values(&unified, buf);
        });
    }

    fn copy_tail_rptr(&self, params: &mut Vec<Value>) -> Option<Value> {
        let mut out_pointer = None;
        self.ctx.flayouts[self.f.id]
            .ret
            .out_pointers(&mut |_, size_t| {
                let entry = self.f.blockmap[lir::Block::entry()].0;
                warn!("assuming first parameter is return pointer");
                let rptr = self.f.builder.block_params(entry)[0];
                assert_eq!(self.f.type_of_value(rptr), size_t);
                out_pointer = Some(rptr);
                params.push(rptr);
            });
        out_pointer
    }

    pub fn tail_call(&mut self, mfunc: MonoFunc, cparams: &[lir::Value]) {
        let mut params = Vec::with_capacity(cparams.len());

        let id = self.ctx.funcmap[mfunc];
        let fname = &self.ctx.lir.functions[mfunc].symbol;
        let cname = &self.ctx.lir.functions[self.f.fkey].symbol;

        let rlayout = &self.ctx.flayouts[id].ret;
        let current_rlayout = &self.ctx.flayouts[self.f.id].ret;

        if rlayout == current_rlayout {
            info!("performing a ret-tail call to {fname} in {cname}");

            // TODO: long-term, it'd be *much* better to know whether this will happen or not when
            // *creating* the initial values. Because right now we very often stack allocate
            // something elsewhere in the function, do stuff with it, then come to here where we
            // will have to memcpy it onto the heap.
            //
            // I'm not sure what the best way to accomplish that would be though. `LIR` not having
            // to care about layout does have a lot of benefits, and I don't want to add a pass
            // in-between.

            let _out_pointer = self.copy_tail_rptr(&mut params);
            let promote =
                self.has_references_to_current_stack(&self.ctx.flayouts[id].params.as_slice());
            self.fparams_from_funcid(promote, id, cparams, &mut params);

            let fref = self.ins().declare_func_in_func(id);
            self.cins().return_call(fref, &params);
        } else {
            let mut has_rptr = false;
            rlayout.out_pointers(&mut |_, _| has_rptr = true);

            if has_rptr
                || self.has_references_to_current_stack(&self.ctx.flayouts[id].params.as_slice())
            {
                info!("refusing tail call due to ret mismatch");

                let layout = self.call_func_id(id, cparams);
                self.return_(false, layout);
            } else {
                info!("performing a ret-tail call to {mfunc} in {fname} without rptr");

                self.fparams_from_funcid(false, id, cparams, &mut params);
                let fref = self.ins().declare_func_in_func(id);
                self.cins().return_call(fref, &params);
            }
        }
    }

    // TODO: We could track whether stack pointers originate from the current function or parent
    // function, and if we do we can optimize tail calls in more situations.
    fn has_references_to_current_stack(&self, params: &[Layout<Type>]) -> bool {
        params.iter().any(Layout::has_stack_pointers)
    }
}
