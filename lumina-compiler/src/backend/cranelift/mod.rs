use crate::debuginfo::BinDebugInfo;
use crate::lir;
use crate::prelude::*;
use crate::target::{Arch, LinuxPlatform, Platform};
use crate::Target;
use cranelift::codegen::ir;
use cranelift::prelude::*;
use cranelift_module::{DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use owo_colors::OwoColorize;
use std::sync::Arc;
use tracing::info_span;

mod debuginfo;
use debuginfo::unwind;
mod layout;
mod ssa;

use layout::{FType, PassBy};

impl Target {
    fn isa(&self) -> isa::Builder {
        match self {
            Target { arch: Arch::X86_64, platform: Platform::Linux { sub } } => match sub {
                LinuxPlatform::Gnu | LinuxPlatform::Musl | LinuxPlatform::Syscall => {
                    isa::lookup_by_name("x86_64-unknown-linux").unwrap()
                }
            },
        }
    }
}

pub fn run(target: Target, dwarf: BinDebugInfo, lir: lir::Output) -> Vec<u8> {
    let mut shared_builder = settings::builder();
    shared_builder.set("opt_level", "speed").unwrap();
    let shared_flags = settings::Flags::new(shared_builder);
    assert!(shared_flags.unwind_info());

    let isa = target.isa().finish(shared_flags).unwrap();

    let objbuilder = ObjectBuilder::new(
        isa.clone(),
        b"lumina".to_vec(),
        cranelift_module::default_libcall_names(),
    )
    .unwrap();
    let mut objmodule = ObjectModule::new(objbuilder);

    let mut structs = layout::Structs::new(&lir.types);

    let vals = lir.val_types.map(|val, ty| {
        let size = structs.size_of(ty) as usize;
        let name = format!("{}___VAL", lir.functions[lir.val_initializers[&val]].symbol);
        let thread_local = false; // TODO: this is something we're gonna want
        let id = objmodule
            .declare_data(&name, Linkage::Export, true, thread_local)
            .unwrap();
        let mut data = cranelift_module::DataDescription::new();
        data.init = cranelift_module::Init::Zeros { size };
        objmodule.define_data(id, &data).unwrap();
        id
    });

    let rotable = lir.read_only_table.map(|ro, (bytes, _ty)| {
        let name = ro.to_string();
        let thread_local = false;
        let id = objmodule
            .declare_data(&name, Linkage::Export, false, thread_local)
            .unwrap();
        let mut data = cranelift_module::DataDescription::new();
        data.init = cranelift_module::Init::Bytes { contents: bytes.0.clone() };
        objmodule.define_data(id, &data).unwrap();
        id
    });

    let externmap = lir
        .extern_funcs
        .iter()
        .map(|(key, func)| {
            let mut typing = structs.records.get_abi_typing(&func.params, &func.returns);
            typing.conv = isa.default_call_conv();

            let sig = structs.signature(&typing);
            let id = objmodule
                .declare_function(&func.symbol, Linkage::Import, &sig)
                .unwrap();

            (*key, FuncHeader { id, typing })
        })
        .collect();

    let funcmap: Map<lir::MonoFunc, FuncHeader> = lir
        .functions
        .iter()
        .map(|(mfunc, func)| {
            let entry = lir::Block::entry();

            info!(
                "lowering signature of {mfunc} {}",
                format!("// {}", func.symbol).dimmed()
            );

            let typing = structs.records.get_abi_typing(
                func.blocks.params(entry).map(|v| func.blocks.type_of(v)),
                &func.returns,
            );

            let sig = structs.signature(&typing);

            assert!(!func.symbol.is_empty());
            let id = objmodule
                .declare_function(&func.symbol, Linkage::Hidden, &sig)
                .unwrap();

            FuncHeader { id, typing }
        })
        .collect();

    let unwindinfo = unwind::UnwindContext::new(&*isa, true);

    let mut ctx = Context::new(
        isa, &vals, &lir, structs, objmodule, funcmap, externmap, rotable, unwindinfo, dwarf,
    );

    let mut cctx = codegen::Context::new();
    let mut fctx = FunctionBuilderContext::new();
    for (mfunc, func) in lir.functions.iter() {
        let _span = info_span!(
            "lowering function expression",
            entity = func.symbol,
            key = mfunc.to_string()
        );
        let _handle = _span.enter();

        let f_dbg_ctx = ssa::Translator::func(&mut ctx, &mut cctx, &mut fctx, func, mfunc);

        let id = ctx.funcmap[mfunc].id;
        if let Err(err) = ctx.objmodule.define_function(id, &mut cctx) {
            panic!("definition error when defining {}:\n {err}", func.symbol);
        }

        let id = ctx.funcmap[mfunc].id;
        ctx.unwindinfo.add_function(id, &cctx, &*ctx.isa);

        f_dbg_ctx.finalize(&mut ctx.debuginfo, id, &cctx);

        cctx.clear();
    }

    ctx.declare_entrypoint(target);

    let mut product = ctx.objmodule.finish();
    ctx.unwindinfo.emit(&mut product);
    ctx.debuginfo.emit(&mut product);

    product.emit().unwrap()
}

#[derive(new)]
pub struct Context<'a> {
    isa: Arc<dyn isa::TargetIsa>,
    val_to_globals: &'a MMap<key::Val, DataId>,
    lir: &'a lir::Output,
    structs: layout::Structs<'a>,
    objmodule: ObjectModule,
    funcmap: Map<lir::MonoFunc, FuncHeader>,
    externmap: HashMap<M<key::Func>, FuncHeader>,
    rotable: MMap<key::ReadOnly, DataId>,
    unwindinfo: unwind::UnwindContext,
    debuginfo: BinDebugInfo,
}

impl<'a> Context<'a> {
    pub fn size_t(&self) -> Type {
        let triple = self.isa.triple();
        Type::triple_pointer_type(triple)
    }

    // Declares a function that runs all the val initialisers, and writes their return types to the
    // global variable mapped to that initialiser.
    fn declare_val_run_and_store(&mut self) -> FuncId {
        let mut func_builder_ctx = FunctionBuilderContext::new();
        let mut clfunc = ir::Function::new();
        let mut builder = FunctionBuilder::new(&mut clfunc, &mut func_builder_ctx);
        builder.func.signature = Signature::new(isa::CallConv::SystemV);

        let entry = builder.create_block();
        builder.seal_block(entry);
        builder.switch_to_block(entry);

        let id = self
            .objmodule
            .declare_function(
                "__lumina_val_initialiser__",
                Linkage::Export,
                &builder.func.signature,
            )
            .unwrap();

        for val in self.val_to_globals.iter() {
            let header = &self.funcmap[self.lir.val_initializers[&val]];
            let init = self
                .objmodule
                .declare_func_in_func(header.id, &mut builder.func);

            let dataid = self.val_to_globals[val];
            let data = self
                .objmodule
                .declare_data_in_func(dataid, &mut builder.func);
            let ptr = builder.ins().symbol_value(types::I64, data);

            let ret_ftype = self.structs.ftype(&header.typing.ret);

            // Attach the struct-return pointer as last parameter if needed
            let params = match &ret_ftype {
                FType::Struct(PassBy::Pointer, _) => vec![ptr],
                _ => vec![],
            };

            let ret = builder.ins().call(init, &params);

            match ret_ftype {
                FType::Struct(PassBy::Value, key) => {
                    let align = self.structs.get(key).align;
                    let mut offset = 0;

                    for v in builder.inst_results(ret).to_vec() {
                        let size = builder.func.dfg.value_type(v).bytes();

                        builder.ins().store(MemFlags::trusted(), v, ptr, offset);

                        let padding = (align - size % align) % align;
                        offset += padding as i32 + size as i32;
                    }
                }
                FType::Scalar(_) => {
                    let r = builder.inst_results(ret)[0];
                    builder.ins().store(MemFlags::trusted(), r, ptr, 0);
                }
                // already done as we passed the data as struct return directly
                FType::ArrayPointer(..) | FType::Struct(PassBy::Pointer, _) | FType::StructZST => {}
            }
        }

        builder.ins().return_(&[]);

        info!("value initialiser function:\n{}", builder.func);

        if let Err(err) = cranelift_codegen::verify_function(&clfunc, self.isa.as_ref()) {
            error!("cranelift_codegen verifier error:\n{err:#?}");
        }

        let mut fctx = codegen::Context::for_function(clfunc);
        self.objmodule.define_function(id, &mut fctx).unwrap();

        self.unwindinfo.add_function(id, &mut fctx, &*self.isa);

        id
    }

    fn declare_entrypoint(&mut self, target: Target) -> FuncId {
        let val_inits_id = self.declare_val_run_and_store();

        let mut func_builder_ctx = FunctionBuilderContext::new();
        let mut clfunc = ir::Function::new();
        let mut builder = FunctionBuilder::new(&mut clfunc, &mut func_builder_ctx);
        builder.func.signature = Signature::new(isa::CallConv::SystemV);

        let entryblock = builder.create_block();
        builder.seal_block(entryblock);
        builder.switch_to_block(entryblock);

        let lumina_main_id = self.funcmap[self.lir.main].id;
        let sys_init_id = self.funcmap[self.lir.sys_init].id;

        let [lumina_main, val_inits, sys_init] =
            [lumina_main_id, val_inits_id, sys_init_id].map(|func_id| {
                self.objmodule
                    .declare_func_in_func(func_id, &mut builder.func)
            });

        match target.platform {
            Platform::Linux { sub: LinuxPlatform::Gnu | LinuxPlatform::Musl } => {
                builder.func.signature.params = vec![
                    AbiParam::new(types::I32),              // argc
                    AbiParam::new(self.isa.pointer_type()), // **argv
                ];
                builder.func.signature.returns = vec![AbiParam::new(types::I32)]; // exit code
                builder.append_block_params_for_function_params(entryblock);
                let id = self
                    .objmodule
                    .declare_function("main", Linkage::Export, &builder.func.signature)
                    .unwrap();

                // Call the val initialiser function
                builder.ins().call(val_inits, &[]);

                // Call the system-specific initialiser
                let [argc, argv] = builder.block_params(entryblock).try_into().unwrap();
                builder.ins().call(sys_init, &[argc, argv]);

                // Call the lumina main function
                builder.ins().call(lumina_main, &[]);

                let exit_code = builder.ins().iconst(types::I32, 0);
                builder.ins().return_(&[exit_code]);

                info!("main:\n{}", builder.func);

                if let Err(err) = cranelift_codegen::verify_function(&clfunc, self.isa.as_ref()) {
                    error!("cranelift_codegen verifier error:\n{err:#?}");
                }

                let mut fctx = codegen::Context::for_function(clfunc);
                self.objmodule.define_function(id, &mut fctx).unwrap();

                id
            }
            Platform::Linux { sub: LinuxPlatform::Syscall } => {
                let id = self
                    .objmodule
                    .declare_function("_start", Linkage::Export, &builder.func.signature)
                    .unwrap();

                // Call the val initialiser function
                builder.ins().call(val_inits, &[]);

                // Call the lumina main function
                builder.ins().call(lumina_main, &[]);

                let syscall = {
                    let syscall_id = match self.objmodule.get_name("x86_64_syscall") {
                        Some(cranelift_module::FuncOrDataId::Func(fid)) => fid,
                        _ => panic!("x86_64_syscall symbol not defined"),
                    };

                    self.objmodule
                        .declare_func_in_func(syscall_id, &mut builder.func)
                };

                // Add `syscall 0 EXIT` at the end of the start function so we don't segfault
                let zero = builder.ins().iconst(types::I64, 0);
                let exit_code = zero;
                let sys_exit = builder.ins().iconst(types::I64, 60);
                builder
                    .ins()
                    .call(syscall, &[exit_code, zero, zero, zero, zero, sys_exit]);
                builder.ins().return_(&[]);

                info!("_entry:\n{}", builder.func);

                if let Err(err) = cranelift_codegen::verify_function(&clfunc, self.isa.as_ref()) {
                    error!("cranelift_codegen verifier error:\n{err}");
                }

                let mut fctx = codegen::Context::for_function(clfunc);
                self.objmodule.define_function(id, &mut fctx).unwrap();

                id
            }
        }
    }
}

#[derive(Clone, Debug)]
struct FuncHeader {
    id: FuncId,
    typing: layout::Typing,
}
