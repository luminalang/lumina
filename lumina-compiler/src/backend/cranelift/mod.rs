use crate::debuginfo::BinDebugInfo;
use crate::lir;
use crate::prelude::*;
use crate::target::{Arch, LinuxPlatform, Platform};
use crate::Target;
use cranelift::codegen::ir;
use cranelift::codegen::isa::CallConv;
use cranelift::prelude::*;
use cranelift_codegen::ir::FuncRef;
use cranelift_entity::PrimaryMap;
use cranelift_module::FuncOrDataId;
use cranelift_module::{DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use ssa::InstHelper;
use std::sync::Arc;
use tracing::info_span;

mod debuginfo;
use debuginfo::unwind;
mod layout;
mod ssa;

use layout::FuncLayout;

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
    shared_builder.enable("preserve_frame_pointers").unwrap();
    shared_builder.enable("unwind_info").unwrap();
    let shared_flags = settings::Flags::new(shared_builder);
    shared_flags.enable_alias_analysis();
    assert!(shared_flags.unwind_info());

    let isa = target.isa().finish(shared_flags).unwrap();

    let objbuilder = ObjectBuilder::new(
        isa.clone(),
        b"lumina".to_vec(),
        cranelift_module::default_libcall_names(),
    )
    .unwrap();
    let mut objmodule = ObjectModule::new(objbuilder);

    let structs = layout::Structs::new(&lir.types);

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

    let rotable = MMap::from(
        lir.read_only_table
            .iter()
            .map(|(module, table)| {
                table
                    .iter()
                    .map(|(ro, bytes)| {
                        let name = format!("{module}:{ro}");
                        let thread_local = false;
                        let id = objmodule
                            .declare_data(&name, Linkage::Export, false, thread_local)
                            .unwrap();
                        let mut data = cranelift_module::DataDescription::new();
                        data.init = cranelift_module::Init::Bytes {
                            contents: bytes.to_vec().into_boxed_slice(),
                        };
                        objmodule.define_data(id, &data).unwrap();
                        id
                    })
                    .collect()
            })
            .collect::<Map<_, _>>(),
    );

    let mut flayouts = PrimaryMap::with_capacity(lir.functions.len() + lir.extern_funcs.len());

    info!("lowering function signatures");

    let externmap = lir
        .extern_funcs
        .iter()
        .map(|(key, func)| {
            let conv = isa.default_call_conv();

            let id = match objmodule.get_name(&func.symbol) {
                Some(FuncOrDataId::Func(id)) => {
                    error!(
                        "skipping FFI declaration {} because of name collision",
                        func.symbol
                    );
                    id
                }
                Some(FuncOrDataId::Data(_)) => {
                    panic!("name collision for FFI element {}", &func.symbol)
                }
                None => {
                    let (flayout, sig) = structs.flayout(conv, &func.params, &func.returns);
                    let id = objmodule
                        .declare_function(&func.symbol, Linkage::Import, &sig)
                        .unwrap();
                    assert_eq!(id, flayouts.push(flayout));
                    id
                }
            };

            (*key, id)
        })
        .collect();

    let funcmap: Map<lir::MonoFunc, FuncId> = lir
        .functions
        .values()
        .map(|func| {
            let conv = CallConv::Tail;

            let params = func.ssa.func_param_types();
            let (flayout, sig) = structs.flayout(conv, params, &func.returns);
            let id = objmodule
                .declare_function(&func.symbol, Linkage::Hidden, &sig)
                .unwrap();
            assert_eq!(id, flayouts.push(flayout));

            id
        })
        .collect();

    let unwindinfo = unwind::UnwindContext::new(&*isa, true);

    let mut ctx = Context::new(
        isa, &vals, &lir, structs, objmodule, funcmap, externmap, flayouts, rotable, unwindinfo,
        dwarf,
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
        let id = ctx.funcmap[mfunc];

        if let Err(err) = ctx.objmodule.define_function(id, &mut cctx) {
            panic!("definition error when defining {}:\n {err}", func.symbol);
        }

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

    funcmap: Map<lir::MonoFunc, FuncId>,
    externmap: HashMap<M<key::Func>, FuncId>,

    flayouts: PrimaryMap<FuncId, FuncLayout>,
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
            let mfunc = self.lir.val_initializers[&val];
            info!(
                "lowering value initialiser {}",
                &self.lir.functions[mfunc].symbol
            );

            let funcid = self.funcmap[mfunc];
            let dataid = self.val_to_globals[val];
            let fret = self.flayouts[funcid].ret.clone();

            let mut func_imports = HashMap::new();
            let mut ins = self.inst(&mut func_imports, &mut builder);
            let ptr = ins.dataid_as_pointer(dataid);

            let call = ins.new_call(0, &fret);
            let vlayout = ins.call_direct(funcid, call);
            ins.write_vlayout_to_ptr(ptr, &vlayout);
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

    fn inst<'v, 't>(
        &'v mut self,
        func_imports: &'v mut HashMap<FuncId, FuncRef>,
        builder: &'v mut FunctionBuilder<'t>,
    ) -> InstHelper<'v, 'a, 't> {
        ssa::InstHelper::new(
            builder,
            &self.structs,
            self.size_t(),
            self.funcmap[self.lir.alloc],
            self.isa.clone(),
            &mut self.objmodule,
            func_imports,
        )
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

        let lumina_main_id = self.funcmap[self.lir.main];
        let sys_init_id = self.funcmap[self.lir.sys_init];

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
                {
                    let fret = self.flayouts[lumina_main_id].ret.clone();
                    let mut func_imports = HashMap::new();
                    let mut ins = self.inst(&mut func_imports, &mut builder);
                    let call = ins.new_call(0, &fret);
                    ins.call_direct(lumina_main_id, call);
                }

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
