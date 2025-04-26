// All functions here are extern function. There is no point for marking them as unsafe.
#![allow(clippy::not_unsafe_ptr_arg_deref)]

use crate::mmtk;
use crate::DummyVM;
use crate::SINGLETON;
use libc::c_char;
use mmtk::memory_manager;
use mmtk::scheduler::GCWorker;
use mmtk::util::opaque_pointer::*;
use mmtk::util::{Address, ObjectReference};
use mmtk::AllocationSemantics;
use mmtk::MMTKBuilder;
use mmtk::Mutator;
use std::cell::OnceCell;
use std::ffi::CStr;
use std::ffi::CString;
use std::sync::OnceLock;

// This file exposes MMTk Rust API to the native code. This is not an exhaustive list of all the APIs.
// Most commonly used APIs are listed in https://docs.mmtk.io/api/mmtk/memory_manager/index.html. The binding can expose them here.

// #[no_mangle]
// pub extern "C" fn mmtk_set_option_from_string(
//     builder: *mut MMTKBuilder,
//     name: *const c_char,
//     value: *const c_char,
// ) -> bool {
//     let builder = unsafe { &mut *builder };
//     let name_str: &CStr = unsafe { CStr::from_ptr(name) };
//     let value_str: &CStr = unsafe { CStr::from_ptr(value) };
//     builder.set_option(name_str.to_str().unwrap(), value_str.to_str().unwrap())
// }

#[no_mangle]
pub extern "C" fn mmtk_set_fixed_heap_size(builder: *mut MMTKBuilder, heap_size: usize) -> bool {
    let builder = unsafe { &mut *builder };
    builder
        .options
        .gc_trigger
        .set(mmtk::util::options::GCTriggerSelector::FixedHeapSize(
            heap_size,
        ))
}

#[no_mangle]
pub unsafe fn mmtk_init() {
    let mut builder = MMTKBuilder::new();

    let success = builder
        .options
        .plan
        .set(mmtk::util::options::PlanSelector::NoGC);

    assert!(success, "could not set gc plan");

    let success =
        builder
            .options
            .gc_trigger
            .set(mmtk::util::options::GCTriggerSelector::FixedHeapSize(
                1048576,
            ));

    assert!(success, "could not set heap size");

    // Create MMTK instance.
    {
        let mmtk = memory_manager::mmtk_init::<DummyVM>(&builder);

        // Set SINGLETON to the instance.
        SINGLETON.set(mmtk).unwrap_or_else(|_| {
            panic!("Failed to set SINGLETON");
        });
    }

    let mutator = VMMutatorThread(VMThread(OpaquePointer::from_address(Address::from_ptr(
        std::ptr::null::<()>(),
    ))));

    let mutator = memory_manager::bind_mutator(mmtk(), mutator);

    mmtk_initialize_collection(VMThread(OpaquePointer::from_address(Address::from_ptr(
        std::ptr::null::<()>(),
    ))));

    MUTATOR = Some(mutator);
}

static mut MUTATOR: Option<Box<Mutator<DummyVM>>> = None;

unsafe fn mutator<'a>() -> &'a mut Mutator<DummyVM> {
    MUTATOR.as_mut().unwrap()
}

// #[no_mangle]
// pub extern "C" fn mmtk_bind_mutator(tls: VMMutatorThread) -> *mut Mutator<DummyVM> {
//     Box::into_raw(memory_manager::bind_mutator(mmtk(), tls))
// }

#[no_mangle]
pub extern "C" fn mmtk_destroy_mutator(mutator: *mut Mutator<DummyVM>) {
    // notify mmtk-core about destroyed mutator
    memory_manager::destroy_mutator(unsafe { &mut *mutator });
    // turn the ptr back to a box, and let Rust properly reclaim it
    let _ = unsafe { Box::from_raw(mutator) };
}

#[no_mangle]
pub unsafe extern "C" fn mmtk_alloc(
    // mutator: *mut Mutator<DummyVM>,
    size: usize,
    align: usize,
    // offset: usize,
    mut semantics: AllocationSemantics,
) -> Address {
    // This just demonstrates that the binding should check against `max_non_los_default_alloc_bytes` to allocate large objects.
    // In pratice, a binding may want to lift this code to somewhere in the runtime where the allocated bytes is constant so
    // they can statically know if a normal allocation or a large object allocation is needed.
    if size
        >= mmtk()
            .get_plan()
            .constraints()
            .max_non_los_default_alloc_bytes
    {
        semantics = AllocationSemantics::Los;
    }
    let mutator = mutator();
    let addr =
        memory_manager::alloc::<DummyVM>(unsafe { &mut *mutator }, size, align, 0, semantics);
    let obj = ObjectReference::from_raw_address(addr).unwrap();
    memory_manager::post_alloc::<DummyVM>(unsafe { &mut *mutator }, obj, size, semantics);
    addr
}

#[no_mangle]
pub extern "C" fn mmtk_post_alloc(
    mutator: *mut Mutator<DummyVM>,
    refer: ObjectReference,
    bytes: usize,
    mut semantics: AllocationSemantics,
) {
    // This just demonstrates that the binding should check against `max_non_los_default_alloc_bytes` to allocate large objects.
    // In pratice, a binding may want to lift this code to somewhere in the runtime where the allocated bytes is constant so
    // they can statically know if a normal allocation or a large object allocation is needed.
    if bytes
        >= mmtk()
            .get_plan()
            .constraints()
            .max_non_los_default_alloc_bytes
    {
        semantics = AllocationSemantics::Los;
    }
    memory_manager::post_alloc::<DummyVM>(unsafe { &mut *mutator }, refer, bytes, semantics)
}

#[no_mangle]
pub extern "C" fn mmtk_start_worker(tls: VMWorkerThread, worker: *mut GCWorker<DummyVM>) {
    let worker = unsafe { Box::from_raw(worker) };
    memory_manager::start_worker::<DummyVM>(mmtk(), tls, worker)
}

#[no_mangle]
pub extern "C" fn mmtk_initialize_collection(tls: VMThread) {
    memory_manager::initialize_collection(mmtk(), tls)
}
