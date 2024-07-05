// extern crate libc;
// extern crate mmtk;

use std::sync::OnceLock;

use mmtk::vm::VMBinding;
use mmtk::MMTK;

pub mod active_plan;
pub mod api;
pub mod collection;
pub mod object_model;
pub mod reference_glue;
pub mod scanning;

pub type DummyVMSlot = mmtk::vm::slot::SimpleSlot;

#[derive(Default)]
pub struct DummyVM;

// Documentation: https://docs.mmtk.io/api/mmtk/vm/trait.VMBinding.html
impl VMBinding for DummyVM {
    type VMObjectModel = object_model::VMObjectModel;
    type VMScanning = scanning::VMScanning;
    type VMCollection = collection::VMCollection;
    type VMActivePlan = active_plan::VMActivePlan;
    type VMReferenceGlue = reference_glue::VMReferenceGlue;
    type VMSlot = DummyVMSlot;
    type VMMemorySlice = mmtk::vm::slot::UnimplementedMemorySlice;

    /// Allowed maximum alignment in bytes.
    const MAX_ALIGNMENT: usize = 1 << 6;
}

pub static GC: OnceLock<Box<MMTK<DummyVM>>> = OnceLock::new();

fn mmtk() -> &'static MMTK<DummyVM> {
    GC.get().unwrap()
}

// MMTK expects the langauge runtime to spawn threads.
//
// This makes sense; since it means Lumina will always be aware of which threads exist
// (including those implicitly created by MMTK)
//
// However; it also means that hooking up the musl/glibc calls for that will be important.
