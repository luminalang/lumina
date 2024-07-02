use mmtk::memory_manager;
use mmtk::plan::AllocationSemantics;
use mmtk::util::address::{Address, ObjectReference};
use mmtk::util::metadata::{
    side_metadata::SideMetadataOffset, side_metadata::SideMetadataSpec, MetadataSpec,
};
use mmtk::util::opaque_pointer::VMThread;
use mmtk::vm::{
    edge_shape::MemorySlice, edge_shape::SimpleEdge, ActivePlan, Collection, ObjectModel,
    ReferenceGlue, Scanning, VMBinding,
};
use std::cell::OnceCell;

#[derive(Default)]
struct TestingBinding;

impl VMBinding for TestingBinding {
    type VMObjectModel = TestingObjectModel;
    type VMScanning = TestingScanning;
    type VMCollection = TestingCollection;
    type VMActivePlan = TestingActivePlan;
    type VMReferenceGlue = TestingReferenceGlue;
    type VMEdge = SimpleEdge;
    type VMMemorySlice = TestingMemorySlice;
}

struct TestingObjectModel;
struct TestingScanning;
struct TestingCollection;
struct TestingActivePlan;
struct TestingReferenceGlue;
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
struct TestingMemorySlice;

// TODO: ???
pub static mut LOCAL_SIDE_METADATA: u8 = 0;

const L_FOR_PTR_SPEC: mmtk::vm::VMLocalForwardingPointerSpec =
    mmtk::vm::VMLocalForwardingPointerSpec::side_first();
const L_FOR_BITS_SPEC: mmtk::vm::VMLocalForwardingBitsSpec =
    mmtk::vm::VMLocalForwardingBitsSpec::side_after(L_FOR_PTR_SPEC.as_spec());
const L_MARK_BIT_SPEC: mmtk::vm::VMLocalMarkBitSpec =
    mmtk::vm::VMLocalMarkBitSpec::side_after(L_FOR_BITS_SPEC.as_spec());
const L_LOS_MARK_NURSERY_SPEC: mmtk::vm::VMLocalLOSMarkNurserySpec =
    mmtk::vm::VMLocalLOSMarkNurserySpec::side_after(L_MARK_BIT_SPEC.as_spec());

// We want to use Side metadata I think for now.
//
// Makes things easier since pointers can be treated like raw pointers in various situations.

impl ObjectModel<TestingBinding> for TestingObjectModel {
    const GLOBAL_LOG_BIT_SPEC: mmtk::vm::VMGlobalLogBitSpec =
        mmtk::vm::VMGlobalLogBitSpec::side_first();
    const LOCAL_FORWARDING_POINTER_SPEC: mmtk::vm::VMLocalForwardingPointerSpec = L_FOR_PTR_SPEC;
    const LOCAL_FORWARDING_BITS_SPEC: mmtk::vm::VMLocalForwardingBitsSpec = L_FOR_BITS_SPEC;
    const LOCAL_MARK_BIT_SPEC: mmtk::vm::VMLocalMarkBitSpec = L_MARK_BIT_SPEC;

    const LOCAL_LOS_MARK_NURSERY_SPEC: mmtk::vm::VMLocalLOSMarkNurserySpec =
        L_LOS_MARK_NURSERY_SPEC;

    fn copy(
        from: mmtk::util::ObjectReference,
        semantics: mmtk::util::copy::CopySemantics,
        copy_context: &mut mmtk::util::copy::GCWorkerCopyContext<TestingBinding>,
    ) -> mmtk::util::ObjectReference {
        todo!()
    }

    fn copy_to(
        from: mmtk::util::ObjectReference,
        to: mmtk::util::ObjectReference,
        region: mmtk::util::Address,
    ) -> mmtk::util::Address {
        todo!()
    }

    fn get_reference_when_copied_to(
        from: mmtk::util::ObjectReference,
        to: mmtk::util::Address,
    ) -> mmtk::util::ObjectReference {
        todo!()
    }

    fn get_current_size(object: mmtk::util::ObjectReference) -> usize {
        todo!()
    }

    fn get_size_when_copied(object: mmtk::util::ObjectReference) -> usize {
        todo!()
    }

    fn get_align_when_copied(object: mmtk::util::ObjectReference) -> usize {
        todo!()
    }

    fn get_align_offset_when_copied(object: mmtk::util::ObjectReference) -> usize {
        todo!()
    }

    fn get_type_descriptor(reference: mmtk::util::ObjectReference) -> &'static [i8] {
        todo!()
    }

    const OBJECT_REF_OFFSET_LOWER_BOUND: isize = 0;

    fn ref_to_object_start(object: mmtk::util::ObjectReference) -> mmtk::util::Address {
        todo!()
    }

    fn ref_to_header(object: mmtk::util::ObjectReference) -> mmtk::util::Address {
        todo!()
    }

    fn ref_to_address(object: mmtk::util::ObjectReference) -> mmtk::util::Address {
        todo!()
    }

    fn address_to_ref(addr: mmtk::util::Address) -> mmtk::util::ObjectReference {
        todo!()
    }

    fn dump_object(object: mmtk::util::ObjectReference) {
        todo!()
    }
}

impl Scanning<TestingBinding> for TestingScanning {
    fn scan_object<EV: mmtk::vm::EdgeVisitor<SimpleEdge>>(
        tls: mmtk::util::VMWorkerThread,
        object: mmtk::util::ObjectReference,
        edge_visitor: &mut EV,
    ) {
        todo!()
    }

    fn notify_initial_thread_scan_complete(partial_scan: bool, tls: mmtk::util::VMWorkerThread) {
        todo!()
    }

    fn scan_roots_in_mutator_thread(
        tls: mmtk::util::VMWorkerThread,
        mutator: &'static mut mmtk::Mutator<TestingBinding>,
        factory: impl mmtk::vm::RootsWorkFactory<SimpleEdge>,
    ) {
        todo!()
    }

    fn scan_vm_specific_roots(
        tls: mmtk::util::VMWorkerThread,
        factory: impl mmtk::vm::RootsWorkFactory<SimpleEdge>,
    ) {
        todo!()
    }

    fn supports_return_barrier() -> bool {
        todo!()
    }

    fn prepare_for_roots_re_scanning() {
        todo!()
    }
}

impl Collection<TestingBinding> for TestingCollection {
    fn stop_all_mutators<F>(tls: mmtk::util::VMWorkerThread, mutator_visitor: F)
    where
        F: FnMut(&'static mut mmtk::Mutator<TestingBinding>),
    {
        todo!()
    }

    fn resume_mutators(tls: mmtk::util::VMWorkerThread) {
        todo!()
    }

    fn block_for_gc(tls: mmtk::util::VMMutatorThread) {
        todo!()
    }

    fn spawn_gc_thread(tls: mmtk::util::VMThread, ctx: mmtk::vm::GCThreadContext<TestingBinding>) {
        todo!()
    }
}

impl ActivePlan<TestingBinding> for TestingActivePlan {
    fn is_mutator(tls: mmtk::util::VMThread) -> bool {
        todo!()
    }

    fn mutator(tls: mmtk::util::VMMutatorThread) -> &'static mut mmtk::Mutator<TestingBinding> {
        todo!()
    }

    fn mutators<'a>() -> Box<dyn Iterator<Item = &'a mut mmtk::Mutator<TestingBinding>> + 'a> {
        todo!()
    }

    fn number_of_mutators() -> usize {
        todo!()
    }
}

impl ReferenceGlue<TestingBinding> for TestingReferenceGlue {
    type FinalizableType = ObjectReference;

    fn clear_referent(new_reference: mmtk::util::ObjectReference) {
        todo!()
    }

    fn get_referent(object: mmtk::util::ObjectReference) -> Option<mmtk::util::ObjectReference> {
        todo!()
    }

    fn set_referent(reff: mmtk::util::ObjectReference, referent: mmtk::util::ObjectReference) {
        todo!()
    }

    fn enqueue_references(
        references: &[mmtk::util::ObjectReference],
        tls: mmtk::util::VMWorkerThread,
    ) {
        todo!()
    }
}

impl MemorySlice for TestingMemorySlice {
    type Edge = SimpleEdge;
    type EdgeIterator = TestingEdgeIterator;

    fn iter_edges(&self) -> Self::EdgeIterator {
        todo!()
    }

    fn object(&self) -> Option<ObjectReference> {
        todo!()
    }

    fn start(&self) -> mmtk::util::Address {
        todo!()
    }

    fn bytes(&self) -> usize {
        todo!()
    }

    fn copy(src: &Self, tgt: &Self) {
        todo!()
    }
}

struct TestingEdgeIterator {}

impl Iterator for TestingEdgeIterator {
    type Item = SimpleEdge;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

#[no_mangle]
pub extern "C" fn lumina_gc_init() {
    unsafe {
        let builder = mmtk::MMTKBuilder::new();
        let vm = mmtk::memory_manager::mmtk_init::<TestingBinding>(&builder);
        if let Err(_) = GC.set(Box::leak(vm)) {
            panic!("lumina GC may only be initialised once");
        }
    }
    todo!();
}

static mut GC: OnceCell<&'static mut mmtk::MMTK<TestingBinding>> = std::cell::OnceCell::new();

#[no_mangle]
pub extern "C" fn lumina_gc_initialize_collection(tls: VMThread) {
    unsafe { memory_manager::initialize_collection(GC.get().unwrap(), tls) }
}

#[no_mangle]
pub extern "C" fn mmtk_alloc(
    mutator: mmtk::Mutator<TestingBinding>,
    size: usize,
    align: usize,
    offset: usize,
    semantics: AllocationSemantics,
) -> Address {
    todo!();
}
