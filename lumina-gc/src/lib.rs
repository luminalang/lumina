use mmtk::plan::AllocationSemantics;
use mmtk::util::address::{Address, ObjectReference};
use mmtk::util::copy::{CopySemantics, GCWorkerCopyContext};
use mmtk::util::metadata::{
    side_metadata::SideMetadataOffset, side_metadata::SideMetadataSpec, MetadataSpec,
};
use mmtk::util::opaque_pointer::VMThread;
use mmtk::util::{VMMutatorThread, VMWorkerThread};
use mmtk::vm;
use mmtk::vm::{
    edge_shape::MemorySlice, edge_shape::SimpleEdge, ActivePlan, Collection, EdgeVisitor,
    GCThreadContext, ObjectModel, ReferenceGlue, RootsWorkFactory, Scanning, VMBinding,
};
use mmtk::{memory_manager, Mutator};
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

const L_FOR_PTR_SPEC: vm::VMLocalForwardingPointerSpec =
    vm::VMLocalForwardingPointerSpec::side_first();
const L_FOR_BITS_SPEC: vm::VMLocalForwardingBitsSpec =
    vm::VMLocalForwardingBitsSpec::side_after(L_FOR_PTR_SPEC.as_spec());
const L_MARK_BIT_SPEC: vm::VMLocalMarkBitSpec =
    vm::VMLocalMarkBitSpec::side_after(L_FOR_BITS_SPEC.as_spec());
const L_LOS_MARK_NURSERY_SPEC: vm::VMLocalLOSMarkNurserySpec =
    vm::VMLocalLOSMarkNurserySpec::side_after(L_MARK_BIT_SPEC.as_spec());

// We want to use Side metadata I think for now.
//
// Makes things easier since pointers can be treated like raw pointers in various situations.

impl ObjectModel<TestingBinding> for TestingObjectModel {
    const GLOBAL_LOG_BIT_SPEC: vm::VMGlobalLogBitSpec = vm::VMGlobalLogBitSpec::side_first();
    const LOCAL_FORWARDING_POINTER_SPEC: vm::VMLocalForwardingPointerSpec = L_FOR_PTR_SPEC;
    const LOCAL_FORWARDING_BITS_SPEC: vm::VMLocalForwardingBitsSpec = L_FOR_BITS_SPEC;
    const LOCAL_MARK_BIT_SPEC: vm::VMLocalMarkBitSpec = L_MARK_BIT_SPEC;

    const LOCAL_LOS_MARK_NURSERY_SPEC: vm::VMLocalLOSMarkNurserySpec = L_LOS_MARK_NURSERY_SPEC;

    fn copy(
        from: ObjectReference,
        semantics: CopySemantics,
        copy_context: &mut GCWorkerCopyContext<TestingBinding>,
    ) -> ObjectReference {
        todo!()
    }

    fn copy_to(from: ObjectReference, to: ObjectReference, region: Address) -> Address {
        todo!()
    }

    fn get_reference_when_copied_to(from: ObjectReference, to: Address) -> ObjectReference {
        todo!()
    }

    fn get_current_size(object: ObjectReference) -> usize {
        todo!()
    }

    fn get_size_when_copied(object: ObjectReference) -> usize {
        todo!()
    }

    fn get_align_when_copied(object: ObjectReference) -> usize {
        todo!()
    }

    fn get_align_offset_when_copied(object: ObjectReference) -> usize {
        todo!()
    }

    fn get_type_descriptor(reference: ObjectReference) -> &'static [i8] {
        todo!()
    }

    const OBJECT_REF_OFFSET_LOWER_BOUND: isize = 0;

    fn ref_to_object_start(object: ObjectReference) -> mmtk::util::Address {
        todo!()
    }

    fn ref_to_header(object: ObjectReference) -> mmtk::util::Address {
        todo!()
    }

    fn ref_to_address(object: ObjectReference) -> mmtk::util::Address {
        todo!()
    }

    fn address_to_ref(addr: Address) -> ObjectReference {
        todo!()
    }

    fn dump_object(object: ObjectReference) {
        todo!()
    }
}

impl Scanning<TestingBinding> for TestingScanning {
    fn scan_object<EV: EdgeVisitor<SimpleEdge>>(
        tls: VMWorkerThread,
        object: ObjectReference,
        edge_visitor: &mut EV,
    ) {
        todo!()
    }

    fn notify_initial_thread_scan_complete(partial_scan: bool, tls: VMWorkerThread) {
        todo!()
    }

    fn scan_roots_in_mutator_thread(
        tls: VMWorkerThread,
        mutator: &'static mut Mutator<TestingBinding>,
        factory: impl RootsWorkFactory<SimpleEdge>,
    ) {
        todo!()
    }

    fn scan_vm_specific_roots(tls: VMWorkerThread, factory: impl RootsWorkFactory<SimpleEdge>) {
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
    fn stop_all_mutators<F>(tls: VMWorkerThread, mutator_visitor: F)
    where
        F: FnMut(&'static mut Mutator<TestingBinding>),
    {
        todo!()
    }

    fn resume_mutators(tls: VMWorkerThread) {
        todo!()
    }

    fn block_for_gc(tls: VMMutatorThread) {
        todo!()
    }

    fn spawn_gc_thread(tls: VMThread, ctx: GCThreadContext<TestingBinding>) {
        todo!()
    }
}

impl ActivePlan<TestingBinding> for TestingActivePlan {
    fn is_mutator(tls: VMThread) -> bool {
        todo!()
    }

    fn mutator(tls: VMMutatorThread) -> &'static mut Mutator<TestingBinding> {
        todo!()
    }

    fn mutators<'a>() -> Box<dyn Iterator<Item = &'a mut Mutator<TestingBinding>> + 'a> {
        todo!()
    }

    fn number_of_mutators() -> usize {
        todo!()
    }
}

impl ReferenceGlue<TestingBinding> for TestingReferenceGlue {
    type FinalizableType = ObjectReference;

    fn clear_referent(new_reference: ObjectReference) {
        todo!()
    }

    fn get_referent(object: ObjectReference) -> Option<mmtk::util::ObjectReference> {
        todo!()
    }

    fn set_referent(reff: ObjectReference, referent: ObjectReference) {
        todo!()
    }

    fn enqueue_references(references: &[ObjectReference], tls: VMWorkerThread) {
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

    fn start(&self) -> Address {
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
pub extern "C" fn lumina_gc_init(trigger_heap_size: usize) {
    unsafe {
        let mut builder = mmtk::MMTKBuilder::new();
        builder
            .options
            .gc_trigger
            .set(mmtk::util::options::GCTriggerSelector::FixedHeapSize(
                trigger_heap_size,
            ));
        let vm = memory_manager::mmtk_init::<TestingBinding>(&builder);
        if let Err(_) = GC.set(*vm) {
            panic!("lumina GC may only be initialised once");
        }
    }
}

static mut GC: OnceCell<mmtk::MMTK<TestingBinding>> = std::cell::OnceCell::new();

#[no_mangle]
pub extern "C" fn lumina_gc_initialize_collection(tls: VMThread) {
    unsafe { memory_manager::initialize_collection(GC.get().unwrap(), tls) }
}

#[no_mangle]
extern "C" fn mmtk_alloc(
    mutator: Mutator<TestingBinding>,
    size: usize,
    align: usize,
    offset: usize,
    semantics: AllocationSemantics,
) -> Address {
    todo!();
}
