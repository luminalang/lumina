use super::{abi, FuncHeader};
use crate::prelude::*;
use cranelift::prelude::*;
use cranelift_module::DataId;
use cranelift_object::ObjectModule;
use std::sync::Arc;

#[derive(new)]
pub struct Context<'a> {
    pub isa: Arc<dyn isa::TargetIsa>,
    pub val_to_globals: &'a MMap<key::Val, DataId>,
    pub lir: &'a lir::Output,
    pub structs: abi::Structs<'a>,
    pub objmodule: ObjectModule,
    pub funcmap: Map<lir::MonoFunc, FuncHeader>,
    pub externmap: HashMap<M<key::Func>, FuncHeader>,
    pub rotable: MMap<key::ReadOnly, DataId>,
}

impl<'a> Context<'a> {
    // pub fn bits_of_ty(&self, ty: &MonoType) -> u32 {
    //     self.lir.types.size_of(ty)
    // }

    // pub fn bits_of_mk(&self, mk: MonoTypeKey) -> u32 {
    //     self.lir.types.size_of(&MonoType::from(mk))
    // }

    // pub fn bits_of_field(&self, mk: MonoTypeKey, field: key::Field) -> u32 {
    //     let ty = self.lir.types.type_of_field(mk, field);
    //     self.bits_of_ty(&ty)
    // }

    // pub fn sum_bits<'t>(
    //     &self,
    //     to: u32,
    //     types: impl IntoIterator<Item = &'t MonoType>,
    // ) -> BitOffset {
    //     BitOffset(
    //         types
    //             .into_iter()
    //             .take(to as usize)
    //             .map(|ty| self.bits_of_ty(ty) as u32)
    //             .sum::<u32>(),
    //     )
    // }

    pub fn ptr(&self) -> Type {
        let triple = self.isa.triple();
        Type::triple_pointer_type(triple)
    }
}
