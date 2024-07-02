use super::FuncHeader;
use crate::prelude::*;
use cranelift::prelude::*;
use cranelift_module::DataId;
use cranelift_object::ObjectModule;
use lir::MonoFunc;
use lir::{BitOffset, MonoType, MonoTypeKey};
use std::sync::Arc;

#[derive(new)]
pub struct Context<'a> {
    pub isa: Arc<dyn isa::TargetIsa>,
    pub val_to_globals: &'a ModMap<key::Val, DataId>,
    pub lir: &'a lir::Output,
    pub objmodule: ObjectModule,
    pub funcmap: Map<lir::MonoFunc, FuncHeader>,
    pub externmap: HashMap<M<key::Func>, FuncHeader>,
    pub rotable: ModMap<key::ReadOnly, DataId>,
}

impl<'a> Context<'a> {
    pub fn bits_of_ty(&self, ty: &MonoType) -> u32 {
        self.lir.types.size_of(ty)
    }

    pub fn bits_of_mk(&self, mk: MonoTypeKey) -> u32 {
        self.lir.types.size_of_defined(mk)
    }

    pub fn bits_of_field(&self, mk: MonoTypeKey, field: key::RecordField) -> u32 {
        let ty = self.lir.types.type_of_field(mk, field);
        self.bits_of_ty(&ty)
    }

    pub fn sum_bits<'t>(
        &self,
        to: u32,
        types: impl IntoIterator<Item = &'t MonoType>,
    ) -> BitOffset {
        BitOffset(
            types
                .into_iter()
                .take(to as usize)
                .map(|ty| self.bits_of_ty(ty) as u32)
                .sum::<u32>(),
        )
    }

    pub fn pointer_type(&self) -> Type {
        let triple = self.isa.triple();
        Type::triple_pointer_type(triple)
    }
}