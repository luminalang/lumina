use super::Context;
use crate::debuginfo::emit::address_for_func;
use crate::debuginfo::{BinDebugInfo, Item};
use crate::lir::MonoFunc;
use cranelift_codegen as codegen;
use cranelift_codegen::binemit::CodeOffset;
use cranelift_codegen::MachSrcLoc;
use cranelift_module::FuncId;
use gimli::write::{AttributeValue, DwarfUnit, Expression, FileId, Range, UnitEntryId};
use lumina_key as key;

pub mod unwind;

impl<'a> Context<'a> {
    pub fn get_or_make_namespace(&mut self, name: &str, item: &Item) -> UnitEntryId {
        let module = item.module();
        let lookup = &mut self.debuginfo.modules[module].functions;

        if lookup.contains_key(item) {
            return lookup[item];
        }

        self.debuginfo.add_item(item.clone(), name)
    }

    pub fn def_function(&mut self, mfunc: MonoFunc) -> FunctionDebugContext {
        let item = &self.lir.functions[mfunc].kind;
        let module = item.module();
        let root = self.debuginfo.find_root(module);

        // TODO: in the future we'll probably end up putting generic annotations in the symbol
        // names instead of the module annotation. If we do we'll need to use a more sensible way
        // to get the function name.
        let name = {
            let symbol = &self.lir.functions[mfunc].symbol;
            let end = symbol.len()
                - symbol
                    .bytes()
                    .rev()
                    .position(|c| c == b':')
                    .map(|i| i + 2)
                    .unwrap_or(0);
            let start = end
                - symbol[..end]
                    .bytes()
                    .rev()
                    .position(|c| c == b':')
                    .map(|i| i)
                    .unwrap_or(0);
            &symbol[start..end]
        };

        let scope = self.get_or_make_namespace(name, item);

        let dwarf = &mut self.debuginfo.units[root];
        let file_id = self.debuginfo.modules[module].file;

        let entry_id = dwarf.unit.add(scope, gimli::DW_TAG_subprogram);
        let entry = dwarf.unit.get_mut(entry_id);
        //let name_id = dwarf.strings.add(name);

        // These will be replaced in FunctionDebugContext::finalize. They are
        // only defined here to ensure that the order of the attributes matches
        // rustc.
        entry.set(gimli::DW_AT_low_pc, AttributeValue::Udata(0));
        entry.set(gimli::DW_AT_high_pc, AttributeValue::Udata(0));

        let mut frame_base_expr = Expression::new();
        frame_base_expr.op_reg(self.debuginfo.stack_pointer_register);
        entry.set(
            gimli::DW_AT_frame_base,
            AttributeValue::Exprloc(frame_base_expr),
        );

        entry.set(gimli::DW_AT_name, AttributeValue::String(name.into()));

        entry.set(
            gimli::DW_AT_decl_file,
            AttributeValue::FileIndex(Some(file_id)),
        );
        const TODO_LINE_ROW: u64 = 1;
        entry.set(gimli::DW_AT_decl_line, AttributeValue::Udata(TODO_LINE_ROW));

        FunctionDebugContext {
            entry_id,
            module,
            function_source_loc: (file_id, TODO_LINE_ROW, 1),
        }
    }
}

pub struct FunctionDebugContext {
    module: key::Module,
    entry_id: UnitEntryId,
    function_source_loc: (FileId, u64, u64),
}

impl FunctionDebugContext {
    pub(crate) fn finalize(
        mut self,
        debug_context: &mut BinDebugInfo,
        func_id: FuncId,
        ctx: &codegen::Context,
    ) {
        let root = debug_context.find_root(self.module);
        let dwarf = &mut debug_context.units[root];

        let end = self.create_debug_lines(dwarf, func_id, ctx);

        debug_context
            .unit_range_list
            .0
            .push(Range::StartLength { begin: address_for_func(func_id), length: u64::from(end) });

        let func_entry = dwarf.unit.get_mut(self.entry_id);
        // Gdb requires both DW_AT_low_pc and DW_AT_high_pc. Otherwise the DW_TAG_subprogram is skipped.
        func_entry.set(
            gimli::DW_AT_low_pc,
            AttributeValue::Address(address_for_func(func_id)),
        );
        // Using Udata for DW_AT_high_pc requires at least DWARF4
        func_entry.set(gimli::DW_AT_high_pc, AttributeValue::Udata(u64::from(end)));
    }

    pub(super) fn create_debug_lines(
        &mut self,
        unit: &mut DwarfUnit,
        func_id: FuncId,
        context: &codegen::Context,
    ) -> CodeOffset {
        let create_row_for_span = |unit: &mut DwarfUnit, source_loc: (FileId, u64, u64)| {
            let (file_id, line, col) = source_loc;

            unit.unit.line_program.row().file = file_id;
            unit.unit.line_program.row().line = line;
            unit.unit.line_program.row().column = col;
            unit.unit.line_program.generate_row();
        };

        unit.unit
            .line_program
            .begin_sequence(Some(address_for_func(func_id)));

        let mut func_end = 0;

        let mcr = context.compiled_code().unwrap();
        for &MachSrcLoc { start, end, .. } in mcr.buffer.get_srclocs_sorted() {
            unit.unit.line_program.row().address_offset = u64::from(start);
            create_row_for_span(unit, self.function_source_loc);
            func_end = end;
        }

        unit.unit.line_program.end_sequence(u64::from(func_end));

        let func_end = mcr.buffer.total_size();

        assert_ne!(func_end, 0);

        let entry = unit.unit.get_mut(self.entry_id);
        entry.set(
            gimli::DW_AT_low_pc,
            AttributeValue::Address(address_for_func(func_id)),
        );
        entry.set(
            gimli::DW_AT_high_pc,
            AttributeValue::Udata(u64::from(func_end)),
        );

        func_end
    }
}
//
// type ObjectSectionId = (object::write::SectionId, object::write::SymbolId);
//
// fn add_debug_reloc(
//     product: &mut ObjectProduct,
//     section_map: &HashMap<SectionId, ObjectSectionId>,
//     from: &ObjectSectionId,
//     reloc: &DebugReloc,
// ) {
//     let (symbol, symbol_offset) = match reloc.name {
//         DebugRelocName::Section(id) => (section_map.get(&id).unwrap().1, 0),
//         DebugRelocName::Symbol(id) => {
//             let id = id.try_into().unwrap();
//             let symbol_id = if id & 1 << 31 == 0 {
//                 product.function_symbol(FuncId::from_u32(id))
//             } else {
//                 product.data_symbol(DataId::from_u32(id & !(1 << 31)))
//             };
//             product
//                 .object
//                 .symbol_section_and_offset(symbol_id)
//                 .unwrap_or((symbol_id, 0))
//         }
//     };
//     product
//         .object
//         .add_relocation(
//             from.0,
//             Relocation {
//                 offset: u64::from(reloc.offset),
//                 symbol,
//                 flags: RelocationFlags::Generic {
//                     kind: reloc.kind,
//                     encoding: RelocationEncoding::Generic,
//                     size: reloc.size * 8,
//                 },
//                 addend: i64::try_from(symbol_offset).unwrap() + reloc.addend,
//             },
//         )
//         .unwrap();
// }
//
// fn add_debug_section(
//     product: &mut ObjectProduct,
//     id: SectionId,
//     data: Vec<u8>,
// ) -> (object::write::SectionId, object::write::SymbolId) {
//     let name = if product.object.format() == object::BinaryFormat::MachO {
//         id.name().replace('.', "__") // machO expects __debug_info instead of .debug_info
//     } else {
//         id.name().to_string()
//     }
//     .into_bytes();
//
//     let segment = product.object.segment_name(StandardSegment::Debug).to_vec();
//     // FIXME use SHT_X86_64_UNWIND for .eh_frame
//     let section_id = product.object.add_section(
//         segment,
//         name,
//         if id == SectionId::DebugStr || id == SectionId::DebugLineStr {
//             SectionKind::DebugString
//         } else if id == SectionId::EhFrame {
//             SectionKind::ReadOnlyData
//         } else {
//             SectionKind::Debug
//         },
//     );
//     product
//         .object
//         .section_mut(section_id)
//         .set_data(data, if id == SectionId::EhFrame { 8 } else { 1 });
//     let symbol_id = product.object.section_symbol(section_id);
//     (section_id, symbol_id)
// }
//
// pub(super) fn address_for_func(func_id: FuncId) -> Address {
//     let symbol = func_id.as_u32();
//     assert!(symbol & 1 << 31 == 0);
//     Address::Symbol { symbol: symbol as usize, addend: 0 }
// }
//
// #[derive(Clone)]
// pub(crate) struct DebugReloc {
//     pub(crate) offset: u32,
//     pub(crate) size: u8,
//     pub(crate) name: DebugRelocName,
//     pub(crate) addend: i64,
//     pub(crate) kind: object::RelocationKind,
// }
//
// #[derive(Clone)]
// pub(crate) enum DebugRelocName {
//     Section(SectionId),
//     Symbol(usize),
// }
//
// /// A [`Writer`] that collects all necessary relocations.
// #[derive(Clone)]
// pub(super) struct WriterRelocate {
//     pub(super) relocs: Vec<DebugReloc>,
//     pub(super) writer: EndianVec<RunTimeEndian>,
// }
//
// impl WriterRelocate {
//     pub(super) fn new(endian: RunTimeEndian) -> Self {
//         WriterRelocate { relocs: Vec::new(), writer: EndianVec::new(endian) }
//     }
// }
//
// impl Writer for WriterRelocate {
//     type Endian = RunTimeEndian;
//
//     fn endian(&self) -> Self::Endian {
//         self.writer.endian()
//     }
//
//     fn len(&self) -> usize {
//         self.writer.len()
//     }
//
//     fn write(&mut self, bytes: &[u8]) -> Result<(), gimli::write::Error> {
//         self.writer.write(bytes)
//     }
//
//     fn write_at(&mut self, offset: usize, bytes: &[u8]) -> Result<(), gimli::write::Error> {
//         self.writer.write_at(offset, bytes)
//     }
//
//     fn write_address(&mut self, address: Address, size: u8) -> Result<(), gimli::write::Error> {
//         match address {
//             Address::Constant(val) => self.write_udata(val, size),
//             Address::Symbol { symbol, addend } => {
//                 let offset = self.len() as u64;
//                 self.relocs.push(DebugReloc {
//                     offset: offset as u32,
//                     size,
//                     name: DebugRelocName::Symbol(symbol),
//                     addend,
//                     kind: object::RelocationKind::Absolute,
//                 });
//                 self.write_udata(0, size)
//             }
//         }
//     }
//
//     fn write_offset(
//         &mut self,
//         val: usize,
//         section: SectionId,
//         size: u8,
//     ) -> Result<(), gimli::write::Error> {
//         let offset = self.len() as u32;
//         self.relocs.push(DebugReloc {
//             offset,
//             size,
//             name: DebugRelocName::Section(section),
//             addend: val as i64,
//             kind: object::RelocationKind::Absolute,
//         });
//         self.write_udata(0, size)
//     }
//
//     fn write_offset_at(
//         &mut self,
//         offset: usize,
//         val: usize,
//         section: SectionId,
//         size: u8,
//     ) -> Result<(), gimli::write::Error> {
//         self.relocs.push(DebugReloc {
//             offset: offset as u32,
//             size,
//             name: DebugRelocName::Section(section),
//             addend: val as i64,
//             kind: object::RelocationKind::Absolute,
//         });
//         self.write_udata_at(offset, 0, size)
//     }
//
//     fn write_eh_pointer(
//         &mut self,
//         address: Address,
//         eh_pe: gimli::DwEhPe,
//         size: u8,
//     ) -> Result<(), gimli::write::Error> {
//         match address {
//             // Address::Constant arm copied from gimli
//             Address::Constant(val) => {
//                 // Indirect doesn't matter here.
//                 let val = match eh_pe.application() {
//                     gimli::DW_EH_PE_absptr => val,
//                     gimli::DW_EH_PE_pcrel => {
//                         // FIXME better handling of sign
//                         let offset = self.len() as u64;
//                         offset.wrapping_sub(val)
//                     }
//                     _ => {
//                         return Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe));
//                     }
//                 };
//                 self.write_eh_pointer_data(val, eh_pe.format(), size)
//             }
//             Address::Symbol { symbol, addend } => match eh_pe.application() {
//                 gimli::DW_EH_PE_pcrel => {
//                     let size = match eh_pe.format() {
//                         gimli::DW_EH_PE_sdata4 => 4,
//                         gimli::DW_EH_PE_sdata8 => 8,
//                         _ => return Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe)),
//                     };
//                     self.relocs.push(DebugReloc {
//                         offset: self.len() as u32,
//                         size,
//                         name: DebugRelocName::Symbol(symbol),
//                         addend,
//                         kind: object::RelocationKind::Relative,
//                     });
//                     self.write_udata(0, size)
//                 }
//                 _ => Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe)),
//             },
//         }
//     }
// }
