use super::Context;
use crate::lir::MonoFunc;
use crate::mir::DwarfDebugInfo;
use cranelift_codegen as codegen;
use cranelift_codegen::binemit::CodeOffset;
use cranelift_codegen::ir::Endianness;
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::MachSrcLoc;
use cranelift_module::{DataId, FuncId};
use cranelift_object::ObjectProduct;
use gimli::write::{
    Address, AttributeValue, DwarfUnit, EndianVec, Expression, FileId, Range, RangeList, Sections,
    UnitEntryId, Writer,
};
use gimli::{AArch64, Register, RiscV, RunTimeEndian, SectionId, X86_64};
use object::write::{Relocation, StandardSegment};
use object::{RelocationEncoding, RelocationFlags, SectionKind};
use std::collections::HashMap;

pub mod unwind;

pub(crate) struct DebugContext {
    endian: RunTimeEndian,

    pub(crate) dwarf: DwarfDebugInfo,
    unit_range_list: RangeList,
    stack_pointer_register: Register,
    namespace_map: HashMap<MonoFunc, UnitEntryId>,
}

pub(crate) struct FunctionDebugContext {
    entry_id: UnitEntryId,
    function_source_loc: (FileId, u64, u64),
}

impl DebugContext {
    pub(crate) fn new(isa: &dyn TargetIsa, dwarf: DwarfDebugInfo) -> Self {
        let endian = match isa.endianness() {
            Endianness::Little => RunTimeEndian::Little,
            Endianness::Big => RunTimeEndian::Big,
        };

        let stack_pointer_register = match isa.triple().architecture {
            target_lexicon::Architecture::Aarch64(_) => AArch64::SP,
            target_lexicon::Architecture::Riscv64(_) => RiscV::SP,
            target_lexicon::Architecture::X86_64 | target_lexicon::Architecture::X86_64h => {
                X86_64::RSP
            }
            _ => Register(u16::MAX),
        };

        DebugContext {
            endian,
            dwarf,
            unit_range_list: RangeList(Vec::new()),
            namespace_map: HashMap::new(),
            stack_pointer_register,
        }
    }

    pub(crate) fn emit(&mut self, product: &mut ObjectProduct) {
        let unit = &mut self.dwarf.unit;
        let unit_range_list_id = unit.unit.ranges.add(self.unit_range_list.clone());
        let root = unit.unit.root();
        let root = unit.unit.get_mut(root);
        root.set(
            gimli::DW_AT_ranges,
            AttributeValue::RangeListRef(unit_range_list_id),
        );

        let mut sections = Sections::new(WriterRelocate::new(self.endian));
        unit.write(&mut sections).unwrap();

        let mut section_map = HashMap::default();
        let _: gimli::Result<()> = sections.for_each_mut(|id, section| {
            if !section.writer.slice().is_empty() {
                let section_id = add_debug_section(product, id, section.writer.take());
                section_map.insert(id, section_id);
            }
            Ok(())
        });

        let _: gimli::Result<()> = sections.for_each(|id, section| {
            if let Some(section_id) = section_map.get(&id) {
                for reloc in &section.relocs {
                    add_debug_reloc(product, &section_map, section_id, reloc);
                }
            }
            Ok(())
        });
    }

    fn item_namespace(&mut self, func: MonoFunc, name: Vec<u8>) -> UnitEntryId {
        let unit = &mut self.dwarf.unit;
        let namespace_name_id = unit.strings.add(name);
        let parent_scope = unit.unit.root();

        let scope = unit.unit.add(parent_scope, gimli::DW_TAG_namespace);

        let scope_entry = unit.unit.get_mut(scope);
        scope_entry.set(
            gimli::DW_AT_name,
            AttributeValue::StringRef(namespace_name_id),
        );

        assert!(self.namespace_map.insert(func, scope).is_none());
        scope
    }
}

impl<'a> Context<'a> {
    fn get_or_make_item_namespace(&mut self, func: MonoFunc) -> UnitEntryId {
        if let Some(id) = self.debuginfo.namespace_map.get(&func) {
            return *id;
        }

        let name = self.lir.functions[func].symbol.as_bytes().to_vec();

        self.debuginfo.item_namespace(func, name)
    }

    pub(crate) fn dwarf_def_function<'tcx>(
        &mut self,
        // unit: &mut DwarfUnit,
        func: MonoFunc,
        // type_dbg: &mut TypeDebugContext<'tcx>,
        // instance: Instance<'tcx>,
        // fn_abi: &'tcx FnAbi<'tcx, Ty<'tcx>>,
        // linkage_name: &str,
        // function_span: Span,
    ) -> FunctionDebugContext {
        let module = self.lir.functions[func].module;
        let symbol = self.lir.functions[func].symbol.as_bytes().to_vec();

        let scope = self.get_or_make_item_namespace(func);
        let unit = &mut self.debuginfo.dwarf.unit;

        let entry_id = unit.unit.add(scope, gimli::DW_TAG_subprogram);
        let entry = unit.unit.get_mut(entry_id);
        // let linkage_name_id = if name != linkage_name {
        //     Some(self.dwarf.strings.add(linkage_name))
        // } else {
        //     None
        // };
        let name_id = unit.strings.add(symbol);

        let file_id = self.debuginfo.dwarf.created_files[module];

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

        let line = 0; // TODO
        entry.set(
            gimli::DW_AT_decl_file,
            AttributeValue::FileIndex(Some(file_id)),
        );
        entry.set(gimli::DW_AT_decl_line, AttributeValue::Udata(line));

        // // Gdb requires DW_AT_name. Otherwise the DW_TAG_subprogram is skipped.
        entry.set(gimli::DW_AT_name, AttributeValue::StringRef(name_id));

        entry.set(gimli::DW_AT_external, AttributeValue::FlagPresent);

        FunctionDebugContext { entry_id, function_source_loc: (file_id, 0, 0) }
    }
}

// TODO: oh fuck, there's a finalization step for functions. So; we can't just yeet it.

impl FunctionDebugContext {
    pub(crate) fn finalize(
        mut self,
        debug_context: &mut DebugContext,
        func_id: FuncId,
        ctx: &codegen::Context,
    ) {
        let end = self.create_debug_lines(&mut debug_context.dwarf.unit, func_id, ctx);

        debug_context
            .unit_range_list
            .0
            .push(Range::StartLength { begin: address_for_func(func_id), length: u64::from(end) });

        let func_entry = debug_context.dwarf.unit.unit.get_mut(self.entry_id);
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

type ObjectSectionId = (object::write::SectionId, object::write::SymbolId);

fn add_debug_reloc(
    product: &mut ObjectProduct,
    section_map: &HashMap<SectionId, ObjectSectionId>,
    from: &ObjectSectionId,
    reloc: &DebugReloc,
) {
    let (symbol, symbol_offset) = match reloc.name {
        DebugRelocName::Section(id) => (section_map.get(&id).unwrap().1, 0),
        DebugRelocName::Symbol(id) => {
            let id = id.try_into().unwrap();
            let symbol_id = if id & 1 << 31 == 0 {
                product.function_symbol(FuncId::from_u32(id))
            } else {
                product.data_symbol(DataId::from_u32(id & !(1 << 31)))
            };
            product
                .object
                .symbol_section_and_offset(symbol_id)
                .unwrap_or((symbol_id, 0))
        }
    };
    product
        .object
        .add_relocation(
            from.0,
            Relocation {
                offset: u64::from(reloc.offset),
                symbol,
                flags: RelocationFlags::Generic {
                    kind: reloc.kind,
                    encoding: RelocationEncoding::Generic,
                    size: reloc.size * 8,
                },
                addend: i64::try_from(symbol_offset).unwrap() + reloc.addend,
            },
        )
        .unwrap();
}

fn add_debug_section(
    product: &mut ObjectProduct,
    id: SectionId,
    data: Vec<u8>,
) -> (object::write::SectionId, object::write::SymbolId) {
    let name = if product.object.format() == object::BinaryFormat::MachO {
        id.name().replace('.', "__") // machO expects __debug_info instead of .debug_info
    } else {
        id.name().to_string()
    }
    .into_bytes();

    let segment = product.object.segment_name(StandardSegment::Debug).to_vec();
    // FIXME use SHT_X86_64_UNWIND for .eh_frame
    let section_id = product.object.add_section(
        segment,
        name,
        if id == SectionId::DebugStr || id == SectionId::DebugLineStr {
            SectionKind::DebugString
        } else if id == SectionId::EhFrame {
            SectionKind::ReadOnlyData
        } else {
            SectionKind::Debug
        },
    );
    product
        .object
        .section_mut(section_id)
        .set_data(data, if id == SectionId::EhFrame { 8 } else { 1 });
    let symbol_id = product.object.section_symbol(section_id);
    (section_id, symbol_id)
}

pub(super) fn address_for_func(func_id: FuncId) -> Address {
    let symbol = func_id.as_u32();
    assert!(symbol & 1 << 31 == 0);
    Address::Symbol { symbol: symbol as usize, addend: 0 }
}

#[derive(Clone)]
pub(crate) struct DebugReloc {
    pub(crate) offset: u32,
    pub(crate) size: u8,
    pub(crate) name: DebugRelocName,
    pub(crate) addend: i64,
    pub(crate) kind: object::RelocationKind,
}

#[derive(Clone)]
pub(crate) enum DebugRelocName {
    Section(SectionId),
    Symbol(usize),
}

/// A [`Writer`] that collects all necessary relocations.
#[derive(Clone)]
pub(super) struct WriterRelocate {
    pub(super) relocs: Vec<DebugReloc>,
    pub(super) writer: EndianVec<RunTimeEndian>,
}

impl WriterRelocate {
    pub(super) fn new(endian: RunTimeEndian) -> Self {
        WriterRelocate { relocs: Vec::new(), writer: EndianVec::new(endian) }
    }
}

impl Writer for WriterRelocate {
    type Endian = RunTimeEndian;

    fn endian(&self) -> Self::Endian {
        self.writer.endian()
    }

    fn len(&self) -> usize {
        self.writer.len()
    }

    fn write(&mut self, bytes: &[u8]) -> Result<(), gimli::write::Error> {
        self.writer.write(bytes)
    }

    fn write_at(&mut self, offset: usize, bytes: &[u8]) -> Result<(), gimli::write::Error> {
        self.writer.write_at(offset, bytes)
    }

    fn write_address(&mut self, address: Address, size: u8) -> Result<(), gimli::write::Error> {
        match address {
            Address::Constant(val) => self.write_udata(val, size),
            Address::Symbol { symbol, addend } => {
                let offset = self.len() as u64;
                self.relocs.push(DebugReloc {
                    offset: offset as u32,
                    size,
                    name: DebugRelocName::Symbol(symbol),
                    addend,
                    kind: object::RelocationKind::Absolute,
                });
                self.write_udata(0, size)
            }
        }
    }

    fn write_offset(
        &mut self,
        val: usize,
        section: SectionId,
        size: u8,
    ) -> Result<(), gimli::write::Error> {
        let offset = self.len() as u32;
        self.relocs.push(DebugReloc {
            offset,
            size,
            name: DebugRelocName::Section(section),
            addend: val as i64,
            kind: object::RelocationKind::Absolute,
        });
        self.write_udata(0, size)
    }

    fn write_offset_at(
        &mut self,
        offset: usize,
        val: usize,
        section: SectionId,
        size: u8,
    ) -> Result<(), gimli::write::Error> {
        self.relocs.push(DebugReloc {
            offset: offset as u32,
            size,
            name: DebugRelocName::Section(section),
            addend: val as i64,
            kind: object::RelocationKind::Absolute,
        });
        self.write_udata_at(offset, 0, size)
    }

    fn write_eh_pointer(
        &mut self,
        address: Address,
        eh_pe: gimli::DwEhPe,
        size: u8,
    ) -> Result<(), gimli::write::Error> {
        match address {
            // Address::Constant arm copied from gimli
            Address::Constant(val) => {
                // Indirect doesn't matter here.
                let val = match eh_pe.application() {
                    gimli::DW_EH_PE_absptr => val,
                    gimli::DW_EH_PE_pcrel => {
                        // FIXME better handling of sign
                        let offset = self.len() as u64;
                        offset.wrapping_sub(val)
                    }
                    _ => {
                        return Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe));
                    }
                };
                self.write_eh_pointer_data(val, eh_pe.format(), size)
            }
            Address::Symbol { symbol, addend } => match eh_pe.application() {
                gimli::DW_EH_PE_pcrel => {
                    let size = match eh_pe.format() {
                        gimli::DW_EH_PE_sdata4 => 4,
                        gimli::DW_EH_PE_sdata8 => 8,
                        _ => return Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe)),
                    };
                    self.relocs.push(DebugReloc {
                        offset: self.len() as u32,
                        size,
                        name: DebugRelocName::Symbol(symbol),
                        addend,
                        kind: object::RelocationKind::Relative,
                    });
                    self.write_udata(0, size)
                }
                _ => Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe)),
            },
        }
    }
}
