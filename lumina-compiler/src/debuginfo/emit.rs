use super::BinDebugInfo;
use cranelift_module::{DataId, FuncId};
use cranelift_object::ObjectProduct;
use gimli::write::{Address, AttributeValue, EndianVec, Sections, Writer};
use gimli::{RunTimeEndian, SectionId};
use object::write::{Relocation, StandardSegment};
use object::{RelocationEncoding, RelocationFlags, SectionKind};
use std::collections::HashMap;

impl BinDebugInfo {
    pub(crate) fn emit(&mut self, product: &mut ObjectProduct) {
        for unit in self.units.values_mut() {
            let unit_range_list_id = unit.unit.ranges.add(self.unit_range_list.clone());
            let root = unit.unit.root();
            let root = unit.unit.get_mut(root);
            root.set(
                gimli::DW_AT_ranges,
                AttributeValue::RangeListRef(unit_range_list_id),
            );

            let mut sections = Sections::new(WriterRelocate::new(self.target.endian()));
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
    }
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

type ObjectSectionId = (object::write::SectionId, object::write::SymbolId);

pub fn add_debug_reloc(
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

pub fn add_debug_section(
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

pub fn address_for_func(func_id: FuncId) -> Address {
    let symbol = func_id.as_u32();
    assert!(symbol & 1 << 31 == 0);
    Address::Symbol { symbol: symbol as usize, addend: 0 }
}
