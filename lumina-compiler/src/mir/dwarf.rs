use crate::ast;
use crate::target::Target;
use gimli::write::{
    Address, AttributeValue, DwarfUnit, FileId, LineProgram, LineString, LineStringTable,
};
use gimli::LineEncoding;
use lumina_collections::Map;
use lumina_key as key;

pub struct DwarfDebugInfo {
    pub unit: DwarfUnit,
    pub created_files: Map<key::Module, FileId>,
}

impl DwarfDebugInfo {
    pub fn init(
        src_dir: std::path::PathBuf,
        target: Target,
        sources: &ast::Sources,
    ) -> DwarfDebugInfo {
        let encoding = gimli::Encoding {
            format: gimli::Format::Dwarf32,
            version: 5,
            address_size: target.int_size() / 8,
        };
        let mut dwarf = DwarfUnit::new(encoding);

        let comp_dir = src_dir.as_os_str().to_string_lossy();
        let main_path = src_dir.join("main.lm");
        let comp_name = main_path.as_os_str().to_string_lossy();
        let line_program = LineProgram::new(
            encoding,
            LineEncoding::default(),
            LineString::new(comp_dir.as_bytes(), encoding, &mut dwarf.line_strings),
            LineString::new(comp_name.as_bytes(), encoding, &mut dwarf.line_strings),
            None,
        );

        dwarf.unit.line_program = line_program;

        let _root = {
            let name = dwarf.strings.add(format!("Lumina"));
            let comp_dir = dwarf.strings.add(comp_dir.as_bytes());

            let producer = format!("lumina with cranelift-{}", cranelift_codegen::VERSION);

            let root = dwarf.unit.root();
            let root = dwarf.unit.get_mut(root);
            root.set(
                gimli::DW_AT_producer,
                AttributeValue::StringRef(dwarf.strings.add(producer)),
            );
            root.set(
                gimli::DW_AT_language,
                AttributeValue::Language(gimli::DW_LANG_Rust),
            );
            root.set(gimli::DW_AT_name, AttributeValue::StringRef(name));

            root.set(gimli::DW_AT_comp_dir, AttributeValue::StringRef(comp_dir));
            root.set(
                gimli::DW_AT_low_pc,
                AttributeValue::Address(Address::Constant(0)),
            );

            dwarf.unit.root()
        };

        let created_files = sources
            .modules()
            .map(|module| {
                // let src_code = sources.get(module);

                let line_program: &mut LineProgram = &mut dwarf.unit.line_program;
                let line_strings: &mut LineStringTable = &mut dwarf.line_strings;

                let dir_id = line_program.default_directory(); // TODO
                let mname = sources.name_of_module(module);
                let file_name = LineString::new(mname, line_program.encoding(), line_strings);
                line_program.add_file(file_name, dir_id, None)
            })
            .collect();

        DwarfDebugInfo { unit: dwarf, created_files }
    }
}
