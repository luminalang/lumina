use crate::lir::MonoFunc;
use crate::target::Arch;
use crate::Target;
use gimli::write::{
    Address, AttributeValue, DirectoryId, DwarfUnit, FileId, LineProgram, LineString, RangeList,
    UnitEntryId,
};
use gimli::LineEncoding;
use gimli::{Register, X86_64};
use key::M;
use lumina_collections::Map;
use lumina_key as key;
use std::collections::HashMap;
use std::path::Path;

pub mod emit;

use lumina_collections::map_key_impl;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Root(u32);
map_key_impl!(Root(u32), "root");

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Item {
    SumConstructorWrapper(M<key::Sum>, key::Variant),
    Defined(M<key::Func>),
    Method(M<key::Impl>, key::Method),
    Autogenerated(key::Module, MonoFunc),
    Lambda(Box<Item>, key::Lambda),
}

pub struct BinDebugInfo {
    pub(crate) modules: Map<key::Module, ModuleDebugInfo<Item>>,
    pub(crate) target: Target,

    pub(crate) unit_range_list: RangeList,
    pub(crate) stack_pointer_register: Register,

    pub(crate) units: Map<Root, DwarfUnit>,
}

fn path_to_debug_section(_unit: &mut DwarfUnit, path: &Path) -> LineString {
    LineString::String(path.to_string_lossy().as_bytes().to_vec())
}

fn module_dir_to_debug(unit: &mut DwarfUnit, fname: &str, path: &Path) -> (LineString, LineString) {
    let dir = path_to_debug_section(unit, path);
    let file = path_to_debug_section(unit, &path.join(fname));

    (dir, file)
}

impl BinDebugInfo {
    pub fn new(target: Target) -> Self {
        BinDebugInfo {
            modules: Map::new(),
            units: Map::new(),
            target,
            unit_range_list: RangeList(Vec::new()),
            stack_pointer_register: match target.arch {
                Arch::X86_64 => X86_64::RSP,
            },
        }
    }

    /// Add a `lib.lm` or `main.lm` module.
    /// Either as a member of a project or as the root of a project
    pub fn add_dir(
        &mut self,
        module: key::Module,
        fname: &str,
        path: &Path,
        parent: Option<key::Module>,
    ) {
        match parent {
            Some(parent) => {
                // let pdir = self.modules[parent].kind.directory(); // i guess dwarf doesn't care?
                let root = self.find_root(parent);

                let unit = &mut self.units[root];
                let (dir_name, lib_name) = module_dir_to_debug(unit, fname, &path);

                let line_program = &mut unit.unit.line_program;

                let dir = line_program.add_directory(dir_name);
                let file = line_program.add_file(lib_name, dir, None);

                let kind = ModuleKind::DirRoot { parent, dir };
                let info = ModuleDebugInfo { file, kind, functions: HashMap::new() };

                self.modules.push_as(module, info);
            }
            None => {
                let encoding = gimli::Encoding {
                    format: gimli::Format::Dwarf32,
                    version: 4,
                    address_size: self.target.int_size() / 8,
                };
                let mut unit = DwarfUnit::new(encoding);

                // TODO: this `main.lm` will create incorrect name for roots of `ext` libraries
                let (dir_name, lib_name) = module_dir_to_debug(&mut unit, "main.lm", &path);

                let mut line_program = LineProgram::new(
                    encoding,
                    LineEncoding::default(),
                    dir_name.clone(),
                    lib_name.clone(),
                    None,
                );

                let dir = line_program.add_directory(dir_name);
                let file = line_program.add_file(lib_name, dir, None);

                {
                    let root = unit.unit.root();
                    let root = unit.unit.get_mut(root);

                    root.set(
                        gimli::DW_AT_producer,
                        AttributeValue::String(
                            format!("Lumina/cranelift-{}", cranelift_codegen::VERSION).into(),
                        ),
                    );
                    root.set(
                        gimli::DW_AT_name,
                        AttributeValue::String(path.display().to_string().into()),
                    );
                    root.set(
                        gimli::DW_AT_low_pc,
                        AttributeValue::Address(Address::Constant(0)),
                    );
                    root.set(gimli::DW_AT_comp_dir, AttributeValue::String("".into()));
                    root.set(
                        gimli::DW_AT_low_pc,
                        AttributeValue::Address(Address::Constant(0)),
                    );
                }

                unit.unit.line_program = line_program;
                let root = self.units.push(unit);

                let kind = ModuleKind::ProjectRoot { root, dir };
                let info = ModuleDebugInfo { file, kind, functions: HashMap::new() };

                self.modules.push_as(module, info);
            }
        }
    }

    pub fn add_file(&mut self, module: key::Module, path: &Path, parent: key::Module) {
        let kind = ModuleKind::Member { parent };

        let root = self.find_root(parent);
        let unit = &mut self.units[root];

        let dir = self.modules[parent].kind.directory().unwrap();

        let lib_name = path_to_debug_section(unit, &path);

        let file = unit.unit.line_program.add_file(lib_name, dir, None);
        let info = ModuleDebugInfo { file, kind, functions: HashMap::new() };

        self.modules.push_as(module, info);
    }

    pub fn add_item(&mut self, item: Item, name: impl Into<Vec<u8>>) -> UnitEntryId {
        let module = item.module();
        let root = self.find_root(module);
        let unit = &mut self.units[root];

        let mut name: Vec<u8> = name.into();
        for b in name.iter_mut() {
            if !b.is_ascii() {
                *b = b'?';
            }
        }

        let parent = match &item {
            Item::Lambda(func, _) => self.modules[module].functions[&func],
            _ => unit.unit.root(),
        };

        let scope = unit.unit.add(parent, gimli::DW_TAG_namespace);

        let scope_entry = unit.unit.get_mut(scope);
        scope_entry.set(gimli::DW_AT_name, AttributeValue::String(name.into()));

        self.modules[module].functions.insert(item, scope);

        scope
    }

    pub(crate) fn find_root(&mut self, m: key::Module) -> Root {
        match self.modules[m].kind {
            ModuleKind::Member { parent } | ModuleKind::DirRoot { parent, .. } => {
                self.find_root(parent)
            }
            ModuleKind::ProjectRoot { root, .. } => root,
        }
    }
}

pub struct ModuleDebugInfo<ItemKey> {
    pub file: FileId,
    pub kind: ModuleKind,

    // Each item declaration creates one UnitEntry.
    //
    // However; each declared function may result in multiple monomorphised functions in the final
    // binary. These are declared as DW_TAG_subprogram units under the item declaration UnitEntry
    pub functions: HashMap<ItemKey, UnitEntryId>,
}

pub enum ModuleKind {
    Member {
        parent: key::Module,
    },
    DirRoot {
        dir: DirectoryId,
        parent: key::Module,
    },
    ProjectRoot {
        root: Root,
        dir: DirectoryId,
    },
}

impl ModuleKind {
    pub fn directory(&self) -> Option<DirectoryId> {
        match self {
            ModuleKind::Member { .. } => None,
            ModuleKind::DirRoot { dir, .. } | ModuleKind::ProjectRoot { dir, .. } => Some(*dir),
        }
    }
}
