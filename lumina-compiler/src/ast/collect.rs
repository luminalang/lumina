//! Read source files from a project and parse items from those files into a flat item list.
//!
//! Methods in trait and impl declarations are also added as their own item in the list.

use super::attr::{FuncAttr, ImplAttr, ProjectAttr, SharedAttr, TypeAttr, UseAttr, ValAttr};
use super::{AST, ImplDeclaration, TypeBody, TypeDeclaration};
use super::{Item, ItemData, ItemKind};
use crate::Files;
use crate::prelude::*;
use lumina_parser as parser;
use std::ffi::OsStr;
use std::fs::DirEntry;
use std::path::Path;

/// Traverses a Lumina project directory structure and collects all declarations into a flat arena.
#[derive(new)]
pub struct Collector<'a, 's> {
    ast: &'a mut AST<'s>,
    // #[new(default)]
    // path: Vec<String>,
    #[new(default)]
    errors: Vec<(parser::Error, key::File)>,
    #[new(default)]
    files: Files,
    // Will get substituted at first call to `incldue_entry`
    // #[new(value = "key::File::from_u32(u32::MAX - 1)")]
    // file: key::File,
}

fn emit_err_file_error(entry: &DirEntry, err: std::io::Error) {
    let err = lumina_util::Error::err("general file error").with_text(format!(
        "Could not read {}: {}",
        entry.path().display(),
        err
    ));

    eprintln!("{err}");
}

impl<'a, 's> Collector<'a, 's> {
    pub fn include_project(&mut self, root_name: &str, src_dir: &Path) {
        self.include_dir(None, root_name, src_dir);
    }

    fn include_dir(
        &mut self,
        parent: Option<key::File>,
        root_name: &str,
        dir: &Path,
    ) -> Option<key::File> {
        let src_dir = std::fs::read_dir(dir).unwrap();

        let entries = src_dir.map(|entry| entry.unwrap()).collect::<Vec<_>>();

        let root_file = entries.iter().find(|entry| {
            let name = entry.file_name();
            let name = name.as_os_str();
            name == OsStr::new(root_name)
        });

        let mut root_key = None;
        match root_file {
            Some(entry) => {
                let path = entry.path();
                match self.include_entry(parent, &path) {
                    Ok(key) => root_key = key,
                    Err(err) => emit_err_file_error(&entry, err),
                }
            }
            // Implicitly create an empty `lib.lm` for folders where one does not exist
            None => {
                let path = dir.join(root_name);
                let key = self.files.add_empty(parent, path);
                root_key = Some(key);
            }
        }

        for entry in entries {
            let path = entry.path();
            if !path.ends_with(root_name) {
                if let Err(err) = self.include_entry(root_key, &path) {
                    emit_err_file_error(&entry, err);
                }
            }
        }

        root_key
    }

    fn include_entry(
        &mut self,
        parent: Option<key::File>,
        path: &Path,
    ) -> Result<Option<key::File>, std::io::Error> {
        let utf8 = path
            .file_name()
            .unwrap()
            .to_str()
            .expect("filename is not valid UTF-8");

        if utf8 == "lib" {
            panic!();
        }

        if path.extension() == Some(&OsStr::new("lm")) && path.is_file() {
            let file_key = self.files.open(parent, path)?;

            unsafe {
                let src = self.files.get_unsafe(file_key);
                self.include(file_key, &src);
            }

            return Ok(Some(file_key));
        }

        if path.is_dir() {
            let dir = self.include_dir(parent, "lib.lm", path);
            return Ok(dir);
        }

        Ok(None)
    }

    fn add_item(&mut self, file: key::File, kind: ItemKind, attr: SharedAttr<'s>) -> key::Item {
        self.ast.items.push(Item { file, attr, kind })
    }

    fn include(&mut self, file: key::File, src: &'s str) {
        let mut parser = parser::Parser::new(src);

        while let Some((_, item)) = parser.item() {
            match item {
                parser::Declaration::ModuleAttribute(_, attrs) => {
                    let mut attr = ProjectAttr::new();
                    attr.parse(attrs);
                    self.ast.attributes.push((file, attr));
                }
                parser::Declaration::Function(declaration) => {
                    self.include_func(file, declaration, |_, fkey| ItemKind::Func(fkey));
                }
                parser::Declaration::Type(declaration) => self.include_type(file, declaration),
                parser::Declaration::Impl(declaration) => self.include_impl(file, declaration),
                parser::Declaration::Use(declaration) => self.include_use(file, declaration),
                parser::Declaration::Alias(declaration) => self.include_alias(file, declaration),
                parser::Declaration::Val(declaration) => self.include_val(file, declaration),
                parser::Declaration::Failure => {
                    todo!("we seem to never construct this in the parser?")
                }
            }
        }

        for err in parser.into_errors() {
            self.errors.push((err, file));
        }
    }

    fn include_func<F>(
        &mut self,
        file: key::File,
        decl: parser::func::Declaration<'s>,
        kind: F,
    ) -> key::Func
    where
        F: FnOnce(&mut Self, key::Func) -> ItemKind,
    {
        let item_key = self.ast.items.next_key();
        let (sattr, attr) = FuncAttr::parse(&decl.attributes);

        let fkey = self.ast.functions.push(ItemData::new(item_key, decl, attr));

        let kind = kind(self, fkey);
        assert_eq!(item_key, self.add_item(file, kind, sattr));

        fkey
    }

    fn include_impl(&mut self, file: key::File, decl: parser::r#impl::Declaration<'s>) {
        let (sattr, attr) = ImplAttr::parse(&decl.attributes);

        let ikey = self.ast.impls.next_key();

        let idecl = ImplDeclaration {
            header: decl.header,
            associations: decl.associations,
            methods: decl
                .methods
                .into_iter()
                .map(|(method_key, func_decl)| {
                    self.include_func(file, func_decl, |this, fkey| {
                        this.ast.method_member_mapping[fkey] = Some(ikey);
                        ItemKind::ImplMethod(ikey, method_key, fkey)
                    })
                })
                .collect(),
        };

        let item_key = self.ast.items.next_key();
        let type_item = self.ast.impls.push(ItemData::new(item_key, idecl, attr));
        assert_eq!(ikey, type_item);
        assert_eq!(item_key, self.add_item(file, ItemKind::Impl(ikey), sattr));
    }

    fn include_type(&mut self, file: key::File, decl: parser::ty::Declaration<'s>) {
        let (sattr, attr) = TypeAttr::parse(&decl.attributes);

        let tkey = self.ast.types.next_key();

        let tdecl = TypeDeclaration {
            header: decl.header,
            body: match decl.body {
                parser::ty::DeclarationBody::Record(body) => TypeBody::Record(body),
                parser::ty::DeclarationBody::Sum(body) => TypeBody::Sum(body),
                parser::ty::DeclarationBody::Trait(body) => TypeBody::Trait {
                    associations: body.associations,
                    methods: body
                        .methods
                        .into_iter()
                        .map(|(method_key, func_decl)| {
                            self.include_func(file, func_decl, |_, fkey| {
                                ItemKind::TraitMethod(tkey, method_key, fkey)
                            })
                        })
                        .collect(),
                },
                parser::ty::DeclarationBody::None => {
                    TypeBody::Record(parser::ty::RecordBody { fields: Map::new() })
                }
            },
        };

        let item_key = self.ast.items.next_key();
        let type_item = ItemData::new(item_key, tdecl, attr);
        assert_eq!(tkey, self.ast.types.push(type_item));
        assert_eq!(item_key, self.add_item(file, ItemKind::Type(tkey), sattr));
    }

    fn include_use(&mut self, file: key::File, declaration: parser::r#use::Declaration<'s>) {
        let item_key = self.ast.items.next_key();
        // TODO: Add parser support for use attributes
        let uattr = UseAttr {};
        let attr = SharedAttr::default();
        let ukey = self
            .ast
            .uses
            .push(ItemData::new(item_key, declaration, uattr));
        assert_eq!(item_key, self.add_item(file, ItemKind::Use(ukey), attr));
    }

    fn include_alias(&mut self, file: key::File, decl: parser::alias::Declaration<'s>) {
        let (sattr, attr) = TypeAttr::parse(&decl.attributes);

        let tkey = self.ast.types.next_key();

        let tdecl = TypeDeclaration {
            header: parser::ty::Header {
                span: decl.name.span,
                name: *decl.name,
                type_params: decl.type_params,
            },
            body: TypeBody::Alias(decl.dst),
        };

        let item_key = self.ast.items.next_key();
        let type_item = ItemData::new(item_key, tdecl, attr);
        assert_eq!(tkey, self.ast.types.push(type_item));
        assert_eq!(item_key, self.add_item(file, ItemKind::Type(tkey), sattr));
    }

    fn include_val(&mut self, file: key::File, declaration: parser::val::Declaration<'s>) {
        let item_key = self.ast.items.next_key();
        let (sattr, attr) = ValAttr::parse(&declaration.attributes);

        let fdeclaration = parser::func::Declaration {
            header: parser::func::Header {
                when: parser::when::Constraints { generics: vec![] },
                typing: declaration.type_.map(|returns| parser::func::Typing {
                    span: returns.span,
                    ptypes: vec![],
                    returns,
                }),
                params: vec![],
                name: declaration.name.tr(declaration.span),
            },
            body: Some(parser::func::Body { expr: declaration.value, where_binds: vec![] }),
            attributes: vec![],
        };

        let fattr = FuncAttr {
            no_mangle: attr.no_mangle,
            precedence: None,
            extern_: attr.extern_,
        };

        let fkey = self
            .ast
            .functions
            .push(ItemData::new(item_key, fdeclaration, fattr));

        let vkey = self.ast.vals.push(fkey);

        assert_eq!(item_key, self.add_item(file, ItemKind::Val(vkey), sattr));
    }

    pub fn finish(self) -> (Vec<(parser::Error, key::File)>, Files) {
        (self.errors, self.files)
    }
}
