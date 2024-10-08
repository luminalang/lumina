use super::{
    attr,
    entities::{FuncBody, ImplDef, TyHeader},
    resolve::{Entity, Mod, Visibility},
    Entities, Lookups, ModuleAttr, NFunc, Sources,
};
use crate::prelude::*;
use crate::Target;
use lumina_parser as parser;
use lumina_parser::{func, r#use, ty, val, when, Error as ParseError, Parser};
use lumina_util::{Spanned, Tr};
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use tracing::info;

const DEFAULT_OPERATOR_PRECEDENCE: u32 = 1000;

pub(crate) struct Collector<'s> {
    pub entities: Entities<'s>,
    pub lookups: Lookups<'s>,
    pub sources: Sources,

    std_lib_directory: PathBuf,
    pub uses: Map<key::Module, Vec<r#use::Declaration<'s>>>,

    pub target: Target,
}

impl<'s> Collector<'s> {
    pub unsafe fn new(std_lib_directory: PathBuf, target: Target) -> Self {
        Self {
            entities: Entities::default(),
            lookups: Lookups::new(),
            sources: Sources::new(),

            std_lib_directory,

            uses: Map::new(),
            target,
        }
    }

    // HACK: this modules is already registered to exist so we
    // need to make sure the buffer lenghts are up to sync even
    // though this one won't be used.
    fn reserve_module_and_err(&mut self, module: key::Module, path: &Path, err: Error) -> Error {
        self.sources.push(module, String::new(), path.to_path_buf());
        self.uses.push_as(module, vec![]);
        err
    }

    pub fn include_dir(&mut self, module: key::Module, path: PathBuf) -> Result<(), Error> {
        let dir = std::fs::read_dir(&path).map_err(|err| {
            self.reserve_module_and_err(module, &path, Error::Dir(err, path.clone()))
        })?;
        let is_entrypoint = module == self.lookups.project;

        let root_name = is_entrypoint.then_some("main.lm").unwrap_or("lib.lm");

        {
            let root = path.join(root_name);
            info!("opening {} as {module}", trim_display(&root));
            let source = match std::fs::read_to_string(&root) {
                Err(err) => {
                    return Err(self.reserve_module_and_err(module, &path, Error::File(err, root)))
                }
                Ok(str) => str,
            };

            let src = self.sources.push(module, source, root);

            self.parse_declarations(module, src);
        }

        for entry in dir {
            let entry = entry.map_err(|err| Error::File(err, path.clone()))?;
            let file_type = entry
                .file_type()
                .map_err(|err| Error::File(err, path.clone()))?;
            let path = entry.path();

            let fname = path.file_name().unwrap();

            if file_type.is_file() && fname == OsStr::new(root_name) {
                continue;
            }

            // prelude has already been included as module key 0
            if is_entrypoint && fname == OsStr::new("prelude") {
                continue;
            }

            // skip non-lumina files
            if file_type.is_file() && path.extension() != Some(OsStr::new("lm")) {
                continue;
            }

            let child = self.lookups.new_member_module(module);
            self.entities.add_module(child);

            let name = path.file_stem().unwrap().to_string_lossy().to_string();
            self.lookups
                .declare_module_link(module, Visibility::Public, name, child);

            if file_type.is_file() {
                info!("opening {} as {child}", trim_display(&path));
                let source =
                    std::fs::read_to_string(&path).map_err(|err| Error::File(err, path.clone()))?;
                let src = self.sources.push(child, source, path);
                self.parse_declarations(child, src);
            } else {
                self.include_dir(child, path)?;
            }
        }

        Ok(())
    }

    fn parse_declarations(&mut self, module: key::Module, src: &'s str) {
        self.uses.push_as(module, Vec::new());
        let mut parser = Parser::new(src);

        while let Some((_, decl)) = parser.declaration() {
            self.include_declaration(module, decl);
        }

        for err in parser.into_errors() {
            self.emit_err(module, err);
        }
    }

    fn include_declaration(&mut self, module: key::Module, decl: parser::Declaration<'s>) {
        let decl = match decl {
            parser::Declaration::ModuleAttribute(_, attr) => {
                let mut mattrs = ModuleAttr::new();
                mattrs.parse(module, &self.sources, attr);

                for (name, item) in mattrs.lang_items.into_iter() {
                    self.entities.add_langitem(module, name, item);
                }

                return;
            }
            decl => decl,
        };

        match decl {
            parser::Declaration::ModuleAttribute(..) => unreachable!(),
            parser::Declaration::Function(func) => {
                self.include_func(module, func, true, |body| body.map(FuncBody::Func));
            }
            parser::Declaration::Type(ty) => {
                self.include_type(module, ty);
            }
            parser::Declaration::Impl(imp) => {
                self.include_impl(module, imp);
            }
            parser::Declaration::Use(import) => self.include_use(module, import),
            parser::Declaration::Val(val) => self.include_val(module, val),
            parser::Declaration::Failure => {}
        }
    }

    fn include_func(
        &mut self,
        module: key::Module,
        func: parser::func::Declaration<'s>,
        can_be_resolved: bool,
        to_body: impl FnOnce(Option<parser::func::Body<'s>>) -> Option<FuncBody<'s>>,
    ) -> Option<key::Func> {
        let name = func.header.name;

        let mut attributes = attr::FuncAttr::parse(module, &self.sources, &func.attributes);

        if attributes.precedence.is_none() && is_op(*func.header.name) {
            attributes.precedence = Some(DEFAULT_OPERATOR_PRECEDENCE);
        }

        let vis = Visibility::from_public_flag(module, attributes.shared.public);

        let poison_body = || {
            FuncBody::Func(func::Body {
                where_binds: vec![],
                expr: parser::Expr::Poison.tr(name.span),
            })
        };

        if is_targetted(&attributes.shared, &self.target) {
            let fkey = self.entities.fheaders.push(module, func.header);
            let body = match to_body(func.body) {
                None => match attributes.extern_.clone() {
                    Some(link_name) => FuncBody::Extern { link_name },
                    None => {
                        self.sources
                            .error("syntax error")
                            .m(module)
                            .eline(name.span, "function is missing its expression")
                            .emit();

                        poison_body()
                    }
                },
                Some(body) => {
                    if attributes.extern_.is_some() {
                        self.sources
                            .warning(
                                "`extern` link name is ignore as the function has a definition",
                            )
                            .m(module)
                            .eline(name.span, "")
                            .emit();
                    };
                    body
                }
            };

            self.entities.fbodies.push_as(fkey, body);
            self.entities.fattributes.push_as(fkey, attributes);
            if can_be_resolved {
                let nfunc = NFunc::Key(fkey.1);
                if let Some(existing) = self.lookups.declare(module, vis, *name, module, nfunc) {
                    self.emit_func_already_exists(module, name.span, existing);
                }
            }

            Some(fkey.1)
        } else {
            None
        }
    }

    fn emit_func_already_exists(&self, module: key::Module, span: Span, existing: Mod<NFunc>) {
        let mods = [module, existing.module];
        let spans = [span, self.name_of_nfunc(existing.module, existing.key).span];
        self.emit_already_exists("function already defined", mods, spans);
    }

    fn emit_already_exists(&self, err: &'static str, mods: [key::Module; 2], spans: [Span; 2]) {
        let mut err = self.sources.error(err).m(mods[0]).eline(spans[0], "");
        if mods[0] == mods[1] {
            err = err.iline(spans[1], "previously defined here");
        }
        err.emit();
    }

    fn name_of_nfunc(&self, module: key::Module, nfunc: NFunc) -> Tr<&'s str> {
        match nfunc {
            NFunc::Key(fkey) => self.entities.fheaders[fkey.inside(module)].name,
            NFunc::Method(tr, method) => {
                let fkey = self.entities.methods[tr.inside(module)][method];
                self.entities.fheaders[fkey.inside(module)].name
            }
            NFunc::SumVar(sum, var) => self.entities.variant_names[sum.inside(module)][var],
            NFunc::Val(val) => {
                let fkey = self.entities.vals[val.inside(module)];
                self.entities.fheaders[fkey.inside(module)].name
            }
        }
    }

    fn include_type(
        &mut self,
        module: key::Module,
        mut ty: parser::ty::Declaration<'s>,
    ) -> Option<key::TypeKind> {
        let name = ty.header.name;
        let span = ty.header.span;

        // desugar `type A` into `type A {}`
        if let ty::DeclarationBody::None = &ty.body {
            ty.body = ty::DeclarationBody::empty_record();
        }

        let attributes = attr::TypeAttr::parse(module, &self.sources, &ty.attributes);

        let visiblity = Visibility::from_public_flag(module, attributes.shared.public);

        if is_targetted(&attributes.shared, &self.target) {
            let kind = match ty.body {
                ty::DeclarationBody::Record(body) => {
                    let record = self
                        .entities
                        .records
                        .push(module, TyHeader::new(attributes, ty.header));

                    let mut fnames = Map::with_capacity(body.fields.len());
                    let mut ftypes = Map::with_capacity(body.fields.len());

                    for (_, (span, name, ty)) in body.fields.into_iter() {
                        fnames.push(name.tr(span));
                        ftypes.push(ty.tr(span));
                    }

                    self.entities.field_names.push_as(record, fnames);
                    self.entities.field_types.push_as(record, ftypes);

                    key::TypeKind::Record(record.1)
                }
                ty::DeclarationBody::Sum(body) => {
                    let sum = self
                        .entities
                        .sums
                        .push(module, TyHeader::new(attributes, ty.header));

                    let mut vnames = Map::with_capacity(body.variants.len());
                    let mut vtypes = Map::with_capacity(body.variants.len());

                    for (_, (span, name, tys)) in body.variants.into_iter() {
                        vnames.push(name.tr(span));
                        vtypes.push(tys);
                    }

                    self.entities.variant_names.push_as(sum, vnames);
                    self.entities.variant_types.push_as(sum, vtypes);

                    key::TypeKind::Sum(sum.1)
                }
                ty::DeclarationBody::Trait(body) => {
                    let trait_ = self
                        .entities
                        .traits
                        .push(module, TyHeader::new(attributes, ty.header));

                    self.entities
                        .associated_types
                        .push(module, body.associations);

                    let to_body = |body| Some(FuncBody::TraitMethod(body, trait_));

                    let methods = self.include_methods_as_functions(module, body.methods, to_body);

                    self.entities.methods.push_as(trait_, methods);

                    key::TypeKind::Trait(trait_.1)
                }
                ty::DeclarationBody::None => {
                    let record = self
                        .entities
                        .records
                        .push(module, TyHeader::new(attributes, ty.header));
                    self.entities.field_types.push_as(record, Map::new());
                    self.entities.field_names.push_as(record, Map::new());
                    key::TypeKind::Record(record.1)
                }
            };

            if let Some(existing) = self.lookups.declare(module, visiblity, name, module, kind) {
                let mods = [module, existing.module];
                let spans = [
                    span,
                    self.entities
                        .header_of_ty(M(existing.module, existing.key))
                        .header
                        .span,
                ];
                self.emit_already_exists("type already defined", mods, spans);
            }

            self.include_tydef_members(module, visiblity, kind);

            Some(kind)
        } else {
            None
        }
    }

    fn include_methods_as_functions(
        &mut self,
        module: key::Module,
        methods: Map<key::Method, parser::func::Declaration<'s>>,
        mut to_body: impl FnMut(Option<parser::func::Body<'s>>) -> Option<FuncBody<'s>>,
    ) -> Map<key::Method, key::Func> {
        methods
            .into_iter()
            .map(|(_, decl)| {
                self.include_func(module, decl, false, &mut to_body)
                    .expect("target not supported on method yet")
            })
            .collect()
    }

    fn include_impl(
        &mut self,
        module: key::Module,
        imp: parser::r#impl::Declaration<'s>,
    ) -> M<key::Impl> {
        if !imp.attributes.is_empty() {
            unimplemented!("impl block attributes");
        }

        let ikey = self.entities.impls[module].next_key().inside(module);

        let to_body = |body: Option<_>| body.map(|body| FuncBody::ImplMethod(body, ikey));

        let impdef = ImplDef { header: imp.header, associations: imp.associations };
        let methods = self.include_methods_as_functions(module, imp.methods, to_body);

        let k = self.entities.impls.push(module, impdef);
        self.entities.imethods.push_as(k, methods);
        k
    }

    fn include_use(&mut self, from: key::Module, import: r#use::Declaration<'s>) {
        if let Some((lib, _)) = self.lookups.lib_should_be_included(import.path.as_slice()) {
            info!("implicitly including standard library {lib}");
            self.parse_lib(import.path.span, from, "std", lib);
        }

        self.uses[from].push(import);
    }

    fn include_val(&mut self, module: key::Module, val: val::Declaration<'s>) {
        let (name, span) = (val.name, val.span);
        let key = self.entities.vals[module].next_key();
        let (header, body, attributes) = val_to_func(key, val);
        let fkey = self.entities.fheaders.push(module, header);
        let visibility = Visibility::from_public_flag(module, attributes.shared.public);
        self.entities.fattributes.push_as(fkey, attributes);
        self.entities.fbodies.push_as(fkey, body);
        self.entities.vals.push_as(key.inside(module), fkey);
        if let Some(existing) =
            self.lookups
                .declare(module, visibility, name, module, NFunc::Val(key))
        {
            self.emit_func_already_exists(module, span, existing);
        }
    }

    fn include_tydef_members(&mut self, module: key::Module, vis: Visibility, kind: key::TypeKind) {
        let clashes = match kind {
            key::TypeKind::Record(rkey) => {
                self.entities.field_names[rkey.inside(module)]
                    .iter()
                    .for_each(|(field, name)| {
                        let rkey = rkey.inside(module);
                        self.lookups
                            .declare_accessor(module, vis, **name, rkey, field);
                    });
                vec![]
            }
            key::TypeKind::Sum(sum) => self.entities.variant_names[sum.inside(module)]
                .iter()
                .filter_map(|(var, name)| {
                    let nfunc = NFunc::SumVar(sum, var);
                    self.lookups
                        .declare(module, vis, **name, module, nfunc)
                        .map(|existing| (name.span, existing))
                })
                .collect(),
            key::TypeKind::Trait(trait_) => self.entities.methods[trait_.inside(module)]
                .iter()
                .filter_map(|(method, fkey)| {
                    let header = &self.entities.fheaders[fkey.inside(module)];
                    let name = *header.name;
                    let nfunc = NFunc::Method(trait_, method);
                    self.lookups
                        .declare(module, vis, name, module, nfunc)
                        .map(|existing| (header.name.span, existing))
                })
                .collect(),
        };

        for (span, existing) in clashes {
            self.emit_func_already_exists(module, span, existing);
        }
    }

    pub fn parse_lib(&mut self, span: Span, from: key::Module, header: &'static str, lib: &'s str) {
        let path = self.std_lib_directory.join(lib);
        let module = self.lookups.new_lib(header, lib.to_string());
        self.entities.add_module(module);
        if let Err(err) = self.include_dir(module, path) {
            self.emit_stdlib_err(span, from, err);
        }
    }

    fn emit_err(&mut self, module: key::Module, err: ParseError) {
        let error = self.sources.error("syntax error").m(module);

        match err {
            ParseError::InvalidAttributes(_, aspan) => {
                error.eline(aspan, "this declaration can not take attributes")
            }
            ParseError::ExpectedButGot(span, exp, got) => {
                error.eline(span, format!("expected {exp} but got this {got}"))
            }
            ParseError::ExpectedTokenButGot(span, exp, got) => error.eline(
                span,
                format!("expected {exp} but got this {}", got.describe()),
            ),
            ParseError::MissingSquareForExtractor(span) => error
                .eline(span, "unexpected expression")
                .text("note: if this is an extractor, it's missing it's closure prefix `#`. "),
            ParseError::BadIndentation(span) => {
                error.eline(span, "invalid indentation level for declaration")
            }
            ParseError::BadDefault(span, _) => {
                error.eline(span, "`default` modifier not allowed for this declaration")
            }
            ParseError::BadIndentForMatch(span, conflict) => error
                .eline(span, "ambigious indentation for match branch")
                .iline(conflict.outer_kw_span, "could either belong to this")
                .iline(conflict.inner_kw_span, "or this"),
            ParseError::Unmatched(span, open) => error.eline(span, format!("unmatched {open}")),
            ParseError::InvalidTraitMember(span) => error
                .eline(span, "invalid trait member")
                .text("only functions and associated types allowed in this context"),
            ParseError::InvalidNestedMatch { previous, new } => {
                error.eline(new, "syntax error").iline(
                    previous,
                    "parenthesis are required when matching on another match expression",
                )
            }
            ParseError::ConflictingBars(conf) => error
                .eline(conf.bar, "invalid bar for match expression")
                .text("TODO: figure out what this error actually means in practice"),
            ParseError::MissingReturnType(span) => error.eline(span, "missing return type"),
            ParseError::NestedWhere { previous, kw } => error
                .eline(kw, "unexpected `where`")
                .iline(previous, "function may only have one set of where-bindings"),
            ParseError::BadHeaderForWhere(span, token) => error.eline(
                span,
                format!("where bindings can not be prefixed by {}", token.describe()),
            ),
        }
        .emit()
    }

    pub fn emit_stdlib_err(&mut self, span: Span, module: key::Module, err: Error) {
        let error = self
            .sources
            .error("could not load standard library")
            .m(module)
            .eline(span, "");

        match err {
            Error::Dir(ioerr, path) => error
                .text(format!(
                    "directory {} not accessible: {ioerr}",
                    path.display()
                ))
                .emit(),
            Error::File(ioerr, path) => error
                .text(format!("file {} not accessible: {ioerr}", path.display()))
                .emit(),
        }
    }

    /// Resolve and set up imports from `use` statements
    pub fn link_up_imports_and_exposed(&mut self) {
        let uses = std::mem::take(&mut self.uses);

        if !uses.is_empty() {
            info!(
                "processing {} imports and their exposed entities",
                uses.len()
            );
        }

        for (module, uses) in uses.into_iter() {
            for import in uses {
                self.link_up_import(module, import)
            }
        }
    }

    fn link_up_import(&mut self, module: key::Module, import: r#use::Declaration<'s>) {
        let path = import.path.as_slice();

        let vis = Visibility::from_public_flag(module, import.public);

        match self.lookups.resolve_module(module, path) {
            Ok(Mod { key: Entity::Module(dst), .. }) => {
                let name = import
                    .assign_to
                    .map(|v| v.value)
                    .unwrap_or_else(|| path.last().unwrap());

                self.lookups
                    .declare_module_link(module, vis, name.to_string(), dst);

                if !import.module_forall_annotation.assignments.is_empty() {
                    panic!("module import parameter syntax has been deprecated in favor of project-wide type parameters");
                }

                self.resolve_exposed_entities(module, dst, vis, import.exposing);
            }
            Ok(_) => panic!("ET: this is not a module, but something else"),
            Err(err) => self
                .sources
                .emit_lookup_err(import.path.span, module, "module", err),
        }
    }

    fn resolve_exposed_entities(
        &mut self,
        module: key::Module,
        dst: key::Module,
        v: Visibility,
        exposing: Vec<Tr<r#use::Exposed<'s>>>,
    ) {
        for exposed in exposing {
            let span = exposed.span;

            match self
                .lookups
                .resolve_entity_in(module, dst, exposed.name, false)
            {
                Ok(entity) => match entity.key {
                    Entity::Func(nfunc) => {
                        self.forbid_members(module, "function", &exposed);
                        if let Some(existing) =
                            self.lookups
                                .declare(module, v, exposed.name, entity.module, nfunc)
                        {
                            self.emit_func_already_exists(module, exposed.span, existing);
                        }
                    }
                    Entity::Type(ty) => {
                        if let Some(existing) =
                            self.lookups
                                .declare(module, v, exposed.name, entity.module, ty)
                        {
                            let espan = self
                                .entities
                                .header_of_ty(M(existing.module, existing.key))
                                .header
                                .span;
                            self.emit_already_exists(
                                "identifier already defined",
                                [module, existing.module],
                                [exposed.span, espan],
                            );
                        }

                        self.expose_type_members(module, v, entity.map(|_| ty), &exposed.members);
                    }
                    Entity::Module(_) => {
                        self.sources
                            .error("import error")
                            .m(module)
                            .eline(span, "item not found")
                            .text("there is however a module of that name")
                            .emit();

                        self.lookups
                            .declare(module, v, exposed.name, module, exposed.span);
                    }
                    Entity::Member(kind, name) => {
                        let tname = self
                            .entities
                            .header_of_ty(kind.inside(entity.module))
                            .header
                            .name;
                        let kind = match kind {
                            key::TypeKind::Sum(_) => "variant",
                            key::TypeKind::Record(_) => "field",
                            key::TypeKind::Trait(_) => "method",
                        };
                        self.sources
                            .error("import error")
                            .m(module)
                            .eline(span, "item not found")
                            .text(format!(
                                "to expose the {kind} named {name}, use `{tname} [{name}]` instead"
                            ))
                            .emit();
                        self.lookups
                            .declare(module, v, exposed.name, module, exposed.span);
                    }
                },

                Err(err) => {
                    self.sources.emit_lookup_err(span, module, "item", err);
                    self.lookups
                        .declare(module, v, exposed.name, module, exposed.span);
                }
            }
        }
    }

    fn forbid_members(
        &mut self,
        module: key::Module,
        kind: &str,
        exposed: &Tr<r#use::Exposed<'s>>,
    ) {
        match &exposed.members {
            r#use::Members::None => {}
            _ => self
                .sources
                .error("import error")
                .m(module)
                .eline(exposed.span, format!("{kind} do not have members"))
                .emit(),
        }
    }

    fn expose_type_members(
        &mut self,
        module: key::Module,
        vis: Visibility,
        ty: Mod<key::TypeKind>,
        members: &r#use::Members<'s>,
    ) {
        let sources = &self.sources;

        match ty.key {
            key::TypeKind::Record(rkey) => {
                let fnames = &self.entities.field_names[rkey.inside(ty.module)];
                match members {
                    r#use::Members::All(_) => fnames.iter().for_each(|(field, n)| {
                        let rkey = rkey.inside(ty.module);
                        self.lookups.declare_accessor(module, vis, **n, rkey, field)
                    }),
                    r#use::Members::Members(names) => {
                        let rkey = rkey.inside(ty.module);
                        names
                            .iter()
                            .for_each(|name| match fnames.find(|n| *n == *name) {
                                None => {
                                    Self::emit_member_not_found(sources, module, *name, "field")
                                }
                                Some(field) => self
                                    .lookups
                                    .declare_accessor(module, vis, **name, rkey, field),
                            })
                    }
                    r#use::Members::None => {}
                }
            }
            key::TypeKind::Sum(sum) => {
                let vnames = &self.entities.variant_names[sum.inside(ty.module)];
                let clashes = match members {
                    r#use::Members::All(_) => vnames
                        .iter()
                        .filter_map(|(var, name)| {
                            let nfunc = NFunc::SumVar(sum, var);
                            self.lookups
                                .declare(module, vis, name, ty.module, nfunc)
                                .map(|existing| (name.span, existing))
                        })
                        .collect(),
                    r#use::Members::Members(names) => names
                        .iter()
                        .filter_map(|name| match vnames.find(|n| *n == *name) {
                            None => {
                                Self::emit_member_not_found(sources, module, *name, "variant");
                                None
                            }
                            Some(variant) => {
                                let nfunc = NFunc::SumVar(sum, variant);
                                self.lookups
                                    .declare(module, vis, name, ty.module, nfunc)
                                    .map(|existing| (name.span, existing))
                            }
                        })
                        .collect(),
                    r#use::Members::None => vec![],
                };

                for (span, existing) in clashes {
                    self.emit_func_already_exists(module, span, existing);
                }
            }
            key::TypeKind::Trait(trait_) => {
                let methods = &self.entities.methods[trait_.inside(ty.module)];
                let clashes = match members {
                    r#use::Members::All(_) => methods
                        .iter()
                        .filter_map(|(m, fkey)| {
                            let header = &self.entities.fheaders[fkey.inside(ty.module)];
                            let nfunc = NFunc::Method(trait_, m);
                            self.lookups
                                .declare(module, vis, *header.name, ty.module, nfunc)
                                .map(|existing| (header.name.span, existing))
                        })
                        .collect(),
                    r#use::Members::Members(names) => names
                        .iter()
                        .filter_map(|name| {
                            match methods.find(|fkey| {
                                self.entities.fheaders[fkey.inside(ty.module)].name == *name
                            }) {
                                None => {
                                    Self::emit_member_not_found(sources, module, *name, "method");
                                    None
                                }
                                Some(m) => {
                                    let nfunc = NFunc::Method(trait_, m);
                                    self.lookups
                                        .declare(module, vis, **name, ty.module, nfunc)
                                        .map(|existing| (name.span, existing))
                                }
                            }
                        })
                        .collect(),
                    r#use::Members::None => vec![],
                };

                for (span, existing) in clashes {
                    self.emit_func_already_exists(module, span, existing);
                }
            }
        }
    }

    fn emit_member_not_found(
        sources: &Sources,
        module: key::Module,
        name: Tr<&'s str>,
        kind: &str,
    ) {
        sources
            .error("import error")
            .m(module)
            .eline(name.span, format!("type does not carry this {kind}"))
            .emit()
    }
}

pub enum Error {
    Dir(std::io::Error, PathBuf),
    File(std::io::Error, PathBuf),
}

fn is_op(str: &str) -> bool {
    b"[:\\!+/*&%@$?^~<>=|-]+".contains(&str.as_bytes()[0])
}

fn val_to_func<'s>(
    key: key::Val,
    val: val::Declaration<'s>,
) -> (func::Header<'s>, FuncBody<'s>, ast::FuncAttr<'s>) {
    (
        func::Header {
            name: val.name.tr(val.span),
            when: when::Constraints::default(),
            params: vec![],
            typing: val.type_.map(|returns| func::Typing {
                span: val.span,
                ptypes: vec![],
                returns,
            }),
        },
        FuncBody::Val(func::Body { expr: val.value, where_binds: vec![] }, key),
        ast::FuncAttr {
            precedence: None,
            no_mangle: false,
            shared: ast::SharedAttr { public: val.public, ..ast::SharedAttr::new() },
            extern_: None,
        },
    )
}

fn trim_display(path: &std::path::Path) -> impl std::fmt::Display + '_ {
    path.iter()
        .skip_while(|name| !["src", "ext", "std"].map(OsStr::new).contains(name))
        .map(|str| str.to_string_lossy())
        .format("/")
}

fn is_targetted(attrs: &attr::SharedAttr<'_>, target: &Target) -> bool {
    attrs.platforms.is_empty() || attrs.platforms.iter().any(|name| target.include_for(name))
}
