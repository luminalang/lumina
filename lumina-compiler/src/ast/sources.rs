use crate::prelude::*;
use lumina_util::LineMode;
use std::cell::Cell;
use std::path::{Path, PathBuf};

pub struct Sources {
    strings: Map<key::Module, Box<str>>,
    paths: Map<key::Module, PathBuf>,
    panicy: bool,
    has_failed: Cell<bool>,
}

impl Sources {
    /// SAFETY: `Sources` may not be dropped while any instances of `'s` exist
    pub unsafe fn new() -> Self {
        Self {
            strings: Map::new(),
            paths: Map::new(),
            panicy: true,
            has_failed: Cell::new(false),
        }
    }

    pub fn modules(&self) -> impl Iterator<Item = key::Module> + 'static {
        self.strings.keys()
    }

    pub fn set_panicy(&mut self, b: bool) {
        self.panicy = b;
    }

    pub fn has_failed(&self) -> bool {
        self.has_failed.get()
    }

    pub fn name_of_module(&self, module: key::Module) -> String {
        let name = self.paths[module].file_stem().unwrap();
        if name == std::ffi::OsStr::new("lib") {
            self.paths[module]
                .parent()
                .unwrap()
                .file_stem()
                .unwrap()
                .to_string_lossy()
                .to_string()
        } else {
            name.to_string_lossy().to_string()
        }
    }

    pub fn push<'s>(&mut self, module: key::Module, str: String, path: PathBuf) -> &'s str {
        self.strings.push_as(module, str.into_boxed_str());
        self.paths.push_as(module, path);
        self.get(module)
    }

    pub fn get_span<'s>(&self, module: key::Module, span: Span) -> &'s str {
        let src = self.get(module);
        span.get_str(src)
    }

    pub fn get_path(&self, module: key::Module) -> &Path {
        &self.paths[module]
    }

    pub fn get_line<'s>(
        &self,
        module: key::Module,
        mut span: Span,
    ) -> (&'s str, std::ops::Range<usize>, usize) {
        let src = self.get(module);

        let linenr = span.get_line_number(src);

        if src.as_bytes()[span.indice as usize] == b'\n' {
            span.indice -= 1;
        }
        let (code, offset_from_start, _) = span.get_line(src);

        let arrow = offset_from_start as usize..offset_from_start as usize + span.length as usize;

        (code, arrow, linenr)
    }

    pub fn get<'s>(&self, module: key::Module) -> &'s str {
        unsafe { &(*(self.strings[module].as_ref() as *const str)) }
    }

    pub fn warning<'a>(&'a self, name: &'static str) -> ErrorBuilder<'a> {
        ErrorBuilder {
            sources: self,
            module: None,
            error: lumina_util::Error::warning(name),
        }
    }

    pub fn error<'a>(&'a self, name: &'static str) -> ErrorBuilder<'a> {
        ErrorBuilder {
            sources: self,
            module: None,
            error: lumina_util::Error::error(name),
        }
    }
}

#[derive(Clone)]
pub struct ErrorBuilder<'a> {
    sources: &'a Sources,
    module: Option<key::Module>,
    error: lumina_util::Error,
}

impl<'a> ErrorBuilder<'a> {
    #[must_use]
    pub fn m(mut self, module: key::Module) -> Self {
        self.module = Some(module);
        self
    }

    #[must_use]
    pub fn line<S: Into<String>>(mut self, span: Span, mode: LineMode, msg: S) -> Self {
        let module = self.module.unwrap();
        let file = self.sources.paths[module].clone();
        let (line, arrow, linenr) = self.sources.get_line(module, span);

        self.error = self.error.with_line(file, linenr, line, arrow, mode, msg);
        self
    }

    #[must_use]
    pub fn eline<S: Into<String>>(self, span: Span, msg: S) -> Self {
        self.line(span, LineMode::Main, msg)
    }

    #[must_use]
    pub fn iline<S: Into<String>>(self, span: Span, msg: S) -> Self {
        self.line(span, LineMode::Info, msg)
    }

    pub fn noiline<S: Into<String>>(self, span: Span, msg: S) -> Self {
        self.line(span, LineMode::InfoWithoutArrow, msg)
    }

    #[must_use]
    pub fn text<S: Into<String>>(mut self, msg: S) -> Self {
        self.error = self.error.with_text(msg);
        self
    }

    #[track_caller]
    pub fn emit(self) {
        self.sources.has_failed.set(true);
        if self.sources.panicy {
            panic!("{}", self.error);
        } else {
            eprintln!("{}", self.error);
        }
    }
}
