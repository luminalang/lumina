use crate::key;
use lumina_util::Span;
use std::cell::RefCell;
use std::path::Path;
use std::sync::Mutex;

pub fn err(name: &'static str) -> ErrorBuilder {
    ErrorBuilder { error: lumina_util::Error::err(name) }
}

pub fn warn(name: &'static str) -> ErrorBuilder {
    ErrorBuilder { error: lumina_util::Error::warning(name) }
}

#[derive(Clone, Copy)]
struct Context {
    file: key::File,
    path: &'static Path,
    src: &'static str,
}

#[must_use]
pub struct ErrorBuilder {
    error: lumina_util::Error,
}

impl ErrorBuilder {
    pub fn emit(self) {
        if let Ok(guard) = EPANIC.lock() {
            if *guard {
                panic!("{}", self.error);
            }
        }

        eprintln!("{}", self.error);
    }

    pub fn text(mut self, text: impl Into<String>) -> Self {
        self.error = self.error.with_text(text);
        self
    }

    pub fn line(mut self, span: Span, text: impl Into<String>) -> Self {
        let ctx = CONTEXT.with(|ctx| ctx.borrow().unwrap());
        let mut file = ctx.path.to_path_buf();

        // HACK: Fix path on folder modules
        if !file.is_file() {
            file.push("lib.lm");
            if !file.is_file() {
                file.pop();
                file.push("main.lm")
            }
        }

        let (line, arrow, linenr) = ctx.get_line(span);
        self.error =
            self.error
                .with_line(file, linenr, line, arrow, lumina_util::LineMode::Main, text);
        self
    }
}

impl Context {
    fn get_line<'s>(&self, mut span: Span) -> (&'s str, std::ops::Range<usize>, usize) {
        let linenr = span.get_line_number(self.src);

        if self.src.as_bytes().get(span.indice as usize) == Some(&b'\n') {
            span.indice -= 1;
        }
        let (code, offset_from_start, _) = span.get_line(self.src);

        let arrow = offset_from_start as usize..offset_from_start as usize + span.length as usize;

        (code, arrow, linenr)
    }
}

// For your own best I would recommend pretending the rest of this module does not exist.
//
// We will be committing various sins and crims. But; it does make the rest of the compiler a bit
// cleaner to work with and allows us to componetize things a lot more without having the overhead
// of having to pass in diagnostics context everywhere.

thread_local! {
    // static CONTEXT: RefCell<Option<Rc<Context>>> = RefCell::new(None);
    static CONTEXT: RefCell<Option<Context>> = RefCell::new(None);
}

static EPANIC: Mutex<bool> = Mutex::new(false);

pub fn set_epanic(flag: bool) {
    *EPANIC.lock().unwrap() = flag;
}

pub unsafe fn switch_file(file: key::File, src: &str, path: &Path) -> Option<key::File> {
    let src = &*(src as *const str);
    let path = &*(path as *const Path);
    CONTEXT
        .with(|v| v.replace(Some(Context { src, path, file })))
        .map(|ctx| ctx.file)
}

pub fn get_str<'a>(span: Span) -> &'a str {
    let ctx = CONTEXT.with(|ctx| ctx.borrow().unwrap());

    span.get_str(ctx.src)
}
