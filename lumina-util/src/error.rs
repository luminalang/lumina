use owo_colors::OwoColorize;
use std::fmt;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

#[derive(Clone)]
pub struct ErrorHandler {
    buffer: Arc<Mutex<Vec<Error>>>,
    pub panicy: bool,
}

impl ErrorHandler {
    pub fn new() -> Self {
        ErrorHandler { buffer: Arc::new(Mutex::new(vec![])), panicy: false }
    }

    pub fn panicy() -> Self {
        ErrorHandler { buffer: Arc::new(Mutex::new(vec![])), panicy: true }
    }

    pub fn call(&self, err: Error) {
        if self.panicy {
            panic!("{err}");
        } else {
            eprintln!("{err}");
        }
        self.buffer.lock().unwrap().push(err);
    }

    pub fn has_failed(&self) -> bool {
        !self.buffer.lock().unwrap().is_empty()
    }
}

#[derive(Clone, Debug)]
pub struct Error {
    pub name: &'static str,
    is_warning: bool,
    contexts: Vec<Context>,
}

impl Error {
    #[must_use]
    pub fn error(name: &'static str) -> Self {
        Error { name, contexts: vec![], is_warning: false }
    }

    #[must_use]
    pub fn warning(name: &'static str) -> Self {
        Error { name, contexts: vec![], is_warning: true }
    }

    #[must_use]
    pub fn with_line<S: Into<String>, OS: Into<String>>(
        mut self,
        file: PathBuf,
        linenr: usize,
        content: OS,
        arrow: std::ops::Range<usize>,
        mode: LineMode,
        message: S,
    ) -> Self {
        let (content, message) = (content.into(), message.into());
        match self.contexts.last_mut() {
            Some(Context::Line(line))
                if line.linenr == linenr
                    && line.file == file
                    && line.message.is_empty()
                    && message.is_empty()
                    && mode == line.mode =>
            {
                line.arrow.push(arrow);
            }
            _ => {
                let line = Line {
                    hide_file: self.hide_file_name(&file),
                    file,
                    linenr,
                    content: content.into(),
                    arrow: vec![arrow],
                    message: message.into(),
                    is_warning: self.is_warning,
                    mode,
                };
                self.contexts.push(Context::Line(line));
            }
        }

        self
    }

    fn hide_file_name(&self, file: &Path) -> bool {
        for ctx in self.contexts.iter().rev() {
            match ctx {
                Context::Line(previous) => return &previous.file == file,
                Context::Text(_) => continue,
            }
        }
        false
    }

    #[must_use]
    pub fn with_text<S: Into<String>>(mut self, message: S) -> Self {
        self.contexts.push(Context::Text(message.into()));
        self
    }

    pub fn call(self, handler: &ErrorHandler) {
        handler.call(self)
    }
}

#[derive(Clone, Debug)]
enum Context {
    Text(String),
    Line(Line),
}

#[derive(Clone, Debug)]
struct Line {
    hide_file: bool,
    file: PathBuf,
    linenr: usize,
    content: String,
    arrow: Vec<std::ops::Range<usize>>,
    message: String,

    is_warning: bool,

    mode: LineMode,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LineMode {
    Main,
    Info,
    InfoWithoutArrow,
    Comment,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_warning {
            writeln!(f, "{}: {}", "warning".yellow(), self.name)?;
        } else {
            writeln!(f, "{}: {}", "error".bright_red(), self.name.red())?;
        }

        for context in self.contexts.iter() {
            match context {
                Context::Line(line) => writeln!(f, "{}", line),
                Context::Text(text) => writeln!(f, " {}", text),
            }?;
        }

        Ok(())
    }
}

fn arrows_to_marker(linelen: usize, arrows: &[std::ops::Range<usize>]) -> String {
    let end = arrows.iter().map(|r| r.end).max().unwrap();

    let mut buf: Vec<u8> = vec![b' '; end];
    for range in arrows {
        buf[range.start] = b'^';
        buf[range.end - 1] = b'^';

        if let Some(bytes) = buf.get_mut(range.start + 1..range.end - 1) {
            for b in bytes {
                *b = b'-';
            }
        }
    }

    if buf.len() > linelen {
        buf.truncate(linelen - 1);
        buf.extend(b"...");
    }

    String::from_utf8(buf).unwrap()
}

impl fmt::Display for Line {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let marker = arrows_to_marker(self.content.len(), &self.arrow);
        if self.hide_file {
            writeln!(f)?;
        } else {
            writeln!(
                f,
                "{}  {}",
                " ".repeat(self.linenr.to_string().len()),
                self.file.display().purple()
            )?;
        }

        match self.mode {
            LineMode::Main => {
                write!(
                    f,
                    " {}\n{}{} {}",
                    self.source_line(),
                    self.arrow_spacing(1),
                    if self.is_warning {
                        marker.yellow().to_string()
                    } else {
                        marker.red().to_string()
                    },
                    self.message.red()
                )
            }
            LineMode::Info if self.message.is_empty() => {
                write!(f, " {}", self.source_line())
            }
            LineMode::Info => {
                write!(
                    f,
                    " {}\n{}{} {}",
                    self.source_line(),
                    self.arrow_spacing(1),
                    marker.bright_blue(),
                    self.message.bright_blue()
                )
            }
            LineMode::InfoWithoutArrow => {
                write!(
                    f,
                    " {} {}\n {} {}",
                    "/".bright_blue(),
                    self.message.bright_blue(),
                    '|'.blue(),
                    self.source_line()
                )
            }
            LineMode::Comment => {
                write!(
                    f,
                    " {} {} {}",
                    self.source_line(),
                    "//".dimmed(),
                    self.message.dimmed()
                )
            }
        }
    }
}

impl Line {
    pub fn source_line(&self) -> String {
        format!("{}{} {}", self.linenr.yellow(), ':'.purple(), &self.content)
    }

    pub fn arrow_spacing(&self, offset: usize) -> String {
        str::repeat(" ", self.linenr.to_string().len() + 2 + offset)
    }
}
