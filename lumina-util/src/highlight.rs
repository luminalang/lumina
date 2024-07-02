use owo_colors::{colors, styles::DimDisplay, FgColorDisplay, OwoColorize};
use std::fmt;
use std::sync::atomic::{AtomicBool, Ordering};

pub fn enable_highlighting(enabled: bool) {
    owo_colors::set_override(enabled);
    IS_ENABLED.store(enabled, Ordering::Relaxed);
}

pub static IS_ENABLED: AtomicBool = AtomicBool::new(true);

pub enum Highlighted<T, F> {
    Off(T),
    On(F),
}

fn highlight<T, F>(v: T, f: fn(T) -> F) -> Highlighted<T, F> {
    if IS_ENABLED.load(Ordering::Relaxed) {
        Highlighted::On(f(v))
    } else {
        Highlighted::Off(v)
    }
}

/// Provides helper methods for consistent color highlighting across modules
pub trait Highlighting: Sized {
    fn keyword(&self) -> Highlighted<&Self, FgColorDisplay<'_, colors::Green, Self>> {
        highlight(self, |a| a.green())
    }

    fn numeric(&self) -> Highlighted<&Self, FgColorDisplay<'_, colors::Magenta, Self>> {
        highlight(self, |a| a.magenta())
    }

    fn path(&self) -> &Self {
        self
    }

    fn type_(&self) -> Highlighted<&Self, FgColorDisplay<'_, colors::Magenta, Self>> {
        highlight(self, |a| a.magenta())
    }

    fn symbol(&self) -> Highlighted<&Self, FgColorDisplay<'_, colors::Cyan, Self>> {
        highlight(self, |a| a.cyan())
    }

    fn comment(&self) -> Highlighted<&Self, DimDisplay<'_, Self>> {
        highlight(self, |a| a.dimmed())
    }
}

impl<T: std::fmt::Display> Highlighting for T {}

impl<T: fmt::Display, F: fmt::Display> fmt::Display for Highlighted<T, F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Highlighted::On(a) => a.fmt(f),
            Highlighted::Off(a) => a.fmt(f),
        }
    }
}
