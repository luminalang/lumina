use crate::env::ENV;
use termion::color;
use termion::color::Fg;

#[derive(Debug, Eq, PartialEq, PartialOrd)]
pub enum RunProfile {
    Release,
    // Improved error messages
    Standard,
    // Improved error messages and runtime debugging output
    Debug,
    // Used for internal Leaf development
    Development,
}

pub fn is_debug() -> bool {
    let p = unsafe { &ENV.deref().args.profile };
    *p == RunProfile::Debug || *p == RunProfile::Development
}
pub fn is_dev() -> bool {
    let p = unsafe { &ENV.deref().args.profile };
    *p == RunProfile::Development
}

pub enum Log<T: std::fmt::Debug> {
    Trace(T),
    Runtime(T),
    //Token(&'a Token),
}

pub fn log<T: std::fmt::Debug>(m: Log<T>) {
    unsafe { ENV.deref().args.profile.write_log(m) }
}

impl RunProfile {
    fn write_log<T: std::fmt::Debug>(&self, log: Log<T>) {
        if *self == RunProfile::Development {
            match log {
                Log::Trace(s) => {
                    eprintln!(" {}!!{} trace: {:?}", Fg(color::Red), Fg(color::Reset), s)
                }
                Log::Runtime(s) => eprintln!(
                    " {}<>{} runtime: {:?}",
                    Fg(color::Green),
                    Fg(color::Reset),
                    s
                ),
            }
        }
    }
}
