use itertools::Itertools;
use std::fmt;

pub struct ParamFmt<'a, H, T> {
    sep: &'static str,
    clause: bool,
    header: &'a H,
    params: &'a [T],
}

impl<'a, H, T> ParamFmt<'a, H, T> {
    pub fn new(header: &'a H, params: &'a [T]) -> Self {
        Self { sep: " ", clause: true, header, params }
    }

    pub fn no_clause(mut self) -> Self {
        self.clause = false;
        self
    }
}

impl<'a, H: fmt::Display, T: fmt::Display> fmt::Display for ParamFmt<'a, H, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.params.is_empty() {
            self.header.fmt(f)
        } else {
            if self.clause {
                '('.fmt(f)?;
            }
            write!(f, "{} {}", self.header, self.params.iter().format(self.sep))?;
            if self.clause {
                ')'.fmt(f)?;
            }
            Ok(())
        }
    }
}

impl<'a, H: fmt::Display, T: fmt::Debug> fmt::Debug for ParamFmt<'a, H, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.params.is_empty() {
            self.header.fmt(f)
        } else {
            if self.clause {
                '('.fmt(f)?;
            }
            write!(
                f,
                "{} {:?}",
                self.header,
                self.params.iter().format(self.sep)
            )?;
            if self.clause {
                ')'.fmt(f)?;
            }
            Ok(())
        }
    }
}
