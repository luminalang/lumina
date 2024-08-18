use itertools::Itertools;
use std::fmt;
use std::ops::{Add, AddAssign};
use std::path::{Path as FilePath, PathBuf};
use tinyvec::TinyVec;

/// An either incomplete or complete path
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier<'src> {
    segments: TinyVec<[&'src str; 4]>,
}

#[derive(Debug)]
pub enum IdentifierError {
    DoubleColon { at: u16 },
}

impl<'src> Identifier<'src> {
    pub fn identifier(s: &'src str) -> Self {
        let location = Identifier::parse(s).unwrap();
        assert_eq!(location.segments.len(), 1);
        location
    }

    pub fn from_raw(s: &'src str) -> Self {
        Identifier {
            segments: {
                let mut vec = TinyVec::new();
                vec.push(s);
                vec
            },
        }
    }

    pub fn parse(s: &'src str) -> Result<Self, IdentifierError> {
        let mut offset = 0;

        // edge-case for cons operator and similar
        if !s.chars().any(|c| c.is_alphabetic()) {
            let mut segments = TinyVec::new();
            segments.push(s);
            return Ok(Identifier { segments });
        }

        s.split(':')
            .map(|segment| {
                if segment == "" {
                    return Err(IdentifierError::DoubleColon { at: offset as u16 });
                }

                offset += segment.len();

                Ok(segment)
            })
            .collect::<Result<TinyVec<_>, _>>()
            .map(|segments| Identifier { segments })
    }

    pub fn from_segments(segments: &[&'src str]) -> Self {
        Self { segments: TinyVec::from(segments) }
    }

    /// Returns true if path only contains a single segment
    pub fn is_name(&self) -> bool {
        self.segments.len() == 1
    }

    /// Returns Some if path only contains a single segment
    pub fn as_name(&self) -> Option<&'src str> {
        if self.is_name() {
            Some(&self.segments[0])
        } else {
            None
        }
    }

    pub fn split_last(&self) -> (&[&'src str], &'src str) {
        let (last, xs) = self.segments.split_last().unwrap();
        (xs, *last)
    }

    pub fn as_slice(&self) -> &[&'src str] {
        self.segments.as_slice()
    }

    pub fn segments(&self) -> usize {
        self.segments.len()
    }

    pub fn path_byte_len(&self) -> usize {
        self.segments.iter().map(|segment| segment.len()).sum()
    }

    pub fn to_filepath(&self, origin: impl FnOnce(&str) -> PathBuf, extension: &str) -> PathBuf {
        let (first, xs) = self.as_slice().split_first().unwrap();
        let mut path = origin(first);
        for segment in xs.iter().copied() {
            path.push(segment);
        }
        path.set_extension(extension);
        path
    }

    pub fn to_directory(
        &self,
        lumina: &FilePath,
        project: &FilePath,
        current: &FilePath,
    ) -> PathBuf {
        let from_lumina = |then| {
            let mut base = lumina.to_path_buf();
            base.push(then);
            base
        };

        let (x, xs) = self.as_slice().split_first().unwrap();

        let mut base = match *x {
            "std" => from_lumina("std"),
            "ext" => from_lumina("ext"),
            "project" => project.join("src"),
            _ => current.to_path_buf(),
        };

        for segment in xs.iter().copied() {
            base.push(segment);
        }

        base
    }
}

impl<'src> AddAssign<&Self> for Identifier<'src> {
    fn add_assign(&mut self, other: &Self) {
        self.segments.extend(other.segments.iter().copied());
    }
}

impl<'src> Add<&Self> for Identifier<'src> {
    type Output = Self;

    fn add(mut self, rhs: &Self) -> Self::Output {
        self += rhs;
        self
    }
}

impl<'src> fmt::Display for Identifier<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.segments.iter().format(":").fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path as FilePath;
    use tinyvec::tiny_vec;

    #[test]
    fn identifier() {
        let mut identifier = Identifier::parse("project:utilities:span").unwrap();
        assert_eq!(
            identifier,
            Identifier { segments: tiny_vec!["project", "utilities", "span"] }
        );

        identifier += &Identifier::identifier("spanned");

        let path = identifier.to_filepath(|_| PathBuf::new(), "lm");
        assert_eq!(&path, FilePath::new("utilities/span/spanned.lm"));

        assert_eq!(identifier.to_string(), "project:utilities:span:spanned")
    }
}
