use crate::prelude::*;
use std::{io::Read, marker::PhantomData, path::PathBuf};

pub struct Files {
    files: PrimaryMap<key::File, Box<str>>,
    paths: PrimaryMap<key::File, PathBuf>,
}

impl Files {
    pub fn new() -> Self {
        Self { files: PrimaryMap::new(), paths: PrimaryMap::new() }
    }

    pub fn open(&mut self, path: impl Into<PathBuf>) -> Result<key::File, FileError> {
        let path = path.into();
        let mut file = std::fs::File::open(&path)?;
        let mut buf = String::with_capacity(40);
        file.read_to_string(&mut buf)?;

        let str = buf.into_boxed_str();
        let key = self.files.push(str);
        assert_eq!(key, self.paths.push(path));

        Ok(key)
    }

    pub fn get(&self, file: key::File) -> &str {
        &self.files[file]
    }

    pub unsafe fn get_unsafe<'s>(&self, file: key::File) -> &'s str {
        &*(&*self.files[file] as *const str)
    }
}

pub type FileError = std::io::Error;
