use std::env;
use std::path::PathBuf;

pub mod cli;
use cli::Arguments;

pub enum Environment {
    Some(Env),
    None,
}

pub struct Env {
    source_folder: PathBuf,
    leaf_path: PathBuf,
    args: Arguments,
}

pub static mut ENV: Environment = Environment::None;

impl Env {
    fn initialize() -> Self {
        let mut env = Env {
            leaf_path: match env::var("LEAFPATH") {
                Ok(path) => PathBuf::from(path),
                Err(_) => PathBuf::from("."),
            },
            source_folder: PathBuf::new(),
            args: Arguments::parse(),
        };
        env.source_folder = {
            let mut path = env
                .args
                .source_file
                .rsplit('/')
                .rev()
                .collect::<Vec<&str>>();
            path.pop();
            let a = path.join("/");
            PathBuf::from(a)
        };
        env
    }
}

impl Environment {
    pub fn initialize() -> Self {
        Environment::Some(Env::initialize())
    }

    fn deref(&self) -> &Env {
        unsafe {
            if let Environment::Some(env) = &ENV {
                &env
            } else {
                panic!("Attempted to reach environment before it was initialized");
            }
        }
    }
}

pub fn leafpath() -> PathBuf {
    unsafe { ENV.deref().leaf_path.clone() }
}

pub fn modpath() -> PathBuf {
    unsafe { ENV.deref().leaf_path.clone().join("modules") }
}

pub fn stdpath() -> PathBuf {
    unsafe {
        let mut path = ENV.deref().leaf_path.clone();
        path.push("modules");
        path.push("std");
        path
    }
}

pub fn entrypoint<'a>() -> &'a str {
    unsafe { &ENV.deref().args.source_file }
}

pub fn sourcefolder() -> PathBuf {
    unsafe { ENV.deref().source_folder.clone() }
}
