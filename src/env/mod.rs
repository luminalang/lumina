use std::env;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct Environment {
    pub leafpath: PathBuf,
    pub entrypoint: PathBuf,
    // TODO: flags and stuff
}

impl Environment {
    pub fn from(entrypoint: PathBuf) -> Self {
        Self {
            entrypoint,
            leafpath: env::var("LEAFPATH")
                .map(|s| Path::new(&s).to_owned())
                .unwrap_or_else(|_| {
                    env::current_dir()
                        .unwrap_or_else(|_| panic!("Could not find leafpath"))
                        .to_owned()
                }),
        }
    }

    pub fn discover<'a>() -> Result<Self, &'a str> {
        let args = env::args();
        if args.len() < 2 {
            Err("you need to provide a filename")
        } else {
            let mut path = Path::new(&env::args().last().unwrap()).to_owned();
            if path.is_relative() {
                let current_dir = env::current_dir().expect("Could not get active directory");
                path = current_dir.join(path);
            }
            if !path.exists() {
                return Err("file does not exist");
            }
            Ok(Environment::from(path))
        }
    }
}
