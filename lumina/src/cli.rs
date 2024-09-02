use clap::{command, Args, Parser, Subcommand};
use directories::BaseDirs;
use std::fs;
use std::path::PathBuf as FilePathBuf;

#[derive(Parser, Debug)]
#[command(
    author,
    version,
    about = "Compiler for the Lumina programming language",
    long_about = None,
    flatten_help = true
)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Create a new Lumina project
    Init(InitFlags),

    /// Build a Lumina project
    Build(BuildFlags),

    /// Build a Lumina project to temporary directory and run it
    Run(BuildFlags),
}

#[derive(Args, Debug)]
pub struct InitFlags {
    #[arg(long)]
    /// Name for the project
    ///
    /// Defaults to directory name
    pub name: Option<String>,

    #[arg(long)]
    /// Author(s) of the project
    ///
    /// Defaults to no authors
    pub authors: Vec<String>,

    #[arg(long, default_value = "1.0")]
    /// Version of the project
    ///
    /// Defaults to "1.0"
    pub version: String,

    #[arg(long, default_value = "false")]
    /// Create a new project even if the destination directory isn't empty.
    ///
    /// Overwrites existing files for new ones that are created, leaving unrecognised files as-is.
    pub force: bool,

    #[arg(long)]
    /// Create and use a custom prelude library. (not recommended)
    pub prelude: Option<FilePathBuf>,

    /// Path to project directory.
    ///
    /// Will create the directory if it does not exist.
    pub path: FilePathBuf,
}

#[derive(Args, Debug)]
pub struct BuildFlags {
    #[arg(short = 't', long)]
    /// Target operating system
    pub target: Option<String>,

    #[arg(long)]
    pub epanic: bool,

    /// Path of output binary
    #[arg(short = 'o', long)]
    pub output: Option<String>,

    /// Path to lumina project, defaults to current directory
    pub project: Option<FilePathBuf>,
}

pub struct Environment {
    pub current_directory: FilePathBuf,
    pub lumina_directory: FilePathBuf,
}

impl Environment {
    pub fn parse() -> Self {
        let dirs = BaseDirs::new().expect("Could not access home directory");

        let lumina_directory = std::env::var("LUMINAPATH")
            .map(|str| FilePathBuf::from(str))
            .unwrap_or_else(|_| {
                let mut local = dirs.data_local_dir().to_owned();
                local.push("lumina");
                fs::create_dir_all(&local).expect("could not create default LUMINAPATH directory");
                local
            });

        Environment {
            current_directory: std::env::current_dir().unwrap(),
            lumina_directory,
        }
    }
}
