use std::path::PathBuf;

#[derive(Debug)]
pub struct Environment {
    pub current_directory: PathBuf,
    pub lumina_directory: PathBuf,
    pub std_directory: PathBuf,
    pub ext_directory: PathBuf,
}
