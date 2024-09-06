#![feature(array_try_map)]

// Exports for integration tests

mod build;
pub use build::{build_project, run_built_binary};
pub mod cli;
