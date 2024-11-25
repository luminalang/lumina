use clap::Parser;
use std::process::ExitCode;
use tracing::info;
use tracing_subscriber::{layer::SubscriberExt, registry::Registry, EnvFilter};
use tracing_tree;

mod build;
use build::{build_project, run_built_binary};
mod cli;
mod init;

fn init_logger() {
    let filter = EnvFilter::from_default_env();

    let layer = tracing_tree::HierarchicalLayer::default()
        .with_writer(std::io::stdout)
        .with_indent_lines(true)
        .with_indent_amount(2)
        .with_verbose_entry(false)
        .with_verbose_exit(false)
        .with_targets(true);

    let subscriber = Registry::default().with(layer).with(filter);

    tracing::subscriber::set_global_default(subscriber).unwrap();
}

fn main() -> ExitCode {
    init_logger();

    info!("parsing command line arguments");
    let cli = cli::Cli::parse_from(std::env::args().take_while(|arg| arg != "--"));

    info!("initialising lumina environment");
    let env = cli::Environment::parse();

    let run_output = matches!(&cli.command, cli::Commands::Run(..));

    match cli.command {
        cli::Commands::Init(settings) => init::create_new_lumina_project(settings),
        cli::Commands::Run(settings) | cli::Commands::Build(settings) => {
            match build_project(env, run_output, settings) {
                Ok(output) if run_output => run_built_binary(&output),
                Ok(_) => ExitCode::SUCCESS,
                Err(code) => code,
            }
        }
    }
}
