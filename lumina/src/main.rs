#![feature(array_try_map)]

use clap::{command, Args, Parser, Subcommand};
use directories::BaseDirs;
use itertools::Itertools;
use lumina_compiler as compiler;
use lumina_compiler::ast;
use lumina_compiler::ast::{CollectError, ConfigError};
use lumina_compiler::backend::link_native_binary;
use lumina_compiler::Target;
use lumina_key as key;
use lumina_key::M;
use lumina_util::Span;
#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;
use std::path::PathBuf as FilePathBuf;
use std::process::Command;
use std::process::ExitCode;
use tracing::info;
use tracing_subscriber::{layer::SubscriberExt, registry::Registry, EnvFilter};
use tracing_tree;

#[derive(Parser, Debug)]
#[command(author, version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Build(BuildFlags),
    Run(BuildFlags),
}

#[derive(Args, Debug)]
pub struct BuildFlags {
    #[arg(short = 't', long)]
    /// Target operating system
    target: Option<String>,

    #[arg(long)]
    epanic: bool,

    /// Path of output binary
    #[arg(short = 'o', long)]
    output: Option<String>,

    /// Path to lumina project, defaults to current directory
    project: Option<FilePathBuf>,
}

struct Environment {
    current_folder: FilePathBuf,
    lumina_folder: FilePathBuf,
}

impl Environment {
    fn parse() -> Self {
        let dirs = BaseDirs::new().expect("Could not access home directory");

        let lumina_folder = std::env::var("LUMINAPATH")
            .map(|str| FilePathBuf::from(str))
            .unwrap_or_else(|_| {
                let mut local = dirs.data_local_dir().to_owned();
                local.push("lumina");
                std::fs::create_dir_all(&local)
                    .expect("could not create default LUMINAPATH directory");
                local
            });

        Environment {
            current_folder: std::env::current_dir().unwrap(),
            lumina_folder,
        }
    }
}

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
    let cli = Cli::parse_from(std::env::args().take_while(|arg| arg != "--"));

    info!("initialising lumina environment");
    let env = Environment::parse();

    let run = matches!(&cli.command, Commands::Run(..));

    match cli.command {
        Commands::Run(settings) | Commands::Build(settings) => {
            let mut project_path = env.current_folder.clone();
            if let Some(path) = settings.project {
                if path.is_absolute() {
                    project_path = path;
                } else {
                    project_path.push(path);
                }
            }

            let target = settings
                .target
                .map(|name| Target::try_from(name.as_str()).unwrap())
                .unwrap_or_else(Target::native);

            let ast = match compiler::ast::parse(
                project_path.clone(),
                env.lumina_folder.clone(),
                settings.epanic,
                target.clone(),
            ) {
                Err(fatal_err) => {
                    eprintln!("{}", project_error(fatal_err));
                    return ExitCode::FAILURE;
                }
                Ok(ast) => ast,
            };

            let pinfo = match project_info(&ast.lookups) {
                Err(err) => {
                    eprintln!("{err}");
                    return ExitCode::FAILURE;
                }
                Ok(pinfo) => pinfo,
            };

            let project_name = ast.config.name.clone();
            let (hir, tenvs, mut iquery) = compiler::hir::run(pinfo, target, ast);

            let (mir, has_failed) = compiler::mir::run(pinfo, target, hir, tenvs, &mut iquery);
            if has_failed {
                eprintln!("aborting compilation due to previous errors");
                return ExitCode::FAILURE;
            }

            let lir = compiler::lir::run(pinfo, target, &iquery, mir);

            let object = compiler::backend::cranelift::run(target, lir);

            let output = match settings.output.as_deref() {
                Some(name) => {
                    let mut path = std::path::PathBuf::from(name);
                    while path.is_dir() {
                        path.push(&project_name);
                    }
                    path
                }
                None if run => {
                    let mut path = std::env::temp_dir();
                    path.push(&project_name);
                    path.set_extension(target.executable_extension());
                    path
                }
                None => {
                    println!(" no output filename specified ");
                    return ExitCode::FAILURE;
                }
            };

            if let Err(code) =
                link_native_binary(target, project_name, &output, env.lumina_folder, object)
            {
                return code;
            }

            if run {
                let excess_arguments = std::env::args().skip_while(|arg| arg != "--").skip(1);

                let result = Command::new(output.clone())
                    .args(excess_arguments)
                    .spawn()
                    .expect("could not run binary")
                    .wait()
                    .unwrap();

                #[cfg(unix)]
                if let Some(signal) = result.signal() {
                    let text = match signal {
                        1 => "SIGHUP".into(),
                        2 => "SIGINT".into(),
                        3 => "SIGQUIT".into(),
                        4 => "SIGILL".into(),
                        5 => "SITTRAP".into(),
                        11 => "SIGSEGV".into(),
                        12 => "SIGSYS".into(),
                        15 => "SIGTERM".into(),
                        _ => signal.to_string(),
                    };

                    println!("{} exited with signal {text}", output.display())
                }

                u8::try_from(result.code().unwrap_or(0))
                    .map(ExitCode::from)
                    .unwrap_or(ExitCode::FAILURE)
            } else {
                ExitCode::SUCCESS
            }
        }
    }
}

fn project_info<'s>(
    lookups: &ast::Lookups<'s>,
) -> Result<compiler::ProjectInfo, lumina_util::Error> {
    fn resolve_or_error<'s, T>(
        lookups: &ast::Lookups<'s>,
        names: &[&'s str],
        f: impl FnOnce(ast::Entity<'s>) -> Option<T>,
    ) -> Result<M<T>, lumina_util::Error> {
        lookups
            .resolve_langitem(names)
            .map_err(|_| {
                lumina_util::Error::error("project error").with_text(format!(
                    "`{}` core item not found",
                    names.iter().format(":")
                ))
            })
            .and_then(|entity| {
                f(entity.key).map(|k| M(entity.module, k)).ok_or_else(|| {
                    lumina_util::Error::error("project error").with_text(format!(
                        "`{}` is the wrong kind of item",
                        names.iter().format(":")
                    ))
                })
            })
    }

    let [main, sys_init, alloc, dealloc] = [
        ["main"].as_slice(),
        &["std", "prelude", "_lumina_sys_init"],
        &["std", "prelude", "alloc"],
        &["std", "prelude", "dealloc"],
    ]
    .try_map(|path| {
        resolve_or_error(lookups, path, |k| match k {
            ast::Entity::Func(ast::NFunc::Key(func)) => Some(func),
            _ => None,
        })
    })?;

    let [closure, size, listable, stringable] = [
        ["std", "prelude", "Closure"].as_slice(),
        &["std", "prelude", "Size"],
        &["std", "prelude", "Listable"],
        &["std", "prelude", "Stringable"],
    ]
    .try_map(|path| {
        resolve_or_error(lookups, path, |k| match k {
            ast::Entity::Type(key::TypeKind::Trait(trait_)) => Some(trait_),
            _ => None,
        })
    })?;

    let reflect_type = resolve_or_error(lookups, &["std", "prelude", "Type"], |k| match k {
        ast::Entity::Type(key::TypeKind::Sum(key)) => Some(key),
        _ => None,
    })?;

    let maybe = resolve_or_error(lookups, &["std", "prelude", "Maybe"], |k| match k {
        ast::Entity::Type(key::TypeKind::Sum(key)) => Some(key),
        _ => None,
    })?;

    let list_default = resolve_or_error(lookups, &["std", "prelude", "List"], |k| match k {
        ast::Entity::Type(kind) => Some(kind),
        _ => None,
    })?;

    let string = resolve_or_error(lookups, &["std", "prelude", "string"], |k| match k {
        ast::Entity::Type(key::TypeKind::Record(key)) => Some(key),
        _ => None,
    })?;

    Ok(compiler::ProjectInfo::new(
        main,
        sys_init,
        closure,
        size,
        (alloc, dealloc),
        reflect_type,
        listable,
        list_default,
        stringable,
        string,
        maybe,
    ))
}

fn project_error(err: compiler::ast::Error) -> lumina_util::Error {
    let error = lumina_util::Error::error("project error");

    match err {
        ast::Error::ProjectNotDir(path) => error.with_text(format!(
            "{}: not a valid lumina project directory",
            path.display()
        )),
        ast::Error::LuminaNotDir(path) => error.with_text(format!(
            "LUMINAPATH does not point to a valid directory: {}",
            path.display()
        )),
        ast::Error::Config(ioerr) => {
            error.with_text(format!("could not open project config: {ioerr}"))
        }
        ast::Error::ConfigError(src, path, conferr) => {
            let mode = lumina_util::LineMode::Main;
            let main = |span: Span, txt: String| {
                let (line, off_start, _) = span.get_line(&src);
                let linenr = span.get_line_number(&src);
                let arrow = off_start as usize..off_start as usize + span.length as usize;
                error.with_line(path, linenr, line, arrow, mode, txt)
            };

            match conferr {
                ConfigError::InvalidDeclaration(span) => {
                    main(span, "invalid config declaration".into())
                }
                ConfigError::InvalidDep(span) => main(span, "invalid dependency".into()),
                ConfigError::InvalidVal(span) => main(span, "unknown val declaration".into()),
                ConfigError::InvalidTy(span) => main(span, "invalid module type parameter".into()),
                ConfigError::Expected(span, exp) => main(span, format!("expected {exp}")),
                ConfigError::InvalidTypeInStr(span) => {
                    main(span, "invalid type in string literal".into())
                }
            }
        }
        ast::Error::SrcDir(ioerr) => error.with_text(format!("could not open src folder: {ioerr}")),
        ast::Error::Collect(cerr) => match cerr {
            CollectError::Dir(ioerr, path) => error
                .with_text(path.display().to_string())
                .with_text(ioerr.to_string()),
            CollectError::File(ioerr, path) => error
                .with_text(path.display().to_string())
                .with_text(ioerr.to_string()),
        },
    }
}
