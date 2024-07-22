pub mod cranelift;

use super::{target::Arch, target::LinuxPlatform, target::Platform, Target};
use std::ffi::OsStr;
use std::fs::{File, ReadDir};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::process::ExitCode;
use tracing::info;

pub fn link_native_binary(
    target: Target,
    project_name: String,
    name: Option<&str>,
    luminapath: PathBuf,
    projectpath: PathBuf,
    object: Vec<u8>,
) -> ExitCode {
    let Some(binary_name) = name else {
        println!(" no output filename specified ");
        return ExitCode::FAILURE;
    };

    let workdir = create_workdir(&luminapath, &project_name);

    let objectfile = {
        let mut path = workdir.join(&project_name);
        path.set_extension(target.object_extension());

        let mut f = File::create(&path).unwrap();
        f.write_all(&object).unwrap();

        path
    };

    let targetdir = luminapath.join("targets");

    let bindir = targetdir.join("bin");

    let mut linker = match target.platform {
        Platform::Linux { sub } => {
            let linuxdir = targetdir.join("linux");
            let sublinuxdir = linuxdir.join(sub.to_string());

            let mut linker = if matches!(sub, LinuxPlatform::Gnu) {
                let mut linker = Command::new("gcc");
                linker.arg("-no-pie").arg("-flto");
                linker
            } else {
                Command::new(bindir.join("ld.lld"))
            };

            linker.arg("-o").arg(binary_name).arg(&objectfile);

            iter_objects(&sublinuxdir, &["o", "a"], |path| {
                linker.arg(path);
            });

            linker.arg(linuxdir.join("syscall.o"));

            linker
        }
    };

    info!("invoking system linker as: {:#?}", linker);

    let status = linker
        .spawn()
        .expect("failed to invoke linker")
        .wait()
        .unwrap();

    if status.success() {
        std::fs::remove_dir_all(workdir).unwrap();
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

fn iter_objects(path: &Path, objs: &[&str], mut f: impl FnMut(PathBuf)) {
    for file in path.read_dir().unwrap() {
        let path = file.unwrap().path();
        if let Some(ext) = path.extension() {
            if objs.iter().any(|o| OsStr::new(*o) == ext) {
                f(path);
            }
        }
    }
}

fn create_workdir(luminapath: &Path, project_name: &str) -> PathBuf {
    let mut workdir = luminapath.to_path_buf();
    workdir.push("workdirs");
    workdir.push(project_name);

    for i in 0.. {
        if !workdir.exists() {
            break;
        }

        workdir.pop();
        workdir.push(format!("{project_name}_{i}"));
    }

    std::fs::create_dir_all(&workdir).expect("unable to create workdir directory in luminapath");

    workdir
}
