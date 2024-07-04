use std::fmt;

#[derive(Clone, Copy)]
pub struct Target {
    pub arch: Arch,
    pub platform: Platform,
}

#[derive(Clone, Copy)]
pub enum Arch {
    X86_64,
}

#[derive(Clone, Copy)]
pub enum Platform {
    Linux { sub: LinuxPlatform },
}

#[derive(Clone, Copy)]
pub enum LinuxPlatform {
    Gnu,
    Musl,
    Syscall,
}

impl Arch {
    fn name(&self) -> &'static str {
        match self {
            Arch::X86_64 => "x86_64",
        }
    }
}

impl TryFrom<&str> for Target {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut iter = value.split('-');

        let arch = match iter.next().ok_or("missing target")? {
            "x86_64" => Arch::X86_64,
            _ => return Err("unsupported CPU architecture"),
        };
        let platform = match iter.next().ok_or("missing platform")? {
            "linux" => Platform::Linux {
                sub: match iter.next() {
                    Some("syscall") => LinuxPlatform::Syscall,
                    Some("musl") => LinuxPlatform::Musl,
                    None | Some("gnu") | Some("") => LinuxPlatform::Gnu,
                    Some(_) => return Err("unknown linux platform"),
                },
            },
            _ => return Err("unsupported platform"),
        };

        Ok(Target { arch, platform })
    }
}

impl Target {
    #[cfg(target_os = "linux")]
    pub fn native() -> Self {
        #[cfg(target_arch = "x86_64")]
        let arch = Arch::X86_64;
        Target { platform: Platform::Linux { sub: LinuxPlatform::Gnu }, arch }
    }

    #[cfg(not(target_os = "linux"))]
    pub fn native() -> Self {
        panic!("unknown platform");
    }

    pub fn name(&self) -> String {
        self.to_string()
    }

    pub fn include_for(&self, name: &str) -> bool {
        let mut iter = name.split('-');

        let targetted = iter.all(|name| match name {
            "unix" => match self.platform {
                Platform::Linux { .. } => true,
            },
            "linux" => match self.platform {
                Platform::Linux { .. } => true,
            },
            "gnu" => matches!(self.platform, Platform::Linux { sub: LinuxPlatform::Gnu }),
            "musl" => matches!(self.platform, Platform::Linux { sub: LinuxPlatform::Musl }),
            "syscall" => matches!(
                self.platform,
                Platform::Linux { sub: LinuxPlatform::Syscall }
            ),
            _ => self.arch.name() == name,
        });

        targetted
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", &self.arch, &self.platform)
    }
}

impl fmt::Display for Arch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.name().fmt(f)
    }
}

impl fmt::Display for Platform {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Platform::Linux { sub } => write!(f, "linux-{sub}"),
        }
    }
}

impl fmt::Display for LinuxPlatform {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LinuxPlatform::Gnu => "gnu".fmt(f),
            LinuxPlatform::Syscall => "syscall".fmt(f),
            LinuxPlatform::Musl => "musl".fmt(f),
        }
    }
}
