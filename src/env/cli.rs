use std::default::Default;
use std::env;

pub struct Arguments {
    pub source_file: String,
    pub profile: RunProfile,
}

pub mod debug;
use debug::RunProfile;

impl Default for Arguments {
    fn default() -> Self {
        #[allow(unused_mut)]
        let mut args = Arguments {
            source_file: String::new(),
            profile: RunProfile::Standard,
        };

        #[cfg(debug_assertions)]
        {
            args.profile = RunProfile::Development;
        }

        args
    }
}

impl Arguments {
    pub fn parse() -> Self {
        let mut arguments = Arguments::default();
        let mut args = env::args().collect::<Vec<String>>();

        if args.len() < 2 {
            if let Ok(p) = env::var("LEAF_TESTFILE") {
                args.push(p);
            } else {
                eprintln!("no args"); // TODO
                std::process::exit(1);
            }
        }
        args.remove(0);

        arguments.source_file.push_str(&args.pop().unwrap());

        for arg in args.iter() {
            if arg.contains('=') {
                let (k, v) = {
                    let mut spl = arg.split('=');
                    (spl.next().unwrap(), spl.next().unwrap())
                };
                match k {
                    "--debug" => {
                        arguments.profile = match v {
                            "0" | "release" => RunProfile::Release,
                            "1" | "standard" => RunProfile::Standard,
                            "2" | "debug" => RunProfile::Debug,
                            _ => {
                                eprintln!("unrecognized debug level `{}`", v);
                                std::process::exit(1);
                            }
                        }
                    }
                    _ => {
                        eprintln!("unrecognized option `{}`", k);
                        std::process::exit(1)
                    }
                };
            } else {
                match arg {
                    _ => {
                        eprintln!("unrecognized flag `{}`", arg);
                        std::process::exit(1);
                    }
                }
            }
        }

        arguments
    }
}
