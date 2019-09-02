use crate::evaler::r#type::Value;
use crate::identifier::r#type::Type;
use crate::parser::prelude;
use std::io::Read;
use std::io::Write;

pub type FunctionID = u16;

type BridgedFunction = fn(Value) -> Value;

const WRITE_STDOUT: u16 = 0;
const READ_STDIN: u16 = 1;
pub const TO_STRING: u16 = 2;

macro_rules! PRIMITIVE_STRING {
    () => {
        Type::Custom(prelude::LEAF_PRIM_FID, prelude::STRING_ID)
    };
}

pub fn get_function(f: FunctionID) -> BridgedFunction {
    match f {
        WRITE_STDOUT => |param| match param {
            Value::Byte(b) => {
                std::io::stdout().write_all(&[b]).ok();
                param
            }
            Value::Struct(PRIMITIVE_STRING!(), str_fields) => {
                let inner = &str_fields[0];
                if let Value::List(list) = inner {
                    let data = list
                        .iter()
                        .map(|v| match &v {
                            Value::Byte(b) => *b,
                            _ => panic!("Corrupt string was used in Rust:WriteStdout: {:?}", list),
                        })
                        .collect::<Vec<u8>>();
                    std::io::stdout().write_all(&data).unwrap();
                    Value::Struct(PRIMITIVE_STRING!(), list.to_vec())
                } else {
                    panic!("Corrupt string was used in Rust:WriteStdout: {:?}", &inner)
                }
            }
            _ => panic!(
                "Bridged function WriteStdout requires `byte` or `string` but got `{:?}`",
                param
            ),
        },
        READ_STDIN => |param| match param {
            Value::Byte(b) => {
                let mut buf: Vec<Value> = Vec::new();
                for c in std::io::stdin().lock().bytes() {
                    let c = c.unwrap();
                    buf.push(Value::Byte(c));
                    if c == b {
                        break;
                    }
                }
                Value::Struct(PRIMITIVE_STRING!(), vec![Value::List(buf)])
            }
            _ => panic!(
                "Bridged function ReadStdin requires `byte` but got `{:?}`",
                param
            ),
        },
        TO_STRING => |param| {
            let s = param.pretty_print().into_bytes();
            Value::Struct(
                PRIMITIVE_STRING!(),
                vec![Value::List(
                    s.iter().map(|v| Value::Byte(*v)).collect::<Vec<Value>>(),
                )],
            )
        },
        _ => panic!("Bridged function {} does not exist", f),
    }
}
