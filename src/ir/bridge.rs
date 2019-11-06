use crate::parser::{ParseFault, Type};

pub fn try_rust_builtin(entries: &[String]) -> Result<Option<(u16, Type)>, ParseFault> {
    if &entries[0] == "rust" {
        if entries.len() != 3 {
            return Err(ParseFault::BridgedWrongPathLen(entries.to_vec()));
        }
        let mut iter = entries[1].bytes();
        let c = iter.next().unwrap();
        if c != b'p' {
            return Err(ParseFault::BridgedFunctionNoMode(c));
        }
        Ok(Some(get_funcid(
            &entries[2],
            u16::from(iter.next().unwrap() - 48),
        )?))
    } else {
        Ok(None)
    }
}

pub fn get_funcid(ident: &str, params: u16) -> Result<(u16, Type), ParseFault> {
    let id = match params {
        2 => match ident {
            "add" => (0, Type::Int),
            "sub" => (1, Type::Int),
            "mul" => (2, Type::Int),
            "div" => (3, Type::Int),
            _ => return Err(ParseFault::BridgedFunctionNotFound(ident.into(), params)),
        },
        _ => return Err(ParseFault::BridgedFunctionNotFound(ident.into(), params)),
    };
    Ok(id)
}
