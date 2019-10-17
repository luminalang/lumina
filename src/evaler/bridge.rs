use crate::parser::Type;

pub fn try_rust_builtin(entries: &[String]) -> Result<Option<(u16, Type)>, ()> {
    if &entries[0] == "rust" {
        if entries.len() != 3 {
            panic!("ET: Wanted 3 entries in identifier for rust builtin call");
        }
        let mut iter = entries[1].bytes();
        if iter.next().unwrap() != b'p' {
            panic!(
                "ET: You need to specify parameter amount for rust builtin functions `rust:p2:add`"
            );
        }
        Ok(Some(get_funcid(
            &entries[2],
            u16::from(iter.next().unwrap() - 48),
        )?))
    } else {
        Ok(None)
    }
}

pub fn get_funcid(ident: &str, params: u16) -> Result<(u16, Type), ()> {
    let id = match params {
        2 => match ident {
            "add" => (0, Type::Int),
            "sub" => (1, Type::Int),
            "mul" => (2, Type::Int),
            "div" => (3, Type::Int),
            _ => panic!(
                "No rust-bridged function taking {} arguments named {}",
                params, ident
            ),
        },
        _ => panic!("No rust-bridged function takes {} parameters", params),
    };
    Ok(id)
}

pub fn get_func_one(id: usize) -> impl FnOnce(u8) -> u8 {
    |n| n
}

#[rustfmt::skip]
pub fn get_func_two(id: usize) -> fn(u8, u8) -> u8 {
    match id {
        0 => |x, y| x + y,
        1 => |x, y| x - y,
        2 => |x, y| x * y,
        3 => |x, y| x / y,
        _ => unreachable!(),
    }
}
