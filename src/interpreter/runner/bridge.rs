use crate::ir::Value;

pub fn get_func_two(id: u16) -> fn(Value, Value) -> Value {
    match id {
        0 => |x, y| {
            if let Value::Int(x) = x {
                if let Value::Int(y) = y {
                    return Value::Int(x + y);
                }
            }
            unreachable!();
        },
        1 => |x, y| {
            if let Value::Int(x) = x {
                if let Value::Int(y) = y {
                    return Value::Int(x - y);
                }
            }
            unreachable!();
        },
        2 => |x, y| {
            if let Value::Int(x) = x {
                if let Value::Int(y) = y {
                    return Value::Int(x * y);
                }
            }
            unreachable!();
        },
        3 => |x, y| {
            if let Value::Int(x) = x {
                if let Value::Int(y) = y {
                    return Value::Int(x / y);
                }
            }
            unreachable!();
        },
        _ => unreachable!(),
    }
}
