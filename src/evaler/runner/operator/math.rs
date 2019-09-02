use super::Operator;
use crate::evaler::r#type::Value;
use crate::identifier::r#type::Type;

#[derive(PartialEq, Clone, Debug)]
pub enum Math {
    Add,
    Sub,
    Div,
    Mul,
}

macro_rules! calculate {
    ($x:expr, $op:tt, $y:expr) => {{
    match $x {
        Value::Int(x) => match $y {
            Value::Int(y) => return Value::Int(*x $op *y),
            Value::Float(y) => return Value::Float(*x as f32 $op *y),
            _ => {},
        },
        Value::Float(x) => match $y {
            Value::Int(y) => return Value::Float(*x $op *y as f32),
            Value::Float(y) => return Value::Float(*x $op *y),
            _ => {},
        },
        _ => {},
    }
    panic!("operator can not be applied to {:?} and {:?}", $x, $y);
    }}
}

impl Operator for Math {
    fn operate(&self, left: &Value, right: &Value) -> Value {
        match self {
            Math::Add => calculate!(left, +, right),
            Math::Sub => calculate!(left, -, right),
            Math::Div => calculate!(left, /, right),
            Math::Mul => calculate!(left, *, right),
        }
    }

    fn type_check(&self, left: Type, right: Type) -> (bool, bool) {
        (left.is_number(), right.is_number())
    }
}
