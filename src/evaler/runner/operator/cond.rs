use super::Operator;
use crate::evaler::r#type::Value;
use crate::identifier::r#type::Type;

#[derive(PartialEq, Clone, Debug)]
pub enum Cond {
    LtE,
    GtE,

    Lt,
    Eq,
    Gt,
}

macro_rules! compare {
    ($x:expr, $op:tt, $y:expr) => {{
        match $x {
         Value::Int(x) => match $y {
            Value::Int(y) => return Value::Bool(*x $op *y),
            Value::Float(y) => return Value::Bool((*x as f32) $op *y),
            _ => {},
        },
        Value::Float(x) => match $y {
            Value::Int(y) => return Value::Bool(*x $op (*y as f32)),
            Value::Float(y) => return Value::Bool(*x $op *y),
            _ => {},
        },
        _ => {},
        }
    panic!("operator can not be applied to {:?} and {:?}", $x, $y);
    }}
}

impl Operator for Cond {
    fn operate(&self, left: &Value, right: &Value) -> Value {
        match self {
            Cond::Lt => compare!(left, <, right),
            Cond::Eq => compare!(left, ==, right),
            Cond::Gt => compare!(left, >, right),
            Cond::LtE => compare!(left, <=, right),
            Cond::GtE => compare!(left, >=, right),
        }
    }
    fn type_check(&self, left: Type, right: Type) -> (bool, bool) {
        match self {
            Cond::Eq => {
                // Compatible with all things, they just need to be the same type
                if left != right {
                    (false, false)
                } else {
                    (true, true)
                }
            }
            _ => {
                // Only number compatible
                (!left.is_number(), right.is_number())
            }
        }
    }
}
