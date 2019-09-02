use super::Operator;
use crate::assume;
use crate::evaler::r#type::Value;
use crate::identifier::r#type::{BaseType, Type};
use crate::parser::prelude;

#[derive(PartialEq, Clone, Debug)]
pub enum List {
    Append,
    Range,
}

impl Operator for List {
    fn operate(&self, left: &Value, right: &Value) -> Value {
        match self {
            List::Append => match left {
                Value::List(left) => {
                    let right = assume!(right, Value::List);
                    Value::List(left.iter().cloned().chain(right.iter().cloned()).collect())
                }
                // WARNING: This code assumes the struct is a string, which *should* be garentueed
                // during parsing.
                Value::Struct(of_t, s) => {
                    let left = assume!(s[0].clone(), Value::List);
                    let right_struct = if let Value::Struct(_, v) = right {
                        v
                    } else {
                        panic!();
                    };
                    let right = assume!(right_struct[0].clone(), Value::List);
                    Value::Struct(
                        of_t.clone(),
                        vec![Value::List(
                            left.into_iter().chain(right.into_iter()).collect(),
                        )],
                    )
                }
                _ => panic!(
                    "the `<>` operator cannot be applied to {:?} and {:?}",
                    left, right
                ),
            },
            List::Range => {
                let left = assume!(left, Value::Int);
                let right = assume!(right, Value::Int);
                Value::List((*left..*right).map(Value::Int).collect::<Vec<Value>>())
            }
        }
    }

    fn type_check(&self, left: Type, right: Type) -> (bool, bool) {
        match self {
            List::Range => (
                left == Type::Base(BaseType::Int),
                right == Type::Base(BaseType::Int),
            ),
            List::Append => {
                // They need to both be List of same type or both be String
                match &left {
                    Type::List(_) => (true, left == right),
                    Type::Custom(fid, tid) => {
                        if *fid == prelude::LEAF_PRIM_FID && *tid == prelude::STRING_ID {
                            (true, left == right)
                        } else {
                            (false, false)
                        }
                    }
                    _ => (false, false),
                }
            }
        }
    }
}
