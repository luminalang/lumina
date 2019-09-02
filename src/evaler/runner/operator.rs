use crate::evaler::r#type::Value;
use crate::identifier::r#type::Type;
use std::fmt;

pub mod math;
use math::Math;

pub mod cond;
use cond::Cond;

pub mod list;
use list::List;

pub trait Operator {
    fn operate(&self, left: &Value, right: &Value) -> Value;
    fn type_check(&self, left: Type, right: Type) -> (bool, bool);
}

#[derive(PartialEq, Clone, Debug)]
pub enum Operators {
    Math(Math),
    Cond(Cond),
    List(List),
}

impl fmt::Display for Operators {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Cond::*;
        use self::List::*;
        use self::Math::*;
        use Operators::*;

        let s = match self {
            Math(Add) => "+",
            Math(Sub) => "-",
            Math(Mul) => "*",
            Math(Div) => "/",
            Cond(Lt) => "<",
            Cond(Eq) => "==",
            Cond(Gt) => ">",
            Cond(LtE) => "<=",
            Cond(GtE) => "=>",
            List(Append) => "<>",
            List(Range) => "..",
        };
        f.write_str(s)
    }
}

impl Operator for Operators {
    fn operate(&self, left: &Value, right: &Value) -> Value {
        match self {
            Operators::Math(op) => op.operate(left, right),
            Operators::Cond(op) => op.operate(left, right),
            Operators::List(op) => op.operate(left, right),
        }
    }
    fn type_check(&self, left: Type, right: Type) -> (bool, bool) {
        match self {
            Operators::Math(op) => op.type_check(left, right),
            Operators::Cond(op) => op.type_check(left, right),
            Operators::List(op) => op.type_check(left, right),
        }
    }
}
