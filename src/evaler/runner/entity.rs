use super::operator::Operators;
use crate::evaler::r#type::Value;
use std::fmt;

type More = Vec<Entity>;

#[derive(Debug, Clone, PartialEq)]
pub enum Entity {
    Function(usize, usize, Vec<Entity>),
    Recurse(Vec<Entity>),
    BridgedFunction(u16, Vec<Entity>),
    IfStatement(Vec<(More, More)>, More),
    EvaluationList(More),
    V(Value),
    ParamValue(usize),
    WhereValue(usize),
    BufferValue(usize),
    ExternClosure(usize, usize, Vec<Entity>),
    LocalClosure(More, Vec<Entity>),
    Group(More),
    Operation(Box<(Entity, Entity)>, Operators),
}

impl fmt::Display for Entity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Entity::Function(_, _, _) => write!(f, "function"),
            Entity::BridgedFunction(_, _) => write!(f, "rust bridged function"),
            Entity::IfStatement(_, _) => write!(f, "if statement"),
            Entity::EvaluationList(_) => write!(f, "list"),
            Entity::V(_) => write!(f, "inlined value"),
            Entity::ParamValue(_) => write!(f, "parameter"),
            Entity::WhereValue(_) => write!(f, "value from where"),
            Entity::BufferValue(_) => write!(f, "value from buffer"),
            Entity::ExternClosure(_, _, _) => write!(f, "external closure"),
            Entity::LocalClosure(_, _) => write!(f, "local closure"),
            Entity::Group(_) => write!(f, "group"),
            Entity::Operation(_, op) => write!(f, "{}", op.to_string()),
            Entity::Recurse(_) => write!(f, "recursion call"),
        }
    }
}
