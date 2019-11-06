pub mod bridge;
mod r#if;
pub mod optimizer;
pub use r#if::If;
mod first;
pub use first::First;
mod value;
pub use value::Value;

#[derive(Debug, Clone)]
pub enum Entity {
    RustCall(u16, Vec<Entity>),
    FunctionCall(u32, Vec<Entity>),
    IfExpression(self::If<Entity>),
    FirstStatement(self::First<Entity>),
    Parameter(u16),

    Inlined(Value),
    Unique,
    /*
    List(Vec<Entity>),
    */
}
