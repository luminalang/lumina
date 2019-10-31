mod r#if;
pub use r#if::If;
mod first;
pub use first::First;

#[derive(Debug)]
pub enum Entity {
    RustCall(u16, Vec<Entity>),
    FunctionCall(u32, Vec<Entity>),
    IfExpression(self::If<Entity>),
    FirstStatement(self::First<Entity>),

    Int(u64),
    Float(f64),
    Bool(bool),
    List(Vec<Entity>),
}
