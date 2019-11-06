#[derive(Debug, Clone)]
pub enum Value {
    Nothing,
    Int(i64),
    Float(f64),
    Bool(bool),
}
