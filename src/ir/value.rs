#[derive(Debug, Clone)]
pub enum Value {
    Nothing,
    Int(i64),
    Float(f64),
    Bool(bool),

    // TODO: This is a terrible way to handle lists.
    // For one, we're storing meta type information for each member of the list
    // rather than just once. So we're wasting a ton of space.
    // This also bloats the full Size of the Value enum, wasting even more memory
    List(Vec<Value>),
}
