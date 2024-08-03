use super::{FuncLower, Value};
use lumina_typesystem::Type;
use tracing::error;

impl<'a> FuncLower<'a> {
    pub fn create_reflection(&mut self, weak: Type) -> Value {
        error!("{}", &weak);

        // let reflect_weak = to_morphization!(self.lir, self.mir, &mut self.current.tmap)
        //     .apply_weak(&Type::defined(self.info.reflect_type, vec![]));

        todo!("reflection API needs to be redesigned, it's not useful if you can't resolve implementations");
    }
}

// const INT: Value = Value::UInt(0, mono::TAG_SIZE);
// const FLOAT: Value = Value::UInt(1, mono::TAG_SIZE);
// const BOOL: Value = Value::UInt(2, mono::TAG_SIZE);
// const NEVER: Value = Value::UInt(3, mono::TAG_SIZE);
// const POISON: Value = Value::UInt(4, mono::TAG_SIZE);
// const POINTER: Value = Value::UInt(5, mono::TAG_SIZE);
// const FN_POINTER: Value = Value::UInt(6, mono::TAG_SIZE);
// const STRUCT: Value = Value::UInt(7, mono::TAG_SIZE);
// const SUM: Value = Value::UInt(8, mono::TAG_SIZE);
// const TUPLE: Value = Value::UInt(9, mono::TAG_SIZE);
// const OBJECT: Value = Value::UInt(10, mono::TAG_SIZE);

// (std:prelude)
//
// pub type Type
//   = Int bool u8
//   | Float
//   | Bool
//   | Never
//   | Poison
//   | Pointer   Type
//   | FnPointer [Type] Type
//   | Struct string [(string, Type)]
//   | Sum    string [(string, [Type])]
//   | Tuple  [Type]
//   | Object string
