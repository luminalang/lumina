#[macro_export]
macro_rules! impl_map_arrow_fmt {
    (<$($l:lifetime),*> $form:path; for $ty:ty; $(($name:literal, $field:ident, $fmt:expr)),*) => {
        impl<$($l),*> $form for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                use itertools::Itertools;

                $({
                    let _name = $name;
                    write!(f, "{_name}:\n  {}", self.$field.iter().map($fmt).format("\n  "))?;
                })*;

                Ok(())
            }
        }
    };
}
