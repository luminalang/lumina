const FLAG_MUT: u8 = 1;
const FLAG_USED: u8 = 2;

pub struct Flag {
    bits: u8,
}

impl std::default::Default for Flag {
    fn default() -> Self {
        Self { bits: 0b00_000_000 }
    }
}
