const _FLAG_MUT: u8 = 1;
const _FLAG_USED: u8 = 2;

#[derive(Clone, Copy)]
pub struct Flag {
    pub bits: u8,
}

impl std::default::Default for Flag {
    fn default() -> Self {
        Self { bits: 0b00_000_000 }
    }
}
