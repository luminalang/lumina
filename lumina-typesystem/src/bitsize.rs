use std::fmt;

#[derive(PartialEq, Eq, Copy, Clone, Debug, PartialOrd, Ord, Hash)]
pub struct Bitsize(pub u8);

impl Default for Bitsize {
    fn default() -> Self {
        Self(64)
    }
}

impl Bitsize {
    pub fn bytes(self) -> u32 {
        self.0 as u32 / 8
    }

    fn default_or_by_min(signed: bool, n: u128) -> Self {
        match signed {
            true if n <= i64::MAX as u128 => Bitsize::default(),
            false if n <= u64::MAX as u128 => Bitsize::default(),
            _ => Bitsize(128),
        }
    }

    pub fn mini(self, signed: bool) -> u128 {
        match signed {
            false => 0,
            true => self.maxi(signed),
        }
    }

    pub fn maxi(self, signed: bool) -> u128 {
        match signed {
            false => self.maxu(),
            true => self.maxu() / 2,
        }
    }

    pub fn maxu(self) -> u128 {
        (1 as u128)
            .checked_shl(self.0 as u32)
            .map(|n| n - 1)
            .unwrap_or(u128::MAX)
    }

    pub fn minimum_for(signed: bool, n: u128) -> Bitsize {
        if signed {
            [
                (i8::MAX as u128, 8),
                (i16::MAX as u128, 16),
                (i32::MAX as u128, 32),
                (i64::MAX as u128, 64),
            ]
        } else {
            [
                (u8::MAX as u128, 8),
                (u16::MAX as u128, 16),
                (u32::MAX as u128, 32),
                (u64::MAX as u128, 64),
            ]
        }
        .iter()
        .find_map(|&(m, b)| (n < m).then_some(Bitsize(b)))
        .unwrap_or(Bitsize(128))
    }
}

impl fmt::Display for Bitsize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}
