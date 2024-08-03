use std::fmt;

#[derive(PartialEq, Eq, Copy, Clone, Debug, PartialOrd, Ord, Hash)]
pub struct IntSize {
    pub signed: bool,
    bits: u8,
}

impl IntSize {
    pub const fn new(signed: bool, bits: u8) -> Self {
        assert!(bits < 67, "int sizes larger than 64 is not supported");
        IntSize { signed, bits }
    }

    pub fn bytes(self) -> u8 {
        self.bits / 8
    }

    pub fn bits(self) -> u8 {
        self.bits
    }

    pub fn max_value(self) -> u64 {
        let max = (1 as u64)
            .checked_shl(self.bits() as u32)
            .map(|n| n - 1)
            .unwrap_or(u64::MAX);

        if self.signed {
            max / 2
        } else {
            max
        }
    }

    pub fn min_value(&self) -> i64 {
        if self.signed {
            (-(self.max_value() as i64)) - 1
        } else {
            0
        }
    }

    pub fn minimum_for(signed: bool, n: u128) -> IntSize {
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
        .find_map(|&(m, b)| (n < m).then(|| IntSize::new(signed, b)))
        .unwrap_or(IntSize::new(signed, 64))
    }

    pub fn fmt_with_default(&self, f: &mut fmt::Formatter, default: u8) -> fmt::Result {
        if self.bits == default {
            if self.signed {
                write!(f, "int")
            } else {
                write!(f, "uint")
            }
        } else {
            write!(f, "{}{}", if self.signed { 'i' } else { 'u' }, self.bits)
        }
    }
}

impl fmt::Display for IntSize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_default(f, 0)
    }
}

#[cfg(test)]
mod tests {
    use super::IntSize;

    #[test]
    fn constraints() {
        let size = IntSize::new(true, 64);
        assert_eq!(size.max_value(), i64::MAX as u64);
        assert_eq!(size.min_value(), i64::MIN);

        let size = IntSize::new(false, 64);
        assert_eq!(size.max_value(), u64::MAX);
        assert_eq!(size.min_value(), u64::MIN as i64);
    }
}
