use std::{
    num::NonZeroU32,
    ops::{BitOr, BitOrAssign},
};

#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct RegisterSet {
    bits: NonZeroU32,
}

impl std::fmt::Debug for RegisterSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, reg) in self.into_iter().enumerate() {
            f.write_str(&reg.to_abi_name())?;
            if i != self.count() - 1 {
                f.write_str(" | ")?
            }
        }
        Ok(())
    }
}

macro_rules! impl_register_set {
    (@impl_consts { index = $index:expr; registers = [$first_reg:ident, $($rest_reg:ident),*] } ) => {
        impl_register_set! {
            @impl_consts {
                index = $index;
                registers = [$first_reg]
            }
        }
        impl_register_set! {
            @impl_consts {
                index = ($index + 1);
                registers = [$($rest_reg),*]
            }
        }
    };

    (@impl_consts { index = $index:expr; registers = [$first_reg:ident] } ) => {
        pub const $first_reg: Self = Self { bits: unsafe { NonZeroU32::new_unchecked(1 << $index) }  };
    };

    (@impl_from_raw { raw = $raw:ident; prev_indicies = [$($match_index:expr => $match_val:expr),* $(,)?]; index = $index:expr; registers = [$first_reg:ident $($rest_reg:ident) *] } ) => {
        impl_register_set! {
            @impl_from_raw {
                raw = $raw;
                prev_indicies = [$index => Register::$first_reg, $($match_index => $match_val),*];
                index = ($index + 1);
                registers = [$($rest_reg) *]
            }
        }
    };

    (@impl_from_raw { raw = $raw:ident; prev_indicies = [$($match_index:expr => $match_val:expr),*]; index = $index:expr; registers = [] } ) => {
            $(if $raw == $match_index { return Some($match_val); })*

        None
    };


    ($($reg:ident => $abi_str:literal ($enum_name:ident)),*) => {
        impl RegisterSet {
            impl_register_set! {
                @impl_consts {
                    index = 0;
                    registers = [$($reg),*]
                }
            }

            pub const fn from_register(register: Register) -> Self {
                match register {
                    $(Register::$enum_name => Self::$reg),*
                }
            }

            pub const fn all() -> Self {
                Self::merge([$(Self::$reg),*])
            }
        }

        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(u8)]
        pub enum Register {
            $($enum_name),*
        }

        impl Register {
            // convert ot a register ABI name e.g "zero", "gp", etc.
            pub const fn to_abi_name(&self) -> &'static str {
                match self {
                    $(Register::$enum_name => $abi_str),*
                }
            }

            /// convert from a case-insensitive ABI register name (e.g. "s1", "rA", "A5")
            pub const fn from_abi_name(abi_name: &str) -> Option<Self> {
                $(
                    if abi_name.len() == $abi_str.len()  {
                        let mut i = 0;
                        while i < $abi_str.len() {
                            if abi_name.as_bytes()[i].to_ascii_lowercase() == $abi_str.as_bytes()[i].to_ascii_lowercase() {
                                break;
                            }
                            if i == $abi_str.len() - 1 {
                                return Some(Register::$enum_name)
                            }
                            i += 1;
                        }
                    }
                )*


                None
            }

            // convert from a raw register index i.e. return register "r<raw>"
            // returns none if raw is > 31
            pub const fn from_raw(raw: u8) -> Option<Self> {
                impl_register_set! {
                    @impl_from_raw {
                        raw = raw;
                        prev_indicies = [];
                        index = 0;
                        registers = [$($enum_name) *]
                    }
                }
            }
        }
    };
}

impl_register_set! {
    ZERO => "zero" (Zero),
    RA => "ra" (Ra),
    SP => "sp" (Sp),
    GP => "gp" (Gp),
    TP => "tp" (Tp),
    T0 => "t0" (T0),
    T1 => "t1" (T1),
    T2 => "t2" (T2),
    FP => "fp" (Fp),
    S1 => "s1" (S1),
    A0 => "a0" (A0),
    A1 => "a1" (A1),
    A2 => "a2" (A2),
    A3 => "a3" (A3),
    A4 => "a4" (A4),
    A5 => "a5" (A5),
    A6 => "a6" (A6),
    A7 => "a7" (A7),
    S2 => "s2" (S2),
    S3 => "s3" (S3),
    S4 => "s4" (S4),
    S5 => "s5" (S5),
    S6 => "s6" (S6),
    S7 => "s7" (S7),
    S8 => "s8" (S8),
    S9 => "s9" (S9),
    S10 => "s10" (S10),
    S11 => "s11" (S11),
    T3 => "t3" (T3),
    T4 => "t4" (T4),
    T5 => "t5" (T5),
    T6 => "t6" (T6)
}

impl Register {
    pub fn is_callee_saved(&self) -> bool {
        // TODO there are more callee registers I THINK
        matches!(
            self,
            Register::S1
                | Register::S2
                | Register::S3
                | Register::S4
                | Register::S5
                | Register::S6
                | Register::S7
        )
    }
}

impl From<Register> for RegisterSet {
    fn from(val: Register) -> Self {
        RegisterSet::from_register(val)
    }
}

impl BitOrAssign for RegisterSet {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = (*self).include(rhs)
    }
}
impl BitOr for RegisterSet {
    fn bitor(self, rhs: Self) -> Self::Output {
        self.include(rhs)
    }

    type Output = Self;
}

impl RegisterSet {
    pub const CALLEE_SAVED: RegisterSet = Self::merge([
        Self::S1,
        Self::S2,
        Self::S3,
        Self::S4,
        Self::S5,
        Self::S6,
        Self::S7,
    ]);

    pub const fn from_raw(bits: u32) -> Option<Self> {
        // using match over map bc map isn't allowed in const context
        match NonZeroU32::new(bits) {
            Some(v) => Some(Self { bits: v }),
            None => None,
        }
    }

    const fn disallowed_bits() -> u32 {
        !Self::all().bits.get()
    }

    pub const fn merge<const N: usize>(values: [Self; N]) -> Self {
        assert!(N > 0);

        let mut i = 1;
        let mut all = values[0].bits.get();
        while i < N {
            all |= values[i].bits.get();
            i += 1;
        }

        match Self::from_raw(all) {
            Some(v) => v,
            None => panic!(
                "merge resulted in empty set, this should be impossible given well formed input"
            ),
        }
    }

    pub const fn count(&self) -> usize {
        self.bits.get().count_ones() as usize
    }

    pub const fn first(&self) -> Register {
        let Some(reg) = Register::from_raw(self.bits.get().trailing_zeros() as u8) else {
            panic!("RegisterSet::first(): bit index could not be converted into a register",);
        };
        reg
    }

    pub const fn last(&self) -> Register {
        let Some(reg) = Register::from_raw(self.bits.get().leading_zeros() as u8) else {
            panic!("RegisterSet::first(): bit index could not be converted into a register");
        };
        reg
    }

    pub const fn include(self, other: Self) -> Self {
        Self::merge([self, other])
    }

    pub const fn complement(self) -> Option<Self> {
        let complement_bits = !self.bits.get() & !Self::disallowed_bits();
        match NonZeroU32::new(complement_bits) {
            Some(bits) => Some(Self { bits }),
            None => None,
        }
    }

    pub const fn intersect(self, other: Self) -> Option<Self> {
        let intersect_bits = self.bits.get() & other.bits.get();
        match NonZeroU32::new(intersect_bits) {
            Some(bits) => Some(Self { bits }),
            None => None,
        }
    }

    pub const fn exclude(self, other: Self) -> Option<Self> {
        let Some(complement) = self.complement() else {
            return None;
        };
        complement.intersect(other)
    }
}

impl IntoIterator for RegisterSet {
    type Item = Register;

    type IntoIter = RegisterSetIter;

    fn into_iter(self) -> Self::IntoIter {
        RegisterSetIter {
            remaining_bits: self.bits.get(),
        }
    }
}

impl<'a> IntoIterator for &'a RegisterSet {
    type Item = Register;

    type IntoIter = RegisterSetIter;

    fn into_iter(self) -> Self::IntoIter {
        RegisterSetIter {
            remaining_bits: self.bits.get(),
        }
    }
}

pub struct RegisterSetIter {
    remaining_bits: u32,
}

impl ExactSizeIterator for RegisterSetIter {
    fn len(&self) -> usize {
        self.remaining_bits.count_ones() as usize
    }
}

impl Iterator for RegisterSetIter {
    type Item = Register;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining_bits == 0 {
            None
        } else {
            let next = self.remaining_bits.trailing_zeros();
            self.remaining_bits &= !(1 << next);
            Some(Register::from_raw(next as u8).expect("invalid register index in bit set"))
        }
    }
}

impl DoubleEndedIterator for RegisterSetIter {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.remaining_bits == 0 {
            None
        } else {
            let next = self.remaining_bits.leading_zeros();
            self.remaining_bits &= !(1 << (u32::BITS - next - 1));
            Some(Register::from_raw(next as u8).expect("invalid register index in bit set"))
        }
    }
}
