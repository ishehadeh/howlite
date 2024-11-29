#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// source https://www.allaboutcircuits.com/technical-articles/introductions-to-risc-v-instruction-set-understanding-this-open-instruction-set-architecture/
pub enum Register {
    Zero = 0,

    /// Return address
    Ra,

    /// stack pointer
    Sp,

    /// Global Pointer
    Gp,

    /// Thread pointer
    Tp,

    // Temporaries
    /// Temporary/ alternate link register
    T0,
    T1,
    T2,

    /// Frame pointer
    Fp,

    /// Saved register
    S1,

    /// Return Value or Argument 1
    A0,

    /// Return or Argument 2
    A1,

    // args
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,

    ///Saved register
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,

    // Temporaries
    T3,
    T4,
    T5,
    T6,
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
    pub fn to_abi_name(&self) -> &'static str {
        match self {
            Register::Zero => "zero",
            Register::Ra => "ra",
            Register::Sp => "sp",
            Register::Gp => "gp",
            Register::Tp => "tp",
            Register::T0 => "t0",
            Register::T1 => "t1",
            Register::T2 => "t2",
            Register::Fp => "fp",
            Register::S1 => "s1",
            Register::A0 => "a0",
            Register::A1 => "a1",
            Register::A2 => "a2",
            Register::A3 => "a3",
            Register::A4 => "a4",
            Register::A5 => "a5",
            Register::A6 => "a6",
            Register::A7 => "a7",
            Register::S2 => "s2",
            Register::S3 => "s3",
            Register::S4 => "s4",
            Register::S5 => "s5",
            Register::S6 => "s6",
            Register::S7 => "s7",
            Register::S8 => "s8",
            Register::S9 => "s9",
            Register::S10 => "s10",
            Register::S11 => "s11",
            Register::T3 => "t3",
            Register::T4 => "t4",
            Register::T5 => "t5",
            Register::T6 => "t6",
        }
    }
}
