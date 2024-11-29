use howlite_typecheck::types::TyInt;

use super::Register;

#[derive(Clone, Debug)]

pub enum Value {
    Slot(Slot),

    /// A constant value.
    /// This is a signal we should build the value at compile time from its type
    Literal,
}

impl Value {
    pub fn is_slot(&self) -> bool {
        if let Value::Slot(_) = self {
            true
        } else {
            false
        }
    }

    pub fn as_slot(&self) -> Option<&Slot> {
        if let Value::Slot(s) = self {
            Some(s)
        } else {
            None
        }
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq)]
/// A machine-friendly reference to a value
pub enum Slot {
    /// if the value is immediate, it can be stored in the immediate part of an instruction
    Immediate(i16),

    /// If the value is can fit, it may be stored in a register
    Register(Register),

    /// A value stored in memory, relative to the address stored in the 'base' slot
    Indirect { base: Box<Slot>, offset: i32 },
}

impl Slot {
    pub const MAX_IMM: i16 = 2047;
    pub const MIN_IMM: i16 = -2048;
}

impl Into<Value> for Slot {
    fn into(self) -> Value {
        Value::Slot(self)
    }
}
