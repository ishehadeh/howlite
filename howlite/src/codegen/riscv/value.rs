use std::collections::HashMap;

use super::Register;

#[derive(Clone, Debug)]

pub enum Value {
    // a collection of named slots
    Map(ValueMap),
    Slot(Slot),
}

impl Value {
    pub fn is_slot(&self) -> bool {
        if let Value::Slot(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_map(&self) -> bool {
        if let Value::Map(_) = self {
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

    pub fn as_map(&self) -> Option<&ValueMap> {
        if let Value::Map(m) = self {
            Some(m)
        } else {
            None
        }
    }

    pub fn slots<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Slot> + 'a> {
        match self {
            Value::Slot(s) => Box::new(std::iter::once(s)),
            Value::Map(m) => Box::new(m.slots()),
        }
    }
}

#[derive(Default, Clone, Debug)]
pub struct ValueMap {
    pub values: HashMap<String, Value>,
}

impl ValueMap {
    pub fn slots<'a>(&'a self) -> impl Iterator<Item = &'a Slot> {
        self.values.values().flat_map(|v| v.slots())
    }
}

impl Into<Value> for ValueMap {
    fn into(self) -> Value {
        Value::Map(self)
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq)]
// A way of keeping some value
pub enum Slot {
    // if the value is immediate, it can be stored in the immediate part of an instruction
    Immediate(i16),

    /// If the value is can fit, it may be stored in a register
    Register(Register),

    /// A value stored in memory, relative to the address stored in the 'base' slot
    Indirect {
        base: Box<Slot>,
        offset: i32,
    },
}

impl Into<Value> for Slot {
    fn into(self) -> Value {
        Value::Slot(self)
    }
}
