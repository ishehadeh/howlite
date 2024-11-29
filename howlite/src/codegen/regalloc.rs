use super::riscv::{Register, RegisterSet};

#[derive(Clone, Debug)]
pub struct RegisterAllocator {
    free_registers: Option<RegisterSet>,
}

impl RegisterAllocator {
    // registers we allocate
    const REGISTERS: RegisterSet = RegisterSet::merge([
        RegisterSet::S1,
        RegisterSet::S2,
        RegisterSet::S3,
        RegisterSet::S4,
        RegisterSet::S5,
        RegisterSet::S6,
        RegisterSet::S7,
        RegisterSet::S8,
        RegisterSet::S9,
        RegisterSet::S10,
        RegisterSet::S11,
    ]);

    pub fn new() -> Self {
        Self {
            free_registers: Some(Self::REGISTERS),
        }
    }

    pub fn allocate(&mut self) -> Option<Register> {
        let Some(free_regs) = self.free_registers else {
            return None;
        };
        let next_free = free_regs.first();
        self.free_registers = free_regs.exclude(next_free.into());
        Some(next_free)
    }

    pub fn free(&mut self, reg: Register) {
        match self.free_registers {
            Some(v) => self.free_registers = Some(v.include(reg.into())),
            None => self.free_registers = Some(reg.into()),
        }
    }
}
