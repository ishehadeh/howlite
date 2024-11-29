use howlite_syntax::{ast, tree::DefaultLinearTreeId};
use regalloc::RegisterAllocator;
use riscv::{asmgen::AssemblyWriter, Register, Slot, Value};
use stack_state::StackState;

use crate::langctx::ScopeId;

pub mod regalloc;
pub mod riscv;
pub mod stack_state;

pub trait CodeGen {
    fn codegen(&self, ctx: &mut CodeGenCtx);
}

pub struct CodeGenCtx {
    pub stack: StackState,
    pub regs: RegisterAllocator,

    pub asm: AssemblyWriter,
    pub scope: ScopeId,
    pub node: DefaultLinearTreeId,
    pub result: Option<Value>,
}

impl CodeGenCtx {
    pub fn alloc_slot(&mut self, size: usize) -> Slot {
        if size <= 8 {
            if let Some(reg) = self.regs.allocate() {
                return Slot::Register(reg);
            }
        }
        let stack_offset = self.stack.alloc(size);
        Slot::Indirect {
            base: Box::new(Slot::Register(Register::Sp)),
            offset: -stack_offset,
        }
    }
    pub fn write_slot(&mut self) -> Slot {
        todo!()
    }
}

impl CodeGen for ast::LiteralInteger {
    fn codegen(&self, ctx: &mut CodeGenCtx) {
        // type checker should have alreday verified this can fit in i128
        ctx.result = Some(Value::Literal);
    }
}

impl CodeGen for ast::LiteralChar {
    fn codegen(&self, ctx: &mut CodeGenCtx) {
        ctx.result = Some(Value::Literal);
    }
}

impl CodeGen for ast::LiteralArray {
    fn codegen(&self, ctx: &mut CodeGenCtx) {
        ctx.result = Some(Value::Literal)
    }
}

impl CodeGen for ast::LiteralStruct {
    fn codegen(&self, ctx: &mut CodeGenCtx) {
        ctx.result = Some(Value::Literal)
    }
}
