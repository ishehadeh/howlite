use howlite_syntax::{ast, tree::DefaultLinearTreeId};
use riscv::{asmgen::AssemblyWriter, RegisterSet, Slot, Value};
use stack_state::StackState;

use crate::langctx::ScopeId;

pub mod riscv;
pub mod stack_state;

pub trait CodeGen {
    fn codegen(&self, ctx: &mut CodeGenCtx);
}

pub struct CodeGenCtx<'a> {
    pub machine: &'a StackState,
    pub asm: AssemblyWriter,
    pub free_registers: RegisterSet,
    pub scope: ScopeId,
    pub node: DefaultLinearTreeId,
    pub result: Option<Value>,
}

impl<'a> CodeGenCtx<'a> {
    pub fn alloc_slot(&mut self, size: usize) -> Slot {}
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
