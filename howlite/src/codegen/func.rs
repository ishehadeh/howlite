use howlite_syntax::ast;

use crate::langctx::lexicalctx::LexicalContext;

use super::{
    riscv::{asmgen::AssemblyWriter, Register, RegisterSet, Slot, Value},
    CodeGen, CodeGenCtx,
};

impl CodeGen for ast::DefFunc {
    fn codegen(&self, ctx: &CodeGenCtx, lexicalctx: &LexicalContext) -> Value {
        let arg_regs = RegisterSet::from([
            Register::A0,
            Register::A1,
            Register::A2,
            Register::A3,
            Register::A4,
            Register::A5,
            Register::A6,
            Register::A7,
        ]);

        // ctx.buffer.d_globl(&self.name);
        // ctx.buffer.label(&self.name);

        let name_sym = lexicalctx.sym_intern(&self.name);
        let def = lexicalctx
            .func_get(name_sym)
            .expect("could not get function definition");

        for param in def.params {
            let size = param.sizeof();
            let val = if size <= 8 {
                Value::Slot(Slot::Register(arg_regs.pop_front_mut().unwrap()));
            } else if size <= 16 {
                Value::Tuple(vec![
                    Slot::Register(arg_regs.pop_front_mut().unwrap()),
                    Slot::Register(arg_regs.pop_front_mut().unwrap()),
                ]);
            } else {
                Value::Slot(Slot::Indirect {
                    base: Box::new(Slot::Register(arg_regs.pop_front_mut().unwrap())),
                    offset: 0,
                    size,
                });
            };
        }

        for (arg_reg_i, p) in f.params.iter().enumerate() {
            let arg_reg_slot = Slot::Register(arg_regs[arg_reg_i]);
            let arg_ty = p.xdata().current_type();
            let value = match arg_ty {
                TypeInfo::Unit => continue,
                TypeInfo::Scalar(_) => arg_reg_slot.into(),
                TypeInfo::Union(_) => todo!("union function args"),
                TypeInfo::Record(_) | TypeInfo::Array(_) => {
                    self.type_info_to_value_memory(&mut buffer, arg_reg_slot, 0, arg_ty)
                }
                TypeInfo::TyRef(_) => panic!("Reference type found during compilation, these should be resolved by the typechecker"),
            };
            self.scopes.set(&p.name, value);
        }
        let res = self.eval_ast(f.body.as_ref());

        let mut prolog = AssemblyWriter::new();
        let mut epilog = AssemblyWriter::new();

        let mut mutated_callee_saved_regs: Vec<_> = res
            .buffer
            .get_regs_write()
            .iter()
            .copied()
            .filter(|r| r.is_callee_saved())
            .collect();
        // this is literally just so we can get nice diffs between asm outputs of compiler versions
        // it should be optional or removed
        mutated_callee_saved_regs.sort();

        // TODO: only push these if a child function is called
        mutated_callee_saved_regs.push(Register::Ra);

        for r in mutated_callee_saved_regs {
            // add 4 since index is negative and store/load read upwards
            let offset = self.stack.alloc(REGISTER_WIDTH) + REGISTER_WIDTH as i32;

            match self.target {
                Arch::RV64I => {
                    prolog.sd(r, -offset as i16, Register::Fp);
                    epilog.ld(r, -offset as i16, Register::Fp);
                }
                Arch::RV32I => {
                    prolog.sw(r, -offset as i16, Register::Fp);
                    epilog.lw(r, -offset as i16, Register::Fp);
                }
            }
        }

        let fp_stack_offset = self.stack.size() as i32 - self.stack.alloc(REGISTER_WIDTH);

        buffer.addi(Register::Sp, Register::Sp, -(self.stack.size() as i16));
        match self.target {
            Arch::RV64I => {
                buffer.sd(Register::Fp, fp_stack_offset as i16, Register::Sp);
            }
            Arch::RV32I => {
                buffer.sw(Register::Fp, fp_stack_offset as i16, Register::Sp);
            }
        }
        buffer.addi(Register::Fp, Register::Sp, self.stack.size() as i16);
        buffer.include(prolog);

        buffer.include(res.buffer);
        if let Some(Value::Slot(out_slot)) = res.result {
            self.load_register(&mut buffer, Register::A0, &out_slot);
        }

        buffer.include(epilog);

        match self.target {
            Arch::RV64I => {
                buffer.ld(Register::Fp, fp_stack_offset as i16, Register::Sp);
            }
            Arch::RV32I => {
                buffer.lw(Register::Fp, fp_stack_offset as i16, Register::Sp);
            }
        }

        buffer.addi(Register::Sp, Register::Sp, self.stack.size() as i16);

        buffer.jr(Register::Ra);

        self.scopes.pop();
        self.stack.reset();
        EvalResult {
            result: Some(Slot::Register(Register::A0).into()),
            buffer,
        }
    }
}
