use clap::{Parser, ValueEnum};
use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
};

use howlite::{
    riscv::ttcompiler::{Arch, Compiler},
    typecheck::typetree::TypeInterpreter,
    util::ast::Declarations,
};

const DEBUG_PRELUDE: &str = r"
.globl __libc_start_main
__libc_start_main:
    jal main
    jal __hw_breakpoint
    ret

.globl __hw_breakpoint
__hw_breakpoint:
    ebreak
    ret

";

#[derive(Clone, Debug, Copy, PartialEq, Eq, ValueEnum)]
pub enum ArgArch {
    RV64I,
    RV32I,
}

impl Into<Arch> for ArgArch {
    fn into(self) -> Arch {
        match self {
            ArgArch::RV64I => Arch::RV64I,
            ArgArch::RV32I => Arch::RV32I,
        }
    }
}

impl ToString for ArgArch {
    fn to_string(&self) -> String {
        match self {
            ArgArch::RV32I => "rv32i",
            ArgArch::RV64I => "rv64i",
        }
        .to_string()
    }
}

#[derive(Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
pub struct Args {
    /// add debug prelude
    #[arg(short, long, default_value_t = false)]
    debug: bool,

    /// add debug prelude
    #[arg(short, long, default_value_t = ArgArch::RV64I)]
    arch: ArgArch,

    /// file to compile, defaults to stdin
    file: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();
    let mut program = String::new();
    if let Some(file) = args.file {
        program = fs::read_to_string(file).unwrap();
    } else {
        io::stdin().read_to_string(&mut program).unwrap();
    };
    let parse_result: howlite::parser::ParseResult = howlite::parser::parse(&program);

    let decl = Declarations::from_ast(&parse_result.ast);
    let mut type_interp = TypeInterpreter::new(decl);
    let type_tree = type_interp.eval_ast(parse_result.ast);

    let mut compiler = Compiler::new(args.arch.into());
    let result = compiler.eval_ast(&type_tree);
    println!(".text\n{}", result.buffer.get_text());

    if args.debug {
        println!("\n{}", DEBUG_PRELUDE)
    }
}
