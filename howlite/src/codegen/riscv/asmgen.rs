use std::collections::HashSet;

use super::Register;
use std::fmt::Write;

#[derive(Clone, Debug, Default)]
pub struct AssemblyWriter {
    reads: HashSet<Register>,
    writes: HashSet<Register>,
    buffer: String,
}

macro_rules! instr_arith {
    ($($instr:ident),*) => {
        $(pub fn $instr(&mut self, rd: Register, rs1: Register, rs2: Register) {
            self.reads.insert(rs1);
            self.reads.insert(rs2);
            self.writes.insert(rd);
            writeln!(
                &mut self.buffer,
                "{} {}, {}, {}",
                stringify!($instr),
                rd.to_abi_name(),
                rs1.to_abi_name(),
                rs2.to_abi_name()
            ).expect("write failed")
        })*
    };

    (imm $($instr:ident),*) => {
        $(pub fn $instr(&mut self, rd: Register, rs1: Register, imm: i16) {
            assert!(imm < 2048, "immediate in arithmetic instructions may not exceed 11bits");
            assert!(imm > -2048, "immediate in arithmetic instructions may not exceed 11bits");

            self.reads.insert(rs1);
            self.writes.insert(rd);
            writeln!(
                &mut self.buffer,
                "{} {}, {}, {}",
                stringify!($instr),
                rd.to_abi_name(),
                rs1.to_abi_name(),
                imm
            ).expect("write failed")
        })*
    };
}

impl AssemblyWriter {
    pub fn from<I: Iterator<Item = AssemblyWriter>>(children: I) -> AssemblyWriter {
        children
            .reduce(|mut p, n| {
                p.include(n);
                p
            })
            .unwrap_or(AssemblyWriter::new())
    }

    pub fn new() -> AssemblyWriter {
        AssemblyWriter::default()
    }

    pub fn get_text(&self) -> &str {
        &self.buffer
    }

    pub fn get_regs_read(&self) -> &HashSet<Register> {
        &self.reads
    }

    pub fn get_regs_write(&self) -> &HashSet<Register> {
        &self.writes
    }

    pub fn include_ref(&mut self, other: &AssemblyWriter) {
        self.reads.extend(other.reads.iter());
        self.writes.extend(other.writes.iter());
        self.buffer.push_str(&other.buffer)
    }

    pub fn include(&mut self, other: AssemblyWriter) {
        self.reads.extend(other.reads);
        self.writes.extend(other.writes);
        self.buffer.push_str(&other.buffer)
    }

    instr_arith!(add, sub, mul, div);
    instr_arith!(imm addi);

    pub fn sd(&mut self, rs2: Register, offset: i16, rs1: Register) {
        assert!(
            offset < 2047,
            "immediate in arithmetic instructions may not exceed 11bits"
        );
        assert!(
            offset > -2048,
            "immediate in arithmetic instructions may not exceed 11bits"
        );
        self.reads.insert(rs1);
        self.reads.insert(rs2);
        writeln!(
            &mut self.buffer,
            "sd {}, {}({})",
            rs2.to_abi_name(),
            offset,
            rs1.to_abi_name(),
        )
        .expect("write failed")
    }

    pub fn sw(&mut self, rs2: Register, offset: i16, rs1: Register) {
        assert!(
            offset < 2047,
            "immediate in arithmetic instructions may not exceed 11bits"
        );
        assert!(
            offset > -2048,
            "immediate in arithmetic instructions may not exceed 11bits"
        );
        self.reads.insert(rs1);
        self.reads.insert(rs2);
        writeln!(
            &mut self.buffer,
            "sw {}, {}({})",
            rs2.to_abi_name(),
            offset,
            rs1.to_abi_name(),
        )
        .expect("write failed")
    }

    pub fn sb(&mut self, rs2: Register, offset: i16, rs1: Register) {
        assert!(
            offset < 2047,
            "immediate in arithmetic instructions may not exceed 11bits"
        );
        assert!(
            offset > -2048,
            "immediate in arithmetic instructions may not exceed 11bits"
        );
        self.reads.insert(rs1);
        self.reads.insert(rs2);
        writeln!(
            &mut self.buffer,
            "sb {}, {}({})",
            rs2.to_abi_name(),
            offset,
            rs1.to_abi_name(),
        )
        .expect("write failed")
    }

    pub fn ld(&mut self, rd: Register, offset: i16, rs1: Register) {
        assert!(
            offset < 2047,
            "immediate in arithmetic instructions may not exceed 11bits"
        );
        assert!(
            offset > -2048,
            "immediate in arithmetic instructions may not exceed 11bits"
        );
        self.reads.insert(rs1);
        self.writes.insert(rd);
        writeln!(
            &mut self.buffer,
            "ld {}, {}({})",
            rd.to_abi_name(),
            offset,
            rs1.to_abi_name(),
        )
        .expect("write failed")
    }

    pub fn lw(&mut self, rd: Register, offset: i16, rs1: Register) {
        assert!(
            offset < 2047,
            "immediate in arithmetic instructions may not exceed 11bits"
        );
        assert!(
            offset > -2048,
            "immediate in arithmetic instructions may not exceed 11bits"
        );
        self.reads.insert(rs1);
        self.writes.insert(rd);
        writeln!(
            &mut self.buffer,
            "lw {}, {}({})",
            rd.to_abi_name(),
            offset,
            rs1.to_abi_name(),
        )
        .expect("write failed")
    }

    pub fn li(&mut self, rd: Register, imm: u64) {
        self.writes.insert(rd);
        writeln!(&mut self.buffer, "li {}, {}", rd.to_abi_name(), imm,).expect("write failed")
    }

    pub fn mv(&mut self, rd: Register, rs1: Register) {
        self.writes.insert(rd);
        self.writes.insert(rs1);
        writeln!(
            &mut self.buffer,
            "mv {}, {}",
            rd.to_abi_name(),
            rs1.to_abi_name()
        )
        .expect("write failed")
    }

    pub fn seqz(&mut self, rd: Register, rs1: Register) {
        self.writes.insert(rd);
        self.reads.insert(rs1);
        writeln!(
            &mut self.buffer,
            "seqz {}, {}",
            rd.to_abi_name(),
            rs1.to_abi_name(),
        )
        .expect("write failed")
    }

    pub fn snez(&mut self, rd: Register, rs1: Register) {
        self.writes.insert(rd);
        self.reads.insert(rs1);
        writeln!(
            &mut self.buffer,
            "snez {}, {}",
            rd.to_abi_name(),
            rs1.to_abi_name(),
        )
        .expect("write failed")
    }

    pub fn slt(&mut self, rd: Register, rs1: Register, rs2: Register) {
        self.writes.insert(rd);
        self.reads.insert(rs1);
        self.reads.insert(rs2);
        writeln!(
            &mut self.buffer,
            "slt {}, {}, {}",
            rd.to_abi_name(),
            rs1.to_abi_name(),
            rs2.to_abi_name(),
        )
        .expect("write failed")
    }

    pub fn sgt(&mut self, rd: Register, rs1: Register, rs2: Register) {
        self.writes.insert(rd);
        self.reads.insert(rs1);
        self.reads.insert(rs2);
        writeln!(
            &mut self.buffer,
            "sgt {}, {}, {}",
            rd.to_abi_name(),
            rs1.to_abi_name(),
            rs2.to_abi_name(),
        )
        .expect("write failed")
    }

    pub fn beq(&mut self, rs1: Register, rs2: Register, label: &str) {
        self.reads.insert(rs1);
        self.reads.insert(rs2);
        writeln!(
            &mut self.buffer,
            "beq {}, {}, {}",
            rs1.to_abi_name(),
            rs2.to_abi_name(),
            label,
        )
        .expect("write failed")
    }

    pub fn j(&mut self, label: &str) {
        writeln!(&mut self.buffer, "j {}", label).expect("write failed")
    }

    pub fn call(&mut self, label: &str) {
        writeln!(&mut self.buffer, "call {}", label).expect("write failed")
    }

    pub fn ret(&mut self) {
        writeln!(&mut self.buffer, "ret").expect("write failed")
    }

    pub fn jr(&mut self, rs1: Register) {
        self.reads.insert(rs1);
        writeln!(&mut self.buffer, "jr {}", rs1.to_abi_name()).expect("write failed")
    }

    pub fn jal(&mut self, label: &str) {
        writeln!(&mut self.buffer, "jal {}", label).expect("write failed")
    }

    pub fn label(&mut self, label: &str) {
        writeln!(&mut self.buffer, "{}:", label).expect("write failed")
    }

    pub fn d_globl(&mut self, label: &str) {
        writeln!(&mut self.buffer, ".globl {}", label).expect("write failed")
    }
}
