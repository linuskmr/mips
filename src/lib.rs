use std::num;
use std::ops::Not;
use std::str::FromStr;
use thiserror::Error;

pub type MachineWord = u32;

// --- Instruction ---

#[derive(Eq, PartialEq, Debug)]
pub enum Instruction {
	InstructionFormatR(InstructionFormatR),
	InstructionFormatI(InstructionI),
	InstructionFormatJ(InstructionJ),
}

#[derive(Error, Debug, PartialEq)]
pub enum ParseInstructionError {
	#[error("Parse opcode error {0}")]
	ParseOpcodeError(#[from] ParseOpcodeError),

	#[error("{0}")]
	RegisterParseError(#[from] RegisterParseError),

	#[error("{0}")]
	FunctionCodeParseError(#[from] FunctionCodeParseError),

	#[error("Parse immediate error: {0}")]
	ParseImmediateError(#[from] num::ParseIntError),
}

impl Into<MachineWord> for Instruction {
	fn into(self) -> MachineWord {
		match self {
			Instruction::InstructionFormatI(format_i) => format_i.into(),
			Instruction::InstructionFormatR(format_r) => format_r.into(),
			Instruction::InstructionFormatJ(format_j) => format_j.into(),
		}
	}
}

impl FromStr for Instruction {
	type Err = ParseInstructionError;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let opcode = s.split_whitespace().next().unwrap_or_default();
		match opcode {
			"add" | "addu" | "and" | "jr" | "nor" | "or" | "sltu" | "sll" | "srl" | "sub" | "subu" => Ok(
				Instruction::InstructionFormatR(InstructionFormatR::from_str(s)?)
			),
			"addi" | "addiu" | "andi" | "beq" | "bne" | "lbu" | "lhu" | "ll" | "lui" | "lw" | "ori" |
			"slti" | "sltiu" | "sb" | "sc" | "sh" | "sw" => Ok(
				Instruction::InstructionFormatI(InstructionI::from_str(s)?)
			),
			"j" | "jal" => Ok(Instruction::InstructionFormatJ(InstructionJ::from_str(s)?)),
			opcode => Err(ParseInstructionError::ParseOpcodeError(ParseOpcodeError::UnknownOpcode(opcode.to_owned()))),
		}
	}
}

// --- Register ---

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
#[repr(u8)]
enum Register {
	Zero = 0,
	At = 1,
	V0 = 2, V1 = 3,
	A0 = 4, A1 = 5, A2 = 6, A3 = 7,
	T0 = 8, T1 = 9, T2 = 10, T3 = 11, T4 = 12, T5 = 13, T6 = 14, T7 = 15,
	S0 = 16, S1 = 17, S2 = 18, S3 = 19, S4 = 20, S5 = 21, S6 = 22, S7 = 23,
	T8 = 24, T9 = 25,
	K0 = 26, K1 = 27,
	Gp = 28,
	Sp = 29,
	Fp = 30,
	Ra = 31,
}

#[derive(Error, Debug, PartialEq)]
pub enum RegisterParseError {
	#[error("Unknown register name {0}")]
	UnknownRegisterName(String),

	#[error("Failed to parse register as integer {0}")]
	NumberParseError(#[from] num::ParseIntError),

	#[error("Illegal register number {0}")]
	IllegalRegisterNumber(u8),

	#[error("Unknown register specifier {0}")]
	InvalidRegisterSpecifier(String),
}

impl FromStr for Register {
	type Err = RegisterParseError;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s {
			register if register.starts_with('$') => match &register[1..] {
				"zero" => Ok(Register::Zero),
				"at" => Ok(Register::At),
				"v0" => Ok(Register::V0),
				"v1" => Ok(Register::V1),
				"a0" => Ok(Register::A0),
				"a1" => Ok(Register::A1),
				"a2" => Ok(Register::A2),
				"a3" => Ok(Register::A3),
				"t0" => Ok(Register::T0),
				"t1" => Ok(Register::T1),
				"t2" => Ok(Register::T2),
				"t3" => Ok(Register::T3),
				"t4" => Ok(Register::T4),
				"t5" => Ok(Register::T5),
				"t6" => Ok(Register::T6),
				"t7" => Ok(Register::T7),
				"t8" => Ok(Register::T8),
				"t9" => Ok(Register::T9),
				"s0" => Ok(Register::S0),
				"s1" => Ok(Register::S1),
				"s2" => Ok(Register::S2),
				"s3" => Ok(Register::S3),
				"s4" => Ok(Register::S4),
				"s5" => Ok(Register::S5),
				"s6" => Ok(Register::S6),
				"s7" => Ok(Register::S7),
				"k0" => Ok(Register::K0),
				"k1" => Ok(Register::K1),
				"gp" => Ok(Register::Gp),
				"sp" => Ok(Register::Sp),
				"fp" => Ok(Register::Fp),
				"ra" => Ok(Register::Ra),
				register_name => Err(RegisterParseError::UnknownRegisterName(register_name.to_owned()))
			},
			s if s.starts_with("0x") => {
				let register_number = s.parse()?;
				if (0..=31).contains(&register_number).not() {
					return Err(RegisterParseError::IllegalRegisterNumber(register_number));
				}
				Ok(Register::A0) // TODO: TryFromPrimitive
			}
			register => Err(RegisterParseError::InvalidRegisterSpecifier(register.to_owned())),
		}
	}
}

#[cfg(test)]
mod tests {
	use std::str::FromStr;
	use crate::Register;

	#[test]
	fn register_from_string() {
		assert_eq!(Register::from_str("$a0"), Ok(Register::A0));
	}
}

// --- Opcodes ---

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
#[repr(u8)]
pub enum OpcodeI {
	AddImmediate = 0x8,
	AddImmediateUnsigned = 0x9,
	AndImmediate = 0xc,
	BranchOnEqual = 0x4,
	BranchOnNotEqual = 0x5,
	LoadByteUnsigned = 0x24,
	LoadHalfwordUnsigned = 0x25,
	LoadLinked = 0x30,
	LoadUpperImmediate = 0xf,
	LoadWord = 0x23,
	OrImmediate = 0xd,
	SetLessThanImmediate = 0xa,
	SetLessThanImmediateUnsigned = 0xb,
	StoreByte = 0x28,
	StoreConditional = 0x38,
	StoreHalfword = 0x29,
	StoreWord = 0x2b,
}

impl FromStr for OpcodeI {
	type Err = ParseOpcodeError;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s {
			"addi" => Ok(OpcodeI::AddImmediate),
			"addiu" => Ok(OpcodeI::AddImmediateUnsigned),
			"andi" => Ok(OpcodeI::AndImmediate),
			"beq" => Ok(OpcodeI::BranchOnEqual),
			"bne" => Ok(OpcodeI::BranchOnNotEqual),
			"lbu" => Ok(OpcodeI::LoadByteUnsigned),
			"lhu" => Ok(OpcodeI::LoadHalfwordUnsigned),
			"ll" => Ok(OpcodeI::LoadLinked),
			"lui" => Ok(OpcodeI::LoadUpperImmediate),
			"lw" => Ok(OpcodeI::LoadWord),
			"ori" => Ok(OpcodeI::OrImmediate),
			"slti" => Ok(OpcodeI::SetLessThanImmediate),
			"sltiu" => Ok(OpcodeI::SetLessThanImmediateUnsigned),
			"sb" => Ok(OpcodeI::StoreByte),
			"sc" => Ok(OpcodeI::StoreConditional),
			"sh" => Ok(OpcodeI::StoreHalfword),
			"sw" => Ok(OpcodeI::StoreWord),
			_ => Err(ParseOpcodeError::UnknownOpcode(s.to_owned())),
		}
	}
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
#[repr(u8)]
pub enum OpcodeJ {
	Jump = 0x2,
	JumpAndLink = 0x3,
}

impl FromStr for OpcodeJ {
	type Err = ParseOpcodeError;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s {
			"j" => Ok(OpcodeJ::Jump),
			"jal" => Ok(OpcodeJ::JumpAndLink),
			_ => Err(ParseOpcodeError::UnknownOpcode(s.to_owned())),
		}
	}
}

#[derive(Debug, PartialEq, Eq, Error)]
pub enum ParseOpcodeError {
	#[error("Unknown opcode {0}")]
	UnknownOpcode(String),
}

// --- FunctionCode ---

#[derive(Eq, PartialEq, Debug)]
enum FunctionCode {
	Add = 0x20,
	AddUnsigned = 0x21,
	And = 0x24,
	JumpRegister = 0x08,
	Nor = 0x27,
	Or = 0x25,
	SetLessThan = 0x2a,
	SetLessThanUnsigned = 0x2b,
	ShiftLeftLogical = 0x00,
	ShiftRightLogical = 0x02,
	Subtract = 0x22,
	SubtractUnsigned = 0x23,
}

#[derive(Error, Debug, PartialEq)]
pub enum FunctionCodeParseError {
	#[error("Unknown instruction {0} for function unit")]
	UnknownInstruction(String),
}

impl FromStr for FunctionCode {
	type Err = FunctionCodeParseError;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s {
			"add" => Ok(FunctionCode::Add),
			"addu" => Ok(FunctionCode::AddUnsigned),
			"and" => Ok(FunctionCode::And),
			"jr" => Ok(FunctionCode::JumpRegister),
			"nor" => Ok(FunctionCode::Nor),
			"or" => Ok(FunctionCode::Or),
			"slt" => Ok(FunctionCode::SetLessThan),
			"sltu" => Ok(FunctionCode::SetLessThanUnsigned),
			"sll" => Ok(FunctionCode::ShiftLeftLogical),
			"srl" => Ok(FunctionCode::ShiftRightLogical),
			"sub" => Ok(FunctionCode::Subtract),
			"subu" => Ok(FunctionCode::SubtractUnsigned),
			opcode => Err(FunctionCodeParseError::UnknownInstruction(opcode.to_owned())),
		}
	}
}

// --- InstructionFormatR ---

#[derive(Eq, PartialEq, Debug)]
pub struct InstructionFormatR {
	// Opcode is always 0
	rs: Register,
	rt: Register,
	rd: Register,
	shamt: u8,
	funct: FunctionCode,
}

impl FromStr for InstructionFormatR {
	type Err = ParseInstructionError;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let instruction_parts = s.split_whitespace().collect::<Vec<_>>();
		let rs = instruction_parts[2].parse()?;
		let rt = instruction_parts[3].parse()?;
		let rd = instruction_parts[1].parse()?;
		let funct = FunctionCode::from_str(&instruction_parts[0])?;
		Ok(InstructionFormatR { rs, rt, rd, funct, shamt: 0 })
	}
}

impl Into<MachineWord> for InstructionFormatR {
	fn into(self) -> MachineWord {
		let mut machine_word = 0_u32;
		// Opcode is always 0
		machine_word |= (self.rs as u32) << 21;
		machine_word |= (self.rt as u32) << 16;
		machine_word |= (self.rd as u32) << 11;
		// Shamt is always 0
		machine_word |= self.funct as u32;
		machine_word
	}
}

// --- InstructionFormatI ---

#[derive(Eq, PartialEq, Debug)]
pub struct InstructionI {
	opcode: OpcodeI,
	rs: Register,
	rt: Register,
	immediate: i16,
}

impl FromStr for InstructionI {
	type Err = ParseInstructionError;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let instruction_parts = s.split_whitespace().collect::<Vec<_>>();
		let opcode = OpcodeI::from_str(&instruction_parts[0])?;
		let rs = instruction_parts[2].parse()?;
		let rt = instruction_parts[1].parse()?;
		let immediate = instruction_parts[3].parse()?;
		Ok(InstructionI { opcode, rs, rt, immediate })
	}
}

impl Into<MachineWord> for InstructionI {
	fn into(self) -> MachineWord {
		let mut machine_word = 0_u32;
		machine_word |= (self.opcode as u32) << 26;
		machine_word |= (self.rs as u32) << 21;
		machine_word |= (self.rt as u32) << 16;
		machine_word |= self.immediate as u32;
		machine_word
	}
}

// --- InstructionFormatJ ---

#[derive(Eq, PartialEq, Debug)]
pub struct InstructionJ {
	opcode: OpcodeJ,
	address: u32,
}

impl FromStr for InstructionJ {
	type Err = ParseInstructionError;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let instruction_parts = s.split_whitespace().collect::<Vec<_>>();
		let opcode = OpcodeJ::from_str(&instruction_parts[0])?;
		let address = instruction_parts[1].parse()?;
		Ok(InstructionJ { opcode, address })
	}
}

impl Into<MachineWord> for InstructionJ {
	fn into(self) -> MachineWord {
		let mut machine_word = 0_u32;
		machine_word |= (self.opcode as u32) << 26;
		machine_word |= self.address as u32;
		machine_word
	}
}

pub struct Instance {
	pub registers: [u32; 32],
	pub mem: Vec<u8>,
	pub code: Vec<Instruction>,
	pub pc: usize,
}

impl Instance {
	pub fn new(code: Vec<Instruction>) -> Self {
		Self {
			registers: [0u32; 32],
			mem: vec![],
			code,
			pc: 0
		}
	}

	pub fn exec(&mut self) {
		while self.pc < self.code.len() {
			let instruction = &self.code[self.pc];

			match instruction {
				Instruction::InstructionFormatJ(instruction) => match instruction.opcode {
					OpcodeJ::Jump => self.pc = instruction.address as usize,
					OpcodeJ::JumpAndLink => {
						self.registers[Register::Ra as usize] = self.pc as u32;
						self.pc = instruction.address as usize;
					}
				},
				Instruction::InstructionFormatI(instruction) => {
					let rt = instruction.rt as usize;
					let rs = instruction.rs as usize;
					let immediate = instruction.immediate as u32;
					match instruction.opcode {
						OpcodeI::AddImmediate => {
							self.registers[rt] = self.registers[rs] + immediate;
						},
						OpcodeI::AddImmediateUnsigned => {
							self.registers[rt] = self.registers[rs] + immediate;
						}

						OpcodeI::AndImmediate => {
							self.registers[rt] = self.registers[rs] | immediate;
						},
						OpcodeI::BranchOnEqual => {
							if self.registers[rs] == self.registers[rt] {
								self.pc = self.pc + 1 + (immediate as usize);
							}
						},
						OpcodeI::BranchOnNotEqual => {
							if self.registers[rs] != self.registers[rt] {
								self.pc = self.pc + 1 + (immediate as usize);
							}
						},
						OpcodeI::LoadByteUnsigned => {
							let addr = (self.registers[rs] + (immediate as u32)) as usize;
							self.registers[rt] = self.mem[addr] as u32;
						},
						OpcodeI::LoadHalfwordUnsigned => {
							let addr = (self.registers[rs] + (immediate as u32)) as usize;
							let mut mem_value = [0u8; 2];
							mem_value.copy_from_slice(&self.mem[addr..addr+2]);
							self.registers[rt] = u16::from_be_bytes(mem_value) as u32;
						},
						OpcodeI::LoadLinked => unimplemented!(),
						OpcodeI::LoadUpperImmediate => {
							let addr = (self.registers[rs] + (immediate as u32)) as usize;
							let mut mem_value = [0u8; 2];
							mem_value.copy_from_slice(&self.mem[addr..addr+2]);
							let mem_value = u16::from_be_bytes(mem_value) as u32;
							self.registers[rt] = mem_value << 16;
						},
						OpcodeI::LoadWord => {
							let addr = (self.registers[rs] + (immediate as u32)) as usize;
							let mut mem_value = [0u8; 4];
							mem_value.copy_from_slice(&self.mem[addr..addr+4]);
							self.registers[rt] = u32::from_be_bytes(mem_value);
						},
						OpcodeI::OrImmediate => {
							self.registers[rt] = self.registers[rs] | (immediate as u32)
						},
						OpcodeI::SetLessThanImmediate => {
							let comparison_result = if (self.registers[rs] as i32) < (immediate as i32) { 1 } else { 0 };
							self.registers[rt] = comparison_result;
						},
						OpcodeI::SetLessThanImmediateUnsigned => {
							let comparison_result = if self.registers[rs] < (immediate as u32) { 1 } else { 0 };
							self.registers[rt] = comparison_result;
						},
						OpcodeI::StoreByte => {
							let addr = (self.registers[rs] + (immediate as u32)) as usize;
							self.mem[addr] = self.registers[rt] as u8;
						},
						OpcodeI::StoreConditional => unimplemented!(),
						OpcodeI::StoreHalfword => {
							let addr = (self.registers[rs] + (immediate as u32)) as usize;
							self.mem[addr..addr+2].copy_from_slice(&(self.registers[rt] as u16).to_le_bytes());
						},
						OpcodeI::StoreWord => {
							let addr = (self.registers[rs] + (immediate as u32)) as usize;
							self.mem[addr..addr+4].copy_from_slice(&self.registers[rt].to_le_bytes());
						},
					}
					self.pc += 1;
				},
				Instruction::InstructionFormatR(instruction) => {
					let rd = instruction.rd as usize;
					let rs = instruction.rs as usize;
					let rt = instruction.rt as usize;
					let shamt = instruction.shamt;
					match instruction.funct {
						FunctionCode::Add => self.registers[rd] = (self.registers[rs] as i16 + self.registers[rt] as i16) as u32,
						FunctionCode::AddUnsigned => self.registers[rd] = self.registers[rs] + self.registers[rt],
						FunctionCode::And => self.registers[rd] = self.registers[rs] & self.registers[rt],
						FunctionCode::JumpRegister => {}
						FunctionCode::Nor => self.registers[rd] = self.registers[rs] | self.registers[rt],
						FunctionCode::Or => self.registers[rd] = self.registers[rs] | self.registers[rt],
						FunctionCode::SetLessThan => self.registers[rd] = if (self.registers[rs] as i16) < (self.registers[rt] as i16) { 1 } else { 0 },
						FunctionCode::SetLessThanUnsigned => self.registers[rd] = if self.registers[rs] < self.registers[rt] { 1 } else { 0 },
						FunctionCode::ShiftLeftLogical => self.registers[rd] = self.registers[rs] << shamt,
						FunctionCode::ShiftRightLogical => self.registers[rd] = self.registers[rs] >> shamt,
						FunctionCode::Subtract => self.registers[rd] = (self.registers[rs] as i16 - self.registers[rt] as i16) as u32,
						FunctionCode::SubtractUnsigned => self.registers[rd] = self.registers[rs] - self.registers[rt],
					}
					self.pc += 1;
				},
			}
		}
	}
}