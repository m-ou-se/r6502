use bitflags::bitflags;

/// A 6502 CPU.
#[derive(Clone)]
pub struct Cpu {
	/// The accumulator register.
	pub a: u8,
	/// General purpose register X.
	pub x: u8,
	/// General purpose register Y.
	pub y: u8,
	/// The stack pointer.
	pub s: u8,
	/// The processor status register.
	pub status: Status,
	/// The program counter.
	pub pc: u16,
	/// The memory.
	pub mem: [u8; 0x10000],
}

bitflags! {
	/// The [CPU][Cpu]'s [processor status register][Cpu::status].
	pub struct Status: u8 {
		/// The carry or not-borrow flag.
		const C = 0b00000001;
		/// The zero flag.
		const Z = 0b00000010;
		/// The interrupt disable flag.
		const I = 0b00000100;
		/// The decimal mode flag.
		const D = 0b00001000;
		/// The overflow flag.
		const V = 0b01000000;
		/// The negative flag.
		const N = 0b10000000;
	}
}

/// An operand to an instruction which may modify it.
#[derive(Debug)]
pub enum MutOp<'a> {
	/// The operand is the [accumulator register][Cpu::a].
	Acc,
	/// The operand is the [X register][Cpu::x].
	X,
	/// The operand is the [Y register][Cpu::y].
	Y,
	/// The operand is the [stack pointer register][Cpu::s].
	Sp,
	/// The operand is located at the (one byte) address stored right after the instruction.
	Zp,
	/// Same as [`Zp`][MutOp::Zp], but [X][Cpu::x] is added to the address before using.
	///
	/// The address wraps around at 256.
	Zpx,
	/// Same as [`Zp`][MutOp::Zp], but [Y][Cpu::y] is added to the address before using.
	///
	/// The address wraps around at 256.
	Zpy,
	/// The operand is located at the (two byte) address stored right after the instruction.
	Abs,
	/// Same as [`Abs`][MutOp::Abs], but [X][Cpu::x] is added to the address before using.
	Abx,
	/// Same as [`Abs`][MutOp::Abs], but [Y][Cpu::y] is added to the address before using.
	Aby,
	/// The operand is located at the (two byte) address that's stored where the [`Zpx`][MutOp::Zpx] operand would be.
	Inx,
	/// The operand is located at the (two byte) address that's stored where the [`Zp`][MutOp::Zp] operand would be, plus [`Y`][Cpu::y].
	Iny,
	/// The operand is given by a `&mut u8`.
	Ref(&'a mut u8),
}

impl<'a> From<&'a mut u8> for MutOp<'a> {
	fn from(x: &'a mut u8) -> MutOp<'a> {
		MutOp::Ref(x)
	}
}

/// An operand to an instruction which only reads it.
#[derive(Debug)]
pub enum Op {
	/// The operand is the byte right after the instruction.
	Imm,
	/// The operand is stored in some mutable location.
	///
	/// See [below](#impl) for short-cuts for these values.
	Mut(MutOp<'static>),
	/// The operand is the given constant.
	Const(u8),
}

impl From<u8> for Op {
	fn from(x: u8) -> Op {
		Op::Const(x)
	}
}

#[allow(non_upper_case_globals)]
#[rustfmt::skip]
/// See [`MutOp`] for the definitions of these operands.
impl Op {
	pub const Acc: Op = Op::Mut(MutOp::Acc);
	pub const X:   Op = Op::Mut(MutOp::X);
	pub const Y:   Op = Op::Mut(MutOp::Y);
	pub const Sp:  Op = Op::Mut(MutOp::Sp);
	pub const Zp:  Op = Op::Mut(MutOp::Zp);
	pub const Zpx: Op = Op::Mut(MutOp::Zpx);
	pub const Zpy: Op = Op::Mut(MutOp::Zpy);
	pub const Abs: Op = Op::Mut(MutOp::Abs);
	pub const Abx: Op = Op::Mut(MutOp::Abx);
	pub const Aby: Op = Op::Mut(MutOp::Aby);
	pub const Inx: Op = Op::Mut(MutOp::Inx);
	pub const Iny: Op = Op::Mut(MutOp::Iny);
}

/// An operand to a [non-conditional jump instruction][Cpu::exec_jmp].
pub enum JmpOp {
	/// The (two byte) target address of the jump is stored in right after the instruction.
	Im2,
	/// The (two byte) target address of the jump is stored at the (two byte) address stored right after the instruction.
	Abs,
	/// The target is address is given as a constant.
	Const(u16),
}

impl From<u16> for JmpOp {
	fn from(x: u16) -> JmpOp {
		JmpOp::Const(x)
	}
}

mod debug;

impl Default for Cpu {
	fn default() -> Self {
		Cpu {
			a: 0,
			x: 0,
			y: 0,
			s: 0xFF,
			status: Status::empty(),
			pc: 0,
			mem: [0u8; 0x10000],
		}
	}
}

/// Memory operations.
impl Cpu {
	pub fn write(&mut self, addr: u16, byte: u8) {
		self.mem[usize::from(addr)] = byte;
	}

	pub fn read(&self, addr: u16) -> u8 {
		self.mem[usize::from(addr)]
	}

	pub fn read_word(&self, addr: u16) -> u16 {
		let low = u16::from(self.mem[usize::from(addr)]);
		let high = u16::from(self.mem[usize::from(addr.wrapping_add(1))]);
		high << 8 | low
	}
}

/// Stack operations.
impl Cpu {
	pub fn push(&mut self, byte: u8) {
		self.write(u16::from(self.s) | 0x0100, byte);
		self.s = self.s.wrapping_sub(1);
	}

	pub fn pop(&mut self) -> u8 {
		self.s = self.s.wrapping_add(1);
		self.read(u16::from(self.s) | 0x0100)
	}

	pub fn push_word(&mut self, word: u16) {
		self.push((word >> 8) as u8);
		self.push(word as u8);
	}

	pub fn pop_word(&mut self) -> u16 {
		let low = u16::from(self.pop());
		let high = u16::from(self.pop());
		high << 8 | low
	}
}

/// Operands.
impl Cpu {
	/// Read a byte at the program counter.
	///
	/// Increments the program counter.
	pub fn read_imm(&mut self) -> u8 {
		let addr = self.pc;
		self.pc = self.pc.wrapping_add(1);
		self.read(addr)
	}

	/// Read a word at the program counter.
	///
	/// Increments the program counter by two.
	pub fn read_im2(&mut self) -> u16 {
		let low = u16::from(self.read_imm());
		let high = u16::from(self.read_imm());
		high << 8 | low
	}

	/// Get the value of an operand.
	///
	/// Depending on the type of operand, this might increment the program
	/// counter by one or two.
	pub fn get_op(&mut self, op: impl Into<Op>) -> u8 {
		match op.into() {
			Op::Const(x) => x,
			Op::Imm => self.read_imm(),
			Op::Mut(op) => *self.get_mut_op(op),
		}
	}

	/// Get mutable access to an operand.
	///
	/// Depending on the type of operand, this might increment the program
	/// counter by one or two.
	///
	/// The resulting reference either refers to one of the registers ([the
	/// accumulator][Cpu::a], [X][Cpu::x], [Y][Cpu::y], or [the stack
	/// pointer][Cpu::s]), or somewhere into [the memory][Cpu::mem].
	pub fn get_mut_op<'a>(&'a mut self, op: MutOp<'a>) -> &'a mut u8 {
		match op {
			MutOp::Acc => &mut self.a,
			MutOp::X => &mut self.x,
			MutOp::Y => &mut self.y,
			MutOp::Sp => &mut self.s,
			MutOp::Zp => {
				let addr = self.read_imm();
				&mut self.mem[usize::from(addr)]
			}
			MutOp::Zpx => {
				let addr = self.read_imm().wrapping_add(self.x);
				&mut self.mem[usize::from(addr)]
			}
			MutOp::Zpy => {
				let addr = self.read_imm().wrapping_add(self.y);
				&mut self.mem[usize::from(addr)]
			}
			MutOp::Abs => {
				let addr = self.read_im2();
				&mut self.mem[usize::from(addr)]
			}
			MutOp::Abx => {
				let addr = self.read_im2().wrapping_add(u16::from(self.x));
				&mut self.mem[usize::from(addr)]
			}
			MutOp::Aby => {
				let addr = self.read_im2().wrapping_add(u16::from(self.y));
				&mut self.mem[usize::from(addr)]
			}
			MutOp::Inx => {
				let imm = self.read_imm();
				let addr = self.read_word(u16::from(imm.wrapping_add(self.x)));
				&mut self.mem[usize::from(addr)]
			}
			MutOp::Iny => {
				let imm = self.read_imm();
				let addr = self
					.read_word(u16::from(imm))
					.wrapping_add(u16::from(self.y));
				&mut self.mem[usize::from(addr)]
			}
			MutOp::Ref(x) => x,
		}
	}

	pub fn get_target(&mut self, op: impl Into<JmpOp>) -> u16 {
		match op.into() {
			JmpOp::Const(x) => x,
			JmpOp::Im2 => self.read_im2(),
			JmpOp::Abs => {
				let imm = self.read_im2();
				self.read_word(imm)
			}
		}
	}
}

/// Execution.
impl Cpu {
	pub fn init(&mut self) {
		self.pc = self.read_word(0xFFFC);
	}

	pub fn step(&mut self) {
		let instruction = self.read_imm();
		self.exec_instruction(instruction);
	}

	#[rustfmt::skip]
	pub fn exec_instruction(&mut self, instruction: u8) {
		match instruction {
			// Subroutine instructions:
			0x00 => self.exec_brk(),
			0x20 => self.exec_jsr(),
			0x40 => self.exec_rti(),
			0x60 => self.exec_rts(),

			// Unconditional jumps:
			0x4C => self.exec_jmp(JmpOp::Im2),
			0x6C => self.exec_jmp(JmpOp::Abs),

			// Conditonal branches:
			0x10 => self.exec_branch(Status::N, false), // BPL
			0x30 => self.exec_branch(Status::N, true ), // BMI
			0x50 => self.exec_branch(Status::V, false), // BVC
			0x70 => self.exec_branch(Status::V, true ), // BVS
			0x90 => self.exec_branch(Status::C, false), // BCC
			0xB0 => self.exec_branch(Status::C, true ), // BCS
			0xD0 => self.exec_branch(Status::Z, false), // BNE
			0xF0 => self.exec_branch(Status::Z, true ), // BEQ

			// Flag instructions:
			0x18 => self.status.remove(Status::C), // CLC
			0x38 => self.status.insert(Status::C), // SEC
			0x58 => self.status.remove(Status::I), // CLI
			0x78 => self.status.insert(Status::I), // SEI
			0xB8 => self.status.remove(Status::V), // CLV
			0xD8 => self.status.remove(Status::D), // CLD
			0xF8 => self.status.insert(Status::D), // SED

			// Stack instructions:
			0x08 => self.exec_php(),
			0x28 => self.exec_plp(),
			0x48 => self.exec_pha(),
			0x68 => self.exec_pla(),

			// Increment and decrement instructions:
			0xE6 => self.exec_inc(MutOp::Zp),
			0xEE => self.exec_inc(MutOp::Abs),
			0xF6 => self.exec_inc(MutOp::Zpx),
			0xFE => self.exec_inc(MutOp::Abx),
			0xE8 => self.exec_inc(MutOp::X), // aka INX
			0xC8 => self.exec_inc(MutOp::Y), // aka INY
			0xC6 => self.exec_dec(MutOp::Zp),
			0xCE => self.exec_dec(MutOp::Abs),
			0xD6 => self.exec_dec(MutOp::Zpx),
			0xDE => self.exec_dec(MutOp::Abx),
			0xCA => self.exec_dec(MutOp::X), // aka DEX
			0x88 => self.exec_dec(MutOp::Y), // aka DEY

			// Store instructions:
			0x81 => self.exec_sta(MutOp::Inx),
			0x85 => self.exec_sta(MutOp::Zp),
			0x8D => self.exec_sta(MutOp::Abs),
			0x91 => self.exec_sta(MutOp::Iny),
			0x95 => self.exec_sta(MutOp::Zpx),
			0x99 => self.exec_sta(MutOp::Aby),
			0x9D => self.exec_sta(MutOp::Abx),
			0x86 => self.exec_stx(MutOp::Zp),
			0x8E => self.exec_stx(MutOp::Abs),
			0x96 => self.exec_stx(MutOp::Zpy),
			0x9A => self.exec_stx(MutOp::Sp), // aka TXS
			0x84 => self.exec_sty(MutOp::Zp),
			0x8C => self.exec_sty(MutOp::Abs),
			0x94 => self.exec_sty(MutOp::Zpx),

			// Load / transfer instructions:
			0xA1 => self.exec_lda(Op::Inx),
			0xA5 => self.exec_lda(Op::Zp),
			0xA9 => self.exec_lda(Op::Imm),
			0xAD => self.exec_lda(Op::Abs),
			0xB1 => self.exec_lda(Op::Iny),
			0xB5 => self.exec_lda(Op::Zpx),
			0xB9 => self.exec_lda(Op::Aby),
			0xBD => self.exec_lda(Op::Abx),
			0x8A => self.exec_lda(Op::X), // aka TXA
			0x98 => self.exec_lda(Op::Y), // aka TYA
			0xA2 => self.exec_ldx(Op::Imm),
			0xA6 => self.exec_ldx(Op::Zp),
			0xAA => self.exec_ldx(Op::Acc), // aka TAX
			0xAE => self.exec_ldx(Op::Abs),
			0xB6 => self.exec_ldx(Op::Zpy),
			0xBA => self.exec_ldx(Op::Sp), // aka TSX
			0xBE => self.exec_ldx(Op::Aby),
			0xA0 => self.exec_ldy(Op::Imm),
			0xA4 => self.exec_ldy(Op::Zp),
			0xA8 => self.exec_ldy(Op::Acc), // aka TAY
			0xAC => self.exec_ldy(Op::Abs),
			0xB4 => self.exec_ldy(Op::Zpx),
			0xBC => self.exec_ldy(Op::Abx),

			// Compare instructions:
			0xC1 => self.exec_cmp(Op::Inx),
			0xC5 => self.exec_cmp(Op::Zp),
			0xC9 => self.exec_cmp(Op::Imm),
			0xCD => self.exec_cmp(Op::Abs),
			0xD1 => self.exec_cmp(Op::Iny),
			0xD5 => self.exec_cmp(Op::Zpx),
			0xD9 => self.exec_cmp(Op::Aby),
			0xDD => self.exec_cmp(Op::Abx),
			0xE0 => self.exec_cpx(Op::Imm),
			0xE4 => self.exec_cpx(Op::Zp),
			0xEC => self.exec_cpx(Op::Abs),
			0xC0 => self.exec_cpy(Op::Imm),
			0xC4 => self.exec_cpy(Op::Zp),
			0xCC => self.exec_cpy(Op::Abs),

			// Bit test instructions:
			0x24 => self.exec_bit(Op::Zp),
			0x2C => self.exec_bit(Op::Abs),

			// Bitwise logic instructions:
			0x01 => self.exec_ora(Op::Inx),
			0x05 => self.exec_ora(Op::Zp),
			0x09 => self.exec_ora(Op::Imm),
			0x0D => self.exec_ora(Op::Abs),
			0x11 => self.exec_ora(Op::Iny),
			0x15 => self.exec_ora(Op::Zpx),
			0x19 => self.exec_ora(Op::Aby),
			0x1D => self.exec_ora(Op::Abx),
			0x21 => self.exec_and(Op::Inx),
			0x25 => self.exec_and(Op::Zp),
			0x29 => self.exec_and(Op::Imm),
			0x2D => self.exec_and(Op::Abs),
			0x31 => self.exec_and(Op::Iny),
			0x35 => self.exec_and(Op::Zpx),
			0x39 => self.exec_and(Op::Aby),
			0x3D => self.exec_and(Op::Abx),
			0x41 => self.exec_eor(Op::Inx),
			0x45 => self.exec_eor(Op::Zp),
			0x49 => self.exec_eor(Op::Imm),
			0x4D => self.exec_eor(Op::Abs),
			0x51 => self.exec_eor(Op::Iny),
			0x55 => self.exec_eor(Op::Zpx),
			0x59 => self.exec_eor(Op::Aby),
			0x5D => self.exec_eor(Op::Abx),

			// Add and subtract instructions:
			0x61 => self.exec_adc(Op::Inx),
			0x65 => self.exec_adc(Op::Zp),
			0x69 => self.exec_adc(Op::Imm),
			0x6D => self.exec_adc(Op::Abs),
			0x71 => self.exec_adc(Op::Iny),
			0x75 => self.exec_adc(Op::Zpx),
			0x79 => self.exec_adc(Op::Aby),
			0x7D => self.exec_adc(Op::Abx),
			0xE1 => self.exec_sbc(Op::Inx),
			0xE5 => self.exec_sbc(Op::Zp),
			0xE9 => self.exec_sbc(Op::Imm),
			0xED => self.exec_sbc(Op::Abs),
			0xF1 => self.exec_sbc(Op::Iny),
			0xF5 => self.exec_sbc(Op::Zpx),
			0xF9 => self.exec_sbc(Op::Aby),
			0xFD => self.exec_sbc(Op::Abx),

			// Shift and rotate instructions:
			0x06 => self.exec_asl(MutOp::Zp),
			0x0A => self.exec_asl(MutOp::Acc),
			0x0E => self.exec_asl(MutOp::Abs),
			0x16 => self.exec_asl(MutOp::Zpx),
			0x1E => self.exec_asl(MutOp::Abx),
			0x46 => self.exec_lsr(MutOp::Zp),
			0x4A => self.exec_lsr(MutOp::Acc),
			0x4E => self.exec_lsr(MutOp::Abs),
			0x56 => self.exec_lsr(MutOp::Zpx),
			0x5E => self.exec_lsr(MutOp::Abx),
			0x26 => self.exec_rol(MutOp::Zp),
			0x2A => self.exec_rol(MutOp::Acc),
			0x2E => self.exec_rol(MutOp::Abs),
			0x36 => self.exec_rol(MutOp::Zpx),
			0x3E => self.exec_rol(MutOp::Abx),
			0x66 => self.exec_ror(MutOp::Zp),
			0x6A => self.exec_ror(MutOp::Acc),
			0x6E => self.exec_ror(MutOp::Abs),
			0x76 => self.exec_ror(MutOp::Zpx),
			0x7E => self.exec_ror(MutOp::Abx),

			// NOP
			0xEA => {},

			// Invalid instructions:
			0x89 | 0x04 | 0x0C | 0x14 | 0x1C | 0x34 | 0x3C | 0x44 |
			0x54 | 0x5C | 0x64 | 0x74 | 0x7C | 0x80 | 0x9C | 0xD4 |
			0xDC | 0xF4 | 0xFC | 0x02 | 0x12 | 0x1A | 0x22 | 0x32 |
			0x3A | 0x42 | 0x52 | 0x5A | 0x62 | 0x72 | 0x7A | 0x82 |
			0x92 | 0x9E | 0xB2 | 0xC2 | 0xD2 | 0xDA | 0xE2 | 0xF2 |
			0xFA | 0x03 | 0x07 | 0x0B | 0x0F | 0x13 | 0x17 | 0x1B |
			0x1F | 0x23 | 0x27 | 0x2B | 0x2F | 0x33 | 0x37 | 0x3B |
			0x3F | 0x43 | 0x47 | 0x4B | 0x4F | 0x53 | 0x57 | 0x5B |
			0x5F | 0x63 | 0x67 | 0x6B | 0x6F | 0x73 | 0x77 | 0x7B |
			0x7F | 0x83 | 0x87 | 0x8B | 0x8F | 0x93 | 0x97 | 0x9B |
			0x9F | 0xA3 | 0xA7 | 0xAB | 0xAF | 0xB3 | 0xB7 | 0xBB |
			0xBF | 0xC3 | 0xC7 | 0xCB | 0xCF | 0xD3 | 0xD7 | 0xDB |
			0xDF | 0xE3 | 0xE7 | 0xEB | 0xEF | 0xF3 | 0xF7 | 0xFB |
			0xFF => self.exec_invalid(instruction),
		}
	}
}

/// Instruction implementations.
impl Cpu {
	pub fn exec_jsr(&mut self) {
		self.push_word(self.pc);
		self.pc = self.read_im2();
	}

	pub fn exec_rts(&mut self) {
		self.pc = self.pop_word().wrapping_add(2);
	}

	pub fn exec_brk(&mut self) {
		self.pc = self.pc.wrapping_add(1);
		if !self.status.contains(Status::I) {
			self.push_word(self.pc);
			self.exec_php();
			self.pc = self.read_word(0xFFFE);
		}
	}

	pub fn exec_rti(&mut self) {
		self.exec_plp();
		self.pc = self.pop_word();
	}

	pub fn exec_jmp(&mut self, op: impl Into<JmpOp>) {
		self.pc = self.get_target(op);
	}

	pub fn exec_branch(&mut self, flag: Status, condition: bool) {
		let offset = i16::from(self.read_imm() as i8) as u16;
		let target = self.pc.wrapping_add(offset);
		if self.status.contains(flag) == condition {
			self.pc = target;
		}
	}

	pub fn exec_php(&mut self) {
		self.push(self.status.bits() | 0b0011_0000);
	}

	pub fn exec_plp(&mut self) {
		self.status = Status::from_bits_truncate(self.pop());
	}

	pub fn exec_pha(&mut self) {
		self.push(self.a);
	}

	pub fn exec_pla(&mut self) {
		self.a = self.pop();
	}

	pub fn exec_inc<'a>(&mut self, op: impl Into<MutOp<'a>>) {
		let op = self.get_mut_op(op.into());
		*op = op.wrapping_add(1);
		let op = *op;
		self.update_n_z(op);
	}

	pub fn exec_dec<'a>(&mut self, op: impl Into<MutOp<'a>>) {
		let op = self.get_mut_op(op.into());
		*op = op.wrapping_sub(1);
		let op = *op;
		self.update_n_z(op);
	}

	pub fn exec_sta<'a>(&mut self, op: impl Into<MutOp<'a>>) {
		*self.get_mut_op(op.into()) = self.a;
	}

	pub fn exec_stx<'a>(&mut self, op: impl Into<MutOp<'a>>) {
		*self.get_mut_op(op.into()) = self.x;
	}

	pub fn exec_sty<'a>(&mut self, op: impl Into<MutOp<'a>>) {
		*self.get_mut_op(op.into()) = self.y;
	}

	pub fn exec_lda(&mut self, op: impl Into<Op>) {
		self.a = self.get_op(op);
		self.update_n_z(self.a);
	}

	pub fn exec_ldx(&mut self, op: impl Into<Op>) {
		self.x = self.get_op(op);
		self.update_n_z(self.x);
	}

	pub fn exec_ldy(&mut self, op: impl Into<Op>) {
		self.y = self.get_op(op);
		self.update_n_z(self.y);
	}

	pub fn exec_cmp(&mut self, op: impl Into<Op>) {
		let op = self.get_op(op);
		self.do_compare(self.a, op);
	}

	pub fn exec_cpx(&mut self, op: impl Into<Op>) {
		let op = self.get_op(op);
		self.do_compare(self.x, op);
	}

	pub fn exec_cpy(&mut self, op: impl Into<Op>) {
		let op = self.get_op(op);
		self.do_compare(self.y, op);
	}

	pub fn exec_bit(&mut self, op: impl Into<Op>) {
		let op = self.get_op(op);
		let result = self.a & op;
		self.update_n_z(result);
		self.status.set(Status::V, result & 0x40 != 0);
	}

	pub fn exec_ora(&mut self, op: impl Into<Op>) {
		self.a |= self.get_op(op);
		self.update_n_z(self.a);
	}

	pub fn exec_and(&mut self, op: impl Into<Op>) {
		self.a &= self.get_op(op);
		self.update_n_z(self.a);
	}

	pub fn exec_eor(&mut self, op: impl Into<Op>) {
		self.a ^= self.get_op(op);
		self.update_n_z(self.a);
	}

	pub fn exec_adc(&mut self, op: impl Into<Op>) {
		if self.status.contains(Status::D) {
			unimplemented!();
		} else {
			let (a, c1) = self.a.overflowing_add(self.status.contains(Status::C) as u8);
			let (a, c2) = a.overflowing_add(self.get_op(op));
			self.a = a;
			self.status.set(Status::C, c1 || c2);
		}
		// TODO: flags: V
		self.update_n_z(self.a);
	}

	pub fn exec_sbc(&mut self, op: impl Into<Op>) {
		if self.status.contains(Status::D) {
			unimplemented!();
		} else {
			let borrow = !self.status.contains(Status::C);
			let (a, b1) = self.a.overflowing_sub(borrow as u8);
			let (a, b2) = a.overflowing_sub(self.get_op(op));
			self.a = a;
			self.status.set(Status::C, !(b1 || b2));
		}
		// TODO: flags: V
		self.update_n_z(self.a);
	}

	pub fn exec_asl<'a>(&mut self, op:  impl Into<MutOp<'a>>) {
		let op = self.get_mut_op(op.into());
		let new_c = *op & 0x80 != 0;
		*op <<= 1;
		let op = *op;
		self.status.set(Status::C, new_c);
		self.update_n_z(op);
	}

	pub fn exec_lsr<'a>(&mut self, op:  impl Into<MutOp<'a>>) {
		let op = self.get_mut_op(op.into());
		let new_c = *op & 1 != 0;
		*op >>= 1;
		let op = *op;
		self.update_n_z(op);
		self.status.set(Status::C, new_c);
	}

	pub fn exec_rol<'a>(&mut self, op:  impl Into<MutOp<'a>>) {
		let c = self.status.contains(Status::C) as u8;
		let op = self.get_mut_op(op.into());
		let new_c = *op & 0x80 != 0;
		*op = *op << 1 | c;
		let op = *op;
		self.status.set(Status::C, new_c);
		self.update_n_z(op);
	}

	pub fn exec_ror<'a>(&mut self, op:  impl Into<MutOp<'a>>) {
		let c = self.status.contains(Status::C) as u8;
		let op = self.get_mut_op(op.into());
		let new_c = *op & 1 != 0;
		*op = *op >> 1 | c << 7;
		let op = *op;
		self.status.set(Status::C, new_c);
		self.update_n_z(op);
	}

	pub fn exec_invalid(&mut self, instruction: u8) {
		panic!("invalid instruction: {:02b}", instruction);
	}

	fn do_compare(&mut self, a: u8, b: u8) {
		let (result, borrow) = a.overflowing_sub(b);
		self.status.set(Status::Z, result == 0);
		self.status.set(Status::C, !borrow);
		self.status.set(Status::N, result & 0x80 != 0);
	}

	fn update_n_z(&mut self, value: u8) {
		self.status.set(Status::N, value & 0x80 != 0);
		self.status.set(Status::Z, value == 0);
	}
}

#[cfg(test)]
mod test;
