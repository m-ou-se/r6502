use super::{Cpu, Status};

impl std::fmt::Debug for Cpu {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "   A │ {:02x}\n", self.a)?;
		write!(f, "   X │ {:02x}\n", self.x)?;
		write!(f, "   Y │ {:02x}\n", self.y)?;
		write!(f, "   S │ {:02x}\n", self.s)?;
		write!(
			f,
			"   P │ {:02x} (N={} V={} D={} I={} Z={} C={})\n",
			self.status,
			self.status.contains(Status::N) as u8,
			self.status.contains(Status::V) as u8,
			self.status.contains(Status::D) as u8,
			self.status.contains(Status::I) as u8,
			self.status.contains(Status::Z) as u8,
			self.status.contains(Status::C) as u8,
		)?;
		write!(f, "  PC │ {:04x}\n", self.pc)?;

		let pc = usize::from(self.pc);
		let sp = usize::from(self.s) + 0x0100;
		let mut skipping = false;
		for i in 0..0x1000 {
			let addr = i * 0x10;
			let data = &self.mem[addr..addr + 0x10];
			let has_pc = pc >= addr && pc < addr + 0x10;
			let has_sp = sp >= addr && sp < addr + 0x10;
			if i > 0 && !has_pc && !has_sp && i != 0x0FFF && data.iter().all(|&x| x == 0) {
				if !skipping {
					f.write_str(".... │\n")?;
					skipping = true;
				}
			} else {
				skipping = false;
				write!(f, "{:04x} │", addr)?;
				for byte in data {
					write!(f, " {:02x}", byte)?;
				}
				f.write_str("\n")?;
				if has_pc {
					write!(f, "  PC │{:>w$}\n", "^^", w = 3 * (pc - addr + 1))?;
				}
				if has_sp {
					write!(f, "   S │{:>w$}\n", "^^", w = 3 * (sp - addr + 1))?;
				}
			}
		}

		Ok(())
	}
}
