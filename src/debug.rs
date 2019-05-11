use super::Cpu;

impl std::fmt::Debug for Cpu {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "   A │ {:02x}\n", self.a)?;
		write!(f, "   X │ {:02x}\n", self.x)?;
		write!(f, "   Y │ {:02x}\n", self.y)?;
		write!(f, "   S │ {:02x}\n", self.s)?;
		write!(f, "   P │ {:02x}\n", self.p)?;
		write!(f, "  PC │ {:04x}\n", self.pc)?;

		// Memory:
		let mut skipping = false;
		for i in 0..0x1000 {
			let addr = i * 0x10;
			let data = &self.mem[addr..addr + 0x10];
			if i > 0 && i != 0x0FFF && data.iter().all(|&x| x == 0) {
				if !skipping {
					f.write_str(".... │\n")?;
					skipping = true;
				}
			} else {
				write!(f, "{:04x} │", addr)?;
				for byte in data {
					write!(f, " {:02x}", byte)?;
				}
				f.write_str("\n")?;
			}
		}

		Ok(())
	}
}
