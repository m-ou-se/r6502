use r6502::Cpu;

fn main() {
	let mut cpu = Cpu::default();
	cpu.init();
	cpu.step();
	println!("{:?}", cpu);
}
