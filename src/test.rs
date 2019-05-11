use super::{Cpu, Status};

#[test]
fn test_cmp() {
	let mut cpu = Cpu::default();

	cpu.exec_lda(0xF0);
	cpu.exec_cmp(0xF0);
	assert_eq!(cpu.status, Status::Z | Status::C);

	cpu.exec_lda(0x12);
	cpu.exec_cmp(0x11);
	assert_eq!(cpu.status, Status::C);

	cpu.exec_lda(0x01);
	cpu.exec_cmp(0xFF);
	assert_eq!(cpu.status, Status::empty());

	cpu.exec_lda(0x7F);
	cpu.exec_cmp(0x80);
	assert_eq!(cpu.status, Status::N);
}

#[test]
fn test_inc() {
	let mut cpu = Cpu::default();
	let mut a = 7;
	cpu.exec_inc(&mut a);
	assert_eq!(a, 8);
	assert_eq!(cpu.status, Status::empty());

	let mut a = 0x7F;
	cpu.exec_inc(&mut a);
	assert_eq!(a, 0x80);
	assert_eq!(cpu.status, Status::N);

	let mut a = 0xFF;
	cpu.exec_inc(&mut a);
	assert_eq!(a, 0x00);
	assert_eq!(cpu.status, Status::Z);
}

#[test]
fn test_dec() {
	let mut cpu = Cpu::default();
	let mut a = 8;
	cpu.exec_dec(&mut a);
	assert_eq!(a, 7);
	assert_eq!(cpu.status, Status::empty());

	let mut a = 0x80;
	cpu.exec_dec(&mut a);
	assert_eq!(a, 0x7F);
	assert_eq!(cpu.status, Status::empty());

	let mut a = 0x00;
	cpu.exec_dec(&mut a);
	assert_eq!(a, 0xFF);
	assert_eq!(cpu.status, Status::N);

	let mut a = 0x01;
	cpu.exec_dec(&mut a);
	assert_eq!(a, 0x00);
	assert_eq!(cpu.status, Status::Z);
}
