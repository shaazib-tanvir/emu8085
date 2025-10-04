use criterion::{Criterion, criterion_group, criterion_main};
use emu8085::asm::AssembledProgram;
use emu8085::emu::CPU;

fn step_forward_benchmark(c: &mut Criterion) {
    let program = AssembledProgram::assemble(".org 2000\n.start\nloop: inr a\njmp loop").unwrap();
    let mut cpu = CPU::new();
    for segment in program.segments() {
        cpu.load_data(segment.data().as_slice(), segment.address());
    }
    cpu.load_entrypoint(program.get_entrypoint());

    c.bench_function("step_forward", |b| b.iter(|| cpu.step_forward()));
}

criterion_group!(benches, step_forward_benchmark);
criterion_main!(benches);
