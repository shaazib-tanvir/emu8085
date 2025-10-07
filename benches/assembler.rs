use criterion::{Criterion, criterion_group, criterion_main};
use emu8085::asm::AssembledProgram;

fn step_forward_benchmark(c: &mut Criterion) {
    c.bench_function("assemble", |b| b.iter(|| {
        std::hint::black_box(AssembledProgram::assemble(include_str!("../examples/sort.asm")).unwrap());
    }));
}

criterion_group!(benches, step_forward_benchmark);
criterion_main!(benches);
