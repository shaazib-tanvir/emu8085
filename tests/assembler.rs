use emu8085::{asm::AssembledProgram, common::Register, emu::CPU};

#[test]
fn assemble_without_errors() {
    let _ = AssembledProgram::assemble(include_str!("../examples/sort.asm"));
    let _ = AssembledProgram::assemble(include_str!("../examples/add.asm"));
    let _ = AssembledProgram::assemble(include_str!("../examples/add_array.asm"));
    let _ = AssembledProgram::assemble(include_str!("../examples/lda.asm"));
}

#[test]
fn saving_and_loading() {
    let program = ".org 2050
.db ff

.org 2000
.start
inr a
hlt";
    let assembled_program = AssembledProgram::assemble(program);
    assert!(assembled_program.is_ok());
    let assembled_program = assembled_program.unwrap();

    assert!(assembled_program.save("examples/test.ex85").is_ok());

    let loaded_program = AssembledProgram::load("examples/test.ex85");
    assert!(loaded_program.is_ok());
    let loaded_program = loaded_program.unwrap();

    let mut cpu = CPU::new();
    for segment in loaded_program.segments() {
        cpu.load_data(segment.data().as_slice(), segment.address());
    }
    cpu.load_entrypoint(loaded_program.get_entrypoint());

    cpu.execute(loaded_program.get_entrypoint());
    assert_eq!(cpu.get_register(Register::A), 1);
    assert_eq!(cpu.get_memory_at(0x2050), 0xff);
}
