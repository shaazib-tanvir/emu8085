use bitflags::bitflags;
use std::collections::VecDeque;
use thiserror::Error;

bitflags! {
    #[derive(Debug)]
    struct Flags: u8 {
        const CARRY = 1 << 0;
        const PARITY = 1 << 2;
        const AUXILIARY_CARRY = 1 << 4;
        const ZERO = 1 << 6;
        const SIGN = 1 << 7;
    }
}

fn pair_to_u16(pair: [u8; 2]) -> u16 {
    (pair[0] as u16) + (pair[1] as u16) << 8
}

fn u16_to_pair(value: u16) -> [u8; 2] {
    [(value & 0x00ff) as u8, ((value & 0xff00) >> 8) as u8]
}

#[derive(Copy, Clone, Debug)]
enum Register {
    B = 0b000,
    C = 0b001,
    D = 0b010,
    E = 0b011,
    H = 0b100,
    L = 0b101,
    A = 0b111,
}

#[derive(Error, Debug)]
pub enum RegisterError {
    #[error("the value {0} does not correspond to a register")]
    Unused(u8),
}

impl TryFrom<u8> for Register {
    type Error = RegisterError;
    fn try_from(value: u8) -> Result<Register, RegisterError> {
        match value {
            x if x == Register::A as u8 => Ok(Register::A),
            x if x == Register::B as u8 => Ok(Register::B),
            x if x == Register::C as u8 => Ok(Register::C),
            x if x == Register::D as u8 => Ok(Register::D),
            x if x == Register::E as u8 => Ok(Register::E),
            x if x == Register::H as u8 => Ok(Register::H),
            x if x == Register::L as u8 => Ok(Register::L),

            _ => Err(RegisterError::Unused(value)),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum RegisterPair {
    BC,
    DE,
    HL,
    SP,
    PC,
}

#[derive(Debug)]
struct RegisterState {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    flags: Flags,
    stack_pointer: u16,
    program_counter: u16,
}

#[derive(Debug)]
struct CPUState {
    memory: [u8; 0x10000],
    registers: RegisterState,
}

impl CPUState {
    fn new() -> Self {
        return Self {
            memory: [0; 0x10000],
            registers: RegisterState {
                a: 0x01,
                b: 0,
                c: 0,
                d: 0,
                e: 0,
                h: 0,
                l: 0,
                flags: Flags::empty(),
                stack_pointer: 0,
                program_counter: 0,
            },
        };
    }

    fn get_register(&self, register: Register) -> u8 {
        match register {
            Register::A => self.registers.a,
            Register::B => self.registers.b,
            Register::C => self.registers.c,
            Register::D => self.registers.d,
            Register::E => self.registers.e,
            Register::H => self.registers.h,
            Register::L => self.registers.l,
        }
    }

    fn get_address(&self) -> u16 {
        self.registers.program_counter
    }

    fn get_memory(&self) -> u8 {
        self.memory[self.get_address() as usize]
    }

    fn get_memory_at(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }

    fn execute_op(&mut self, operation: Operation) {
        match operation {
            Operation::Register(register_op) => {
                match register_op.register {
                    Register::A => self.registers.a = register_op.new_value,
                    Register::B => self.registers.b = register_op.new_value,
                    Register::C => self.registers.c = register_op.new_value,
                    Register::D => self.registers.d = register_op.new_value,
                    Register::E => self.registers.e = register_op.new_value,
                    Register::H => self.registers.h = register_op.new_value,
                    Register::L => self.registers.l = register_op.new_value,
                }
            },
            Operation::RegisterPair(rp_op) => {
                match rp_op.register {
                    RegisterPair::BC => {
                        self.registers.b = u16_to_pair(rp_op.new_value)[0];
                        self.registers.c = u16_to_pair(rp_op.new_value)[1];
                    }
                    RegisterPair::PC => {
                        self.registers.program_counter = rp_op.new_value;
                    }
                    RegisterPair::DE => {
                        self.registers.d = u16_to_pair(rp_op.new_value)[0];
                        self.registers.e = u16_to_pair(rp_op.new_value)[1];
                    }
                    RegisterPair::SP => {
                        self.registers.stack_pointer = rp_op.new_value;
                    }
                    RegisterPair::HL => {
                        self.registers.h = u16_to_pair(rp_op.new_value)[0];
                        self.registers.l = u16_to_pair(rp_op.new_value)[1];
                    }
                }
            },
            Operation::Memory(memory_op) => {
                self.memory[memory_op.address as usize] = memory_op.new_value;
            },
        }
    }

    fn undo_op(&mut self, operation: Operation) {
        match operation {
            Operation::Register(register_op) => {
                match register_op.register {
                    Register::A => self.registers.a = register_op.old_value,
                    Register::B => self.registers.b = register_op.old_value,
                    Register::C => self.registers.c = register_op.old_value,
                    Register::D => self.registers.d = register_op.old_value,
                    Register::E => self.registers.e = register_op.old_value,
                    Register::H => self.registers.h = register_op.old_value,
                    Register::L => self.registers.l = register_op.old_value,
                }
            },
            Operation::RegisterPair(rp_op) => {
                match rp_op.register {
                    RegisterPair::BC => {
                        self.registers.b = u16_to_pair(rp_op.old_value)[0];
                        self.registers.c = u16_to_pair(rp_op.old_value)[1];
                    }
                    RegisterPair::PC => {
                        self.registers.program_counter = rp_op.old_value;
                    }
                    RegisterPair::DE => {
                        self.registers.d = u16_to_pair(rp_op.old_value)[0];
                        self.registers.e = u16_to_pair(rp_op.old_value)[1];
                    }
                    RegisterPair::SP => {
                        self.registers.stack_pointer = rp_op.old_value;
                    }
                    RegisterPair::HL => {
                        self.registers.h = u16_to_pair(rp_op.old_value)[0];
                        self.registers.l = u16_to_pair(rp_op.old_value)[1];
                    }
                }
            },
            Operation::Memory(memory_op) => {
                self.memory[memory_op.address as usize] = memory_op.old_value;
            },
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct RegisterOperation {
    old_value: u8,
    new_value: u8,
    register: Register,
}

#[derive(Copy, Clone, Debug)]
struct RegisterPairOperation {
    old_value: u16,
    new_value: u16,
    register: RegisterPair,
}

#[derive(Copy, Clone, Debug)]
struct MemoryOperation {
    address: u16,
    old_value: u8,
    new_value: u8,
}

#[derive(Copy, Clone, Debug)]
enum Operation {
    Register(RegisterOperation),
    RegisterPair(RegisterPairOperation),
    Memory(MemoryOperation),
}

#[derive(Debug)]
struct Command {
    index: u64,
    operation: Operation,
}

#[derive(Debug)]
pub struct CPU {
    cpu_state: CPUState,
    commands: VecDeque<Command>,
    instruction_count: u64,
}

macro_rules! convertible {
    ($(#[$meta:meta])*
     $vis:vis enum $name:ident {
         $($field:ident $(= $value:literal)? $(,)?)*
    }) => {
        $(#[$meta])*
        $vis enum $name {
            $($field $(=  $value)?,)*
        }

        impl TryFrom<u8> for $name {
            type Error = OpCodeError;
            fn try_from(value: u8) -> Result<Self, OpCodeError> {
                match value {
                    $(x if x == $name::$field as u8 => Ok($name::$field),)*
                    _ => Err(OpCodeError::Unused(value)),
                }
            }
        }
    }
}

convertible! {
    #[derive(Debug)]
    pub enum OpCode {
        Nop,
        LxiB,
        StaxB,
        InxB,
        InrB,
        DcrB,
        MviB,
        Rlc,
        DadB = 0x09,
        LdaxB,
        DcxB,
        InrC,
        DcrC,
        MviC,
        Rrc,
        LxiD = 0x11,
        StaxD,
        InxD,
        InrD,
        DcrD,
        MviD,
        Ral,
        DadD = 0x19,
        LdaxD,
        DcxD,
        InrE,
        DcrE,
        MviE,
        Rar,
        Rim,
        LxiH,
        Shld,
        InxH,
        InrH,
        DcrH,
        MviH,
        Daa,
        DadH = 0x29,
        Lhld,
        DcxH,
        InrL,
        DcrL,
        MviL,
        Cma,
        Sim,
        LxiSP,
        Sta,
        InxSp,
        InrM,
        DcrM,
        MviM,
        Stc,
        DadSP = 0x39,
        Lda,
        DcxSP,
        InrA,
        DcrA,
        MviA,
        Cmc,
        MovBB,
        MovBC,
        MovBD,
        MovBE,
        MovBH,
        MovBL,
        MovBM,
        MovBA,
        MovCB,
        MovCC,
        MovCD,
        MovCE,
        MovCH,
        MovCL,
        MovCM,
        MovCA,
        MovDB,
        MovDC,
        MovDD,
        MovDE,
        MovDH,
        MovDL,
        MovDM,
        MovDA,
        MovEB,
        MovEC,
        MovED,
        MovEE,
        MovEH,
        MovEL,
        MovEM,
        MovEA,
        MovHB,
        MovHC,
        MovHD,
        MovHE,
        MovHH,
        MovHL,
        MovHM,
        MovHA,
        MovLB,
        MovLC,
        MovLD,
        MovLE,
        MovLH,
        MovLL,
        MovLM,
        MovLA,
        MovMB,
        MovMC,
        MovMD,
        MovME,
        MovMH,
        MovML,
        Hlt,
        MovMA,
        MovAB,
        MovAC,
        MovAD,
        MovAE,
        MovAH,
        MovAL,
        MovAM,
        MovAA,
        AddB,
        AddC,
        AddD,
        AddE,
        AddH,
        AddL,
        AddM,
        AddA,
        AdcB,
        AdcC,
        AdcD,
        AdcE,
        AdcH,
        AdcL,
        AdcM,
        AdcA,
        SubB,
        SubC,
        SubD,
        SubE,
        SubH,
        SubL,
        SubM,
        SubA,
        SbbB,
        SbbC,
        SbbD,
        SbbE,
        SbbH,
        SbbL,
        SbbM,
        SbbA,
        AnaB,
        AnaC,
        AnaD,
        AnaE,
        AnaH,
        AnaL,
        AnaM,
        AnaA,
        XraB,
        XraC,
        XraD,
        XraE,
        XraH,
        XraL,
        XraM,
        XraA,
        OraB,
        OraC,
        OraD,
        OraE,
        OraH,
        OraL,
        OraM,
        OraA,
        CmpB,
        CmpC,
        CmpD,
        CmpE,
        CmpH,
        CmpL,
        CmpM,
        CmpA,
        Rnz,
        PopB,
        Jnz,
        Jmp,
        Cnz,
        PushB,
        Adi,
        Rst0,
        Rz,
        Ret,
        JZ,
        Cz = 0xCC,
        Call,
        Aci,
        Rst1,
        Rnc,
        PopD,
        Jnc,
        Out,
        Cnc,
        PushD,
        Sui,
        Rst2,
        Rc,
        Jc = 0xDA,
        In,
        Cc,
        Sbi = 0xDE,
        Rst3,
        Rpo,
        PopH,
        Jpo,
        Xthl,
        Cpo,
        PushH,
        Ani,
        Rst4,
        Rpe,
        Pchl,
        Jpe,
        Xchg,
        Cpe,
        Xri = 0xEE,
        Rst5,
        Rp,
        PopPSW,
        Jp,
        Di,
        Cp,
        PushPSW,
        Ori,
        Rst,
        Rm,
        Sphl,
        Jm,
        Ei,
        Cm,
        Cpi = 0xFE,
        Rst7
    }
}

#[derive(Error, Debug)]
pub enum OpCodeError {
    #[error("the opcode {0} is unused")]
    Unused(u8),
}

const LIMIT: usize = 100;

#[derive(Error, Debug)]
#[error(transparent)]
pub enum StepError {
    OpCodeError(#[from] OpCodeError),
    RegisterError(#[from] RegisterError),
    #[error("the opcode {0:?} is currently unimplemented")]
    Unimplemented(OpCode),
}

impl CPU {
    pub fn new() -> Self {
        return Self {
            cpu_state: CPUState::new(),
            commands: VecDeque::new(),
            instruction_count: 0,
        };
    }

    fn push_command(&mut self, operation: Operation) {
        if self.commands.len() >= LIMIT {
            self.commands.pop_front();
        }

        self.commands.push_back(Command {
            operation,
            index: self.instruction_count,
        });
    }

    pub fn load_data(&mut self, data: &[u8], address: u16) {
        assert!(data.len() - address as usize <= 0x10000);
        self.cpu_state.memory[(address as usize)..(address as usize) + data.len()].copy_from_slice(data);
    }

    fn read_byte(&mut self) -> u8 {
        let value = self.cpu_state.get_memory();
        let op = Operation::RegisterPair(RegisterPairOperation {
            old_value: self.cpu_state.get_address(),
            new_value: self.cpu_state.get_address() + 1,
            register: RegisterPair::PC,
        });
        self.cpu_state.execute_op(op);
        self.push_command(op);
        value
    }

    fn read_double_bytes(&mut self) -> u16 {
        let value0 = self.cpu_state.get_memory();
        let value1 = self
            .cpu_state
            .get_memory_at(self.cpu_state.get_address() + 1);
        let op = Operation::RegisterPair(RegisterPairOperation {
            old_value: self.cpu_state.get_address(),
            new_value: self.cpu_state.get_address() + 2,
            register: RegisterPair::PC,
        });
        self.cpu_state.execute_op(op);
        self.push_command(op);

        pair_to_u16([value0, value1])
    }

    pub fn step(&mut self) -> Result<(), StepError> {
        let instruction = self.read_byte();
        let opcode = OpCode::try_from(instruction)?;

        match opcode {
            OpCode::Nop => {}
            OpCode::MovAA
            | OpCode::MovAB
            | OpCode::MovAC
            | OpCode::MovAD
            | OpCode::MovAE
            | OpCode::MovAH
            | OpCode::MovAL
            | OpCode::MovAM
            | OpCode::MovBA
            | OpCode::MovBB
            | OpCode::MovBC
            | OpCode::MovBD
            | OpCode::MovBE
            | OpCode::MovBH
            | OpCode::MovBL
            | OpCode::MovBM
            | OpCode::MovCA
            | OpCode::MovCB
            | OpCode::MovCC
            | OpCode::MovCD
            | OpCode::MovCE
            | OpCode::MovCH
            | OpCode::MovCL
            | OpCode::MovCM
            | OpCode::MovDA
            | OpCode::MovDB
            | OpCode::MovDC
            | OpCode::MovDD
            | OpCode::MovDE
            | OpCode::MovDH
            | OpCode::MovDL
            | OpCode::MovDM
            | OpCode::MovEA
            | OpCode::MovEB
            | OpCode::MovEC
            | OpCode::MovED
            | OpCode::MovEE
            | OpCode::MovEH
            | OpCode::MovEL
            | OpCode::MovEM
            | OpCode::MovHA
            | OpCode::MovHB
            | OpCode::MovHC
            | OpCode::MovHD
            | OpCode::MovHE
            | OpCode::MovHH 
            | OpCode::MovHL
            | OpCode::MovHM
            | OpCode::MovLA
            | OpCode::MovLB
            | OpCode::MovLC
            | OpCode::MovLD
            | OpCode::MovLE
            | OpCode::MovLH 
            | OpCode::MovLL
            | OpCode::MovLM
            | OpCode::MovMA
            | OpCode::MovMB
            | OpCode::MovMC
            | OpCode::MovMD
            | OpCode::MovME
            | OpCode::MovMH 
            | OpCode::MovML => {
                let source: u8 = instruction & 0b00000111;
                let destination: u8 = (instruction & 0b00111000) >> 3;
                let source_register = Register::try_from(source);
                let destination_register = Register::try_from(destination);

                if source_register.is_ok() && destination_register.is_ok() {
                    let source_register = source_register.unwrap();
                    let destination_register = destination_register.unwrap();
                    let op = Operation::Register(RegisterOperation {
                        old_value: self.cpu_state.get_register(destination_register),
                        new_value: self.cpu_state.get_register(source_register),
                        register: destination_register,
                    });

                    self.cpu_state.execute_op(op);
                    self.push_command(op);
                } else if source_register.is_ok() {
                    let source_register = source_register.unwrap();
                    let op = Operation::Memory(MemoryOperation {
                        address: self.cpu_state.get_address(),
                        old_value: self.cpu_state.get_memory(),
                        new_value: self.cpu_state.get_register(source_register)
                    });

                    self.cpu_state.execute_op(op);
                    self.push_command(op);
                } else if destination_register.is_ok() {
                    let destination_register = destination_register.unwrap();
                    let op = Operation::Register(RegisterOperation {
                        old_value: self.cpu_state.get_register(destination_register),
                        new_value: self.cpu_state.get_memory(),
                        register: destination_register,
                    });

                    self.cpu_state.execute_op(op);
                    self.push_command(op);
                } else {
                    return Err(StepError::RegisterError(source_register.err().unwrap()));
                }
            }
            _ => {
                return Err(StepError::Unimplemented(opcode));
            }
        }

        self.instruction_count += 1;
        Ok(())
    }
}
