use bitflags::bitflags;
use std::collections::VecDeque;
use thiserror::Error;

bitflags! {
    #[derive(Copy, Clone, Debug)]
    struct Flags: u8 {
        const CARRY = 1 << 0;
        const PARITY = 1 << 2;
        const AUXILIARY_CARRY = 1 << 4;
        const ZERO = 1 << 6;
        const SIGN = 1 << 7;
    }
}

fn pair_to_u16(pair: [u8; 2]) -> u16 {
    (pair[0] as u16) + ((pair[1] as u16) << 8)
}

fn u16_to_pair(value: u16) -> [u8; 2] {
    [(value & 0x00ff) as u8, ((value & 0xff00) >> 8) as u8]
}

fn get_parity(value: u8) -> bool {
    let mut result = 0;
    for i in 0..8 {
        result ^= (value & (1 << i)) >> i;
    }

    return result == 0;
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
    BC = 0b00,
    DE = 0b01,
    HL = 0b10,
    SP = 0b11,
    PC,
}

#[derive(Error, Debug)]
pub enum RegisterPairError {
    #[error("the value {0} does not correspond to a register pair")]
    Unused(u8),
}

impl TryFrom<u8> for RegisterPair {
    type Error = RegisterPairError;
    fn try_from(value: u8) -> Result<Self, RegisterPairError> {
        match value {
            x if x == RegisterPair::BC as u8 => Ok(RegisterPair::BC),
            x if x == RegisterPair::DE as u8 => Ok(RegisterPair::DE),
            x if x == RegisterPair::HL as u8 => Ok(RegisterPair::HL),
            x if x == RegisterPair::SP as u8 => Ok(RegisterPair::SP),
            _ => Err(RegisterPairError::Unused(value)),
        }
    }
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
                a: 0,
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

    fn get_flags(&self) -> Flags {
        self.registers.flags
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

    fn get_register_pair(&self, rp: RegisterPair) -> u16 {
        match rp {
            RegisterPair::BC => pair_to_u16([self.registers.c, self.registers.b]),
            RegisterPair::DE => pair_to_u16([self.registers.e, self.registers.d]),
            RegisterPair::HL => pair_to_u16([self.registers.l, self.registers.h]),
            RegisterPair::PC => self.registers.program_counter,
            RegisterPair::SP => self.registers.stack_pointer,
        }
    }

    fn get_address_pc(&self) -> u16 {
        self.registers.program_counter
    }

    fn get_address_hl(&self) -> u16 {
        pair_to_u16([self.registers.l, self.registers.h])
    }

    fn get_memory_pc(&self) -> u8 {
        self.memory[self.get_address_pc() as usize]
    }

    fn get_memory_hl(&self) -> u8 {
        let address = pair_to_u16([self.registers.l, self.registers.h]);
        self.get_memory_at(address)
    }

    fn get_memory_at(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }

    fn execute_op(&mut self, operation: Operation) {
        match operation {
            Operation::Register(register_op) => match register_op.register {
                Register::A => self.registers.a = register_op.new_value,
                Register::B => self.registers.b = register_op.new_value,
                Register::C => self.registers.c = register_op.new_value,
                Register::D => self.registers.d = register_op.new_value,
                Register::E => self.registers.e = register_op.new_value,
                Register::H => self.registers.h = register_op.new_value,
                Register::L => self.registers.l = register_op.new_value,
            },
            Operation::RegisterPair(rp_op) => match rp_op.register {
                RegisterPair::BC => {
                    self.registers.c = u16_to_pair(rp_op.new_value)[0];
                    self.registers.b = u16_to_pair(rp_op.new_value)[1];
                }
                RegisterPair::PC => {
                    self.registers.program_counter = rp_op.new_value;
                }
                RegisterPair::DE => {
                    self.registers.e = u16_to_pair(rp_op.new_value)[0];
                    self.registers.d = u16_to_pair(rp_op.new_value)[1];
                }
                RegisterPair::SP => {
                    self.registers.stack_pointer = rp_op.new_value;
                }
                RegisterPair::HL => {
                    self.registers.l = u16_to_pair(rp_op.new_value)[0];
                    self.registers.h = u16_to_pair(rp_op.new_value)[1];
                }
            },
            Operation::Memory(memory_op) => {
                self.memory[memory_op.address as usize] = memory_op.new_value;
            }
            Operation::Flags(flags_op) => {
                self.registers.flags = flags_op.new_flags;
            }
        }
    }

    fn undo_op(&mut self, operation: Operation) {
        match operation {
            Operation::Register(register_op) => match register_op.register {
                Register::A => self.registers.a = register_op.old_value,
                Register::B => self.registers.b = register_op.old_value,
                Register::C => self.registers.c = register_op.old_value,
                Register::D => self.registers.d = register_op.old_value,
                Register::E => self.registers.e = register_op.old_value,
                Register::H => self.registers.h = register_op.old_value,
                Register::L => self.registers.l = register_op.old_value,
            },
            Operation::RegisterPair(rp_op) => match rp_op.register {
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
            },
            Operation::Memory(memory_op) => {
                self.memory[memory_op.address as usize] = memory_op.old_value;
            }
            Operation::Flags(flags_op) => {
                self.registers.flags = flags_op.old_flags;
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct FlagsOperation {
    old_flags: Flags,
    new_flags: Flags,
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
    Flags(FlagsOperation),
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
        Jz,
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
    #[error("there was no register pair corresponding to {0}")]
    RegisterPairError(u8),
    #[error("the opcode {0:?} is currently unimplemented")]
    Unimplemented(OpCode),
    #[error("the program recieved a halt signal")]
    Halt,
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
        self.cpu_state.memory[(address as usize)..(address as usize) + data.len()]
            .copy_from_slice(data);
    }

    fn read_byte(&mut self) -> u8 {
        let value = self.cpu_state.get_memory_pc();
        let op = Operation::RegisterPair(RegisterPairOperation {
            old_value: self.cpu_state.get_address_pc(),
            new_value: self.cpu_state.get_address_pc() + 1,
            register: RegisterPair::PC,
        });
        self.cpu_state.execute_op(op);
        self.push_command(op);
        value
    }

    fn read_double_bytes(&mut self) -> u16 {
        let value0 = self.cpu_state.get_memory_pc();
        let value1 = self
            .cpu_state
            .get_memory_at(self.cpu_state.get_address_pc() + 1);
        let op = Operation::RegisterPair(RegisterPairOperation {
            old_value: self.cpu_state.get_address_pc(),
            new_value: self.cpu_state.get_address_pc() + 2,
            register: RegisterPair::PC,
        });
        self.cpu_state.execute_op(op);
        self.push_command(op);

        pair_to_u16([value0, value1])
    }

    fn push_jmp(&mut self) {
        let address = self.read_double_bytes();
        let op = Operation::RegisterPair(RegisterPairOperation {
            old_value: self.cpu_state.get_address_pc(),
            new_value: address,
            register: RegisterPair::PC,
        });

        self.cpu_state.execute_op(op);
        self.push_command(op);
    }

    fn push_sub_mem_op(&mut self, difference: u8, carry: bool) {
        let mut new_flags = self.cpu_state.get_flags().clone();

        new_flags.set(Flags::CARRY, carry);
        new_flags.set(Flags::ZERO, difference == 0);
        new_flags.set(Flags::SIGN, (0b10000000 & difference) != 0);
        new_flags.set(Flags::PARITY, get_parity(difference));

        let sub_op = Operation::Memory(MemoryOperation {
            old_value: self.cpu_state.get_memory_hl(),
            new_value: difference,
            address: self.cpu_state.get_address_hl(),
        });

        self.cpu_state.execute_op(sub_op);
        self.push_command(sub_op);

        let flags_op = Operation::Flags(FlagsOperation {
            old_flags: self.cpu_state.get_flags(),
            new_flags: new_flags,
        });

        self.cpu_state.execute_op(flags_op);
        self.push_command(flags_op);
    }

    fn push_sub_op(&mut self, difference: u8, carry: bool, destination: Register) {
        let mut new_flags = self.cpu_state.get_flags().clone();

        new_flags.set(Flags::CARRY, carry);
        new_flags.set(Flags::ZERO, difference == 0);
        new_flags.set(Flags::SIGN, (0b10000000 & difference) != 0);
        new_flags.set(Flags::PARITY, get_parity(difference));

        let sub_op = Operation::Register(RegisterOperation {
            old_value: self.cpu_state.get_register(destination),
            new_value: difference,
            register: destination,
        });

        self.cpu_state.execute_op(sub_op);
        self.push_command(sub_op);

        let flags_op = Operation::Flags(FlagsOperation {
            old_flags: self.cpu_state.get_flags(),
            new_flags: new_flags,
        });

        self.cpu_state.execute_op(flags_op);
        self.push_command(flags_op);
    }

    fn push_add_mem_op(&mut self, sum: u8, carry: bool) {
        let mut new_flags = self.cpu_state.get_flags().clone();

        new_flags.set(Flags::CARRY, carry);
        new_flags.set(Flags::ZERO, sum == 0);
        new_flags.set(Flags::SIGN, (0b10000000 & sum) != 0);
        new_flags.set(Flags::PARITY, get_parity(sum));

        let add_op = Operation::Memory(MemoryOperation {
            old_value: self.cpu_state.get_memory_hl(),
            new_value: sum,
            address: self.cpu_state.get_address_hl(),
        });

        self.cpu_state.execute_op(add_op);
        self.push_command(add_op);

        let flags_op = Operation::Flags(FlagsOperation {
            old_flags: self.cpu_state.get_flags(),
            new_flags: new_flags,
        });

        self.cpu_state.execute_op(flags_op);
        self.push_command(flags_op);
    }

    fn push_add_op(&mut self, sum: u8, carry: bool, destination: Register) {
        let mut new_flags = self.cpu_state.get_flags().clone();

        new_flags.set(Flags::CARRY, carry);
        new_flags.set(Flags::ZERO, sum == 0);
        new_flags.set(Flags::SIGN, (0b10000000 & sum) != 0);
        new_flags.set(Flags::PARITY, get_parity(sum));

        let add_op = Operation::Register(RegisterOperation {
            old_value: self.cpu_state.get_register(destination),
            new_value: sum,
            register: destination,
        });

        self.cpu_state.execute_op(add_op);
        self.push_command(add_op);

        let flags_op = Operation::Flags(FlagsOperation {
            old_flags: self.cpu_state.get_flags(),
            new_flags: new_flags,
        });

        self.cpu_state.execute_op(flags_op);
        self.push_command(flags_op);
    }

    pub fn step_forward(&mut self) -> Result<(), StepError> {
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
                        address: self.cpu_state.get_address_hl(),
                        old_value: self.cpu_state.get_memory_hl(),
                        new_value: self.cpu_state.get_register(source_register),
                    });

                    self.cpu_state.execute_op(op);
                    self.push_command(op);
                } else if destination_register.is_ok() {
                    let destination_register = destination_register.unwrap();
                    let op = Operation::Register(RegisterOperation {
                        old_value: self.cpu_state.get_register(destination_register),
                        new_value: self.cpu_state.get_memory_hl(),
                        register: destination_register,
                    });

                    self.cpu_state.execute_op(op);
                    self.push_command(op);
                } else {
                    return Err(StepError::RegisterError(source_register.err().unwrap()));
                }
            }
            OpCode::MviA
            | OpCode::MviB
            | OpCode::MviC
            | OpCode::MviD
            | OpCode::MviE
            | OpCode::MviH
            | OpCode::MviL
            | OpCode::MviM => {
                let destination = (instruction & 0b00111000) >> 3;
                let destination_register = Register::try_from(destination);
                let value = self.read_byte();

                if destination_register.is_ok() {
                    let destination_register = destination_register.unwrap();
                    let op = Operation::Register(RegisterOperation {
                        old_value: self.cpu_state.get_register(destination_register),
                        new_value: value,
                        register: destination_register,
                    });

                    self.cpu_state.execute_op(op);
                    self.push_command(op);
                } else {
                    let op = Operation::Memory(MemoryOperation {
                        address: self.cpu_state.get_address_hl(),
                        old_value: self.cpu_state.get_memory_hl(),
                        new_value: value,
                    });

                    self.cpu_state.execute_op(op);
                    self.push_command(op);
                }
            }
            OpCode::LxiB | OpCode::LxiD | OpCode::LxiH => {
                let rp_code = (instruction & 0b00110000) >> 4;
                let rp = match rp_code {
                    0b00 => Some(RegisterPair::BC),
                    0b01 => Some(RegisterPair::DE),
                    0b10 => Some(RegisterPair::HL),
                    _ => None,
                };

                let value = self.read_double_bytes();

                if rp.is_none() {
                    return Err(StepError::RegisterPairError(rp_code));
                }

                let rp = rp.unwrap();
                let op = Operation::RegisterPair(RegisterPairOperation {
                    old_value: self.cpu_state.get_register_pair(rp),
                    new_value: value,
                    register: rp,
                });

                self.cpu_state.execute_op(op);
                self.push_command(op);
            }
            OpCode::AddA
            | OpCode::AddB
            | OpCode::AddC
            | OpCode::AddD
            | OpCode::AddE
            | OpCode::AddH
            | OpCode::AddL
            | OpCode::AddM => {
                let source = instruction & 0b00000111;
                let source_register = Register::try_from(source);

                if source_register.is_ok() {
                    let source_register = source_register.unwrap();
                    let (sum, carry) = self
                        .cpu_state
                        .get_register(Register::A)
                        .overflowing_add(self.cpu_state.get_register(source_register));
                    self.push_add_op(sum, carry, Register::A);
                } else {
                    let (sum, carry) = self
                        .cpu_state
                        .get_register(Register::A)
                        .overflowing_add(self.cpu_state.get_memory_hl());
                    self.push_add_op(sum, carry, Register::A);
                }
            }
            OpCode::AdcA
            | OpCode::AdcB
            | OpCode::AdcC
            | OpCode::AdcD
            | OpCode::AdcE
            | OpCode::AdcH
            | OpCode::AdcL
            | OpCode::AdcM => {
                let source = instruction & 0b00000111;
                let source_register = Register::try_from(source);

                if source_register.is_ok() {
                    let source_register = source_register.unwrap();
                    let old_carry = self.cpu_state.get_flags().contains(Flags::CARRY) as u8;
                    let (sum0, carry0) = self
                        .cpu_state
                        .get_register(Register::A)
                        .overflowing_add(self.cpu_state.get_register(source_register));
                    let (sum1, carry1) = sum0.overflowing_add(old_carry);
                    self.push_add_op(sum1, carry1 && carry0, Register::A);
                } else {
                    let old_carry = self.cpu_state.get_flags().contains(Flags::CARRY) as u8;
                    let (sum0, carry0) = self
                        .cpu_state
                        .get_register(Register::A)
                        .overflowing_add(self.cpu_state.get_memory_hl());
                    let (sum1, carry1) = sum0.overflowing_add(old_carry);
                    self.push_add_op(sum1, carry1 && carry0, Register::A);
                }
            }
            OpCode::Adi => {
                let value = self.read_byte();
                let (sum, carry) = self
                    .cpu_state
                    .get_register(Register::A)
                    .overflowing_add(value);

                self.push_add_op(sum, carry, Register::A);
            }
            OpCode::Aci => {
                let value = self.read_byte();
                let old_carry = self.cpu_state.get_flags().contains(Flags::CARRY) as u8;
                let (sum0, carry0) = self
                    .cpu_state
                    .get_register(Register::A)
                    .overflowing_add(value);
                let (sum1, carry1) = sum0.overflowing_add(old_carry);

                self.push_add_op(sum1, carry0 && carry1, Register::A);
            }
            OpCode::InrA
            | OpCode::InrB
            | OpCode::InrC
            | OpCode::InrD
            | OpCode::InrE
            | OpCode::InrH
            | OpCode::InrL
            | OpCode::InrM => {
                let destination: u8 = (instruction & 0b00111000) >> 3;
                let destination_register = Register::try_from(destination);

                if destination_register.is_ok() {
                    let destination_register = destination_register.unwrap();
                    let (sum, carry) = self
                        .cpu_state
                        .get_register(destination_register)
                        .overflowing_add(1);
                    self.push_add_op(sum, carry, destination_register);
                } else {
                    let (sum, carry) = self.cpu_state.get_memory_hl().overflowing_add(1);
                    self.push_add_mem_op(sum, carry);
                }
            }
            OpCode::InxB | OpCode::InxD | OpCode::InxH | OpCode::InxSp => {
                let destination = (0b00110000 & instruction) >> 4;
                let destination_rp = RegisterPair::try_from(destination);

                if let Ok(destination_rp) = destination_rp {
                    let sum = self
                        .cpu_state
                        .get_register_pair(destination_rp)
                        .wrapping_add(1);
                    let op = Operation::RegisterPair(RegisterPairOperation {
                        old_value: self.cpu_state.get_register_pair(destination_rp),
                        new_value: sum,
                        register: destination_rp,
                    });

                    self.cpu_state.execute_op(op);
                    self.push_command(op);
                } else {
                    return Err(StepError::RegisterPairError(destination));
                }
            }
            OpCode::DadB | OpCode::DadD | OpCode::DadH | OpCode::DadSP => {
                let source = (0b00110000 & instruction) >> 4;
                let source_rp = RegisterPair::try_from(source);

                if source_rp.is_ok() {
                    let source_rp = source_rp.unwrap();
                    let (sum, carry) = self
                        .cpu_state
                        .get_register_pair(source_rp)
                        .overflowing_add(self.cpu_state.get_register_pair(RegisterPair::HL));
                    let mut new_flags = self.cpu_state.get_flags().clone();
                    new_flags.set(Flags::CARRY, carry);

                    let add_op = Operation::RegisterPair(RegisterPairOperation {
                        old_value: self.cpu_state.get_register_pair(RegisterPair::HL),
                        new_value: sum,
                        register: RegisterPair::HL,
                    });
                    self.cpu_state.execute_op(add_op);
                    self.push_command(add_op);

                    let flags_op = Operation::Flags(FlagsOperation {
                        old_flags: self.cpu_state.get_flags(),
                        new_flags: new_flags,
                    });
                    self.cpu_state.execute_op(flags_op);
                    self.push_command(flags_op);
                } else {
                    return Err(StepError::RegisterPairError(source));
                }
            }
            OpCode::Sui => {
                let value = self.read_byte();
                // to future me: yes, the carry flag is the fucking borrow flag in this and someone
                // thought this is a good idea
                let (difference, carry) = self
                    .cpu_state
                    .get_register(Register::A)
                    .overflowing_sub(value);
                self.push_sub_op(difference, carry, Register::A);
            }
            OpCode::Sbi => {
                let value = self.read_byte();

                let old_carry = self.cpu_state.get_flags().contains(Flags::CARRY) as u8;
                let (difference0, carry0) = self
                    .cpu_state
                    .get_register(Register::A)
                    .overflowing_sub(value);
                let (difference1, carry1) = difference0.overflowing_sub(old_carry);
                self.push_sub_op(difference1, carry0 && carry1, Register::A);
            }
            OpCode::SubA
            | OpCode::SubB
            | OpCode::SubC
            | OpCode::SubD
            | OpCode::SubE
            | OpCode::SubH
            | OpCode::SubL
            | OpCode::SubM => {
                let source = 0b00000111 & instruction;
                let source_register = Register::try_from(source);

                if let Ok(source_register) = source_register {
                    let (difference, carry) = self
                        .cpu_state
                        .get_register(Register::A)
                        .overflowing_sub(self.cpu_state.get_register(source_register));
                    self.push_sub_op(difference, carry, Register::A);
                } else {
                    let (difference, carry) = self
                        .cpu_state
                        .get_register(Register::A)
                        .overflowing_sub(self.cpu_state.get_memory_hl());
                    self.push_sub_op(difference, carry, Register::A);
                }
            }
            OpCode::SbbA
            | OpCode::SbbB
            | OpCode::SbbC
            | OpCode::SbbD
            | OpCode::SbbE
            | OpCode::SbbH
            | OpCode::SbbL
            | OpCode::SbbM => {
                let source = 0b00000111 & instruction;
                let source_register = Register::try_from(source);

                let old_carry = self.cpu_state.get_flags().contains(Flags::CARRY) as u8;
                if let Ok(source_register) = source_register {
                    let (difference0, carry0) = self
                        .cpu_state
                        .get_register(Register::A)
                        .overflowing_sub(self.cpu_state.get_register(source_register));
                    let (difference1, carry1) = difference0.overflowing_sub(old_carry);
                    self.push_sub_op(difference1, carry0 && carry1, Register::A);
                } else {
                    let (difference0, carry0) = self
                        .cpu_state
                        .get_register(Register::A)
                        .overflowing_sub(self.cpu_state.get_memory_hl());
                    let (difference1, carry1) = difference0.overflowing_sub(old_carry);
                    self.push_sub_op(difference1, carry0 && carry1, Register::A);
                }
            }
            // OpCode::CmpA
            // | OpCode::CmpB
            // | OpCode::CmpC
            // | OpCode::CmpD
            // | OpCode::CmpE
            // | OpCode::CmpH
            // | OpCode::CmpL
            // | OpCode::CmpM => {
            //     let source = instruction & 0b00000111;
            //     let source_register = Register::try_from(source);
            //
            //     if let Ok(source_register) = source_register {
            //         let mut new_flags = self.cpu_state.get_flags().clone();
            //         new_flags.set(Flags::ZERO, );
            //     } else {
            //     }
            // }
            OpCode::DcrA
            | OpCode::DcrB
            | OpCode::DcrC
            | OpCode::DcrD
            | OpCode::DcrE
            | OpCode::DcrH
            | OpCode::DcrL
            | OpCode::DcrM => {
                let destination = (instruction & 0b00111000) >> 3;
                let destination_register = Register::try_from(destination);

                if let Ok(destination_register) = destination_register {
                    let (difference, carry) = self
                        .cpu_state
                        .get_register(destination_register)
                        .overflowing_sub(1);
                    self.push_sub_op(difference, carry, destination_register);
                } else {
                    let (difference, carry) = self.cpu_state.get_memory_hl().overflowing_sub(1);
                    self.push_sub_mem_op(difference, carry);
                }
            }
            OpCode::DcxB | OpCode::DcxD | OpCode::DcxH | OpCode::DcxSP => {
                let destination = (0b00110000 & instruction) >> 4;
                let destination_rp = RegisterPair::try_from(destination);

                if let Ok(destination_rp) = destination_rp {
                    let difference = self
                        .cpu_state
                        .get_register_pair(destination_rp)
                        .wrapping_sub(1);
                    let op = Operation::RegisterPair(RegisterPairOperation {
                        old_value: self.cpu_state.get_register_pair(destination_rp),
                        new_value: difference,
                        register: destination_rp,
                    });

                    self.cpu_state.execute_op(op);
                    self.push_command(op);
                } else {
                    return Err(StepError::RegisterPairError(destination));
                }
            }
            OpCode::Jmp => {
                self.push_jmp();
            }
            OpCode::Hlt => {
                return Err(StepError::Halt);
            }
            OpCode::Jc => {
                if self.cpu_state.get_flags().contains(Flags::CARRY) {
                    self.push_jmp();
                } else {
                    _ = self.read_double_bytes();
                }
            }
            OpCode::Jnc => {
                if !self.cpu_state.get_flags().contains(Flags::CARRY) {
                    self.push_jmp();
                } else {
                    _ = self.read_double_bytes();
                }
            }
            OpCode::Jz => {
                if self.cpu_state.get_flags().contains(Flags::ZERO) {
                    self.push_jmp();
                } else {
                    _ = self.read_double_bytes();
                }
            }
            OpCode::Jnz => {
                if !self.cpu_state.get_flags().contains(Flags::ZERO) {
                    self.push_jmp();
                } else {
                    _ = self.read_double_bytes();
                }
            }
            OpCode::Jp => {
                if !self.cpu_state.get_flags().contains(Flags::SIGN) {
                    self.push_jmp();
                } else {
                    _ = self.read_double_bytes();
                }
            }
            OpCode::Jm => {
                if self.cpu_state.get_flags().contains(Flags::SIGN) {
                    self.push_jmp();
                } else {
                    _ = self.read_double_bytes();
                }
            }
            OpCode::Jpe => {
                if self.cpu_state.get_flags().contains(Flags::PARITY) {
                    self.push_jmp();
                } else {
                    _ = self.read_double_bytes();
                }
            }
            OpCode::Jpo => {
                if !self.cpu_state.get_flags().contains(Flags::PARITY) {
                    self.push_jmp();
                } else {
                    _ = self.read_double_bytes();
                }
            }
            _ => {
                return Err(StepError::Unimplemented(opcode));
            }
        }

        self.instruction_count += 1;
        Ok(())
    }

    pub fn execute(&mut self, entrypoint: u16) {
        self.cpu_state.registers.program_counter = entrypoint;
        loop {
            if let Err(err) = self.step_forward() {
                match err {
                    StepError::Halt => break,
                    _ => {
                        eprintln!("{}", err);
                    }
                }
            }
        }
    }
}
