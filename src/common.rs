use thiserror::Error;

#[derive(Error, Debug)]
pub enum OpCodeError {
    #[error("the opcode {0} is unused")]
    Unused(u8),
}

macro_rules! convertible {
    (#[$($meta:meta)*]
     $vis:vis enum $name:ident {
         $($field:ident $(= $value:literal)? $(,)?)*
    }) => {
        #[$($meta)*]
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
    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum OpCode {
        Nop = 0x00,
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
        LxiSp,
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
pub enum RegMemError {
    #[error("the value `{0}` does not correspond to a register / memory")]
    UnusedByte(u8),
    #[error("the value `{0}` does not correspond to a register / memory")]
    UnusedStr(String),
}

#[derive(Copy, Clone, Debug)]
pub enum RegMem {
    B = 0b000,
    C = 0b001,
    D = 0b010,
    E = 0b011,
    H = 0b100,
    L = 0b101,
    M = 0b110,
    A = 0b111,
}

impl TryFrom<u8> for RegMem {
    type Error = RegMemError;
    fn try_from(value: u8) -> Result<RegMem, RegMemError> {
        match value {
            x if x == RegMem::A as u8 => Ok(RegMem::A),
            x if x == RegMem::B as u8 => Ok(RegMem::B),
            x if x == RegMem::C as u8 => Ok(RegMem::C),
            x if x == RegMem::D as u8 => Ok(RegMem::D),
            x if x == RegMem::E as u8 => Ok(RegMem::E),
            x if x == RegMem::H as u8 => Ok(RegMem::H),
            x if x == RegMem::L as u8 => Ok(RegMem::L),
            x if x == RegMem::M as u8 => Ok(RegMem::M),

            _ => Err(RegMemError::UnusedByte(value)),
        }
    }
}

impl TryFrom<&str> for RegMem {
    type Error = RegMemError;
    fn try_from(value: &str) -> Result<Self, RegMemError> {
        match value {
            "a" => Ok(RegMem::A),
            "b" => Ok(RegMem::B),
            "c" => Ok(RegMem::C),
            "d" => Ok(RegMem::D),
            "e" => Ok(RegMem::E),
            "h" => Ok(RegMem::H),
            "l" => Ok(RegMem::L),
            "m" => Ok(RegMem::M),
            _ => Err(RegMemError::UnusedStr(value.to_string())),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Register {
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
pub enum RegisterPair {
    BC = 0b00,
    DE = 0b01,
    HL = 0b10,
    SP = 0b11,
    PC,
}

#[derive(Error, Debug)]
pub enum RegisterPairError {
    #[error("the value `{0}` does not correspond to a register pair")]
    UnusedByte(u8),
    #[error("the value `{0}` does not correspond to a register pair")]
    UnusedStr(String),
}

impl TryFrom<u8> for RegisterPair {
    type Error = RegisterPairError;
    fn try_from(value: u8) -> Result<Self, RegisterPairError> {
        match value {
            x if x == RegisterPair::BC as u8 => Ok(RegisterPair::BC),
            x if x == RegisterPair::DE as u8 => Ok(RegisterPair::DE),
            x if x == RegisterPair::HL as u8 => Ok(RegisterPair::HL),
            x if x == RegisterPair::SP as u8 => Ok(RegisterPair::SP),
            _ => Err(RegisterPairError::UnusedByte(value)),
        }
    }
}

impl TryFrom<&str> for RegisterPair {
    type Error = RegisterPairError;
    fn try_from(value: &str) -> Result<Self, RegisterPairError> {
        match value {
            "b" => Ok(RegisterPair::BC),
            "d" => Ok(RegisterPair::DE),
            "h" => Ok(RegisterPair::HL),
            "sp" => Ok(RegisterPair::SP),
            _ => Err(RegisterPairError::UnusedStr(value.to_string())),
        }
    }
}
