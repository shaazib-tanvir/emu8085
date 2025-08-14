use std::num::ParseIntError;

use crate::common::{OpCode, RegMem, RegMemError, RegisterPair, RegisterPairError};
use thiserror::Error;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mov_parse() {
        let instruction = Instruction::parse("mov a,m");
        if let Ok(Instruction::NoData(instruction)) = instruction {
            assert_eq!(instruction.opcode, OpCode::MovAM);
        } else {
            panic!("expected rm,rm instruction, found {:?}", instruction);
        }
    }

    #[test]
    fn mvi_parse() {
        let instruction = Instruction::parse("mvi m,1f");
        if let Ok(Instruction::ImByte(instruction)) = instruction {
            assert_eq!(instruction.opcode, OpCode::MviM);
            assert_eq!(instruction.operand, 0x1f);
        } else {
            panic!("expected rm,value instruction, found {:?}", instruction);
        }
    }

    #[test]
    fn lxi_parse() {
        let instruction = Instruction::parse("lxi h, 2050");
        if let Ok(Instruction::ImWord(instruction)) = instruction {
            assert_eq!(instruction.opcode, OpCode::LxiH);
            match instruction.operand {
                Word::U16(value) => assert_eq!(value, 0x2050),
                _ => panic!("value is `{:?}` instead of 0x2050", instruction.operand),
            }
        } else {
            panic!("expected rp,value instruction, found {:?}", instruction);
        }
    }

    #[test]
    fn dcx_parse() {
        let instruction = Instruction::parse("dcx h");
        if let Ok(Instruction::NoData(instruction)) = instruction {
            assert_eq!(instruction.opcode, OpCode::DcxH);
        } else {
            panic!("expected rp instruction, found {:?}", instruction);
        }
    }

    #[test]
    fn dad_parse() {
        let instruction = Instruction::parse("dad d");
        if let Ok(Instruction::NoData(instruction)) = instruction {
            assert_eq!(instruction.opcode, OpCode::DadD);
        } else {
            panic!("expected rp instruction, found {:?}", instruction);
        }
    }

    #[test]
    fn sui_parse() {
        let instruction = Instruction::parse("sui 4d");
        if let Ok(Instruction::ImByte(instruction)) = instruction {
            assert_eq!(instruction.opcode, OpCode::Sui);
            assert_eq!(instruction.operand, 0x4d);
        } else {
            panic!("expected im byte instruction, found {:?}", instruction);
        }
    }
}

#[derive(Debug)]
enum Word {
    U16(u16),
    Label(String),
}

#[derive(Debug)]
struct InstructionImByte {
    opcode: OpCode,
    operand: u8,
}

#[derive(Debug)]
struct InstructionImWord {
    opcode: OpCode,
    operand: Word,
}

#[derive(Debug)]
struct InstructionNoData {
    opcode: OpCode,
}

#[derive(Debug)]
enum Instruction {
    ImByte(InstructionImByte),
    ImWord(InstructionImWord),
    NoData(InstructionNoData),
}

#[derive(Error, Debug)]
enum OperandParseError {
    #[error("expected `{expected}` operands, got `{got}`")]
    InsufficientOperands { expected: u8, got: u8 },
    #[error("expected no operands")]
    NoOp,
    #[error(transparent)]
    RegMem(#[from] RegMemError),
    #[error(transparent)]
    Rp(#[from] RegisterPairError),
    #[error("failed to parse: {0}")]
    ParseU8(ParseIntError),
    #[error("{0}")]
    Label(String),
}

#[derive(Error, Debug)]
enum InstructionError {
    #[error(transparent)]
    OperandParse(#[from] OperandParseError),
    #[error(transparent)]
    RegMem(#[from] RegMemError),
    #[error("the mnemonic `{0}` does not exist or is unimplemented")]
    UnknownMnemonic(String),
    #[error("fatal error: unreachable code")]
    Unreachable,
}

fn parse_label(label: &str) -> Result<Word, OperandParseError> {
    let mut chars = label.chars();
    let first_char = chars.next();
    if let Some(first_char) = first_char {
        if !first_char.is_alphabetic() {
            return Err(OperandParseError::Label(
                "labels must start with an alphabet".to_string(),
            ));
        }
    } else {
        return Ok(Word::Label(label.to_string()));
    }

    for char in chars {
        if char.is_alphanumeric() {
            return Err(OperandParseError::Label(
                "labels must only contain alphanumeric characters".to_string(),
            ));
        }
    }

    return Ok(Word::Label(label.to_string()));
}

fn parse_rm_rm(operands: &str) -> Result<(RegMem, RegMem), OperandParseError> {
    let operands = operands.split_once(",");
    if let Some(operands) = operands {
        let destination = RegMem::try_from(operands.0)?;
        let source = RegMem::try_from(operands.1)?;

        Ok((destination, source))
    } else {
        Err(OperandParseError::InsufficientOperands {
            expected: 2,
            got: 1,
        })
    }
}

fn parse_rm_byte(operands: &str) -> Result<(RegMem, u8), OperandParseError> {
    let operands = operands.split_once(",");
    if let Some(operands) = operands {
        let destination = RegMem::try_from(operands.0)?;
        let value = u8::from_str_radix(operands.1, 16);
        if value.is_err() {
            return Err(OperandParseError::ParseU8(value.err().unwrap()));
        }

        let value: u8 = value.unwrap();

        Ok((destination, value))
    } else {
        Err(OperandParseError::InsufficientOperands {
            expected: 2,
            got: 1,
        })
    }
}

fn parse_rm_word(operands: &str) -> Result<(RegisterPair, Word), OperandParseError> {
    let operands = operands.split_once(",");
    if let Some(operands) = operands {
        let destination = RegisterPair::try_from(operands.0)?;
        let value = u16::from_str_radix(operands.1, 16);
        if value.is_err() {
            let value = parse_label(operands.1)?;
            Ok((destination, value))
        } else {
            let value: Word = Word::U16(value.unwrap());
            Ok((destination, value))
        }
    } else {
        Err(OperandParseError::InsufficientOperands {
            expected: 2,
            got: 1,
        })
    }
}

fn parse_byte(operands: &str) -> Result<u8, OperandParseError> {
    let value = u8::from_str_radix(operands, 16);
    match value {
        Ok(value) => Ok(value),
        Err(value) => Err(OperandParseError::ParseU8(value)),
    }
}

fn parse_word(operands: &str) -> Result<Word, OperandParseError> {
    let value = u16::from_str_radix(operands, 16);
    if value.is_err() {
        let value = parse_label(operands)?;
        Ok(value)
    } else {
        let value: Word = Word::U16(value.unwrap());
        Ok(value)
    }
}

fn parse_rp(operands: &str) -> Result<RegisterPair, OperandParseError> {
    let rp = RegisterPair::try_from(operands)?;

    Ok(rp)
}

fn parse_rm(operands: &str) -> Result<RegMem, OperandParseError> {
    let rm = RegMem::try_from(operands)?;

    Ok(rm)
}

impl Instruction {
    fn parse(line: &str) -> Result<Self, InstructionError> {
        let line_split = line.split_once(" ");
        let (mnemonic, operands) = if let Some(line_split) = line_split {
            (line_split.0, Some(line_split.1.replace(" ", "")))
        } else {
            (line, None)
        };

        match mnemonic {
            "mov" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 2,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let (destination, source) = parse_rm_rm(&operands)?;
                let opcode = 0b01000000 + ((destination as u8) << 3) + (source as u8);
                let opcode = OpCode::try_from(opcode).unwrap();

                Ok(Instruction::NoData(InstructionNoData { opcode: opcode }))
            }
            "mvi" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 2,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let (destination, value) = parse_rm_byte(&operands)?;

                let opcode = 0b00000110 + ((destination as u8) << 3);
                let opcode = OpCode::try_from(opcode).unwrap();

                Ok(Instruction::ImByte(InstructionImByte {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "lxi" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 2,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let (destination, value) = parse_rm_word(&operands)?;
                let opcode = 0b00000001 + ((destination as u8) << 4);
                let opcode = OpCode::try_from(opcode).unwrap();

                Ok(Instruction::ImWord(InstructionImWord {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "stax" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let rp = parse_rp(&operands)?;
                let opcode = match rp {
                    RegisterPair::BC => Ok(OpCode::StaxB),
                    RegisterPair::DE => Ok(OpCode::StaxD),
                    _ => Err(InstructionError::Unreachable),
                }?;

                Ok(Instruction::NoData(InstructionNoData { opcode: opcode }))
            }
            "ldax" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let rp = parse_rp(&operands)?;
                let opcode = match rp {
                    RegisterPair::BC => Ok(OpCode::LdaxB),
                    RegisterPair::DE => Ok(OpCode::LdaxD),
                    _ => Err(InstructionError::Unreachable),
                }?;

                Ok(Instruction::NoData(InstructionNoData { opcode: opcode }))
            }
            "sta" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_word(&operands)?;
                let opcode = OpCode::Sta;

                Ok(Instruction::ImWord(InstructionImWord {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "lda" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_word(&operands)?;
                let opcode = OpCode::Lda;

                Ok(Instruction::ImWord(InstructionImWord {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "shld" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_word(&operands)?;
                let opcode = OpCode::Shld;

                Ok(Instruction::ImWord(InstructionImWord {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "lhld" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_word(&operands)?;
                let opcode = OpCode::Lhld;

                Ok(Instruction::ImWord(InstructionImWord {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "xchg" => {
                if operands.is_some() {
                    return Err(InstructionError::OperandParse(OperandParseError::NoOp));
                }

                Ok(Instruction::NoData(InstructionNoData {
                    opcode: OpCode::Xchg,
                }))
            }
            "jmp" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_word(&operands)?;
                let opcode = OpCode::Jmp;

                Ok(Instruction::ImWord(InstructionImWord {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "jc" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_word(&operands)?;
                let opcode = OpCode::Jc;

                Ok(Instruction::ImWord(InstructionImWord {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "jnc" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_word(&operands)?;
                let opcode = OpCode::Jnc;

                Ok(Instruction::ImWord(InstructionImWord {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "jz" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_word(&operands)?;
                let opcode = OpCode::Jz;

                Ok(Instruction::ImWord(InstructionImWord {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "jnz" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_word(&operands)?;
                let opcode = OpCode::Jnz;

                Ok(Instruction::ImWord(InstructionImWord {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "jp" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_word(&operands)?;
                let opcode = OpCode::Jp;

                Ok(Instruction::ImWord(InstructionImWord {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "jm" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_word(&operands)?;
                let opcode = OpCode::Jm;

                Ok(Instruction::ImWord(InstructionImWord {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "jpe" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_word(&operands)?;
                let opcode = OpCode::Jpe;

                Ok(Instruction::ImWord(InstructionImWord {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "jpo" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_word(&operands)?;
                let opcode = OpCode::Jpo;

                Ok(Instruction::ImWord(InstructionImWord {
                    opcode: opcode,
                    operand: value,
                }))
            }
            "pchl" => {
                if operands.is_some() {
                    return Err(InstructionError::OperandParse(OperandParseError::NoOp));
                }

                Ok(Instruction::NoData(InstructionNoData {
                    opcode: OpCode::Pchl,
                }))
            }
            "inr" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let rm = parse_rm(&operands)?;

                let opcode = 0b00000100 + ((rm as u8) << 3);
                let opcode = OpCode::try_from(opcode).unwrap();

                Ok(Instruction::NoData(InstructionNoData { opcode: opcode }))
            }
            "dcr" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let rm = parse_rm(&operands)?;

                let opcode = 0b00000101 + ((rm as u8) << 3);
                let opcode = OpCode::try_from(opcode).unwrap();

                Ok(Instruction::NoData(InstructionNoData { opcode: opcode }))
            }
            "inx" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let rp = parse_rp(&operands)?;

                let opcode = 0b00000011 + ((rp as u8) << 4);
                let opcode = OpCode::try_from(opcode).unwrap();

                Ok(Instruction::NoData(InstructionNoData { opcode: opcode }))
            }
            "dcx" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let rp = parse_rp(&operands)?;

                let opcode = 0b00001011 + ((rp as u8) << 4);
                let opcode = OpCode::try_from(opcode).unwrap();

                Ok(Instruction::NoData(InstructionNoData { opcode: opcode }))
            }
            "add" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let rm = parse_rm(&operands)?;

                let opcode = 0b10000000 + (rm as u8);
                let opcode = OpCode::try_from(opcode).unwrap();

                Ok(Instruction::NoData(InstructionNoData { opcode: opcode }))
            }
            "adc" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let rm = parse_rm(&operands)?;

                let opcode = 0b10001000 + (rm as u8);
                let opcode = OpCode::try_from(opcode).unwrap();

                Ok(Instruction::NoData(InstructionNoData { opcode: opcode }))
            }
            "adi" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_byte(&operands)?;

                Ok(Instruction::ImByte(InstructionImByte {
                    opcode: OpCode::Aci,
                    operand: value,
                }))
            }
            "aci" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_byte(&operands)?;

                Ok(Instruction::ImByte(InstructionImByte {
                    opcode: OpCode::Aci,
                    operand: value,
                }))
            }
            "dad" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let rp = parse_rp(&operands)?;

                let opcode = 0b00001001 + ((rp as u8) << 4);
                let opcode = OpCode::try_from(opcode).unwrap();

                Ok(Instruction::NoData(InstructionNoData { opcode: opcode }))
            }
            "sub" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let rm = parse_rm(&operands)?;

                let opcode = 0b10010000 + (rm as u8);
                let opcode = OpCode::try_from(opcode).unwrap();

                Ok(Instruction::NoData(InstructionNoData { opcode: opcode }))
            }
            "sbb" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let rm = parse_rm(&operands)?;

                let opcode = 0b10011000 + (rm as u8);
                let opcode = OpCode::try_from(opcode).unwrap();

                Ok(Instruction::NoData(InstructionNoData { opcode: opcode }))
            }
            "sui" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_byte(&operands)?;

                Ok(Instruction::ImByte(InstructionImByte {
                    opcode: OpCode::Sui,
                    operand: value,
                }))
            }
            "sbi" => {
                if operands.is_none() {
                    return Err(InstructionError::OperandParse(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_byte(&operands)?;

                Ok(Instruction::ImByte(InstructionImByte {
                    opcode: OpCode::Sbi,
                    operand: value,
                }))
            }
            _ => Err(InstructionError::UnknownMnemonic(mnemonic.to_string())),
        }
    }
}
