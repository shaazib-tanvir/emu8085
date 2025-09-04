use std::collections::HashMap;
use std::fs;
use std::fs::read_to_string;
use std::io::{Read, Write};
use std::{num::ParseIntError, path::Path};

use crate::common::{
    OpCode, RegMem, RegMemError, RegisterPair, RegisterPairError, pair_to_u16, u16_to_pair,
};
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

    #[test]
    fn db_parse() {
        let directive = Directive::parse("db ff");
        if let Ok(Directive::Db(value)) = directive {
            assert_eq!(value, 0xff);
        } else {
            panic!("expected directive db, found {:?}", directive);
        }
    }

    #[test]
    fn program_parse() {
        let program = ".org 2000
.start
lxi h, 2050
mov a, m";
        let program = Program::parse(program);
        assert!(program.is_ok(), "program did not parse: {:?}", program);

        let program = program.unwrap();
        let section = program.sections.get(0).unwrap();
        assert_eq!(section.units.len(), 4);
        assert_eq!(
            section.units.get(0).cloned().unwrap(),
            IntermediateUnit {
                line: Line {
                    line: ".org 2000".to_string(),
                    line_number: 0,
                },
                unit: Unit::Directive(Directive::Org(0x2000)),
                address: 0x2000,
            }
        );
        assert_eq!(
            section.units.get(1).cloned().unwrap(),
            IntermediateUnit {
                line: Line {
                    line: ".start".to_string(),
                    line_number: 1,
                },
                unit: Unit::Directive(Directive::Start),
                address: 0x2000,
            }
        );
        assert_eq!(
            section.units.get(2).cloned().unwrap(),
            IntermediateUnit {
                line: Line {
                    line: "lxi h, 2050".to_string(),
                    line_number: 2,
                },
                unit: Unit::Instruction(Instruction::ImWord(InstructionImWord {
                    opcode: OpCode::LxiH,
                    operand: Word::U16(0x2050),
                })),
                address: 0x2000,
            }
        );
        assert_eq!(
            section.units.get(3).cloned().unwrap(),
            IntermediateUnit {
                line: Line {
                    line: "mov a, m".to_string(),
                    line_number: 3,
                },
                unit: Unit::Instruction(Instruction::NoData(InstructionNoData {
                    opcode: OpCode::MovAM,
                })),
                address: 0x2003,
            }
        );
    }

    #[test]
    fn program_assemble() {
        let program = ".org 2000
.start
lxi h, 2050
mov a, m";
        let assembled_program = AssembledProgram::assemble(program);
        assert!(assembled_program.is_ok());
        let assembled_program = assembled_program.unwrap();

        assert!(assembled_program.segments.get(0).is_some());
        let segment = assembled_program.segments.get(0).unwrap();
        assert_eq!(segment.address, 0x2000);
        assert_eq!(*segment.data.get(0).unwrap(), OpCode::LxiH as u8);
        assert_eq!(*segment.data.get(1).unwrap(), 0x50);
        assert_eq!(*segment.data.get(2).unwrap(), 0x20);
        assert_eq!(*segment.data.get(3).unwrap(), OpCode::MovAM as u8);
    }

    #[test]
    fn db_assemble() {
        let program = ".org 2050
.db ff

.org 2000
.start
hlt";
        let assembled_program = AssembledProgram::assemble(program);
        assert!(assembled_program.is_ok());
        let assembled_program = assembled_program.unwrap();

        assert_eq!(assembled_program.segments.get(0).unwrap().address, 0x2050);
        assert_eq!(*assembled_program.segments.get(0).unwrap().data.get(0).unwrap(), 0xff);
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Word {
    U16(u16),
    Label(String),
}

#[derive(Debug, PartialEq, Clone)]
struct InstructionImByte {
    opcode: OpCode,
    operand: u8,
}

#[derive(Debug, PartialEq, Clone)]
struct InstructionImWord {
    opcode: OpCode,
    operand: Word,
}

#[derive(Debug, PartialEq, Clone)]
struct InstructionNoData {
    opcode: OpCode,
}

#[derive(Debug, PartialEq, Clone)]
enum Instruction {
    ImByte(InstructionImByte),
    ImWord(InstructionImWord),
    NoData(InstructionNoData),
}

#[derive(Error, Debug)]
pub enum OperandParseError {
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
    #[error("failed to parse: {0}")]
    ParseU16(ParseIntError),
    #[error("{0}")]
    Label(String),
}

#[derive(Error, Debug)]
pub enum InstructionError {
    #[error(transparent)]
    OperandParse(#[from] OperandParseError),
    #[error(transparent)]
    RegMem(#[from] RegMemError),
    #[error("the mnemonic `{0}` does not exist or is unimplemented")]
    UnknownMnemonic(String),
    #[error("fatal error: unreachable code")]
    Unreachable,
}

fn check_label(label: &str) -> Option<OperandParseError> {
    let mut chars = label.chars();
    let first_char = chars.next();
    if let Some(first_char) = first_char {
        if !first_char.is_alphabetic() {
            return Some(OperandParseError::Label(
                "labels must start with an alphabet".to_string(),
            ));
        }
    } else {
        return Some(OperandParseError::Label(
            "labels must be non-empty".to_string(),
        ));
    }

    for char in chars {
        if !char.is_alphanumeric() {
            return Some(OperandParseError::Label(
                "labels must only contain alphanumeric characters".to_string(),
            ));
        }
    }

    None
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
        return Err(OperandParseError::Label(
            "labels must be non-empty".to_string(),
        ));
    }

    for char in chars {
        if !char.is_alphanumeric() {
            return Err(OperandParseError::Label(
                "labels must only contain alphanumeric characters".to_string(),
            ));
        }
    }

    Ok(Word::Label(label.to_string()))
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

                Ok(Instruction::NoData(InstructionNoData { opcode }))
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
                    opcode,
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
                    opcode,
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

                Ok(Instruction::NoData(InstructionNoData { opcode }))
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

                Ok(Instruction::NoData(InstructionNoData { opcode }))
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
                    opcode,
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
                    opcode,
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
                    opcode,
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
                    opcode,
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
                    opcode,
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
                    opcode,
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
                    opcode,
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
                    opcode,
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
                    opcode,
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
                    opcode,
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
                    opcode,
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
                    opcode,
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
                    opcode,
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

                Ok(Instruction::NoData(InstructionNoData { opcode }))
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

                Ok(Instruction::NoData(InstructionNoData { opcode }))
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

                Ok(Instruction::NoData(InstructionNoData { opcode }))
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

                Ok(Instruction::NoData(InstructionNoData { opcode }))
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

                Ok(Instruction::NoData(InstructionNoData { opcode }))
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

                Ok(Instruction::NoData(InstructionNoData { opcode }))
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

                Ok(Instruction::NoData(InstructionNoData { opcode }))
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

                Ok(Instruction::NoData(InstructionNoData { opcode }))
            }
            "cmp" => {
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

                let opcode = 0b10111000 + (rm as u8);
                let opcode = OpCode::try_from(opcode).unwrap();

                Ok(Instruction::NoData(InstructionNoData { opcode }))
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

                Ok(Instruction::NoData(InstructionNoData { opcode }))
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
            "hlt" => {
                if operands.is_some() {
                    return Err(InstructionError::OperandParse(OperandParseError::NoOp));
                }

                Ok(Instruction::NoData(InstructionNoData {
                    opcode: OpCode::Hlt,
                }))
            }
            _ => Err(InstructionError::UnknownMnemonic(mnemonic.to_string())),
        }
    }
}

#[derive(Error, Debug)]
pub enum DirectiveError {
    #[error("no directive `{0}` exists or is unimplimented")]
    UnknownDirective(String),
    #[error(transparent)]
    Operand(#[from] OperandParseError),
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Directive {
    Org(u16),
    Db(u8),
    Rs,
    Start,
}

impl Directive {
    fn parse(line: &str) -> Result<Directive, DirectiveError> {
        let line_split = line.split_once(" ");
        let (mnemonic, operands);

        match line_split {
            Some(line_split) => {
                (mnemonic, operands) = (line_split.0, Some(line_split.1));
            }
            None => {
                (mnemonic, operands) = (line, None);
            }
        }

        match mnemonic {
            "org" => {
                if operands.is_none() {
                    return Err(DirectiveError::Operand(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let address = u16::from_str_radix(operands, 16);
                if let Err(err) = address {
                    Err(DirectiveError::Operand(OperandParseError::ParseU16(err)))
                } else {
                    let address = address.unwrap();
                    Ok(Directive::Org(address))
                }
            }
            "db" => {
                if operands.is_none() {
                    return Err(DirectiveError::Operand(
                        OperandParseError::InsufficientOperands {
                            expected: 1,
                            got: 0,
                        },
                    ));
                }

                let operands = operands.unwrap();
                let value = parse_byte(operands)?;

                Ok(Directive::Db(value))
            }
            "rs" => {
                if operands.is_some() {
                    return Err(DirectiveError::Operand(OperandParseError::NoOp));
                }

                Ok(Directive::Rs)
            }
            "start" => {
                if operands.is_some() {
                    return Err(DirectiveError::Operand(OperandParseError::NoOp));
                }

                Ok(Directive::Start)
            }
            _ => Err(DirectiveError::UnknownDirective(mnemonic.to_string())),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Unit {
    Instruction(Instruction),
    Directive(Directive),
}

#[derive(Debug, PartialEq, Clone)]
struct IntermediateUnit {
    line: Line,
    unit: Unit,
    address: u16,
}

#[derive(Debug)]
pub struct Section {
    address: u16,
    units: Vec<IntermediateUnit>,
}

#[derive(Debug)]
pub struct Program {
    sections: Vec<Section>,
    label_table: HashMap<String, u16>,
    entrypoint: u16,
}

#[derive(Error, Debug)]
pub enum ProgramErrorData {
    #[error(transparent)]
    Instruction(#[from] InstructionError),
    #[error(transparent)]
    Directive(#[from] DirectiveError),
    #[error("no .org directive found, could not determine address")]
    NoOrg,
    #[error("unset labels can't appear twice in a row")]
    UnsetLabel,
    #[error("no entrypoint directive found")]
    NoEntryPoint,
}

#[derive(Clone, PartialEq)]
pub struct Line {
    line: String,
    line_number: usize,
}

impl Line {
    fn new(line: String, line_number: usize) -> Self {
        Line { line, line_number }
    }
}

impl std::fmt::Debug for Line {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(formatter, "{} | {}", self.line_number, self.line)?;
        Ok(())
    }
}

impl std::fmt::Display for Line {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(formatter, "{} | {}", self.line_number, self.line)?;
        Ok(())
    }
}

#[derive(Error, Debug)]
#[error("{line:?}: {data}")]
pub struct ProgramErrorWithLine {
    line: Line,
    data: ProgramErrorData,
}

#[derive(Error, Debug)]
#[error("{data}")]
pub struct ProgramErrorWithoutLine {
    data: ProgramErrorData,
}

#[derive(Error, Debug)]
pub enum ProgramError {
    #[error(transparent)]
    WithLine(#[from] ProgramErrorWithLine),
    #[error(transparent)]
    WithoutLine(#[from] ProgramErrorWithoutLine),
}

impl ProgramError {
    fn from<E>(data: E, line: Option<Line>) -> ProgramError
    where
        E: Into<ProgramErrorData>,
    {
        match line {
            Some(line) => ProgramError::WithLine(ProgramErrorWithLine {
                line,
                data: Into::into(data),
            }),
            None => ProgramError::WithoutLine(ProgramErrorWithoutLine {
                data: Into::into(data),
            }),
        }
    }

    fn from_result<T, E>(result: Result<T, E>, line: Option<Line>) -> Result<T, ProgramError>
    where
        E: Into<ProgramErrorData>,
    {
        match result {
            Ok(value) => Ok(value),
            Err(error) => Err(ProgramError::from(error, line)),
        }
    }
}

#[derive(Error, Debug)]
pub enum AssembleError {
    #[error(transparent)]
    Program(#[from] ProgramError),
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error("{line}: label `{label}` was not found")]
    LabelNotFound { line: Line, label: String },
}

pub struct Segment {
    address: u16,
    data: Vec<u8>,
}

impl Segment {
    pub fn address(&self) -> u16 {
        self.address
    }

    pub fn data(&self) -> &Vec<u8> {
        &self.data
    }
}

pub struct AssembledProgram {
    entrypoint: u16,
    segments: Vec<Segment>,
}

impl Program {
    fn add_unit(
        line_unit: &str,
        line: Line,
        mut address: Option<u16>,
        mut entrypoint: Option<u16>,
    ) -> Result<(IntermediateUnit, u16, Option<u16>), ProgramErrorData> {
        if let Some(line_unit) = line_unit.strip_prefix(".") {
            let directive = Directive::parse(line_unit)?;

            if let Directive::Org(addr) = directive {
                address = Some(addr);
            }

            if address.is_none() {
                return Err(ProgramErrorData::NoOrg);
            }

            let intermediate_unit = IntermediateUnit {
                line,
                unit: Unit::Directive(directive),
                address: address.unwrap(),
            };

            match directive {
                Directive::Db(_) => {
                    address = Some(address.unwrap() + 1);
                }
                Directive::Rs => {
                    address = Some(address.unwrap() + 1);
                }
                Directive::Start => {
                    entrypoint = Some(address.unwrap());
                }
                _ => {}
            }

            Ok((intermediate_unit, address.unwrap(), entrypoint))
        } else {
            if address.is_none() {
                return Err(ProgramErrorData::NoOrg);
            }

            let instruction = Instruction::parse(line_unit)?;

            let intermediate_unit = IntermediateUnit {
                line,
                unit: Unit::Instruction(instruction.clone()),
                address: address.unwrap(),
            };

            match instruction {
                Instruction::ImByte(_) => {
                    address = Some(address.unwrap() + 2);
                }
                Instruction::ImWord(_) => {
                    address = Some(address.unwrap() + 3);
                }
                Instruction::NoData(_) => {
                    address = Some(address.unwrap() + 1);
                }
            }

            Ok((intermediate_unit, address.unwrap(), entrypoint))
        }
    }

    fn parse(program: &str) -> Result<Program, ProgramError> {
        let lines = program.lines();
        let mut address = None;
        let mut entrypoint = None;
        let mut last_label = None;

        let mut sections = vec![];
        let mut label_table = HashMap::new();

        for (line_number, line) in lines.enumerate() {
            let line_split = line.split_once(":");
            let (label, line_unit);
            match line_split {
                Some(line_split) => {
                    if let Some(error) = check_label(line_split.0) {
                        return Err(ProgramError::from(
                            ProgramErrorData::Instruction(InstructionError::OperandParse(error)),
                            Some(Line::new(line.to_string(), line_number)),
                        ));
                    }

                    (label, line_unit) = (Some(line_split.0), line_split.1);
                }
                None => {
                    (label, line_unit) = (None, line);
                }
            }

            let line_unit = line_unit.trim();

            if label.is_none() && line_unit.is_empty() {
                continue;
            } else if label.is_some() && line_unit.is_empty() {
                if last_label.is_some() {
                    return Err(ProgramError::from(
                        ProgramErrorData::UnsetLabel,
                        Some(Line::new(line.to_string(), line_number)),
                    ));
                }

                last_label = label;
            } else if label.is_none() && !line_unit.is_empty() {
                let intermediate_unit;
                let addr;
                (intermediate_unit, addr, entrypoint) = ProgramError::from_result(
                    Self::add_unit(
                        line_unit,
                        Line::new(line.to_string(), line_number),
                        address,
                        entrypoint,
                    ),
                    Some(Line::new(line.to_string(), line_number)),
                )?;

                if let Unit::Directive(directive) = intermediate_unit.unit {
                    if let Directive::Org(address) = directive {
                        sections.push(Section {
                            address: address,
                            units: vec![],
                        });
                    }
                }

                address = Some(addr);
                let section = sections.last_mut().unwrap();
                let unit_address = intermediate_unit.address;
                section.units.push(intermediate_unit);

                if last_label.is_some() {
                    label_table.insert(last_label.unwrap().to_string(), unit_address);
                    last_label = None
                }
            } else {
                let intermediate_unit;
                let addr;
                (intermediate_unit, addr, entrypoint) = ProgramError::from_result(
                    Self::add_unit(
                        line_unit,
                        Line::new(line.to_string(), line_number),
                        address,
                        entrypoint,
                    ),
                    Some(Line::new(line.to_string(), line_number)),
                )?;

                if let Unit::Directive(directive) = intermediate_unit.unit {
                    if let Directive::Org(address) = directive {
                        sections.push(Section {
                            address: address,
                            units: vec![],
                        });
                    }
                }

                address = Some(addr);
                let section = sections.last_mut().unwrap();
                let unit_address = intermediate_unit.address;
                section.units.push(intermediate_unit);


                if last_label.is_some() {
                    label_table.insert(last_label.unwrap().to_string(), unit_address);
                    last_label = None
                }

                let label = label.unwrap();
                label_table.insert(label.to_string(), unit_address);
            }
        }

        if entrypoint.is_none() {
            return Err(ProgramError::from(ProgramErrorData::NoEntryPoint, None));
        }

        Ok(Self {
            sections,
            label_table,
            entrypoint: entrypoint.unwrap(),
        })
    }
}

impl AssembledProgram {
    pub fn assemble(program: &str) -> Result<AssembledProgram, AssembleError> {
        let program = Program::parse(program)?;
        let mut segments = vec![];
        for section in &program.sections {
            let mut data = vec![];

            for intermediate_unit in &section.units {
                match &intermediate_unit.unit {
                    Unit::Directive(directive) => match directive {
                        Directive::Db(value) => {
                            data.push(*value);
                        }
                        Directive::Rs => {
                            data.push(0);
                        }
                        _ => {}
                    },
                    Unit::Instruction(instruction) => match instruction {
                        Instruction::NoData(instruction) => {
                            data.push(instruction.opcode as u8);
                        }
                        Instruction::ImByte(instruction) => {
                            data.push(instruction.opcode as u8);
                            data.push(instruction.operand);
                        }
                        Instruction::ImWord(instruction) => {
                            data.push(instruction.opcode as u8);
                            let addr: u16 = match &instruction.operand {
                                Word::Label(label) => {
                                    let address = program.label_table.get(label);
                                    if address.is_none() {
                                        return Err(AssembleError::LabelNotFound {
                                            line: intermediate_unit.line.clone(),
                                            label: label.to_string(),
                                        });
                                    }

                                    *address.unwrap()
                                }
                                Word::U16(address) => *address,
                            };

                            let [lower, higher] = u16_to_pair(addr);
                            data.push(lower);
                            data.push(higher);
                        }
                    },
                }
            }

            segments.push(Segment {
                address: section.address,
                data,
            });
        }

        Ok(Self {
            entrypoint: program.entrypoint,
            segments,
        })
    }

    pub fn assemble_file<P: AsRef<Path>>(path: P) -> Result<AssembledProgram, AssembleError> {
        let contents = read_to_string(path)?;
        Self::assemble(&contents)
    }

    pub fn get_entrypoint(&self) -> u16 {
        self.entrypoint
    }

    pub fn segments(&self) -> &Vec<Segment> {
        &self.segments
    }

    // pub fn save<P: AsRef<Path>>(&self, path: P) -> std::io::Result<()> {
    //     let mut file = fs::File::create(&path)?;
    //     file.write_all(&u16_to_pair(self.get_entrypoint()))?;
    //     file.write_all(&self.memory)?;
    //
    //     Ok(())
    // }

    // pub fn load<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
    //     let mut memory: [u8; 0x10000] = [0; 0x10000];
    //     let mut entrypoint: [u8; 2] = [0; 2];
    //
    //     let mut file = fs::File::open(&path)?;
    //     file.read_exact(&mut entrypoint)?;
    //     file.read_exact(&mut memory)?;
    //
    //     Ok(AssembledProgram {
    //         entrypoint: pair_to_u16(entrypoint),
    //         memory,
    //     })
    // }
}
