use emu8085::emu::CPU;
use criterion::{Criterion, criterion_group, criterion_main};

const ROW_COUNT: usize = 4;
const COLUMN_COUNT: usize = 0x10;

#[inline]
fn nibble_to_hex_digit(nibble: u8) -> u8 {
    if nibble < 10 {
        b'0' + nibble
    } else {
        b'a' + nibble - 10
    }
}

#[inline]
unsafe fn byte_to_hex(string: &mut str, value: u8) {
    let mut value = value;
    let lb = nibble_to_hex_digit(value % 16);
    value /= 16;
    let hb = nibble_to_hex_digit(value % 16);
    unsafe {
        let string = string.as_bytes_mut();
        string[0] = hb;
        string[1] = lb;
    }
}

struct MemoryUIState {
    address: String,
    bytes: [[String; COLUMN_COUNT]; ROW_COUNT],
}

impl MemoryUIState {
    fn update(&mut self, cpu: &CPU) {
        for i in 0..ROW_COUNT {
            for j in 0..COLUMN_COUNT {
                let k = (j + i * COLUMN_COUNT) as u16
                    + u16::from_str_radix(&self.address, 16).unwrap_or_default();
                unsafe {
                    byte_to_hex(&mut self.bytes[i][j], cpu.get_memory_at(k));
                }
            }
        }
    }
}

impl From<&CPU> for MemoryUIState {
    fn from(cpu: &CPU) -> Self {
        let mut bytes: [[String; COLUMN_COUNT]; ROW_COUNT] = Default::default();
        for i in 0..ROW_COUNT {
            for j in 0..COLUMN_COUNT {
                let k = j + i * COLUMN_COUNT;
                bytes[i][j] = format!("{:02x}", cpu.get_memory_at(k as u16));
            }
        }

        Self {
            address: "0000".to_string(),
            bytes,
        }
    }
}

fn memory_ui_update(c: &mut Criterion) {
    let cpu = CPU::new();
    let mut memory_ui = MemoryUIState::from(&cpu);
    c.bench_function("memory_ui_update", |b| b.iter(|| memory_ui.update(&cpu)));
}

criterion_group!(benches, memory_ui_update);
criterion_main!(benches);
