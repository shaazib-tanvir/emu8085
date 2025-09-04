use std::{
    hash::{DefaultHasher, Hash, Hasher},
    sync::{Arc, Mutex},
};

use eframe::CreationContext;
use egui::{
    Align, Button, Color32, FontData, FontDefinitions, FontFamily, FontId, Response, RichText,
    TextBuffer, TextEdit, Ui, Vec2, Widget, text::LayoutJob,
};

use crate::emu::{CPU, Flags, FlagsOperation, Operation, RegisterOperation, StepError};
use crate::{asm::AssembledProgram, common::Register};

const ROW_COUNT: usize = 4;
const COLUMN_COUNT: usize = 0x10;

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
                self.bytes[i][j] = format!("{:02x}", cpu.get_memory_at(k));
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

struct RegisterUIState {
    a: String,
    b: String,
    c: String,
    d: String,
    e: String,
    h: String,
    l: String,
    carry_flag: bool,
    parity_flag: bool,
    auxiliary_carry_flag: bool,
    zero_flag: bool,
    sign_flag: bool,
}

impl RegisterUIState {
    fn get(&mut self, register: Register) -> &mut String {
        match register {
            Register::A => &mut self.a,
            Register::B => &mut self.b,
            Register::C => &mut self.c,
            Register::D => &mut self.d,
            Register::E => &mut self.e,
            Register::H => &mut self.h,
            Register::L => &mut self.l,
        }
    }

    fn update(&mut self, cpu: &CPU) {
        self.a = format!("{:02x}", cpu.get_register(Register::A));
        self.b = format!("{:02x}", cpu.get_register(Register::B));
        self.c = format!("{:02x}", cpu.get_register(Register::C));
        self.d = format!("{:02x}", cpu.get_register(Register::D));
        self.e = format!("{:02x}", cpu.get_register(Register::E));
        self.h = format!("{:02x}", cpu.get_register(Register::H));
        self.l = format!("{:02x}", cpu.get_register(Register::L));
        self.carry_flag = cpu.get_flags().contains(Flags::CARRY);
        self.parity_flag = cpu.get_flags().contains(Flags::PARITY);
        self.auxiliary_carry_flag = cpu.get_flags().contains(Flags::AUXILIARY_CARRY);
        self.zero_flag = cpu.get_flags().contains(Flags::ZERO);
        self.sign_flag = cpu.get_flags().contains(Flags::SIGN);
    }
}

impl From<&CPU> for RegisterUIState {
    fn from(cpu: &CPU) -> Self {
        Self {
            a: format!("{:02x}", cpu.get_register(Register::A)),
            b: format!("{:02x}", cpu.get_register(Register::B)),
            c: format!("{:02x}", cpu.get_register(Register::C)),
            d: format!("{:02x}", cpu.get_register(Register::D)),
            e: format!("{:02x}", cpu.get_register(Register::E)),
            h: format!("{:02x}", cpu.get_register(Register::H)),
            l: format!("{:02x}", cpu.get_register(Register::L)),
            carry_flag: cpu.get_flags().contains(Flags::CARRY),
            parity_flag: cpu.get_flags().contains(Flags::PARITY),
            auxiliary_carry_flag: cpu.get_flags().contains(Flags::AUXILIARY_CARRY),
            zero_flag: cpu.get_flags().contains(Flags::ZERO),
            sign_flag: cpu.get_flags().contains(Flags::SIGN),
        }
    }
}

struct Editor {
    code: String,
    status_bar: Arc<Mutex<String>>,
    last_hash: u64,
    galley: Option<Arc<egui::Galley>>,
}

impl Editor {
    fn new() -> Self {
        Self {
            last_hash: 0,
            galley: None,
            code: "".to_string(),
            status_bar: Arc::new(Mutex::new("".to_string())),
        }
    }
}

struct EditorWidget<'editor> {
    editor: &'editor mut Editor,
}

impl<'editor> EditorWidget<'editor> {
    fn new(editor: &'editor mut Editor) -> Self {
        Self { editor }
    }
}

impl<'editor> Widget for EditorWidget<'editor> {
    fn ui(self, ui: &mut Ui) -> Response {
        let editor = self.editor;
        let mut layouter = |ui: &Ui, buf: &dyn TextBuffer, wrap_width| {
            let code_text = buf.as_str();
            let mut hasher = DefaultHasher::new();
            code_text.hash(&mut hasher);
            let hash = hasher.finish();

            if editor.last_hash == hash && editor.galley.is_some() {
                return editor.galley.clone().unwrap();
            }

            let mut layout_job =
                LayoutJob::simple("".to_string(), FontId::default(), Color32::GRAY, wrap_width);

            for line in code_text.split_inclusive("\n") {
                let (label, unit) = match line.split_once(":") {
                    Some(value) => (Some(value.0), value.1),
                    None => (None, line),
                };

                if let Some(label) = label {
                    layout_job.append(
                        &(label.to_string() + ":"),
                        0.0,
                        egui::TextFormat {
                            color: Color32::from_hex("#fb4934").unwrap(),
                            ..Default::default()
                        },
                    );
                }

                let mut prefix: String = "".to_string();
                for char in unit.chars() {
                    if char.is_whitespace() {
                        prefix += &char.to_string();
                    } else {
                        break;
                    }
                }
                let unit = unit.trim_start();

                if !prefix.is_empty() {
                    layout_job.append(&prefix, 0.0, egui::TextFormat::default());
                }

                if !unit.is_empty() {
                    let (mnemonic, operands) = match unit.split_once(" ") {
                        Some(value) => (value.0, Some(value.1)),
                        None => (unit, None),
                    };

                    if mnemonic.starts_with(".") && !mnemonic.is_empty() {
                        layout_job.append(
                            mnemonic,
                            0.0,
                            egui::TextFormat {
                                color: Color32::from_hex("#83a598").unwrap(),
                                ..Default::default()
                            },
                        );
                    } else if !mnemonic.is_empty() {
                        layout_job.append(
                            mnemonic,
                            0.0,
                            egui::TextFormat {
                                color: Color32::from_hex("#b8bb26").unwrap(),
                                ..Default::default()
                            },
                        );
                    }

                    if let Some(operands) = operands {
                        layout_job.append(
                            &(" ".to_string() + operands),
                            0.0,
                            egui::TextFormat::default(),
                        );
                    }
                }
            }

            let galley = ui.fonts(|f| f.layout_job(layout_job));
            editor.galley = Some(galley.clone());
            editor.last_hash = hash;
            galley
        };
        let code_widget = egui::TextEdit::multiline(&mut editor.code)
            .lock_focus(true)
            .layouter(&mut layouter);

        ui.add_sized(ui.available_size(), code_widget)
    }
}

struct FlagWidget<'a> {
    flag: Flags,
    flag_text: &'a str,
    color: Color32,
    cpu: &'a mut CPU,
    flag_value: &'a mut bool,
}

impl<'a> FlagWidget<'a> {
    fn new(
        flag: Flags,
        flag_text: &'a str,
        color: Color32,
        cpu: &'a mut CPU,
        flag_value: &'a mut bool,
    ) -> Self {
        Self {
            flag,
            flag_text,
            color,
            cpu,
            flag_value,
        }
    }
}

impl<'a> Widget for FlagWidget<'a> {
    fn ui(self, ui: &mut Ui) -> Response {
        ui.vertical(|ui| {
            let text = if *self.flag_value { "1" } else { "0" };
            let label_button = egui::Button::new(
                RichText::new(self.flag_text)
                    .color(Color32::BLACK)
                    .text_style(egui::TextStyle::Body),
            )
            .fill(self.color)
            .sense(egui::Sense::empty());
            let toggle = ui.toggle_value(self.flag_value, text);
            ui.add_sized(toggle.rect.size(), label_button);

            if toggle.changed() {
                let mut new_flags = self.cpu.get_flags();
                new_flags.toggle(self.flag);

                self.cpu.execute_op(Operation::Flags(FlagsOperation {
                    old_flags: self.cpu.get_flags(),
                    new_flags,
                }));
            }

            toggle
        })
        .response
    }
}

struct RegisterWidget<'a> {
    register: Register,
    color: Color32,
    cpu: &'a mut CPU,
    register_ui: &'a mut RegisterUIState,
}

impl<'a> RegisterWidget<'a> {
    fn new(
        register: Register,
        color: Color32,
        cpu: &'a mut CPU,
        register_ui: &'a mut RegisterUIState,
    ) -> Self {
        Self {
            register,
            cpu,
            color,
            register_ui,
        }
    }
}

impl<'a> Widget for RegisterWidget<'a> {
    fn ui(self, ui: &mut Ui) -> Response {
        ui.vertical(|ui| {
            let register_text = self.register_ui.get(self.register);
            let text_edit = TextEdit::singleline(register_text)
                .char_limit(2)
                .horizontal_align(Align::Center)
                .vertical_align(Align::Center);
            let label_button = egui::Button::new(
                RichText::new(format!("{}", self.register))
                    .color(Color32::BLACK)
                    .text_style(egui::TextStyle::Body),
            )
            .fill(self.color)
            .sense(egui::Sense::empty());
            let value = ui.add_sized([120.0, 25.0], text_edit);
            ui.add_sized([120.0, 25.0], label_button);

            if value.changed() {
                if !register_text.is_empty() && u8::from_str_radix(register_text, 16).is_err() {
                    self.register_ui.a = "00".to_string();
                    self.cpu.execute_op(Operation::Register(RegisterOperation {
                        old_value: self.cpu.get_register(self.register),
                        new_value: 0,
                        register: self.register,
                    }));
                } else if u8::from_str_radix(register_text, 16).is_ok() {
                    let value = u8::from_str_radix(register_text, 16).unwrap();
                    self.cpu.execute_op(Operation::Register(RegisterOperation {
                        old_value: self.cpu.get_register(self.register),
                        new_value: value,
                        register: self.register,
                    }));
                }
            }

            value
        })
        .response
    }
}

struct MemoryCellWidget<'a> {
    address: u16,
    byte: &'a mut String,
    cpu: &'a mut CPU,
}

impl<'a> MemoryCellWidget<'a> {
    fn new(address: u16, byte: &'a mut String, cpu: &'a mut CPU) -> Self {
        Self { address, byte, cpu }
    }
}

impl<'a> Widget for MemoryCellWidget<'a> {
    fn ui(self, ui: &mut Ui) -> Response {
        let text_edit = TextEdit::singleline(self.byte)
            .char_limit(2)
            .horizontal_align(Align::Center)
            .vertical_align(Align::Center);
        let response = ui.add(text_edit);

        if response.changed() {
            let value = u8::from_str_radix(self.byte, 16);

            if !self.byte.is_empty() && value.is_err() {
                self.byte.clear();
                self.byte.push_str("00");
            } else {
                let value = value.unwrap_or_default();
                self.cpu.load_data(&[value], self.address);
            }
        }

        response
    }
}

pub struct App {
    cpu: Arc<Mutex<CPU>>,
    running: Arc<Mutex<bool>>,
    editor: Editor,
    register_ui: Arc<Mutex<RegisterUIState>>,
    memory_ui: Arc<Mutex<MemoryUIState>>,
}

impl App {
    pub fn new(creation_context: &CreationContext<'_>) -> Self {
        let mut fonts = FontDefinitions::default();
        fonts.font_data.insert(
            "SourceCodePro-Regular".to_owned(),
            std::sync::Arc::new(FontData::from_static(include_bytes!(
                "../assets/SourceCodePro-Regular.ttf"
            ))),
        );
        fonts
            .families
            .get_mut(&FontFamily::Proportional)
            .unwrap()
            .insert(0, "SourceCodePro-Regular".to_owned());
        creation_context.egui_ctx.set_fonts(fonts);

        let cpu = CPU::new();
        let register_ui = RegisterUIState::from(&cpu);
        let memory_ui = MemoryUIState::from(&cpu);

        Self {
            cpu: Arc::new(Mutex::new(cpu)),
            register_ui: Arc::new(Mutex::new(register_ui)),
            memory_ui: Arc::new(Mutex::new(memory_ui)),
            running: Arc::new(Mutex::new(false)),
            editor: Editor::new(),
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |_ui| {
            let mut running = self.running.lock().unwrap();
            let mut cpu = self.cpu.lock().unwrap();
            egui::Window::new("Editor")
                .default_width(800.0)
                .default_height(600.0)
                .show(ctx, |ui| {
                    egui::TopBottomPanel::top("Assemble Buttons").show_inside(ui, |ui| {
                        let execute_button = Button::new("▶");
                        const PROGRAM_DONE_MESSAGE: &str = "program executed successfully";

                        if ui
                            .add_enabled(!*running, execute_button)
                            .on_hover_text("Execute")
                            .clicked()
                        {
                            let program = AssembledProgram::assemble(&self.editor.code);
                            match program {
                                Ok(program) => {
                                    for segment in program.segments() {
                                        cpu.load_data(
                                            segment.data().as_slice(),
                                            segment.address(),
                                        );
                                    }

                                    *self.editor.status_bar.lock().unwrap() =
                                        "program started executing".to_string();
                                    *running = true;
                                    let entrypoint = program.get_entrypoint();

                                    let running = self.running.clone();
                                    let cpu = self.cpu.clone();
                                    let status_bar = self.editor.status_bar.clone();
                                    let ctx = ctx.clone();
                                    let register_ui = self.register_ui.clone();
                                    let memory_ui = self.memory_ui.clone();

                                    let _ = std::thread::spawn(move || {
                                        {
                                            cpu.lock().unwrap().load_entrypoint(entrypoint);
                                        }

                                        loop {
                                            let start = std::time::Instant::now();
                                            {
                                                let mut cpu = cpu.lock().unwrap();
                                                if let Err(err) = cpu.step_forward() {
                                                    *status_bar.lock().unwrap() = match err {
                                                        StepError::Halt => {
                                                            *running.lock().unwrap() = false;
                                                            PROGRAM_DONE_MESSAGE.to_string()
                                                        }
                                                        _ => err.to_string(),
                                                    };

                                                    *running.lock().unwrap() = false;
                                                    break;
                                                };

                                                register_ui.lock().unwrap().update(&cpu);
                                                memory_ui.lock().unwrap().update(&cpu);

                                                ctx.request_repaint();
                                            }

                                            if start.elapsed() <= std::time::Duration::from_millis(10) {
                                                std::thread::sleep(std::time::Duration::from_millis(10) - start.elapsed());
                                            }
                                        }
                                    });
                                }
                                Err(err) => {
                                    *self.editor.status_bar.lock().unwrap() = err.to_string();
                                }
                            }
                        }

                        ui.allocate_space(ui.spacing().item_spacing);
                    });
                    egui::TopBottomPanel::bottom("Status Bar Panel").show_inside(ui, |ui| {
                        ui.allocate_space(ui.spacing().item_spacing);
                        ui.label(self.editor.status_bar.lock().unwrap().to_string());
                    });
                    ui.add(EditorWidget::new(&mut self.editor));
                });
            egui::Window::new("Registers").show(ctx, |ui| {
                let mut register_ui = self.register_ui.lock().unwrap();
                ui.add(RegisterWidget::new(
                    Register::A,
                    Color32::from_hex("#a51723").unwrap(),
                    &mut cpu,
                    &mut register_ui,
                ));

                ui.horizontal(|ui| {
                    ui.add(RegisterWidget::new(
                        Register::B,
                        Color32::from_hex("#00822f").unwrap(),
                        &mut cpu,
                        &mut register_ui,
                    ));
                    ui.add(RegisterWidget::new(
                        Register::C,
                        Color32::from_hex("#00822f").unwrap(),
                        &mut cpu,
                        &mut register_ui,
                    ));
                });

                ui.horizontal(|ui| {
                    ui.add(RegisterWidget::new(
                        Register::D,
                        Color32::from_hex("#0e6bb7").unwrap(),
                        &mut cpu,
                        &mut register_ui,
                    ));
                    ui.add(RegisterWidget::new(
                        Register::E,
                        Color32::from_hex("#0e6bb7").unwrap(),
                        &mut cpu,
                        &mut register_ui,
                    ));
                });

                ui.horizontal(|ui| {
                    ui.add(RegisterWidget::new(
                        Register::H,
                        Color32::from_hex("#f9d222").unwrap(),
                        &mut cpu,
                        &mut register_ui,
                    ));
                    ui.add(RegisterWidget::new(
                        Register::L,
                        Color32::from_hex("#f9d222").unwrap(),
                        &mut cpu,
                        &mut register_ui,
                    ));
                });

                ui.horizontal(|ui| {
                    ui.add(FlagWidget::new(
                        Flags::SIGN,
                        "S",
                        Color32::from_hex("#ea7cc4").unwrap(),
                        &mut cpu,
                        &mut register_ui.sign_flag,
                    ));
                    ui.add(FlagWidget::new(
                        Flags::ZERO,
                        "Z",
                        Color32::from_hex("#ea7cc4").unwrap(),
                        &mut cpu,
                        &mut register_ui.zero_flag,
                    ));
                    ui.add(FlagWidget::new(
                        Flags::PARITY,
                        "P",
                        Color32::from_hex("#ea7cc4").unwrap(),
                        &mut cpu,
                        &mut register_ui.parity_flag,
                    ));
                    ui.add(FlagWidget::new(
                        Flags::CARRY,
                        "C",
                        Color32::from_hex("#ea7cc4").unwrap(),
                        &mut cpu,
                        &mut register_ui.carry_flag,
                    ));
                });
            });

            egui::Window::new("Memory").show(ctx, |ui| {
                let mut memory_ui = self.memory_ui.lock().unwrap();
                ui.horizontal(|ui| {
                    let address_field = TextEdit::singleline(&mut memory_ui.address).char_limit(4);
                    ui.label("Address: ");
                    if ui.add_sized([40.0, 20.0], address_field).changed() {
                        if !memory_ui.address.is_empty()
                            && u16::from_str_radix(&memory_ui.address, 16).is_err()
                        {
                            memory_ui.address = "0000".to_string();
                        }

                        memory_ui.update(&cpu);
                    }
                });
                ui.allocate_space(Vec2::new(0.0, ui.style().spacing.item_spacing.y));

                egui::Grid::new("Memory Grid").show(ui, |ui| {
                    for i in 0..ROW_COUNT {
                        let base_address =
                            u16::from_str_radix(&memory_ui.address, 16).unwrap_or_default();

                        ui.label(format!(
                            "{:04x}:",
                            base_address.wrapping_add((i * COLUMN_COUNT) as u16)
                        ));
                        for j in 0..COLUMN_COUNT {
                            let address = base_address + j as u16;
                            ui.add(MemoryCellWidget::new(
                                address,
                                &mut memory_ui.bytes[i][j],
                                &mut cpu,
                            ));
                        }
                        ui.end_row();
                    }
                });
            });
        });
    }
}
