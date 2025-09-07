use std::{
    hash::{DefaultHasher, Hash, Hasher},
    sync::Arc,
};

use eframe::CreationContext;
use egui::{
    text::LayoutJob, Align, Button, Color32, FontData, FontDefinitions, FontFamily, FontId, Label,
    Response, RichText, Sense, TextBuffer, TextEdit, Ui, Vec2, Widget, WidgetText,
};
use web_time::{Duration, Instant};

use crate::emu::{Flags, FlagsOperation, Operation, RegisterOperation, StepError, CPU};
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
    status_bar: String,
    last_hash: u64,
    galley: Option<Arc<egui::Galley>>,
}

impl Editor {
    fn new() -> Self {
        Self {
            last_hash: 0,
            galley: None,
            code: "".to_string(),
            status_bar: "".to_string(),
        }
    }
}

struct EditorWidget<'editor> {
    editor: &'editor mut Editor,
    enabled: bool,
}

impl<'editor> EditorWidget<'editor> {
    fn new(editor: &'editor mut Editor, enabled: bool) -> Self {
        Self { editor, enabled }
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

        let line_count = if !editor.code.is_empty() {
            editor.code.split("\n").count()
        } else {
            1
        };
        let code_widget = egui::TextEdit::multiline(&mut editor.code)
            .interactive(self.enabled)
            .lock_focus(true)
            .layouter(&mut layouter);

        ui.with_layout(
            egui::Layout::left_to_right(Align::Min).with_main_wrap(false),
            |ui| {
                ui.vertical(|ui| {
                    let mut line_number_string = "".to_string();
                    for line_number in 0..line_count {
                        line_number_string += &format!("{}\n", line_number);
                    }

                    ui.add_space(2.0);
                    let line_number_label = Label::new(WidgetText::RichText(Arc::new(
                        RichText::new(&line_number_string).size(14.0),
                    )));
                    ui.add(line_number_label);
                });
                ui.add_sized(ui.available_size(), code_widget)
            },
        )
        .response
    }
}

struct FlagWidget<'a> {
    flag: Flags,
    flag_text: &'a str,
    color: Color32,
    cpu: &'a mut CPU,
    flag_value: &'a mut bool,
    enabled: bool,
}

impl<'a> FlagWidget<'a> {
    fn new(
        flag: Flags,
        flag_text: &'a str,
        color: Color32,
        cpu: &'a mut CPU,
        flag_value: &'a mut bool,
        enabled: bool,
    ) -> Self {
        Self {
            flag,
            flag_text,
            color,
            cpu,
            flag_value,
            enabled,
        }
    }
}

impl<'a> Widget for FlagWidget<'a> {
    fn ui(self, ui: &mut Ui) -> Response {
        ui.vertical(|ui| {
            let text = if *self.flag_value { "1" } else { "0" };
            let label_button = Button::new(
                RichText::new(self.flag_text)
                    .color(Color32::BLACK)
                    .text_style(egui::TextStyle::Body),
            )
            .fill(self.color)
            .sense(egui::Sense::empty());
            let toggle = Button::selectable(*self.flag_value, text).sense(if self.enabled {
                Sense::click_and_drag()
            } else {
                Sense::hover()
            });
            let mut toggle = ui.add(toggle);
            // let toggle = ui.toggle_value(self.flag_value, text);
            ui.add_sized(toggle.rect.size(), label_button);

            if toggle.clicked() {
                *self.flag_value = !*self.flag_value;
                toggle.mark_changed();
            }

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
    enabled: bool,
}

impl<'a> RegisterWidget<'a> {
    fn new(
        register: Register,
        color: Color32,
        cpu: &'a mut CPU,
        register_ui: &'a mut RegisterUIState,
        enabled: bool,
    ) -> Self {
        Self {
            register,
            cpu,
            color,
            register_ui,
            enabled,
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
                .vertical_align(Align::Center)
                .interactive(self.enabled);
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
    enabled: bool,
}

impl<'a> MemoryCellWidget<'a> {
    fn new(address: u16, byte: &'a mut String, cpu: &'a mut CPU, enabled: bool) -> Self {
        Self {
            address,
            byte,
            cpu,
            enabled,
        }
    }
}

impl<'a> Widget for MemoryCellWidget<'a> {
    fn ui(self, ui: &mut Ui) -> Response {
        let text_edit = TextEdit::singleline(self.byte)
            .char_limit(2)
            .horizontal_align(Align::Center)
            .vertical_align(Align::Center)
            .interactive(self.enabled);
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
    cpu: CPU,
    last_update: Instant,
    running: bool,
    paused: bool,
    editor: Editor,
    register_ui: RegisterUIState,
    memory_ui: MemoryUIState,
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
            cpu: cpu,
            register_ui: register_ui,
            memory_ui: memory_ui,
            running: false,
            paused: false,
            last_update: Instant::now(),
            editor: Editor::new(),
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |_ui| {
            egui::Window::new("Editor")
                .default_width(800.0)
                .default_height(600.0)
                .show(ctx, |ui| {
                    egui::TopBottomPanel::top("Assemble Buttons").show_inside(ui, |ui| {
                        ui.horizontal(|ui| {
                            let execute_button = Button::new(if !self.running || self.paused {
                                "▶"
                            } else {
                                "⏸"
                            });
                            let stop_button = Button::new("■");
                            const PROGRAM_DONE_MESSAGE: &str = "program executed successfully";

                            if ui
                                .add(execute_button)
                                .on_hover_text(if !self.running {
                                    "Execute"
                                } else if !self.paused {
                                    "Pause"
                                } else {
                                    "Resume"
                                })
                                .clicked()
                            {
                                if !self.running {
                                    let program = AssembledProgram::assemble(&self.editor.code);
                                    match program {
                                        Ok(program) => {
                                            for segment in program.segments() {
                                                self.cpu.load_data(
                                                    segment.data().as_slice(),
                                                    segment.address(),
                                                );
                                            }

                                            self.editor.status_bar =
                                                "program started executing".to_string();
                                            self.running = true;
                                            self.cpu.load_entrypoint(program.get_entrypoint());

                                            if let Err(err) = self.cpu.step_forward() {
                                                self.editor.status_bar = match err {
                                                    StepError::Halt => {
                                                        self.running = false;
                                                        PROGRAM_DONE_MESSAGE.to_string()
                                                    }
                                                    _ => err.to_string(),
                                                };

                                                self.running = false;
                                            };

                                            self.register_ui.update(&self.cpu);
                                            self.memory_ui.update(&self.cpu);

                                            self.last_update = Instant::now();

                                            ctx.request_repaint_after(Duration::from_millis(10));
                                        }
                                        Err(err) => {
                                            self.editor.status_bar = err.to_string();
                                        }
                                    }
                                } else if !self.paused {
                                    self.paused = true;
                                    self.editor.status_bar = "program paused".to_string();
                                } else {
                                    self.paused = false;
                                    ctx.request_repaint_after(Duration::from_millis(10));
                                    self.editor.status_bar = "program resumed".to_string();
                                }
                            }

                            let elapsed = self.last_update.elapsed();
                            if self.running && !self.paused && elapsed >= Duration::from_millis(10)
                            {
                                if let Err(err) = self.cpu.step_forward() {
                                    self.editor.status_bar = match err {
                                        StepError::Halt => {
                                            self.running = false;
                                            PROGRAM_DONE_MESSAGE.to_string()
                                        }
                                        _ => err.to_string(),
                                    };

                                    self.running = false;
                                };

                                self.register_ui.update(&self.cpu);
                                self.memory_ui.update(&self.cpu);

                                self.last_update = Instant::now();
                                ctx.request_repaint_after(Duration::from_millis(10));
                            } else if self.running && !self.paused {
                                ctx.request_repaint_after(Duration::from_millis(10) - elapsed);
                            }

                            if ui
                                .add_enabled(self.running, stop_button)
                                .on_hover_text("Stop")
                                .clicked()
                            {
                                self.paused = false;
                                self.running = false;
                                self.editor.status_bar = "program stopped".to_string();
                            }
                        });

                        ui.allocate_space(ui.spacing().item_spacing);
                    });
                    egui::TopBottomPanel::bottom("Status Bar Panel").show_inside(ui, |ui| {
                        ui.allocate_space(ui.spacing().item_spacing);
                        ui.label(self.editor.status_bar.to_string());
                    });
                    ui.add(EditorWidget::new(&mut self.editor, !self.running));
                });
            egui::Window::new("Registers").show(ctx, |ui| {
                ui.add(RegisterWidget::new(
                    Register::A,
                    Color32::from_hex("#a51723").unwrap(),
                    &mut self.cpu,
                    &mut self.register_ui,
                    !self.running || self.paused,
                ));

                ui.horizontal(|ui| {
                    ui.add(RegisterWidget::new(
                        Register::B,
                        Color32::from_hex("#00822f").unwrap(),
                        &mut self.cpu,
                        &mut self.register_ui,
                        !self.running || self.paused,
                    ));
                    ui.add(RegisterWidget::new(
                        Register::C,
                        Color32::from_hex("#00822f").unwrap(),
                        &mut self.cpu,
                        &mut self.register_ui,
                        !self.running || self.paused,
                    ));
                });

                ui.horizontal(|ui| {
                    ui.add(RegisterWidget::new(
                        Register::D,
                        Color32::from_hex("#0e6bb7").unwrap(),
                        &mut self.cpu,
                        &mut self.register_ui,
                        !self.running || self.paused,
                    ));
                    ui.add(RegisterWidget::new(
                        Register::E,
                        Color32::from_hex("#0e6bb7").unwrap(),
                        &mut self.cpu,
                        &mut self.register_ui,
                        !self.running || self.paused,
                    ));
                });

                ui.horizontal(|ui| {
                    ui.add(RegisterWidget::new(
                        Register::H,
                        Color32::from_hex("#f9d222").unwrap(),
                        &mut self.cpu,
                        &mut self.register_ui,
                        !self.running || self.paused,
                    ));
                    ui.add(RegisterWidget::new(
                        Register::L,
                        Color32::from_hex("#f9d222").unwrap(),
                        &mut self.cpu,
                        &mut self.register_ui,
                        !self.running || self.paused,
                    ));
                });

                ui.horizontal(|ui| {
                    ui.add(FlagWidget::new(
                        Flags::SIGN,
                        "S",
                        Color32::from_hex("#ea7cc4").unwrap(),
                        &mut self.cpu,
                        &mut self.register_ui.sign_flag,
                        !self.running || self.paused,
                    ));
                    ui.add(FlagWidget::new(
                        Flags::ZERO,
                        "Z",
                        Color32::from_hex("#ea7cc4").unwrap(),
                        &mut self.cpu,
                        &mut self.register_ui.zero_flag,
                        !self.running || self.paused,
                    ));
                    ui.add(FlagWidget::new(
                        Flags::PARITY,
                        "P",
                        Color32::from_hex("#ea7cc4").unwrap(),
                        &mut self.cpu,
                        &mut self.register_ui.parity_flag,
                        !self.running || self.paused,
                    ));
                    ui.add(FlagWidget::new(
                        Flags::CARRY,
                        "C",
                        Color32::from_hex("#ea7cc4").unwrap(),
                        &mut self.cpu,
                        &mut self.register_ui.carry_flag,
                        !self.running || self.paused,
                    ));
                });
            });

            egui::Window::new("Memory").show(ctx, |ui| {
                ui.horizontal(|ui| {
                    let address_field =
                        TextEdit::singleline(&mut self.memory_ui.address).char_limit(4);
                    ui.label("Address: ");
                    if ui.add_sized([40.0, 20.0], address_field).changed() {
                        if !self.memory_ui.address.is_empty()
                            && u16::from_str_radix(&self.memory_ui.address, 16).is_err()
                        {
                            self.memory_ui.address = "0000".to_string();
                        }

                        self.memory_ui.update(&self.cpu);
                    }
                });
                ui.allocate_space(Vec2::new(0.0, ui.style().spacing.item_spacing.y));

                egui::Grid::new("Memory Grid").show(ui, |ui| {
                    for i in 0..ROW_COUNT {
                        let base_address =
                            u16::from_str_radix(&self.memory_ui.address, 16).unwrap_or_default();

                        ui.label(format!(
                            "{:04x}:",
                            base_address.wrapping_add((i * COLUMN_COUNT) as u16)
                        ));
                        for j in 0..COLUMN_COUNT {
                            let address = base_address + j as u16;
                            ui.add(MemoryCellWidget::new(
                                address,
                                &mut self.memory_ui.bytes[i][j],
                                &mut self.cpu,
                                !self.running || self.paused,
                            ));
                        }
                        ui.end_row();
                    }
                });
            });
        });
    }
}
