use std::{
    hash::{DefaultHasher, Hash, Hasher},
    sync::Arc,
};

use eframe::CreationContext;
use egui::{
    text::LayoutJob, Align, Color32, FontData, FontDefinitions, FontFamily, FontId, Label, Rect,
    Response, RichText, TextBuffer, TextEdit, Ui, Vec2, Widget,
};

use crate::emu::{Operation, RegisterOperation, CPU};
use crate::{asm::AssembledProgram, common::Register};

struct RegisterUIState {
    a: String,
    b: String,
    c: String,
    d: String,
    e: String,
    h: String,
    l: String,
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

                if !unit.is_empty() {
                    let (mnemonic, operands) = match unit.split_once(" ") {
                        Some(value) => (value.0, Some(value.1)),
                        None => (unit, None),
                    };

                    if mnemonic.starts_with(".") {
                        layout_job.append(
                            mnemonic,
                            0.0,
                            egui::TextFormat {
                                color: Color32::from_hex("#83a598").unwrap(),
                                ..Default::default()
                            },
                        );
                    } else {
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
            register: register,
            cpu,
            color,
            register_ui,
        }
    }
}

impl<'a> Widget for RegisterWidget<'a> {
    fn ui(self, ui: &mut Ui) -> Response {
        let register_text = self.register_ui.get(self.register);
        let text_edit = TextEdit::singleline(register_text)
            .char_limit(2)
            .horizontal_align(Align::Center)
            .vertical_align(Align::Center);
        let value = ui.add_sized([120.0, 25.0], text_edit);

        if value.changed() {
            if !register_text.is_empty() && u8::from_str_radix(register_text, 16).is_err() {
                self.register_ui.a = "00".to_string();
            } else if u8::from_str_radix(register_text, 16).is_ok() {
                let value = u8::from_str_radix(register_text, 16).unwrap();
                self.cpu.execute_op(Operation::Register(RegisterOperation {
                    old_value: self.cpu.get_register(self.register),
                    new_value: value,
                    register: self.register,
                }));
            }
        }

        let rect = value.rect;
        ui.painter().rect_filled(
            Rect::from_min_size(
                rect.left_bottom() + Vec2::new(0.0, ui.spacing().item_spacing.y),
                Vec2::new(120.0, 25.0),
            ),
            2.0,
            self.color,
        );
        ui.add_sized(
            [120.0, 25.0],
            Label::new(RichText::new(format!("{}", self.register)).color(Color32::BLACK)),
        )
    }
}

pub struct App {
    cpu: CPU,
    editor: Editor,
    register_ui: RegisterUIState,
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

        Self {
            cpu,
            register_ui,
            editor: Editor::new(),
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            egui::Window::new("Editor")
                .default_width(800.0)
                .default_height(600.0)
                .show(ctx, |ui| {
                    egui::TopBottomPanel::top("Assemble Buttons").show_inside(ui, |ui| {
                        if ui.button("▶").on_hover_text("Execute").clicked() {
                            let program = AssembledProgram::assemble(&self.editor.code);
                            match program {
                                Ok(program) => {
                                    self.cpu.load_data(&program.get_memory(), 0);
                                    self.cpu.execute(program.get_entrypoint());
                                    self.editor.status_bar =
                                        "program executed successfully".to_string();
                                    self.register_ui.update(&self.cpu);
                                }
                                Err(err) => {
                                    self.editor.status_bar = err.to_string();
                                }
                            }
                        }

                        ui.allocate_space(ui.spacing().item_spacing);
                    });
                    egui::TopBottomPanel::bottom("Status Bar Panel").show_inside(ui, |ui| {
                        ui.allocate_space(ui.spacing().item_spacing);
                        ui.label(self.editor.status_bar.to_string());
                    });
                    ui.add(EditorWidget::new(&mut self.editor));
                });
            egui::Window::new("Registers").show(ctx, |ui| {
                ui.add(RegisterWidget::new(
                    Register::A,
                    Color32::from_hex("#a51723").unwrap(),
                    &mut self.cpu,
                    &mut self.register_ui,
                ));
            });
        });
    }
}
