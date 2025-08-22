use std::{
    hash::{DefaultHasher, Hash, Hasher},
    sync::Arc,
};

use eframe::CreationContext;
use egui::{
    Color32, FontData, FontDefinitions, FontFamily, FontId, RichText, TextBuffer, Ui,
    WidgetText, text::LayoutJob,
};
use egui_dock::{DockArea, DockState, NodeIndex, TabStyle, TabViewer};

use crate::{
    common::{Register, RegisterPair},
    emu::CPU,
};

struct DebugUIState {
    a: String,
    b: String,
    c: String,
    d: String,
    e: String,
    h: String,
    l: String,
    flags: String,
    stack_pointer: String,
    program_counter: String,
}

impl From<&CPU> for DebugUIState {
    fn from(value: &CPU) -> Self {
        DebugUIState {
            a: format!("{:0>2x}", value.get_register(Register::A)),
            b: format!("{:0>2x}", value.get_register(Register::B)),
            c: format!("{:0>2x}", value.get_register(Register::C)),
            d: format!("{:0>2x}", value.get_register(Register::D)),
            e: format!("{:0>2x}", value.get_register(Register::E)),
            h: format!("{:0>2x}", value.get_register(Register::H)),
            l: format!("{:0>2x}", value.get_register(Register::L)),
            program_counter: format!("{:0>4x}", value.get_rp(RegisterPair::PC)),
            stack_pointer: format!("{:0>4x}", value.get_rp(RegisterPair::SP)),
            flags: value.get_flags().to_string(),
        }
    }
}

struct AppState {
    cpu: CPU,
    editor: Editor,
    debug_ui: DebugUIState,
}

impl AppState {
    fn new() -> Self {
        let cpu = CPU::new();
        let debug_ui = DebugUIState::from(&cpu);

        Self {
            editor: Editor::new(),
            cpu,
            debug_ui,
        }
    }
}

fn text_with_label(ui: &mut Ui, field: &mut String, label: &str, color: Color32) -> egui::Response {
    let inner_response = ui.vertical(|ui| {
        ui.spacing_mut().item_spacing.y = 1.0;
        let mut layouter = |ui: &egui::Ui, buffer: &dyn egui::TextBuffer, wrap_width: f32| {
            let mut layout_job = LayoutJob::simple("".to_string(), FontId::default(), Color32::GRAY, wrap_width);
            let mut value = buffer.as_str().to_string();
            for character in buffer.as_str().chars() {
                if character.is_digit(16) {
                    value += &character.to_string();
                }
            }

            layout_job.append(&value, 0.0, egui::TextFormat::default());

            ui.fonts(|f| f.layout_job(layout_job))
        };
        let text_edit = egui::TextEdit::singleline(field).layouter(&mut layouter)
            .horizontal_align(egui::Align::Center)
            .vertical_align(egui::Align::Center)
            .char_limit(2);
        let label = egui::Label::new(WidgetText::RichText(Arc::new(
            RichText::new(label)
                .color(Color32::BLACK)
                .background_color(color),
        )));

        let response = ui.add_sized([50.0, 30.0], text_edit);
        let rect = response.rect;
        ui.painter().rect_filled(
            egui::Rect::from_min_size(rect.left_bottom() + egui::Vec2::new(0.0, ui.spacing().item_spacing.y), egui::vec2(50.0, 20.0)),
            2.0,
            color,
        );
        ui.add_sized([50.0, 20.0], label);

        response
    });

    inner_response.response
}

impl TabViewer for AppState {
    type Tab = Tab;

    fn title(&mut self, tab: &mut Self::Tab) -> WidgetText {
        WidgetText::Text(tab.title.clone())
    }

    fn ui(&mut self, ui: &mut Ui, tab: &mut Self::Tab) {
        match &mut tab.tab_type {
            TabType::Editor => {
                let editor = &mut self.editor;
                let mut layouter = |ui: &Ui, buf: &dyn TextBuffer, wrap_width| {
                    let code_text = buf.as_str();
                    let mut hasher = DefaultHasher::new();
                    code_text.hash(&mut hasher);
                    let hash = hasher.finish();

                    if editor.last_hash == hash && editor.galley.is_some() {
                        return editor.galley.clone().unwrap();
                    }

                    let mut layout_job = LayoutJob::simple(
                        "".to_string(),
                        FontId::default(),
                        Color32::GRAY,
                        wrap_width,
                    );

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
                ui.add_sized(ui.available_size(), code_widget);
            }
            TabType::Registers => {
                egui::Grid::new("Registers").spacing([15.0, 15.0]).max_col_width(20.0).show(ui, |ui| {
                    text_with_label(ui, &mut self.debug_ui.a, "A", Color32::from_hex("#7a0918").unwrap());
                    ui.end_row();

                    text_with_label(ui, &mut self.debug_ui.b, "B", Color32::from_hex("#007d8d").unwrap());
                    text_with_label(ui, &mut self.debug_ui.c, "C", Color32::from_hex("#007d8d").unwrap());
                    ui.end_row();
                });
            }
        }
    }

    fn on_rect_changed(&mut self, tab: &mut Self::Tab) {
    }

    fn tab_style_override(&self, tab: &Self::Tab, style: &TabStyle) -> Option<TabStyle> {
        match tab.tab_type {
            TabType::Registers => {
                let mut new_style = style.clone();
                new_style.tab_body.inner_margin = egui::Margin::same(10);
                Some(new_style)
            }
            _ => None
        }
    }

    fn is_closeable(&self, _tab: &Self::Tab) -> bool {
        false
    }

    fn scroll_bars(&self, tab: &Self::Tab) -> [bool; 2] {
        match tab.tab_type {
            TabType::Registers => [false, false],
            TabType::Editor => [true, true],
        }
    }
}

struct Editor {
    code: String,
    last_hash: u64,
    galley: Option<Arc<egui::Galley>>,
}

impl Editor {
    fn new() -> Self {
        Self {
            last_hash: 0,
            galley: None,
            code: "".to_string(),
        }
    }
}

enum TabType {
    Editor,
    Registers,
}

struct Tab {
    title: String,
    tab_type: TabType,
}

struct Tabs {
    dock_state: DockState<Tab>,
}

impl Tabs {
    fn new() -> Self {
        let tabs = vec![Tab {
            title: "Editor".to_string(),
            tab_type: TabType::Editor,
        }];

        let mut dock_state = DockState::new(tabs);
        let tree = dock_state.main_surface_mut();
        tree.split_right(
            NodeIndex::root(),
            0.8,
            vec![Tab {
                title: "Registers".to_string(),
                tab_type: TabType::Registers,
            }],
        );

        Tabs { dock_state }
    }

    fn ui(&mut self, ui: &mut Ui, app_state: &mut AppState) {
        DockArea::new(&mut self.dock_state)
            .show_leaf_close_all_buttons(false)
            .show_close_buttons(false)
            .show_inside(ui, app_state);
    }
}

pub struct App {
    tabs: Tabs,
    state: AppState,
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

        Self {
            tabs: Tabs::new(),
            state: AppState::new(),
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            self.tabs.ui(ui, &mut self.state);
        });
    }
}
