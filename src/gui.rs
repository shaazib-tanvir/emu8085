use std::{
    hash::{DefaultHasher, Hash, Hasher},
    sync::Arc,
};

use eframe::CreationContext;
use egui::{
    text::LayoutJob, Color32, FontData, FontDefinitions, FontFamily, FontId, RichText, TextBuffer, Ui, WidgetText
};
use egui_dock::{DockArea, DockState, NodeIndex, TabViewer};

use crate::{common::Register, emu::CPU};

struct AppState {
    cpu: CPU,
    editor: Editor,
}

impl AppState {
    fn new() -> Self {
        Self {
            editor: Editor::new(),
            cpu: CPU::new(),
        }
    }
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
                ui.vertical(|ui| {
                    let mut value = self.cpu.get_register(Register::A).to_string();
                    ui.text_edit_singleline(&mut value);
                    ui.label(WidgetText::RichText(Arc::new(RichText::new("A").color(Color32::RED).background_color(Color32::BLACK))));
                });
            }
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
        tree.split_right(NodeIndex::root(), 0.8, vec![Tab {
            title: "Registers".to_string(),
            tab_type: TabType::Registers,
        }]);

        Tabs {
            dock_state,
        }
    }

    fn ui(&mut self, ui: &mut Ui, app_state: &mut AppState) {
        DockArea::new(&mut self.dock_state)
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
