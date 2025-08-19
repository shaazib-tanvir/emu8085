use eframe::CreationContext;
use egui::{text::LayoutJob, Color32, FontData, FontDefinitions, FontFamily, FontId, TextBuffer, Ui, WidgetText};
use egui_dock::{DockArea, DockState, TabViewer};

struct Viewer;

impl TabViewer for Viewer {
    type Tab = Tab;

    fn title(&mut self, tab: &mut Self::Tab) -> WidgetText {
        WidgetText::Text(tab.title.clone())
    }

    fn ui(&mut self, ui: &mut Ui, tab: &mut Self::Tab) {
        match &mut tab.state {
            TabState::Editor(editor) => {
                let mut layouter = |ui: &Ui, buf: &dyn TextBuffer, wrap_width| {
                        let mut layout_job = LayoutJob::simple("".to_string(), FontId::default(), Color32::GRAY, wrap_width);
                        let code_text = buf.as_str();
                        for line in code_text.split_inclusive("\n") {
                            let (label, unit) = match line.split_once(":") {
                                Some(value) => (Some(value.0), value.1),
                                None => (None, line),
                            };

                            if let Some(label) = label {
                                layout_job.append(&(label.to_string() + ":"), 0.0, egui::TextFormat {
                                    color: Color32::from_hex("#fb4934").unwrap(),
                                    ..Default::default()
                                });
                            }

                            if !unit.is_empty() {
                                let (mnemonic, operands) = match unit.split_once(" ") {
                                    Some(value) => (value.0, Some(value.1)),
                                    None => (unit, None),
                                };

                                if mnemonic.starts_with(".") {
                                    layout_job.append(mnemonic, 0.0, egui::TextFormat{
                                        color: Color32::from_hex("#83a598").unwrap(),
                                        ..Default::default()
                                    });
                                } else {
                                    layout_job.append(mnemonic, 0.0, egui::TextFormat{
                                        color: Color32::from_hex("#b8bb26").unwrap(),
                                        ..Default::default()
                                    });
                                }

                                if let Some(operands) = operands {
                                    layout_job.append(&(" ".to_string() + operands), 0.0, egui::TextFormat::default());
                                }
                            }
                        }

                        ui.fonts(|f| f.layout_job(layout_job))
                };
                let code_widget = egui::TextEdit::multiline(&mut editor.code)
                    .lock_focus(true)
                    .layouter(&mut layouter);
                ui.add_sized(ui.available_size(), code_widget);
            }
        }
    }
}

struct Editor {
    code: String,
}

impl Editor {
    fn new() -> Self {
        Self {
            code: "".to_string(),
        }
    }
}

enum TabState {
    Editor(Editor),
}

struct Tab {
    title: String,
    state: TabState,
}

struct Tabs {
    dock_state: DockState<Tab>,
}

impl Tabs {
    fn new() -> Self {
        let tabs = vec![Tab {
            title: "Editor".to_string(),
            state: TabState::Editor(Editor::new()),
        }];

        Tabs {
            dock_state: DockState::new(tabs),
        }
    }

    fn ui(&mut self, ui: &mut Ui) {
        DockArea::new(&mut self.dock_state)
            .show_close_buttons(false)
            .show_inside(ui, &mut Viewer);
    }
}

pub struct App {
    tabs: Tabs,
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

        Self { tabs: Tabs::new() }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.set_max_width(500.0);
            ui.set_max_height(500.0);
            self.tabs.ui(ui);
        });
    }
}
