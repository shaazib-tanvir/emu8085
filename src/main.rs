use emu8085::gui::App;

fn main() {
    let native_options = eframe::NativeOptions{
        viewport: egui::ViewportBuilder::default().with_inner_size([1920.0, 1080.0]).with_min_inner_size([400.0, 300.0]).with_maximized(true),
        ..Default::default()
    };
    eframe::run_native("emu8085", native_options, Box::new(|cc| Ok(Box::new(App::new(cc))))).unwrap();
}
