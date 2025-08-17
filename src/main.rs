use emu8085::gui::App;

fn main() {
    let native_options = eframe::NativeOptions::default();
    eframe::run_native("emu8085", native_options, Box::new(|cc| Ok(Box::new(App::new(cc))))).unwrap();
}
