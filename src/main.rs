use emu8085::gui::App;
use emu8085::asm::AssembledProgram;
use emu8085::emu::CPU;

fn main() {
    // let program = AssembledProgram::assemble_file("examples/lda.asm").unwrap_or_else(|err| {
    //     println!("{}", err);
    //     std::process::exit(1);
    // });
    // let mut cpu = CPU::new();
    // cpu.load_data(&program.get_memory(), 0);
    // cpu.execute(program.get_entrypoint());
    // println!("{:?}", cpu);
    // program.save("examples/lda.ex85").unwrap();

    let native_options = eframe::NativeOptions{
        viewport: egui::ViewportBuilder::default().with_inner_size([1280.0, 720.0]).with_min_inner_size([400.0, 300.0]).with_maximized(true),
        ..Default::default()
    };
    eframe::run_native("emu8085", native_options, Box::new(|cc| Ok(Box::new(App::new(cc))))).unwrap();
}
