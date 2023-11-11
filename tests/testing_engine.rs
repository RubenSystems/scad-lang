use main::compile;

mod testing_engine {
    use std::process::Command;

	fn compile_c_program(path: String) {
		let _ = Command::new("sh")
        .arg("-c")
        .arg(format!("clang {}/test.c -o {}/test -O3", path, path))
        .output()
        .expect("Failed to execute command");
	}

	fn compile_scad_program(path: String) {
		
	}
}