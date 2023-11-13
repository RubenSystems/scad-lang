use crate::compile;
use std::{
    process::Command,
    time::{Duration, Instant},
};

pub struct TestOutput {
    pub output: String,
    pub duration: Duration,
}

pub struct CompTestOutput {
    pub c_test: TestOutput,
    pub scad_test: TestOutput,
}

pub fn compile_c_program(path: &str) {
    let cmd = format!("clang {}/test.c -o {}/testc -O3", path, path);
    let _ = Command::new("sh")
        .arg("-c")
        .arg(&cmd)
        .output()
        .expect("Failed to execute command");
}

pub fn compile_scad_program(path: &str) {
    let _ = compile(&format!("{path}/test.scad"), &format!("{path}/testscad"));
}

pub fn run(exe: &str) -> TestOutput {
    let start = Instant::now();
    let output = Command::new("sh")
        .arg("-c")
        .arg(format!("./{}", exe))
        .output()
        .expect("Failed to execute command");
    let end = Instant::now();
    let elapsed_time = start - end;

    TestOutput {
        output: String::from_utf8(output.stdout).unwrap(),
        duration: elapsed_time,
    }
}

pub fn run_test(path: &str) -> CompTestOutput {
    compile_c_program(&path);
    compile_scad_program(&path);

    let c_out = run(&format!("{path}/testc"));
    let scad_out = run(&format!("{path}/testscad"));

    CompTestOutput {
        c_test: c_out,
        scad_test: scad_out,
    }
}
