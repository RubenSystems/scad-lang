pub mod frontend;
pub mod testing;

use crate::frontend::high_level_ir::ast_types::Statement;
use crate::frontend::high_level_ir::hir_parser::{parse, SCADParser};
use crate::frontend::mid_level_ir::mir_desugar::{rename_variable_reassignment, rename_variables};
use crate::frontend::mid_level_ir::mir_opt::{
    get_referenced, mir_variable_fold, remove_unused_variables,
};
use crate::frontend::mid_level_ir::parsers::parse_program;
use crate::frontend::type_system::context::Context;

use crate::frontend::type_system::mir_to_tir::transform_mir_to_tir;
use crate::frontend::type_system::tir_types::{MonoType, TIRType};
use crate::frontend::type_system::type_engine::{instantiate, w_algo, WAlgoInfo};
use frontend::mid_level_ir::{mir_ast_types::SSAExpression, mir_translators::statement_l1_to_l2};

use frontend::high_level_ir::hir_parser::Rule;
use pest::Parser;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Write;

use std::path::Path;
use std::process::Command;

pub fn compile(path: &str, out_name: &str) -> std::io::Result<()> {
    let prog_path = Path::new(path);

    let prog = std::fs::read_to_string(prog_path).expect("Error reading file");
    let parsed_result = SCADParser::parse(Rule::program, &prog).unwrap();

    let code: Vec<String> = parsed_result
        .flat_map(|pair| {
            if let Rule::EOI = pair.as_rule() {
                None
            } else {
                Some(parse(pair.into_inner()))
            }
        })
        .map(|s| statement_l1_to_l2(s, Box::new(|_| SSAExpression::Noop)))
        .map(|ssa| ssa.to_llvm_ir(None, false))
        .collect();

    let tmp_builtins = r#"
        declare i32 @printf(i8*, ...)
        
        @.str_nl = private constant [2 x i8] c"\0A\00"
        @.str_star = private constant [2 x i8] c"*\00"
        @.str_space = private constant [2 x i8] c" \00"
        
        define void @new_line() #0 {
            %t0 = getelementptr [2 x i8], [2 x i8]* @.str_nl, i32 0, i32 0
            %1 = call i32 (i8*, ...) @printf(i8* %t0)
            ret void
        }
        
        define void @print_star() #0 {
            %t0 = getelementptr [2 x i8], [2 x i8]* @.str_star, i32 0, i32 0
            %1 = call i32 (i8*, ...) @printf(i8* %t0)
            ret void
        }
        
        define void @print_space() #0 {
            %t0 = getelementptr [2 x i8], [2 x i8]* @.str_space, i32 0, i32 0
            %1 = call i32 (i8*, ...) @printf(i8* %t0)
            ret void
        }
        
        define void @skip() #0 {
            ret void
        }
        
        @.str = private constant [4 x i8] c"%d\0A\00"
        
        define void @print_int(i32 %x) {
            %t0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
            call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
            ret void
        }

        define i32 @scad.inbuilts.add(i32 noundef %a, i32 noundef %b) {
            %add = add nsw i32 %b, %a
            ret i32 %add
        }
    "#;

    let parent_dir = prog_path.parent().unwrap().to_string_lossy();
    let parent_display_dir = if parent_dir == "" {
        "".into()
    } else {
        format!("{parent_dir}/")
    };

    let unoptimiesed_path = format!("{}unop.ll", parent_display_dir);
    let optimised_path = format!("{}opt.ll", parent_display_dir);
    let optimised_compiled_path = format!("{}opt.o", parent_display_dir);
    let mut output = File::create(&unoptimiesed_path)?;
    write!(output, "{tmp_builtins}\n{}", code.join("\n"))?;

    // opt -O3 -S in_file.ll > out_file.ll
    let _ = Command::new("sh")
        .arg("-c")
        .arg(format!(
            "/opt/homebrew/opt/llvm/bin/opt -O3 -S {unoptimiesed_path} > {optimised_path}"
        ))
        .output()
        .expect("Failed to execute command");

    let _ = Command::new("sh")
        .arg("-c")
        .arg(format!(
            "/opt/homebrew/opt/llvm/bin/llc -filetype=obj {optimised_path}"
        ))
        .output()
        .expect("Failed to execute command");

    let _ = Command::new("sh")
        .arg("-c")
        .arg(format!("clang {optimised_compiled_path} -o {out_name}"))
        .output()
        .expect("Failed to execute command");

    Ok(())
}

fn main() -> std::io::Result<()> {
    let _args: Vec<String> = std::env::args().collect();

    // compile(&args[1], &args[2])?;

    let test_prog = r#"
        fn main() 1xi32; 

        fn main() 1xi32 {
            let mut x: 1xi32 = {100, 200};

            x
        };

    "#;

    let parsed_result = SCADParser::parse(Rule::program, test_prog).unwrap();

    let raw_statements: Vec<Statement> = parsed_result
        .flat_map(|pair| {
            if let Rule::EOI = pair.as_rule() {
                None
            } else {
                Some(parse(pair.into_inner()))
            }
        })
        .collect();
    let unop_code = parse_program(raw_statements, Box::new(|_| SSAExpression::Noop));

    let code = rename_variables(unop_code, vec!["test".into()], &mut HashSet::new());
    let code = rename_variable_reassignment(code, &mut HashMap::new());

    // Optimiser
    let code = mir_variable_fold(code, HashMap::new());
    let referenced_vars = get_referenced(&code.0);
    let code = remove_unused_variables(code.0, &referenced_vars);
    // endof optimiser

    println!("{code:#?}");

    let mut consumable_context = Context::new();

    consumable_context.add_type_for_name(
        "scad_core_arithmetic_add".into(),
        TIRType::MonoType(MonoType::Application {
            c: "->".into(),
            types: vec![
                MonoType::Application {
                    c: "f32".into(),
                    types: vec![],
                },
                MonoType::Application {
                    c: "->".into(),
                    types: vec![
                        MonoType::Application {
                            c: "f32".into(),
                            types: vec![],
                        },
                        MonoType::Application {
                            c: "f32".into(),
                            types: vec![],
                        },
                    ],
                },
            ],
        }),
    );
    consumable_context.add_type_for_name(
        "scad_core_arithmetic_add".into(),
        TIRType::MonoType(MonoType::Application {
            c: "->".into(),
            types: vec![
                MonoType::Application {
                    c: "1xi32".into(),
                    types: vec![],
                },
                MonoType::Application {
                    c: "->".into(),
                    types: vec![
                        MonoType::Application {
                            c: "1xi32".into(),
                            types: vec![],
                        },
                        MonoType::Application {
                            c: "1xi32".into(),
                            types: vec![],
                        },
                    ],
                },
            ],
        }),
    );

    // println!("{:#?}", consumable_context);

    let (tir, ctx) = transform_mir_to_tir(code, consumable_context);
    println!("\n\n{:#?}\n\n", tir);

    let (_, _, context) = w_algo(
        ctx,
        WAlgoInfo {
            retry_count: 0,
            req_type: None,
        },
        &tir,
    )
    .unwrap();

    println!("{:#?}", context);
    // println!("{tpe:?}");

    Ok(())
}

mod tests {
    use crate::testing::run_test;

    fn test_programs(path: &str) {
        let test_output = run_test(path);
        println!("C Speed: {}", test_output.c_test.duration.as_nanos());
        println!("SCaD Speed: {}", test_output.scad_test.duration.as_nanos());
        println!(
            "Speed up: {}",
            (test_output.scad_test.duration - test_output.c_test.duration).as_nanos()
        );
        assert_eq!(test_output.scad_test.output, test_output.c_test.output);
    }

    #[test]
    fn basic_program() {
        test_programs("test_programs/basic");
    }

    #[test]
    fn basic_conditional_1() {
        test_programs("test_programs/basic_conditional_1");
    }
}
