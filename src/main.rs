pub mod frontend;
pub mod testing;

use crate::frontend::high_level_ir::ast_types::Statement;
use crate::frontend::high_level_ir::hir_parser::{parse, SCADParser};
use crate::frontend::mid_level_ir::mir_desugar::{rename_variable_reassignment, rename_variables};

use crate::frontend::mid_level_ir::parsers::parse_program;
use crate::frontend::type_system::context::Context;

use crate::frontend::high_level_ir::ast_types::FailureCopy;
use crate::frontend::mid_level_ir::liveness_analysis::unalive_vars;
use crate::frontend::mid_level_ir::mir_ast_types::SSAExpression;
use crate::frontend::type_system::mir_to_tir::transform_mir_to_tir;
use crate::frontend::type_system::tir_types::{MonoType, TIRType};
use crate::frontend::type_system::type_engine::{w_algo, WAlgoInfo};

use crate::frontend::high_level_ir::hir_parser::Rule;
use crate::frontend::mid_level_ir::ffi::ffi_ssa_expr;
use pest::Parser;
use std::collections::{HashMap, HashSet};

fn main() -> std::io::Result<()> {
    let _args: Vec<String> = std::env::args().collect();

    // compile(&args[1], &args[2])?;

    let test_prog = r#"
    fn do(a: 2xi32, b: 2xi32) 2xi32 {
        @{300, 400}
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
    // let code = mir_variable_fold(code, HashMap::new());
    // let referenced_vars = get_referenced(&code.0);
    // let code = remove_unused_variables(code.0, &referenced_vars);
    // endof optimiser

    // println!("{code:#?}");

    let mut consumable_context = Context::new();

    consumable_context.add_type_for_name(
        "@print".into(),
        TIRType::MonoType(MonoType::Application {
            dimensions: None,
            c: "->".into(),
            types: vec![
                MonoType::Variable("any_vec_any".into()),
                MonoType::Variable("any_vec_any".into()),
            ],
        }),
    );

    consumable_context.add_type_for_name(
        "@drop.i32".into(),
        TIRType::MonoType(MonoType::Application {
            dimensions: None,
            c: "->".into(),
            types: vec![
                MonoType::Variable("@drop.a".into()),
                MonoType::Variable("@drop.b".into()),
            ],
        }),
    );

    consumable_context.add_type_for_name(
        "@drop.tensori32".into(),
        TIRType::MonoType(MonoType::Application {
            dimensions: None,
            c: "->".into(),
            types: vec![
                MonoType::Variable("@drop.a".into()),
                MonoType::Variable("@drop.b".into()),
            ],
        }),
    );

    consumable_context.add_type_for_name(
        "@add".into(),
        TIRType::MonoType(MonoType::Application {
            c: "->".into(),
            dimensions: None,
            types: vec![
                MonoType::Variable("@any_adding_type".into()),
                MonoType::Application {
                    c: "->".into(),
                    dimensions: None,
                    types: vec![
                        MonoType::Variable("@any_adding_type".into()),
                        MonoType::Variable("@any_adding_type".into()),
                    ],
                },
            ],
        }),
    );

    consumable_context.add_type_for_name(
        "@index.i32".into(),
        TIRType::MonoType(MonoType::Application {
            c: "->".into(),
            dimensions: None,
            types: vec![
                MonoType::Variable("@any_tensor_type".into()),
                MonoType::Application {
                    c: "->".into(),
                    dimensions: None,
                    types: vec![
                        MonoType::Variable("@any_index_type".into()),
                        MonoType::Application {
                            c: "i32".into(),
                            dimensions: None,
                            types: vec![],
                        },
                    ],
                },
            ],
        }),
    );

    // println!("{:#?}", consumable_context);

    let (tir, ctx) = transform_mir_to_tir(code.fcopy(), consumable_context);
    println!("\n\n{:#?}\n\n", code);

    let (_, _, context) = w_algo(
        ctx,
        WAlgoInfo {
            retry_count: 0,
            req_type: None,
        },
        &tir,
    )
    .unwrap();

    println!("\n\n{:#?}\n\n", context);
    let code = unalive_vars(code, vec![], &context);
    _ = ffi_ssa_expr(std::mem::ManuallyDrop::new(code));
    // println!("{tpe:?}");

    Ok(())
}

mod tests {
    // use crate::testing::run_test;

    // fn test_programs(path: &str) {
    //     let test_output = run_test(path);
    //     println!("C Speed: {}", test_output.c_test.duration.as_nanos());
    //     println!("SCaD Speed: {}", test_output.scad_test.duration.as_nanos());
    //     println!(
    //         "Speed up: {}",
    //         (test_output.scad_test.duration - test_output.c_test.duration).as_nanos()
    //     );
    //     assert_eq!(test_output.scad_test.output, test_output.c_test.output);
    // }

    // #[test]
    // fn basic_program() {
    //     test_programs("test_programs/basic");
    // }

    // #[test]
    // fn basic_conditional_1() {
    //     test_programs("test_programs/basic_conditional_1");
    // }
}
