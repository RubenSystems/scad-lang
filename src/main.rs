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
use crate::frontend::type_system::type_engine::{w_algo, WAlgoInfo};
use crate::frontend::mid_level_ir::mir_ast_types::SSAExpression;
use crate::frontend::high_level_ir::ast_types::FailureCopy;
use crate::frontend::mid_level_ir::liveness_analysis::unalive_vars;

use crate::frontend::mid_level_ir::ffi::ffi_ssa_expr;
use crate::frontend::high_level_ir::hir_parser::Rule;
use pest::Parser;
use std::collections::{HashMap, HashSet};

fn main() -> std::io::Result<()> {
    let _args: Vec<String> = std::env::args().collect();

    // compile(&args[1], &args[2])?;

    let test_prog = r#"
    fn main() 2xi32;

    fn add_200(a: 2xi32, b: 2xi32) 2xi32 {
        @add(a: a, b: @add(a: b, b: @{200, 200}))
    };

    fn main() 2xi32 {
        let mut x: 2xi32 = @{700, 800};
        let mut y: 2xi32 = @{800, 900};
        let mut z: 2xi32 = @{800, 200};

        let mut super_x : 2xi32 = add_200(a: x, b: y);

        @print(val: super_x);

        let mut does_it_work: 2xi32 = if true {
            super_x
        } else {
            @add(a: x, b: y)
        };

        @print(value: does_it_work);

        does_it_work
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
    let code = unalive_vars(code, vec![]).0;
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
        "@drop".into(),
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
        "@index".into(),
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
                        MonoType::Variable("@any_tensor_type_2".into()),
                    ],
                },
            ],
        }),
    );

    // println!("{:#?}", consumable_context);

    let (tir, ctx) = transform_mir_to_tir(code.fcopy(), consumable_context);
    println!("\n\n{:#?}\n\n", code);
    // println!("\n\n{:#?}\n\n", tir);

    let (_, _, context) = w_algo(
        ctx,
        WAlgoInfo {
            retry_count: 0,
            req_type: None,
        },
        &tir,
    )
    .unwrap();

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
