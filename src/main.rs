pub mod core;
pub mod frontend;

use crate::frontend::error::{ErrorPool, SCADError};
use crate::frontend::high_level_ir::ast_types::Statement;
use crate::frontend::high_level_ir::hir_parser::{parse, SCADParser};
use crate::frontend::mid_level_ir::mir_desugar::{rename_variable_reassignment, rename_variables};

use crate::frontend::mid_level_ir::mir_opt::{
    get_referenced, mir_variable_fold, remove_unused_variables,
};
use crate::frontend::mid_level_ir::parsers::parse_program;

use crate::frontend::mid_level_ir::liveness_analysis::unalive_vars;
use crate::frontend::mid_level_ir::mir_ast_types::SSAExpression;

use crate::frontend::high_level_ir::hir_parser::Rule;
use crate::frontend::mid_level_ir::ffi::ffi_conversion::ffi_ssa_expr;
use crate::frontend::type_system::type_engine::extract_type_information;
use pest::Parser;
use std::collections::{HashMap, HashSet};

fn main() -> std::io::Result<()> {
    let _args: Vec<String> = std::env::args().collect();

    // compile(&args[1], &args[2])?;

    let test_prog = r#"
    fn idx(row: ii, col: ii) ii {
        @addi(a: @muli(a: row, b: 2_ii), b: col)
    };
    
    fn get(container: 4xi32, row: ii, column: ii) i32 {
        let gtidx = idx(r: row, c: column);
        @index.i32(c: container, idx: gtidx)
    };
    
    fn dot(a: 4xi32, b: 4xi32) 4xi32 {
        let result = {0_i32, 0_i32, 0_i32, 0_i32};
    
        for i: 0_ii -> 2_ii {
            for j: 0_ii -> 2_ii {
                for k: 0_ii -> 2_ii {
                    let res = @muli(a: get(container: a, r: i, c: k), b: get(container: a, r: k, c: j));
                    let existing = @index.i32(container: result, idx: idx(r: i, c: j));
    
                    @set.i32(container: result, index: idx(r: i, c: j), value: @addi(a: res, b: existing));
                };
            };
        };
    
    
        result
    };
    
    
    
    fn main() i32 {
    
        let a = {1_i32, 2_i32, 3_i32, 4_i32};
        let b = {1_i32, 2_i32, 3_i32, 4_i32};
    
        let dot_val = dot(a: a, b: b);
    
        for i: 0_ii -> 4_ii {
            @print(a: @index.i32(container: dot_val, index: i));
        };
    
    
        0_i32
    };
    
    
    "#;
    let _counter: usize = 0;
    let mut location_pool = ErrorPool::new();
    let parsed_result = match SCADParser::parse(Rule::program, test_prog) {
        Ok(p) => p,
        Err(e) => {
            println!("{e:#?}");
            let error = SCADError::from_parse_error(e);
            println!("{error} ");
            todo!()
        }
    };
    let raw_statements: Vec<Statement> = parsed_result
        .flat_map(|pair| {
            if let Rule::EOI = pair.as_rule() {
                None
            } else {
                Some(parse(pair.into_inner(), &mut location_pool).unwrap())
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

    // println!("{:#?}", consumable_context);

    let context = match extract_type_information(&code, &location_pool) {
        Ok(e) => e,
        Err(e) => {
            println!("{e}");
            return Ok(());
        }
    };

    // println!("\n\n{:#?}\n\n",context);
    let code = unalive_vars(code, vec![]);
    println!("{context:#?}");
    let _ = ffi_ssa_expr(code, "", &context, &location_pool);

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
