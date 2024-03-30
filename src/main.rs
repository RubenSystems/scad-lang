pub mod core;
pub mod frontend;
pub mod testing;

use crate::core::typedefs::create_types_for_core;
use crate::frontend::error::{ErrorPool, SCADError};
use crate::frontend::high_level_ir::ast_types::Statement;
use crate::frontend::high_level_ir::hir_parser::{parse, SCADParser};
use crate::frontend::mid_level_ir::mir_desugar::{rename_variable_reassignment, rename_variables};

use crate::frontend::mid_level_ir::mir_opt::{
    get_referenced, mir_variable_fold, remove_unused_variables,
};
use crate::frontend::mid_level_ir::parsers::parse_program;

use crate::frontend::high_level_ir::ast_types::FailureCopy;
use crate::frontend::mid_level_ir::liveness_analysis::unalive_vars;
use crate::frontend::mid_level_ir::mir_ast_types::SSAExpression;
use crate::frontend::type_system::mir_to_tir::transform_mir_to_tir;

use crate::frontend::type_system::type_engine::{w_algo, WAlgoInfo};

use crate::frontend::high_level_ir::hir_parser::Rule;
use crate::frontend::mid_level_ir::ffi::ffi_ssa_expr;
use pest::Parser;
use std::collections::{HashMap, HashSet};

fn main() -> std::io::Result<()> {
    let _args: Vec<String> = std::env::args().collect();

    // compile(&args[1], &args[2])?;

    let test_prog = r#"


    
    fn idx(row: ii, col: ii) ii {
        @add(a: @mul(a: row, b: 1024_ii), b: col)
    };
    
    fn idx_32(row: i32, col: i32) i32 {
        @add(a: @mul(a: row , b: 1024_i32), b: col )
    };
    
    fn get(container: 1048576xi32, row: ii, column: ii) i32 {
        let gtidx = idx(r: row, c: column);
        @index.i32(c: container, idx: gtidx)
    };
    
    fn transpose(a: 1048576xi32) 1048576xi32 {
        let new = @empty(fill: 0_i32, size: 1048576_ii);
    
        for i: 0_ii -> 1024_ii {
            for j: 0_ii -> 1024_ii {
                let tmp1 = get(c: a, r: i, c: j);
                @set.i32(c: new, in: idx(r: j, c: i), v: tmp1);
            };
        };
    
        new
    };
    
    fn sum(a: 4xi32) i32 {
        let t = {0_i32};
        for i: 0_ii -> 4_ii {
            let cval = @index.i32(c: a, idx: i); 
            let tval = @index.i32(c: t, idx: 0_ii);
    
            @set.i32(c: t, i: 0_ii, v: @add(a: cval, b: tval)) ;
        };
        
        let x = @index.i32(c: t, idx: 0_ii);
        x
    };
    
    fn tile_mul_and_sum(a: 1048576xi32, b: 1048576xi32, a_off: ii, b_off: ii) i32 {
        let t = {0_i32};
        let acc = @empty(f: 0_i32, s: 4_ii);
        for i: 0_ii -> 256_ii {
            let a_get = @vec.load(v: a, off: a_off, size: 4_ii);
            let b_get = @vec.load(v: b, off: b_off, size: 4_ii);
    
            let mul_res = @mul.v(a: a_get, b: b_get);
            @vec.store(out: acc, val: mul_res, offset: 0_ii);
            let s = sum(a: acc);
    
            let tval = @index.i32(c: t, idx: 0_ii);
            @set.i32(c: t, i: 0_ii, v: @add(a: s, b: tval)) ;
    
    
        };
    
        @index.i32(c: t, idx: 0_ii)
    };
    
    fn dot(a: 1048576xi32, b: 1048576xi32, result: 1048576xi32) i32 {
        let trn = transpose(mat: b);
    
        for i: 0_ii->1024_ii {
            for j: 0_ii->1024_ii step 1 unroll 2 {
    
                let sv = tile_mul_and_sum(a: a, b: trn, a_off: i, b_off: j);
    
                @set.i32(container: result, index: idx(r: i, c: j), value: sv);
            };		
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
    let code = mir_variable_fold(code, HashMap::new());
    let referenced_vars = get_referenced(&code.0);
    let code = remove_unused_variables(code.0, &referenced_vars);
    // endof optimiser

    // println!("{code:#?}");

    // println!("{:#?}", consumable_context);
    let consumable_context = create_types_for_core();
    let (tir, ctx) = transform_mir_to_tir(code.fcopy(), consumable_context);

    let (_, _, _context) = match w_algo(
        ctx,
        WAlgoInfo {
            retry_count: 0,
            req_type: None,
            pool: &location_pool,
        },
        &tir,
    ) {
        Ok(v) => v,
        Err(e) => {
            print!("{e}");
            unreachable!()
        }
    };

    // println!("\n\n{:#?}\n\n",context);
    let code = unalive_vars(code, vec![]);
    // println!("{code:#?}");
    let _ = ffi_ssa_expr(std::mem::ManuallyDrop::new(code));

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
