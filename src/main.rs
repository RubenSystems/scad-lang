/*

    This file is for testing purposes 

*/

pub mod core;
pub mod frontend;

use crate::frontend::error::{ErrorPool, SCADError};
use crate::frontend::high_level_ir::ast_types::Statement;
use crate::frontend::high_level_ir::hir_parser::{parse, SCADParser};
use crate::frontend::mid_level_ir::mir_desugar::{rename_variable_reassignment, rename_variables};

use crate::frontend::mid_level_ir::mir_opt::{
    get_referenced, mir_constant_propagate, remove_unused_variables,
};
use crate::frontend::mid_level_ir::mir_translators::TranslatorInformation;
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

    fn je(a: i32) i32 {
        0_i32
    };
    fn main() i32 {
        je(a: 100)
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
    let unop_code = parse_program(
        raw_statements,
        Box::new(|_| SSAExpression::Noop),
        TranslatorInformation {
            tensor_type_info: None,
        },
    );

    let code = rename_variables(unop_code, vec!["test".into()], &mut HashSet::new());
    let code = rename_variable_reassignment(code, &mut HashMap::new());
    // Optimiser
    let code = mir_constant_propagate(code, HashMap::new());
    let referenced_vars = get_referenced(&code.0);
    let code = remove_unused_variables(code.0, &referenced_vars);
    // endof optimiser

    println!("{code:#?}");

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
    // println!("{context:#?}");
    let _ = ffi_ssa_expr(code, "", &context, &location_pool).expect("failed to generate");

    Ok(())
}
