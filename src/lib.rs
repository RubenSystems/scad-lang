pub mod frontend;
pub mod testing;

use crate::frontend::high_level_ir::ast_types::{FailureCopy, Statement};
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


fn compile() -> SSAExpression {
    let _args: Vec<String> = std::env::args().collect();

    let test_prog = r#"
        fn main(a: i32, b: i32) 2xi32; 

        fn main(a: i32, b: i32) 2xi32 {
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

    let consumable_context = Context::new();

    let (tir, ctx) = transform_mir_to_tir(code.fcopy(), consumable_context);
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

    code
}
