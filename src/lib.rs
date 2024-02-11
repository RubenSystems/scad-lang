pub mod frontend;
pub mod testing;

use crate::frontend::high_level_ir::ast_types::{FailureCopy, Statement};
use crate::frontend::high_level_ir::hir_parser::{parse, SCADParser};
use crate::frontend::mid_level_ir::ffi::ffi_ssa_expr;
use crate::frontend::mid_level_ir::mir_desugar::{rename_variable_reassignment, rename_variables};
use crate::frontend::mid_level_ir::mir_opt::{
    get_referenced, mir_variable_fold, remove_unused_variables,
};
use crate::frontend::mid_level_ir::parsers::parse_program;
use crate::frontend::type_system::context::Context;

use crate::frontend::type_system::mir_to_tir::transform_mir_to_tir;

use crate::frontend::type_system::tir_types::{MonoType, TIRType};
use crate::frontend::type_system::type_engine::{w_algo, WAlgoInfo};
use frontend::mid_level_ir::ffi::FFIHIRExpr;
use frontend::mid_level_ir::mir_ast_types::SSAExpression;

use frontend::high_level_ir::hir_parser::Rule;
use pest::Parser;
use std::collections::{HashMap, HashSet};

#[no_mangle]
pub extern "C" fn compile() -> std::mem::ManuallyDrop<FFIHIRExpr> {
    let _args: Vec<String> = std::env::args().collect();

    let test_prog = r#"

    fn main() 2xi32;

    fn add_and_print() 2xi32;

    fn add_and_print(m: 2xi32) 2xi32 {
        let mut y: 2xi32 = {700, 800};
        let mut k: 2xi32 = @add(a: y, b: m);
        @print(value: k);
        k
    };

    fn main() 2xi32 {
        let mut x: 2xi32 = {500, 600};
        let mut y: 2xi32 = add_and_print(m: x);
        y
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
    let code = mir_variable_fold(code, HashMap::new()).0;
    let referenced_vars = get_referenced(&code);
    // let code = remove_unused_variables(code.0, &referenced_vars);
    // endof optimiser

    let mut consumable_context = Context::new();

    consumable_context.add_type_for_name(
        "@print".into(),
        TIRType::MonoType(MonoType::Application {
            dimensions: None,
                    c: "->".into(),
                    types: vec![
                        MonoType::Variable("@any_printing_type".into()),
                        MonoType::Variable("@any_printing_type".into()),
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
    let (tir, ctx) = transform_mir_to_tir(code.fcopy(), consumable_context);
    println!("\n\n{:#?}\n\n", code);
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

    let code_res = std::mem::ManuallyDrop::new(code);
    std::mem::ManuallyDrop::new(ffi_ssa_expr(code_res))
}
