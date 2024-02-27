pub mod core;
pub mod frontend;
pub mod testing;

use crate::core::typedefs::create_types_for_core;
use crate::frontend::high_level_ir::ast_types::{FailureCopy, Statement};
use crate::frontend::high_level_ir::hir_parser::{parse, SCADParser};
use crate::frontend::mid_level_ir::ffi::{ffi_ssa_expr, TypeQueryEngine};
use crate::frontend::mid_level_ir::liveness_analysis::unalive_vars;
use crate::frontend::mid_level_ir::mir_desugar::{rename_variable_reassignment, rename_variables};

use crate::frontend::mid_level_ir::parsers::parse_program;


use crate::frontend::type_system::mir_to_tir::transform_mir_to_tir;


use crate::frontend::type_system::type_engine::{w_algo, WAlgoInfo};
use frontend::mid_level_ir::ffi::{FFIHIRExpr, FFIType};
use frontend::mid_level_ir::mir_ast_types::SSAExpression;

use frontend::high_level_ir::hir_parser::Rule;
use pest::Parser;
use std::collections::{HashMap, HashSet};
use std::ffi::{c_char, c_void, CStr};
use std::fs::File;
use std::io::Read;


#[no_mangle]
pub extern "C" fn query(
    query_engine: *mut c_void,
    query: *const c_char,
) -> std::mem::ManuallyDrop<FFIType> {
    let qep: &TypeQueryEngine =
        unsafe { std::mem::transmute(query_engine as *mut TypeQueryEngine) };
    let c_str = unsafe { CStr::from_ptr(query) };
    let str_slice: &str = c_str.to_str().expect("Failed to convert CStr to str");
    let tir = std::mem::ManuallyDrop::new(qep.get_type_for(str_slice));
    println!("{tir:#?}");
    tir
}

#[no_mangle]
pub extern "C" fn compile(
    filename: *const c_char,
    context_query_engine: *mut *mut c_void,
) -> std::mem::ManuallyDrop<FFIHIRExpr> {
    let c_str = unsafe { CStr::from_ptr(filename) };

    // Convert CStr to a string slice
    let str_slice: &str = c_str.to_str().expect("Failed to convert CStr to str");

    // Convert string slice to a String

    println!("reading from {str_slice}");

    let mut file = File::open(str_slice).unwrap();

    // Read the contents of the file into a string
    let mut program = String::new();
    file.read_to_string(&mut program).unwrap();

    let parsed_result = SCADParser::parse(Rule::program, program.as_str()).unwrap();

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
    // let code = mir_variable_fold(code, HashMap::new()).0;
    // let _referenced_vars = get_referenced(&code);
    // let code = remove_unused_variables(code, &referenced_vars);
    // endof optimiser

    let consumable_context = create_types_for_core();

    let (tir, ctx) = transform_mir_to_tir(code.fcopy(), consumable_context);
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

    println!("{context:#?} == context");
    let code = unalive_vars(code, vec![]);
    let query_engine = TypeQueryEngine::new(context);
    let qep = Box::into_raw(Box::new(query_engine));

    unsafe { *context_query_engine = qep as *mut c_void };

    // println!("{:#?}", context);
    // println!("{tpe:?}");

    println!("\n\n{:#?}\n\n", code);
    let code_res = std::mem::ManuallyDrop::new(code);
    std::mem::ManuallyDrop::new(ffi_ssa_expr(code_res))
}
