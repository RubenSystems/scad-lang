//===----------------------------------------------------------------------===// 
///
/// The SCaD frontend. 
/// 
/// This is the frontend module for scad and contains methods that can be 
/// called by a  backend including compile, drop and query methods for a 
/// context query engine
/// 
//===----------------------------------------------------------------------===//


pub mod core;
pub mod frontend;

use crate::frontend::error::SCADError;
use crate::frontend::high_level_ir::ast_types::Statement;
use crate::frontend::high_level_ir::hir_parser::{parse, SCADParser};
use crate::frontend::mid_level_ir::ffi::ffi_types::ProgramOut;
use crate::frontend::mid_level_ir::liveness_analysis::unalive_vars;
use crate::frontend::mid_level_ir::mir_desugar::{rename_variable_reassignment, rename_variables};

use crate::frontend::mid_level_ir::parsers::parse_program;

use frontend::error::ErrorPool;
use frontend::mid_level_ir::ffi::ffi_drop::drop_ffi_data;
use frontend::mid_level_ir::ffi::ffi_types::OutData;
use frontend::mid_level_ir::ffi::{
    ffi_conversion::ffi_ssa_expr,
    ffi_types::{FFIHIRExpr, FFIType},
    type_query_engine::TypeQueryEngine,
};
use frontend::mid_level_ir::mir_ast_types::SSAExpression;

use frontend::high_level_ir::hir_parser::Rule;
use frontend::mid_level_ir::mir_opt::{get_referenced, mir_constant_propagate, remove_unused_variables};
use frontend::mid_level_ir::mir_translators::TranslatorInformation;
use frontend::type_system::type_engine::extract_type_information;
use pest::Parser;
use std::collections::{HashMap, HashSet};
use std::ffi::{c_char, c_void, CStr};
use std::fs::File;
use std::io::Read;


/*
    Query: 

        - provide a query engine
        - provide a query

    this will return the type of a query. 
*/
#[no_mangle]
pub extern "C" fn query(
    query_engine: *mut c_void,
    query: *const c_char,
) -> FFIType {
    let qep: &TypeQueryEngine =
        unsafe { std::mem::transmute(query_engine as *mut TypeQueryEngine) };
    let c_str = unsafe { CStr::from_ptr(query) };
    let str_slice: &str = c_str.to_str().expect("Failed to convert CStr to str");
    let tir = qep.get_type_for(str_slice);
    tir
}

/*
    function to drop the ir provided by compile method
    this should be called to free the outdata 
*/
#[no_mangle]
pub extern "C" fn drop_ir(ffi: OutData) {
    drop_ffi_data(ffi)
}


/*
    Compile a SCaD program 

        - a file name containing a scad program
        - a pointer to a pointer to a context query engine 

    the compiler will generate a cqe and overwrite the pointer (warning!)

    the compiler will then generate the LIR and return it to the caller 
*/
#[no_mangle]
pub unsafe extern "C" fn compile(
    filename: *const c_char,
    context_query_engine: *mut *mut c_void,
) -> OutData {
    let c_str = unsafe { CStr::from_ptr(filename) };

    // Convert CStr to a string slice
    let str_slice: &str = c_str.to_str().expect("Failed to convert CStr to str");

    // Convert string slice to a String

    let mut file = File::open(str_slice).unwrap();

    // Read the contents of the file into a string
    let mut program = String::new();
    file.read_to_string(&mut program).unwrap();

    let parsed_result = match SCADParser::parse(Rule::program, &program) {
        Ok(p) => p,
        Err(e) => {
            let error = SCADError::from_parse_error(e);
            println!("{error} ");
            return OutData {
                compiled: false,
                program: ProgramOut { error: 0 },
            };
        }
    };

    let mut location_pool = ErrorPool::new();

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

    // perform desugaring
    let code = rename_variables(unop_code, vec!["prog".into()], &mut HashSet::new());
    let code = rename_variable_reassignment(code, &mut HashMap::new());

    // extract type information
    let context = match extract_type_information(&code, &location_pool) {
        Ok(e) => e,
        Err(e) => {
            println!("{e}");
            return OutData {
                compiled: false,
                program: ProgramOut { error: 0 },
            };
        }
    };

    // Run optimisation and liveness passes
    let code = mir_constant_propagate(code, HashMap::new());
    let referenced_vars = get_referenced(&code.0);
    let code = remove_unused_variables(code.0, &referenced_vars);
    let code = unalive_vars(code, vec![]);

    let ffiex = match ffi_ssa_expr(code, "", &context, &location_pool) {
        Ok(e) => e,
        Err(e) => {
            println!("{e}");
            return OutData {
                compiled: false,
                program: ProgramOut { error: 0 },
            };
        }
    };
    // generate query engine for type information
    let out = OutData {
        compiled: true,
        program: ProgramOut {
            program: std::mem::ManuallyDrop::new(ffiex),
        },
    };

    let query_engine = TypeQueryEngine::new(context);
    let qep = Box::into_raw(Box::new(query_engine));

    unsafe { *context_query_engine = qep as *mut c_void };

    // Make it so code is not automatically dropped using std::mem::manually drop
    out
}
