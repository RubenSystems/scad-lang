//===- parsers.rs - AST -> MIR parser -----*- rust -*-===//
//
// Part of the SCaD Compiler,
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file defines functions designed to accept the L1 code
/// reperesented in AST form and convert it into a L2 (MIR) code
///
//===----------------------------------------------------------------------===//
use crate::frontend::high_level_ir::ast_types::{Block, ExpressionBlock, Statement};

use crate::frontend::high_level_ir::ast_types::FailureCopy;

use super::{
    mir_ast_types::SSAExpression,
    mir_translators::{expression_l1_to_l2, statement_l1_to_l2, ContinuationFunction},
};

static mut CURRENT_TMP_INDEX: u128 = 0;

pub fn generate_register_name() -> String {
    let val = unsafe { CURRENT_TMP_INDEX };
    unsafe { CURRENT_TMP_INDEX += 1 };
    format!("tmp{val}")
}

pub fn op_to_llvm(op: &str) -> String {
    match op {
        "+" => "add".into(),
        "*" => "mul".into(),
        "-" => "sub".into(),
        "/" => "sdiv".into(),
        ">=" => "icmp sle".into(),
        "==" => "icmp eq".into(),
        _ => todo!(),
    }
}

// pub fn parse_function_block(blk: Block) -> Vec<SSAExpression> {
//     let length = blk.statements.len();
//     let block_statements: Vec<SSAExpression> = blk
//         .statements
//         .into_iter()
//         .enumerate()
//         .map(|(idx, statement)| {
//             if idx == length - 1 {
//                 match statement {
//                     Statement::Expression(e) => {
//                         expression_l1_to_l2(e, Box::new(|e| SSAExpression::Return { val: e }))
//                     }
//                     _ => statement_l1_to_l2(statement, Box::new(|_| SSAExpression::Noop)),
//                 }
//             } else {
//                 statement_l1_to_l2(statement, Box::new(|_| SSAExpression::Noop))
//             }
//         })
//         .collect();

//     block_statements
// }

pub fn parse_block(blk: Block, k: ContinuationFunction) -> Vec<SSAExpression> {
    blk.statements
        .iter()
        .map(|s| statement_l1_to_l2(s.fcopy(), Box::new(|_| SSAExpression::Noop)))
        .collect()
}

pub fn parse_expression_block(
    blk: ExpressionBlock,
    k: ContinuationFunction,
) -> Vec<SSAExpression> {
    let mut ssa_expressions: Vec<SSAExpression> = blk
        .statements
        .iter()
        .map(|s| statement_l1_to_l2(s.fcopy(), Box::new(|_| SSAExpression::Noop)))
        .collect();

    ssa_expressions.push(expression_l1_to_l2(*blk.expression, Box::new(k)));

    ssa_expressions
}
