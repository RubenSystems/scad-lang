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
use crate::frontend::high_level_ir::ast_types::{Block, Statement};

use crate::frontend::high_level_ir::ast_types::FailureCopy;

use super::{
    mir_ast_types::{SSAExpression},
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
        _ => todo!(),
    }
}

pub fn parse_function_block(blk: Block) -> Vec<SSAExpression> {
    let length = blk.statements.len();
    let block_statements: Vec<SSAExpression> = blk
        .statements
        .into_iter()
        .enumerate()
        .map(|(idx, statement)| {
            if idx == length - 1 {
                match statement {
                    Statement::Expression(e) => {
                        expression_l1_to_l2(e, Box::new(|e| SSAExpression::Return { val: e }))
                    }
                    _ => statement_l1_to_l2(statement, Box::new(|_| SSAExpression::Noop)),
                }
            } else {
                statement_l1_to_l2(statement, Box::new(|_| SSAExpression::Noop))
            }
        })
        .collect();

    block_statements
}

pub fn parse_anonymous_block(blk: Block, k: ContinuationFunction) -> Vec<SSAExpression> {
    let length = blk.statements.len();
    let mut block_statements: Vec<SSAExpression> = blk.statements[..length - 1]
        .iter()
        .enumerate()
        .map(|(_, statement)| {
            statement_l1_to_l2(statement.fcopy(), Box::new(|_| SSAExpression::Noop))
        })
        .collect();

    let last_blk_stmt = blk.statements[length - 1].fcopy();
    let last_expression = match last_blk_stmt {
        Statement::Expression(e) => expression_l1_to_l2(e.fcopy(), Box::new(|e| k(e))),
        _ => statement_l1_to_l2(last_blk_stmt, Box::new(|_| SSAExpression::Noop)),
    };
    block_statements.push(last_expression);

    block_statements
}
