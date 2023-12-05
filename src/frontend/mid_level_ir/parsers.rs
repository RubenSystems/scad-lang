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

pub fn parse_program(prog: Vec<Statement>, k: ContinuationFunction) -> SSAExpression {
    match prog.as_slice() {
        [head] => {
            match head {
                // currently can only render functions
                Statement::FunctionDefinition(_) => statement_l1_to_l2(head.fcopy(), k),
                _ => todo!(),
            }
        }
        [head, rest @ ..] => match head {
            Statement::FunctionDefinition(_) => {
                let rest_clone: Vec<Statement> = rest.iter().map(|x| x.fcopy()).collect();
                statement_l1_to_l2(head.fcopy(), Box::new(|_| parse_program(rest_clone, k)))
            }
            _ => todo!(),
        },
        [] => SSAExpression::Noop,
    }
}

pub fn parse_block(_blk: Block, _k: ContinuationFunction) -> Vec<SSAExpression> {
    // blk.statements
    //     .iter()
    //     .map(|s| statement_l1_to_l2(s.fcopy(), Box::new(|_| SSAExpression::Noop)))
    //     .collect()
    todo!()
}

pub fn parse_expression_block(blk: ExpressionBlock, k: ContinuationFunction) -> SSAExpression {
    match blk.statements.as_slice() {
        [head] => {
            let expression_clone = blk.expression.fcopy();
            statement_l1_to_l2(
                head.fcopy(),
                Box::new(|_| expression_l1_to_l2(expression_clone, k)),
            )
        }
        [head, rest @ ..] => {
            let rest_clone = rest.iter().map(|x| x.fcopy()).collect();
            statement_l1_to_l2(
                head.fcopy(),
                Box::new(|_| {
                    let new_blk = ExpressionBlock {
                        statements: rest_clone,
                        expression: blk.expression,
                    };
                    parse_expression_block(new_blk, k)
                }),
            )
        }
        [] => {
            let expression_clone = blk.expression.fcopy();
            expression_l1_to_l2(expression_clone, k)
        }
    }

    // let mut ssa_expressions: Vec<SSAExpression> = blk
    //     .statements
    //     .iter()
    //     .map(|s| statement_l1_to_l2(s.fcopy(), Box::new(|_| SSAExpression::Noop)))
    //     .collect();

    // ssa_expressions.push(expression_l1_to_l2(*blk.expression, Box::new(k)));

    // ssa_expressions
}
