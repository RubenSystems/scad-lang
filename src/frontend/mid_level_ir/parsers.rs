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
use crate::frontend::high_level_ir::ast_types::{
    Block, ExpressionBlock, ForLoop, Statement, StatementBlock,
};

use crate::frontend::high_level_ir::ast_types::FailureCopy;

use super::mir_ast_types::SSAValue;
use super::{
    mir_ast_types::SSAExpression,
    mir_translators::{expression_l1_to_l2, statement_l1_to_l2, ContinuationFunction},
};
use crate::frontend::high_level_ir::ast_types::IntegerWidth;

static mut CURRENT_TMP_INDEX: u128 = 0;

pub fn get_number_for_name() -> u128 {
    let val = unsafe { CURRENT_TMP_INDEX };
    unsafe { CURRENT_TMP_INDEX += 1 };
    val
}

pub fn generate_register_name() -> String {
    format!("tmp{}", get_number_for_name())
}

pub fn generate_label_name() -> String {
    format!("label{}", get_number_for_name())
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
                Statement::FunctionDefinition(_, _)
                | Statement::FunctionDecleration(_, _)
                | Statement::VariableDecl(_, _) => statement_l1_to_l2(head.fcopy(), k),
                _ => todo!(),
            }
        }
        [head, rest @ ..] => match head {
            Statement::FunctionDefinition(_, _) | Statement::FunctionDecleration(_, _) => {
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
        [head] => statement_l1_to_l2(
            head.fcopy(),
            Box::new(|_| expression_l1_to_l2(*blk.expression, k)),
        ),
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
        [] => expression_l1_to_l2(*blk.expression, k),
    }
}

pub fn parse_statement_block(blk: StatementBlock) -> SSAExpression {
    match blk.statements.as_slice() {
        [head] => statement_l1_to_l2(head.fcopy(), Box::new(|_| SSAExpression::Noop)),
        [head, rest @ ..] => {
            let rest_clone = rest.iter().map(|x| x.fcopy()).collect();
            statement_l1_to_l2(
                head.fcopy(),
                Box::new(|_| {
                    let new_blk = StatementBlock {
                        statements: rest_clone,
                    };
                    parse_statement_block(new_blk)
                }),
            )
        }
        [] => SSAExpression::Noop,
    }
}

pub fn parse_for_block(
    og: StatementBlock,
    blk: StatementBlock,
    unroll_count: usize,
    lp: ForLoop,
    step: i128,
    pid: usize,
) -> SSAExpression {

    if blk.statements.len() == 1 {
        // last statement, send it to be unrolled
        statement_l1_to_l2(
            blk.statements[0].fcopy(),
            Box::new(move |_| {
                if unroll_count == 0 {
                    SSAExpression::Noop
                } else {
                    SSAExpression::VariableDecl {
                        name: lp.variable.0.clone(),
                        vtype: None,
                        e1: SSAValue::FunctionCall {
                            name: "@add".into(),
                            parameters: vec![
                                SSAValue::Integer {
                                    value: step,
                                    width: IntegerWidth::IndexType,
                                    pool_id: pid,
                                },
                                SSAValue::VariableReference(lp.variable.0.clone(), pid),
                            ],
                            pool_id: pid,
                        },
                        e2: Box::new(parse_for_block(og.fcopy(), og, unroll_count - 1, lp, step, pid)),
                        pool_id: pid,
                    }
                }
            }),
        )
    } else if blk.statements.len() > 1 {
        let rest_clone = blk.statements[1..].iter().map(|x| x.fcopy()).collect();
        statement_l1_to_l2(
            blk.statements.first().unwrap().fcopy(),
            Box::new(move |_| {
                let new_blk = StatementBlock {
                    statements: rest_clone,
                };
                parse_for_block(og, new_blk, unroll_count, lp, step, pid)
            }),
        )
    } else {
        SSAExpression::Noop
    }
}
