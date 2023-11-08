use crate::frontend::parser::ast_types::{Block, Statement};

use super::{
    mir_ast_types::SSAExpression,
    mir_translators::{expression_ssa_transformation, statement_ssa_translation},
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
        _ => todo!(),
    }
}

pub fn parse_function_block(blk: Block) -> Vec<SSAExpression> {
    let length = blk.statements.len();
    let mut block_statements: Vec<SSAExpression> = blk
        .statements
        .into_iter()
        .enumerate()
        .map(|(idx, statement)| {
            if idx == length - 1 {
                match statement {
                    Statement::Expression(e) => expression_ssa_transformation(
                        e,
                        Box::new(|e| SSAExpression::Return { val: e }),
                    ),
                    _ => statement_ssa_translation(statement, Box::new(|_| SSAExpression::Noop)),
                }
            } else {
                statement_ssa_translation(statement, Box::new(|_| SSAExpression::Noop))
            }
        })
        .collect();

    block_statements
}

pub fn parse_anonymous_block(blk: Block) -> Vec<SSAExpression> {
    let length = blk.statements.len();
    let mut block_statements: Vec<SSAExpression> = blk
        .statements
        .into_iter()
        .enumerate()
        .map(|(idx, statement)| {
            if idx == length - 1 {
                match statement {
                    Statement::Expression(e) => {
                        let tmp_name = generate_register_name();
                        expression_ssa_transformation(
                            e,
                            Box::new(|e| SSAExpression::RegisterDecl {
                                name: tmp_name,
                                e1: e,
                                e2: Box::new(SSAExpression::Noop),
                            }),
                        )
                    }
                    _ => statement_ssa_translation(statement, Box::new(|_| SSAExpression::Noop)),
                }
            } else {
                statement_ssa_translation(statement, Box::new(|_| SSAExpression::Noop))
            }
        })
        .collect();

    block_statements
}
