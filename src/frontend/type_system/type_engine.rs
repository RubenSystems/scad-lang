// given an HIR expression, return a type

use crate::frontend::high_level_ir::ast_types::Expression;

fn get_type(expression: Expression) {
    match expression {
        Expression::InfixOperation(e) => todo!(),
        Expression::Float(_) => todo!(),
        Expression::Integer(_) => todo!(),
        Expression::CharArray(_) => todo!(),
        Expression::Identifier(_) => todo!(),
        Expression::IfControlFlow {
            if_blocks,
            else_block,
        } => todo!(),
        Expression::FunctionCall(_) => todo!(),
        Expression::Block(_) => todo!(),
    }
}
