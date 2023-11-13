use crate::frontend::{
    high_level_ir::ast_types::{Block, ConditionalBlock, Expression, FailureCopy, Statement},
    mid_level_ir::mir_ast_types::SSAConditionalBlock,
};

use super::{
    mir_ast_types::{SSAExpression, SSAValue},
    parsers::{generate_register_name, parse_anonymous_block, parse_function_block},
};

pub type ContinuationFunction = Box<dyn FnOnce(SSAValue) -> SSAExpression>;

pub fn expression_l1_to_l2(exp: Expression, k: ContinuationFunction) -> SSAExpression {
    match exp {
        Expression::InfixOperation(e) => {
            let op = e.op.0.clone();

            let k1 = |y1| {
                let k2 = |y2| {
                    let tmp_name = generate_register_name();
                    let operation = SSAValue::Operation {
                        lhs: Box::new(y1),
                        op: op,
                        rhs: Box::new(y2),
                    };
                    SSAExpression::RegisterDecl {
                        name: tmp_name.clone(),
                        e1: operation,
                        e2: Box::new(k(SSAValue::RegisterReference(tmp_name))),
                    }
                };
                expression_l1_to_l2(*e.rhs, Box::new(k2))
            };
            expression_l1_to_l2(*e.lhs, Box::new(k1))
        }
        Expression::Float(f) => k(SSAValue::Float(f.0)),
        Expression::Integer(i) => k(SSAValue::Integer(i.0)),
        Expression::CharArray(_) => todo!(),
        Expression::Identifier(x) => {
            let id = x.0;
            let tmp_name = generate_register_name();
            SSAExpression::VariableReference {
                name: id,
                tmp_name: tmp_name.clone(),
                e2: Box::new(k(SSAValue::VariableDereference(tmp_name))),
            }
        }
        Expression::Block(b) => SSAExpression::Block(parse_anonymous_block(b, k)),
        Expression::FunctionCall(f) => {
            fn aux(
                args: Vec<Expression>,
                vals: Vec<SSAValue>,
                function_name: String,
                k: Box<dyn FnOnce(SSAValue) -> SSAExpression>,
            ) -> SSAExpression {
                match args.as_slice() {
                    [] => {
                        let tmp_name = generate_register_name();

                        SSAExpression::RegisterDecl {
                            name: tmp_name.clone(),
                            e1: SSAValue::FunctionCall {
                                name: function_name,
                                parameters: vals.iter().map(|x| x.fcopy()).collect(),
                            },
                            e2: Box::new(k(SSAValue::RegisterReference(tmp_name))),
                        }
                    }
                    [head, rest @ ..] => {
                        let rest_copy: Vec<Expression> = rest.iter().map(|v| v.fcopy()).collect();
                        let mut val_copy: Vec<SSAValue> = vals.iter().map(|v| v.fcopy()).collect();

                        expression_l1_to_l2(
                            head.fcopy(),
                            Box::new(|x| {
                                val_copy.push(x);
                                aux(rest_copy, val_copy, function_name, k)
                            }),
                        )
                    }
                }
            }

            aux(
                f.args.iter().map(|(_, exp)| exp.fcopy()).collect(),
                vec![],
                f.identifier.0,
                k,
            )
        }
        Expression::IfControlFlow {
            if_blocks,
            else_block,
        } => {
            let _tmp_name = generate_register_name();
            // ===== Warning: viewer discression is advised =====

            // let block_copy = if_block.block.fcopy();
            fn aux(
                conditionals: Vec<ConditionalBlock>,
                else_block: Option<Box<Block>>,
                k: ContinuationFunction,
            ) -> SSAExpression {
                let conditionals_copy: Vec<ConditionalBlock> =
                    conditionals.iter().map(|x| x.fcopy()).collect();
                match conditionals_copy.as_slice() {
                    [] => unreachable!("WHAYYYY you can't have a nothing if!"),
                    [head] => {
                        let condition_copy = head.condition.fcopy();
                        let block_copy = head.block.fcopy();
                        expression_l1_to_l2(
                            condition_copy,
                            Box::new(|cond| SSAExpression::ConditionalBlock {
                                if_block: Box::new(SSAConditionalBlock {
                                    condition: cond,
                                    block: Box::new(SSAExpression::Block(parse_anonymous_block(
                                        block_copy,
                                        Box::new(|_| SSAExpression::Noop),
                                    ))),
                                }),
                                e2: Box::new(match else_block {
                                    Some(e) => SSAExpression::Block(parse_anonymous_block(*e, k)),
                                    _ => SSAExpression::Noop,
                                }),
                            }),
                        )
                    }
                    [head, rest @ ..] => {
                        let condition_copy = head.condition.fcopy();
                        let block_copy = head.block.fcopy();
                        let rest_copy: Vec<ConditionalBlock> =
                            rest.iter().map(|x| x.fcopy()).collect();
                        expression_l1_to_l2(
                            condition_copy,
                            Box::new(|cond| SSAExpression::ConditionalBlock {
                                if_block: Box::new(SSAConditionalBlock {
                                    condition: cond,
                                    block: Box::new(SSAExpression::Block(parse_anonymous_block(
                                        block_copy,
                                        Box::new(|_| SSAExpression::Noop),
                                    ))),
                                }),
                                e2: Box::new(aux(rest_copy, else_block, k)),
                            }),
                        )
                    }
                }
            }

            aux(if_blocks, else_block, k)
        }
    }
}

pub fn statement_l1_to_l2(statement: Statement, _k: ContinuationFunction) -> SSAExpression {
    match statement {
        Statement::ConstDecl(c) => expression_l1_to_l2(
            c.expression,
            Box::new(|val| SSAExpression::ConstDecl {
                name: c.identifier.0,
                e1: val,
                e2: Box::new(SSAExpression::Noop),
            }),
        ),
        Statement::VariableDecl(v) => expression_l1_to_l2(
            v.expression,
            Box::new(|val| SSAExpression::VariableDecl {
                name: v.identifier.0,
                e1: val,
                e2: Box::new(SSAExpression::Noop),
            }),
        ),
        Statement::Loop(_) => todo!(),
        Statement::Expression(exp) => expression_l1_to_l2(exp, Box::new(|_| SSAExpression::Noop)),
        Statement::FunctionDefinition(f) => SSAExpression::FuncDecl {
            name: f.identifier.0,
            args: f.args.into_iter().map(|e| (e.0 .0, e.1)).collect(),
            block: parse_function_block(f.block),
        },
    }
}
