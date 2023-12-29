use crate::frontend::{
    high_level_ir::ast_types::{Expression, FailureCopy, Statement},
    mid_level_ir::mir_ast_types::SSAConditionalBlock,
};

use super::{
    mir_ast_types::{Phi, SSAExpression, SSALabeledBlock, SSAValue},
    parsers::{generate_label_name, generate_register_name, parse_expression_block},
};

pub type ContinuationFunction = Box<dyn FnOnce(SSAValue) -> SSAExpression>;

pub fn expression_l1_to_l2(exp: Expression, k: ContinuationFunction) -> SSAExpression {
    match exp {
        Expression::InfixOperation(_e) => todo!(),
        Expression::Float(f) => k(SSAValue::Float(f.0)),
        Expression::Bool(f) => k(SSAValue::Bool(f)),
        Expression::Integer(i) => k(SSAValue::Integer(i.0)),
        Expression::CharArray(_) => todo!(),
        Expression::Identifier(x) => k(SSAValue::RegisterReference(x.0)),
        Expression::Block(b) => SSAExpression::Block(Box::new(parse_expression_block(b, k))),
        Expression::FunctionCall(f) => {
            fn aux(
                args: Vec<Expression>,
                vals: Vec<SSAValue>,
                function_name: String,
                k: ContinuationFunction,
            ) -> SSAExpression {
                match args.as_slice() {
                    [] => {
                        let tmp_name = generate_register_name();

                        SSAExpression::VariableDecl {
                            name: tmp_name.clone(),
                            vtype: None,
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
        Expression::ConditionalExpressionControlFlowControl {
            if_blocks,
            else_block,
        } => {
            let gen = |x| k(x);
            let block = if_blocks.first().unwrap().fcopy();

            let if_label = generate_label_name();
            let else_label = generate_label_name();

            let inner_if_label = if_label.clone();
            let inner_else_label = else_label.clone();

            expression_l1_to_l2(
                block.condition,
                Box::new(move |condition| SSAExpression::ConditionalBlock {
                    if_block: SSAConditionalBlock {
                        condition,
                        block: SSALabeledBlock {
                            label: inner_if_label.clone(),
                            block: Box::new(expression_l1_to_l2(
                                Expression::Block(block.block),
                                Box::new(move |res| SSAExpression::VariableDecl {
                                    name: format!("{inner_if_label}_result"),
                                    vtype: None,
                                    e1: res,
                                    e2: Box::new(SSAExpression::Noop),
                                }),
                            )),
                        },
                    },
                    else_block: SSALabeledBlock {
                        label: inner_else_label.clone(),
                        block: Box::new(expression_l1_to_l2(
                            Expression::Block(*else_block),
                            Box::new(move |res| SSAExpression::VariableDecl {
                                name: format!("{inner_else_label}_result"),
                                vtype: None,
                                e1: res,
                                e2: Box::new(SSAExpression::Noop),
                            }),
                        )),
                    },
                    e2: Box::new(gen(SSAValue::Phi(vec![
                        Phi {
                            value: SSAValue::RegisterReference(format!("{if_label}_result")),
                            branch_name: if_label,
                        },
                        Phi {
                            value: SSAValue::RegisterReference(format!("{else_label}_result")),
                            branch_name: else_label,
                        },
                    ]))),
                }),
            )
        }
    }
}

pub fn statement_l1_to_l2(statement: Statement, _k: ContinuationFunction) -> SSAExpression {
    match statement {
        Statement::ConstDecl(c) => {
            let gen = |x| _k(x);

            expression_l1_to_l2(
                c.expression,
                Box::new(|val| SSAExpression::VariableDecl {
                    name: c.identifier.0,
                    vtype: Some(c.subtype),
                    e1: val.fcopy(),
                    e2: Box::new(gen(val)),
                }),
            )
        }
        Statement::VariableDecl(_v) => todo!(),
        Statement::Expression(exp) => expression_l1_to_l2(exp, _k),
        Statement::FunctionDefinition(f) => {
            let gen = |x| _k(x);
            SSAExpression::FuncDecl {
                name: f.identifier.0,
                args: f.args.into_iter().map(|e| (e.0 .0, e.1)).collect(),
                ret_type: Some(f.return_type),
                block: Box::new(parse_expression_block(
                    f.block,
                    Box::new(|v| SSAExpression::Return { val: v }),
                )),
                e2: Box::new(gen(SSAValue::Nothing)),
            }
        }
        Statement::ProcedureDefinition(_f) => {
            //     SSAExpression::FuncDecl {
            //     name: f.identifier.0,
            //     args: f.args.into_iter().map(|e| (e.0 .0, e.1)).collect(),
            //     block: parse_block(f.block, Box::new(|_| SSAExpression::Noop)),
            // };
            todo!()
        }
        Statement::Block(_blk) => todo!(),
        Statement::FunctionDecleration(f) => {
            let gen = |x| _k(x);
            SSAExpression::FuncForwardDecl {
                name: f.identifier.0,
                args: f.args.into_iter().map(|e| (e.0 .0, e.1)).collect(),
                ret_type: Some(f.return_type),
                e2: Box::new(gen(SSAValue::Nothing)),
            }
        }
        Statement::ProcedureDecleration(_) => todo!(),
    }
}
