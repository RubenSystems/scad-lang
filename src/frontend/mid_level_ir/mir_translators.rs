use crate::frontend::{
    high_level_ir::ast_types::{Expression, FailureCopy, IntegerWidth, Statement},
    mid_level_ir::mir_ast_types::SSAConditionalBlock,
};

use super::{
    mir_ast_types::{SSAExpression, SSALabeledBlock, SSAValue},
    parsers::{
        generate_label_name, generate_register_name, parse_expression_block, parse_statement_block,
    },
};

pub type ContinuationFunction = Box<dyn FnOnce(SSAValue) -> SSAExpression>;

pub fn expression_l1_to_l2(exp: Expression, k: ContinuationFunction) -> SSAExpression {
    match exp {
        Expression::Tensor(v) => {
            fn aux(
                mut vec_expr: Vec<Expression>,
                mut vals: Vec<SSAValue>,
                k: ContinuationFunction,
            ) -> SSAExpression {
                if vec_expr.len() >= 1 {
                    expression_l1_to_l2(
                        vec_expr.remove(0),
                        Box::new(|x| {
                            vals.push(x);
                            aux(vec_expr, vals, k)
                        }),
                    )
                } else {
                    let tmp = generate_register_name();
                    SSAExpression::VariableDecl {
                        name: tmp.clone(),
                        vtype: None,
                        e1: SSAValue::Tensor(vals),
                        e2: Box::new(k(SSAValue::VariableReference(tmp))),
                    }
                }
            }

            aux(v, vec![], k)
        }
        Expression::InfixOperation(_e) => todo!(),
        Expression::Float(f) => k(SSAValue::Float {
            value: f.value,
            width: f.width,
        }),
        Expression::Bool(f) => k(SSAValue::Bool(f)),
        Expression::Integer(i) => k(SSAValue::Integer {
            value: i.value,
            width: i.width,
        }),
        Expression::CharArray(_) => todo!(),
        Expression::Identifier(x) => k(SSAValue::VariableReference(x.0)),
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
                            e2: Box::new(k(SSAValue::VariableReference(tmp_name))),
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
                f.args.into_iter().map(|(_, exp)| exp).collect(),
                vec![],
                f.identifier.0,
                k,
            )
        }
        Expression::ConditionalExpressionControlFlowControl {
            if_blocks,
            else_block,
        } => {
            let gen = k;
            let block = if_blocks.first().unwrap().fcopy();

            let if_label = generate_label_name();
            let else_label = generate_label_name();

            let inner_if_label = if_label.clone();
            let inner_else_label = else_label.clone();

            let cond_tmp_name = generate_register_name();
            expression_l1_to_l2(
                block.condition,
                Box::new(move |condition| SSAExpression::VariableDecl {
                    name: cond_tmp_name.clone(),
                    vtype: None,
                    e1: SSAValue::ConditionalBlock {
                        if_block: SSAConditionalBlock {
                            condition: Box::new(condition),
                            block: SSALabeledBlock {
                                label: inner_if_label.clone(),
                                block: Box::new(expression_l1_to_l2(
                                    Expression::Block(block.block),
                                    Box::new(move |res| SSAExpression::Yield { val: res }),
                                )),
                            },
                        },
                        else_block: SSALabeledBlock {
                            label: inner_else_label.clone(),
                            block: Box::new(expression_l1_to_l2(
                                Expression::Block(*else_block),
                                Box::new(move |res| SSAExpression::Yield { val: res }),
                            )),
                        },
                    },
                    e2: Box::new(gen(SSAValue::VariableReference(cond_tmp_name))),
                }),
            )
        }
        // Expression::Cast(c) => k(SSAValue::Cast { value: Box::new(expression_l1_to_l2(c.expr, k)), to: c.to_type }),
        Expression::Cast(c) => expression_l1_to_l2(
            *c.expr,
            Box::new(|value| {
                k(SSAValue::Cast {
                    value: Box::new(value),
                    to: c.to_type,
                })
            }),
        ),
    }
}

pub fn statement_l1_to_l2(statement: Statement, _k: ContinuationFunction) -> SSAExpression {
    match statement {
        Statement::ConstDecl(_) => todo!("bota"),
        Statement::VariableDecl(v) => {
            let gen = _k;

            expression_l1_to_l2(
                v.expression,
                Box::new(|val| SSAExpression::VariableDecl {
                    name: v.identifier.0,
                    vtype: v.subtype,
                    e1: val.fcopy(),
                    e2: Box::new(gen(val)),
                }),
            )
        }
        Statement::Expression(exp) => expression_l1_to_l2(exp, _k),
        Statement::FunctionDefinition(f) => {
            let gen = _k;
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
            let gen = _k;
            SSAExpression::FuncForwardDecl {
                name: f.identifier.0,
                args: f.args.into_iter().map(|e| (e.0 .0, e.1)).collect(),
                ret_type: Some(f.return_type),
                e2: Box::new(gen(SSAValue::Nothing)),
            }
        }
        Statement::ProcedureDecleration(_) => todo!(),
        Statement::ForLoop(f) => {
            let gen = _k;
            SSAExpression::ForLoop {
                iv: f.variable.0,
                from: SSAValue::Integer {
                    value: f.from as i128,
                    width: IntegerWidth::IndexType,
                },
                to: SSAValue::Integer {
                    value: f.to as i128,
                    width: IntegerWidth::IndexType,
                },
                block: Box::new(parse_statement_block(f.block)),
                e2: Box::new(gen(SSAValue::Nothing)),
            }
        }
    }
}
