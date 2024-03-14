use crate::frontend::{
    error::PoolID,
    high_level_ir::ast_types::{Expression, FailureCopy, IntegerWidth, Statement},
    mid_level_ir::mir_ast_types::SSAConditionalBlock,
};

use super::{
    mir_ast_types::{SSAExpression, SSALabeledBlock, SSAValue},
    parsers::{
        generate_label_name, generate_register_name, parse_expression_block, parse_for_block,
        parse_statement_block,
    },
};

pub type ContinuationFunction = Box<dyn FnOnce(SSAValue) -> SSAExpression>;

pub fn expression_l1_to_l2(exp: Expression, k: ContinuationFunction) -> SSAExpression {
    match exp {
        Expression::Tensor(v, pid) => {
            fn aux(
                mut vec_expr: Vec<Expression>,
                mut vals: Vec<SSAValue>,
                pid: usize,
                k: ContinuationFunction,
            ) -> SSAExpression {
                if vec_expr.len() >= 1 {
                    expression_l1_to_l2(
                        vec_expr.remove(0),
                        Box::new(move |x| {
                            vals.push(x);
                            aux(vec_expr, vals, pid, k)
                        }),
                    )
                } else {
                    let tmp = generate_register_name();
                    SSAExpression::VariableDecl {
                        name: tmp.clone(),
                        vtype: None,
                        e1: SSAValue::Tensor(vals, pid),
                        e2: Box::new(k(SSAValue::VariableReference(tmp, pid))),
                        pool_id: pid,
                    }
                }
            }

            aux(v, vec![], pid, k)
        }
        Expression::InfixOperation(_e, _) => todo!(),
        Expression::Float(f, pid) => k(SSAValue::Float {
            value: f.value,
            width: f.width,
            pool_id: pid,
        }),
        Expression::Bool(f, pid) => k(SSAValue::Bool(f, pid)),
        Expression::Integer(i, pid) => k(SSAValue::Integer {
            value: i.value,
            width: i.width,
            pool_id: pid,
        }),
        Expression::CharArray(_, _) => todo!(),
        Expression::Identifier(x, pid) => k(SSAValue::VariableReference(x.0, pid)),
        Expression::Block(b, pid) => {
            SSAExpression::Block(Box::new(parse_expression_block(b, k)), pid)
        }
        Expression::FunctionCall(f, pid) => {
            fn aux(
                args: Vec<Expression>,
                vals: Vec<SSAValue>,
                function_name: String,
                pool_id: PoolID,
                k: ContinuationFunction,
            ) -> SSAExpression {
                match args.as_slice() {
                    [] => {
                        let tmp_name = generate_register_name();

                        SSAExpression::VariableDecl {
                            name: tmp_name.clone(),
                            vtype: None,
                            pool_id,
                            e1: SSAValue::FunctionCall {
                                name: function_name,
                                pool_id,
                                parameters: vals.iter().map(|x| x.fcopy()).collect(),
                            },
                            e2: Box::new(k(SSAValue::VariableReference(tmp_name, pool_id))),
                        }
                    }
                    [head, rest @ ..] => {
                        let rest_copy: Vec<Expression> = rest.iter().map(|v| v.fcopy()).collect();
                        let mut val_copy: Vec<SSAValue> = vals.iter().map(|v| v.fcopy()).collect();

                        expression_l1_to_l2(
                            head.fcopy(),
                            Box::new(move |x| {
                                val_copy.push(x);
                                aux(rest_copy, val_copy, function_name, pool_id, k)
                            }),
                        )
                    }
                }
            }

            aux(
                f.args.into_iter().map(|(_, exp)| exp).collect(),
                vec![],
                f.identifier.0,
                pid,
                k,
            )
        }
        Expression::ConditionalExpressionControlFlowControl {
            if_blocks,
            else_block,
            pool_id,
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
                                    Expression::Block(block.block, pool_id),
                                    Box::new(move |res| SSAExpression::Yield { val: res, pool_id }),
                                )),
                            },
                        },
                        else_block: SSALabeledBlock {
                            label: inner_else_label.clone(),
                            block: Box::new(expression_l1_to_l2(
                                Expression::Block(*else_block, pool_id),
                                Box::new(move |res| SSAExpression::Yield { val: res, pool_id }),
                            )),
                        },
                        pool_id,
                    },
                    e2: Box::new(gen(SSAValue::VariableReference(cond_tmp_name, pool_id))),
                    pool_id,
                }),
            )
        }
        // Expression::Cast(c) => k(SSAValue::Cast { value: Box::new(expression_l1_to_l2(c.expr, k)), to: c.to_type }),
        Expression::Cast(c, p) => expression_l1_to_l2(
            *c.expr,
            Box::new(move |value| {
                k(SSAValue::Cast {
                    value: Box::new(value),
                    to: c.to_type,
                    pool_id: p,
                })
            }),
        ),
    }
}

pub fn statement_l1_to_l2(statement: Statement, _k: ContinuationFunction) -> SSAExpression {
    match statement {
        Statement::ConstDecl(_, _) => todo!("bota"),
        Statement::VariableDecl(v, p) => {
            let gen = _k;

            expression_l1_to_l2(
                v.expression,
                Box::new(move |val| SSAExpression::VariableDecl {
                    name: v.identifier.0,
                    vtype: v.subtype,
                    e1: val.fcopy(),
                    e2: Box::new(gen(val)),
                    pool_id: p,
                }),
            )
        }
        Statement::Expression(exp, _p) => expression_l1_to_l2(exp, _k),
        Statement::FunctionDefinition(f, p) => {
            let gen = _k;
            SSAExpression::FuncDecl {
                name: f.identifier.0,
                args: f.args.into_iter().map(|e| (e.0 .0, e.1)).collect(),
                ret_type: Some(f.return_type),
                block: Box::new(parse_expression_block(
                    f.block,
                    Box::new(move |v| SSAExpression::Return { val: v, pool_id: p }),
                )),
                e2: Box::new(gen(SSAValue::Nothing)),
                pool_id: p,
            }
        }
        Statement::ProcedureDefinition(_f, _) => todo!(),
        Statement::Block(_blk, _) => todo!(),
        Statement::FunctionDecleration(f, pid) => {
            let gen = _k;
            SSAExpression::FuncForwardDecl {
                name: f.identifier.0,
                args: f.args.into_iter().map(|e| (e.0 .0, e.1)).collect(),
                ret_type: Some(f.return_type),
                e2: Box::new(gen(SSAValue::Nothing)),
                pool_id: pid,
            }
        }
        Statement::ProcedureDecleration(_, _) => todo!(),
        Statement::ForLoop(f, pid) => {
            let gen = _k;
            SSAExpression::ForLoop {
                block: Box::new(parse_for_block(f.block.fcopy(), f.unroll, f.fcopy(), pid)),
                iv: f.variable.0,
                from: SSAValue::Integer {
                    value: f.from as i128,
                    width: IntegerWidth::IndexType,
                    pool_id: pid,
                },
                to: SSAValue::Integer {
                    value: f.to as i128,
                    width: IntegerWidth::IndexType,
                    pool_id: pid,
                },
                parallel: f.parallel,
                e2: Box::new(gen(SSAValue::Nothing)),
                pool_id: pid,
                step: SSAValue::Integer {
                    value: (f.step + f.unroll) as i128,
                    width: IntegerWidth::IndexType,
                    pool_id: pid,
                },
            }
        }
        Statement::WhileLoop(w, pid) => {
            let gen = _k;
            let tmp_var = generate_label_name();
            let cond = SSAValue::VariableReference(tmp_var.clone(), pid);
            let cond_expr = expression_l1_to_l2(
                w.condition,
                Box::new(move |cond| SSAExpression::VariableDecl {
                    name: tmp_var,
                    vtype: None,
                    e1: cond,
                    e2: Box::new(SSAExpression::Noop),
                    pool_id: pid,
                }),
            );
            SSAExpression::WhileLoop {
                cond,
                block: Box::new(parse_statement_block(w.block)),
                e2: Box::new(gen(SSAValue::Nothing)),
                pool_id: pid,
                cond_expr: Box::new(cond_expr),
            }
        }
    }
}
