//===----------------------------------------------------------------------===//
///
/// Transform program to a almost-SSA representation 
///
//===----------------------------------------------------------------------===//



use crate::frontend::{
    error::PoolID,
    high_level_ir::ast_types::{Expression, FailureCopy, IntegerWidth, Statement, Type},
    mid_level_ir::mir_ast_types::SSAConditionalBlock,
};

use super::{
    mir_ast_types::{SSAExpression, SSALabeledBlock, SSAValue},
    parsers::{
        for_block_induction_variable, generate_label_name, generate_register_name,
        parse_expression_block, parse_statement_block,
    },
};

/*
    Define funciton for continuation passing style. 

    The general idea with this conversion is that all values must be 
    brought out of the definition and converted into their own varible. once 
    this process is completed, the expression must be generated. the values 
    that are hoisted out of the expression are passed into the function and the 
    expression render uses these to generate the new expression. 
*/
pub type ContinuationFunction = Box<dyn FnOnce(SSAValue) -> SSAExpression>;


/*
    Information that needs to be 'pushed down' to lower recursive 
    iterations
*/
#[derive(Clone)]
pub struct TranslatorInformation {
    pub tensor_type_info: Option<Type>,
}

pub fn expression_l1_to_l2(
    exp: Expression,
    k: ContinuationFunction,
    info: TranslatorInformation,
) -> SSAExpression {
    match exp {
        Expression::Tensor(v, pid) => {
            fn aux(
                mut vec_expr: Vec<Expression>,
                mut vals: Vec<SSAValue>,
                pid: usize,
                k: ContinuationFunction,
                info: TranslatorInformation,
            ) -> SSAExpression {
                let ioc = info.clone();
                if vec_expr.len() >= 1 {
                    expression_l1_to_l2(
                        vec_expr.remove(0),
                        Box::new(move |x| {
                            vals.push(x);
                            aux(vec_expr, vals, pid, k, info.clone())
                        }),
                        ioc,
                    )
                } else {
                    let tmp = generate_register_name();
                    SSAExpression::VariableDecl {
                        name: tmp.clone(),
                        vtype: info.tensor_type_info,
                        e1: SSAValue::Tensor(vals, pid),
                        e2: Box::new(k(SSAValue::VariableReference(tmp, pid))),
                        pool_id: pid,
                    }
                }
            }

            aux(v, vec![], pid, k, info)
        }
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
        Expression::Identifier(x, pid) => k(SSAValue::VariableReference(x.0, pid)),
        Expression::Block(b, pid) => {
            SSAExpression::Block(Box::new(parse_expression_block(b, k, info)), pid)
        }
        Expression::FunctionCall(f, pid) => {
            fn aux(
                args: Vec<Expression>,
                vals: Vec<SSAValue>,
                function_name: String,
                pool_id: PoolID,
                k: ContinuationFunction,
                info: TranslatorInformation,
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
                        let ioc = info.clone();
                        expression_l1_to_l2(
                            head.fcopy(),
                            Box::new(move |x| {
                                val_copy.push(x);
                                aux(rest_copy, val_copy, function_name, pool_id, k, info)
                            }),
                            ioc,
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
                info,
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
            let ioc = info.clone();
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
                                    info.clone(),
                                )),
                            },
                        },
                        else_block: SSALabeledBlock {
                            label: inner_else_label.clone(),
                            block: Box::new(expression_l1_to_l2(
                                Expression::Block(*else_block, pool_id),
                                Box::new(move |res| SSAExpression::Yield { val: res, pool_id }),
                                info,
                            )),
                        },
                        pool_id,
                    },
                    e2: Box::new(gen(SSAValue::VariableReference(cond_tmp_name, pool_id))),
                    pool_id,
                }),
                ioc,
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
            info,
        ),
    }
}

pub fn statement_l1_to_l2(
    statement: Statement,
    _k: ContinuationFunction,
    info: TranslatorInformation,
) -> SSAExpression {
    match statement {
        Statement::ConstDecl(_, _) => todo!("bota"),
        Statement::VariableDecl(v, p) => {
            let gen = _k;
            let vcpy = v.subtype.clone();
            expression_l1_to_l2(
                v.expression,
                Box::new(move |val| SSAExpression::VariableDecl {
                    name: v.identifier.0,
                    vtype: v.subtype,
                    e1: val.fcopy(),
                    e2: Box::new(gen(val)),
                    pool_id: p,
                }),
                TranslatorInformation {
                    tensor_type_info: vcpy,
                },
            )
        }
        Statement::Expression(exp, _p) => expression_l1_to_l2(exp, _k, info),
        Statement::FunctionDefinition(f, p) => {
            let gen = _k;
            SSAExpression::FuncDecl {
                name: f.identifier.0,
                args: f.args.into_iter().map(|e| (e.0 .0, e.1)).collect(),
                ret_type: Some(f.return_type),
                block: Box::new(parse_expression_block(
                    f.block,
                    Box::new(move |v| SSAExpression::Return { val: v, pool_id: p }),
                    info,
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
            let new_step = (f.step * (f.unroll + 1)) as i128;
            let ioc = info.clone();
            let ioctt = info.clone();
            expression_l1_to_l2(
                f.from.fcopy(),
                Box::new(move |from_var| {
                    expression_l1_to_l2(
                        f.to.fcopy(),
                        Box::new(move |to_var| SSAExpression::ForLoop {
                            block: Box::new(for_block_induction_variable(
                                f.block.fcopy(),
                                f.block.fcopy(),
                                f.unroll,
                                f.fcopy(),
                                f.step as i128,
                                f.vector_iv,
                                pid,
                                info,
                            )),
                            iv: f.variable.0,
                            from: from_var,
                            to: to_var,
                            parallel: f.parallel,
                            e2: Box::new(gen(SSAValue::Nothing)),
                            pool_id: pid,
                            step: SSAValue::Integer {
                                value: new_step,
                                width: Some(IntegerWidth::IndexType),
                                pool_id: pid,
                            },
                        }),
                        ioctt,
                    )
                }),
                ioc,
            )
        }
        Statement::WhileLoop(w, pid) => {
            let gen = _k;
            let tmp_var = generate_label_name();
            let cond = SSAValue::VariableReference(tmp_var.clone(), pid);
            let ioc = info.clone();
            let cond_expr = expression_l1_to_l2(
                w.condition,
                Box::new(move |cond| SSAExpression::VariableDecl {
                    name: tmp_var,
                    vtype: None,
                    e1: cond,
                    e2: Box::new(SSAExpression::Noop),
                    pool_id: pid,
                }),
                info,
            );
            SSAExpression::WhileLoop {
                cond,
                block: Box::new(parse_statement_block(w.block, ioc)),
                e2: Box::new(gen(SSAValue::Nothing)),
                pool_id: pid,
                cond_expr: Box::new(cond_expr),
            }
        }
    }
}
