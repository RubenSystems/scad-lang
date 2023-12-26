use std::collections::HashMap;

use crate::frontend::{
    high_level_ir::ast_types::FailureCopy,
    mid_level_ir::mir_ast_types::{Phi, SSALabeledBlock},
};

use super::mir_ast_types::{SSAConditionalBlock, SSAExpression, SSAValue};

fn mir_val_variable_fold(val: SSAValue, env: &HashMap<String, SSAValue>) -> SSAValue {
    match val.fcopy() {
        SSAValue::RegisterReference(r) => match env.get(&r) {
            Some(nval) => nval.fcopy(),
            None => val,
        },
        SSAValue::VariableDereference(v) => match env.get(&v) {
            Some(nval) => nval.fcopy(),
            None => val,
        },
        SSAValue::Phi(p) => {
            println!("HI!{env:#?}");
            println!("YO!{p:#?}");
            SSAValue::Phi(
                p.into_iter()
                    .map(|x| Phi {
                        branch_name: x.branch_name,
                        value: mir_val_variable_fold(x.value, env),
                    })
                    .collect(),
            )
        }
        SSAValue::Integer(i) => SSAValue::Integer(i),
        SSAValue::Float(f) => SSAValue::Float(f),
        SSAValue::Bool(b) => SSAValue::Bool(b),
        SSAValue::FunctionCall { name, parameters } => SSAValue::FunctionCall {
            name,
            parameters: parameters
                .into_iter()
                .map(|x| mir_val_variable_fold(x, env))
                .collect(),
        },
        SSAValue::Nothing => SSAValue::Nothing,
        _ => todo!(),
    }
}

pub fn mir_variable_fold(
    expr: SSAExpression,
    mut env: HashMap<String, SSAValue>,
) -> (SSAExpression, HashMap<String, SSAValue>) {
    match expr {
        SSAExpression::RegisterDecl {
            name,
            vtype,
            e1,
            e2,
        } => {
            let optimised_val = mir_val_variable_fold(e1, &env);
            env.insert(name.clone(), optimised_val.fcopy());

            let (optimised_rest, env) = mir_variable_fold(*e2, env);

            (
                SSAExpression::RegisterDecl {
                    name,
                    vtype,
                    e1: optimised_val,
                    e2: Box::new(optimised_rest),
                },
                env,
            )
        }
        SSAExpression::VariableDecl {
            name: _,
            vtype: _,
            e1: _,
            e2: _,
        } => todo!(),
        SSAExpression::ConstDecl {
            name: _,
            vtype: _,
            e1: _,
            e2: _,
        } => todo!(),
        SSAExpression::FuncDecl {
            name,
            args,
            ret_type,
            block,
            e2,
        } => {
            let env_cpy1 = env.iter().map(|(n, v)| (n.clone(), v.fcopy()));
            let env_cpy2 = env.iter().map(|(n, v)| (n.clone(), v.fcopy()));
            let (op_block, _env) = mir_variable_fold(*block, env_cpy1.collect());
            // COPY ENV HERE
            let (op_e2, env) = mir_variable_fold(*e2, env_cpy2.collect());

            (
                SSAExpression::FuncDecl {
                    name,
                    args,
                    ret_type,
                    block: Box::new(op_block),
                    e2: Box::new(op_e2),
                },
                env,
            )
        }
        SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2,
        } => {
            let (op, env) = mir_variable_fold(*e2, env);
            (
                SSAExpression::FuncForwardDecl {
                    name,
                    args,
                    ret_type,
                    e2: Box::new(op),
                },
                env,
            )
        }
        SSAExpression::Noop => (SSAExpression::Noop, env),
        SSAExpression::Return { val } => (
            SSAExpression::Return {
                val: mir_val_variable_fold(val, &env),
            },
            env,
        ),
        SSAExpression::VariableReference {
            name: _,
            tmp_name: _,
            e2: _,
        } => todo!(),
        SSAExpression::Block(b) => mir_variable_fold(*b, env),
        SSAExpression::ConditionalBlock {
            if_block,
            else_block,
            e2,
        } => {
            // let ev1: HashMap<String, SSAValue> =
            //     env.iter().map(|(x, y)| (x.clone(), y.fcopy())).collect();
            // let ev2: HashMap<String, SSAValue> =
            //     env.iter().map(|(x, y)| (x.clone(), y.fcopy())).collect();

            let (op_if_block, env) = mir_variable_fold(*if_block.block.block, env);
            let op_if = SSAConditionalBlock {
                condition: mir_val_variable_fold(if_block.condition, &env),
                block: SSALabeledBlock {
                    label: if_block.block.label,
                    block: Box::new(op_if_block),
                },
            };

            let (op_else_block, env) = mir_variable_fold(*else_block.block, env);
            let op_else = SSALabeledBlock {
                label: else_block.label,
                block: Box::new(op_else_block),
            };

            let (e2, env) = mir_variable_fold(*e2, env);

            (
                SSAExpression::ConditionalBlock {
                    if_block: op_if,
                    else_block: op_else,
                    e2: Box::new(e2),
                },
                env,
            )
        }
        SSAExpression::Conditional(_) => todo!(),
    }
}
