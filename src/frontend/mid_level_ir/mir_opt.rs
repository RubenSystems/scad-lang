use std::collections::{HashMap, HashSet};

use crate::frontend::{
    high_level_ir::ast_types::FailureCopy,
    mid_level_ir::mir_ast_types::{Phi, SSALabeledBlock},
};

use super::mir_ast_types::{SSAConditionalBlock, SSAExpression, SSAValue};

fn foldable(val: &SSAValue) -> bool {
    match val {
        SSAValue::RegisterReference(_) => true,
        SSAValue::Phi(_) => false,
        SSAValue::Integer(_) => true,
        SSAValue::Float(_) => true,
        SSAValue::Bool(_) => true,
        SSAValue::Operation {
            lhs: _,
            op: _,
            rhs: _,
        } => todo!(),
        SSAValue::FunctionCall {
            name: _,
            parameters: _,
        } => false,
        SSAValue::Nothing => false,
    }
}

fn get_referenced_value(val: &SSAValue) -> HashSet<String> {
    match val {
        SSAValue::RegisterReference(v) => {
            let mut new = HashSet::new();
            new.insert(v.clone());
            new
        }
        SSAValue::Phi(p) => {
            let mut set: HashSet<String> = HashSet::new();

            for i in p {
                let subset = get_referenced_value(&i.value);
                set.extend(subset);
            }
            set
        }
        SSAValue::Integer(_) => HashSet::new(),
        SSAValue::Float(_) => HashSet::new(),
        SSAValue::Bool(_) => HashSet::new(),
        SSAValue::Operation { lhs, op, rhs } => todo!(),
        SSAValue::FunctionCall { name, parameters } => {
            let mut set: HashSet<String> = HashSet::new();
            for i in parameters {
                let subset = get_referenced_value(&i);
                set.extend(subset);
            }
            set
        }
        SSAValue::Nothing => HashSet::new(),
    }
}

pub fn get_referenced(expr: &SSAExpression) -> HashSet<String> {
    match expr {
        SSAExpression::VariableDecl {
            name,
            vtype,
            e1,
            e2,
        } => {
            let mut hs = get_referenced_value(e1);
            hs.extend(get_referenced(e2));
            hs
        }
        SSAExpression::FuncDecl {
            name,
            args,
            ret_type,
            block,
            e2,
        } => {
            let mut hs = get_referenced(e2);
            hs.extend(get_referenced(&block));
            hs
        }
        SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2,
        } => get_referenced(e2),
        SSAExpression::Noop => HashSet::new(),
        SSAExpression::Return { val } => get_referenced_value(val),
        SSAExpression::Block(b) => get_referenced(b),
        SSAExpression::ConditionalBlock {
            if_block,
            else_block,
            e2,
        } => {
            let mut hs = get_referenced_value(&if_block.condition);
            hs.extend(get_referenced(&if_block.block.block));
            hs.extend(get_referenced(&else_block.block));
            hs.extend(get_referenced(&e2));

            hs
        }
    }
}

pub fn remove_unused_variables(expr: SSAExpression, used: &HashSet<String>) -> SSAExpression {
    match expr {
        SSAExpression::VariableDecl {
            name,
            vtype,
            e1,
            e2,
        } => {
            if !used.contains(&name) {
                remove_unused_variables(*e2, used)
            } else {
                SSAExpression::VariableDecl {
                    name,
                    vtype,
                    e1,
                    e2: Box::new(remove_unused_variables(*e2, used)),
                }
            }
        }
        SSAExpression::FuncDecl {
            name,
            args,
            ret_type,
            block,
            e2,
        } => SSAExpression::FuncDecl {
            name,
            args,
            ret_type,
            block: Box::new(remove_unused_variables(*block, &used)),
            e2: Box::new(remove_unused_variables(*e2, &used)),
        },
        SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2,
        } => SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2: Box::new(remove_unused_variables(*e2, used)),
        },
        SSAExpression::Noop => SSAExpression::Noop,
        SSAExpression::Return { val } => SSAExpression::Return { val },
        SSAExpression::Block(b) => {
            SSAExpression::Block(Box::new(remove_unused_variables(*b, used)))
        }
        SSAExpression::ConditionalBlock {
            if_block,
            else_block,
            e2,
        } => SSAExpression::ConditionalBlock {
            if_block: SSAConditionalBlock {
                condition: if_block.condition,
                block: SSALabeledBlock {
                    label: if_block.block.label,
                    block: Box::new(remove_unused_variables(*if_block.block.block, used)),
                },
            },
            else_block: SSALabeledBlock {
                label: else_block.label,
                block: Box::new(remove_unused_variables(*else_block.block, used)),
            },
            e2: Box::new(remove_unused_variables(*e2, used)),
        },
    }
}

fn mir_val_variable_fold(val: SSAValue, env: &HashMap<String, SSAValue>) -> SSAValue {
    match val.fcopy() {
        SSAValue::RegisterReference(r) => match env.get(&r) {
            Some(nval) => nval.fcopy(),
            None => val,
        },
        SSAValue::Phi(p) => SSAValue::Phi(
            p.into_iter()
                .map(|x| Phi {
                    branch_name: x.branch_name,
                    value: mir_val_variable_fold(x.value, env),
                })
                .collect(),
        ),
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
        SSAExpression::VariableDecl {
            name,
            vtype,
            e1,
            e2,
        } => {
            let optimised_val = mir_val_variable_fold(e1, &env);
            if foldable(&optimised_val) {
                env.insert(name.clone(), optimised_val.fcopy());
            }

            let (optimised_rest, env) = mir_variable_fold(*e2, env);

            (
                SSAExpression::VariableDecl {
                    name,
                    vtype,
                    e1: optimised_val,
                    e2: Box::new(optimised_rest),
                },
                env,
            )
        }
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
    }
}
