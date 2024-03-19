use std::collections::{HashMap, HashSet};

use crate::frontend::{
    high_level_ir::ast_types::FailureCopy,
    mid_level_ir::mir_ast_types::{Phi, SSALabeledBlock},
};

use super::mir_ast_types::{SSAConditionalBlock, SSAExpression, SSAValue};

fn foldable(val: &SSAValue) -> bool {
    match val {
        SSAValue::VariableReference(_, _) => true,
        SSAValue::Phi(_) => false,
        SSAValue::Integer {
            value: _,
            width: _,
            pool_id,
        } => true,
        SSAValue::Float {
            value: _,
            width: _,
            pool_id,
        } => true,
        SSAValue::Bool(_, _) => true,
        SSAValue::Operation {
            lhs: _,
            op: _,
            rhs: _,
            pool_id,
        } => todo!(),
        SSAValue::FunctionCall {
            name: _,
            parameters: _,
            pool_id,
        } => false,
        SSAValue::Nothing => false,
        SSAValue::Tensor(_, _) => false,
        SSAValue::ConditionalBlock {
            if_block: _,
            else_block: _,
            pool_id,
        } => false,
        SSAValue::Cast {
            value: _,
            to: _,
            pool_id,
        } => true,
    }
}

fn get_referenced_value(val: &SSAValue) -> HashSet<String> {
    match val {
        SSAValue::VariableReference(v, _) => {
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
        SSAValue::Integer {
            value: _,
            width: _,
            pool_id,
        } => HashSet::new(),
        SSAValue::Float {
            value: _,
            width: _,
            pool_id,
        } => HashSet::new(),
        SSAValue::Bool(_, _) => HashSet::new(),
        SSAValue::Operation {
            lhs: _,
            op: _,
            rhs: _,
            pool_id,
        } => todo!(),
        SSAValue::FunctionCall {
            name: _,
            parameters,
            pool_id,
        } => {
            let mut set: HashSet<String> = HashSet::new();
            for i in parameters {
                let subset = get_referenced_value(i);
                set.extend(subset);
            }
            set
        }
        SSAValue::Nothing => HashSet::new(),
        SSAValue::Tensor(v, _) => {
            let mut set: HashSet<String> = HashSet::new();

            for i in v {
                let subset = get_referenced_value(&i);
                set.extend(subset);
            }
            set
        }
        SSAValue::ConditionalBlock {
            if_block,
            else_block,
            pool_id,
        } => {
            let mut hs = get_referenced_value(&if_block.condition);
            hs.extend(get_referenced(&if_block.block.block));
            hs.extend(get_referenced(&else_block.block));

            hs
        }
        SSAValue::Cast {
            value: _,
            to: _,
            pool_id,
        } => todo!(),
    }
}

pub fn get_referenced(expr: &SSAExpression) -> HashSet<String> {
    match expr {
        SSAExpression::VariableDecl {
            name: _,
            vtype: _,
            e1,
            e2,
            pool_id,
        } => {
            let mut hs = get_referenced_value(e1);
            hs.extend(get_referenced(e2));
            hs
        }
        SSAExpression::FuncDecl {
            name: _,
            args: _,
            ret_type: _,
            block,
            e2,
            pool_id,
        } => {
            let mut hs = get_referenced(e2);
            hs.extend(get_referenced(block));
            hs
        }
        SSAExpression::FuncForwardDecl {
            name: _,
            args: _,
            ret_type: _,
            e2,
            pool_id,
        } => get_referenced(e2),
        SSAExpression::Noop => HashSet::new(),
        SSAExpression::Return { val, pool_id } => get_referenced_value(val),
        SSAExpression::Block(b, _) => get_referenced(b),
        SSAExpression::Yield { val, pool_id } => get_referenced_value(val),
        SSAExpression::ForLoop {
            iv,
            from,
            to,
            block,
            parallel,
            e2,
            step,
            pool_id,
        } => {
            let mut hs = get_referenced_value(from);
            hs.extend(get_referenced_value(to));
            hs.extend(get_referenced(block));
            hs.extend(get_referenced(e2));

            hs
        }
        SSAExpression::WhileLoop {
            cond_expr,
            cond,
            block,
            e2,
            pool_id,
        } => {
            let mut hs = get_referenced(cond_expr);
            hs.extend(get_referenced_value(cond));
            hs.extend(get_referenced(block));
            hs.extend(get_referenced(e2));

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
            pool_id,
        } => {
            if !used.contains(&name) {
                remove_unused_variables(*e2, used)
            } else {
                SSAExpression::VariableDecl {
                    name,
                    vtype,
                    e1,
                    e2: Box::new(remove_unused_variables(*e2, used)),
                    pool_id,
                }
            }
        }
        SSAExpression::FuncDecl {
            name,
            args,
            ret_type,
            block,
            e2,
            pool_id,
        } => SSAExpression::FuncDecl {
            name,
            args,
            ret_type,
            block: Box::new(remove_unused_variables(*block, used)),
            e2: Box::new(remove_unused_variables(*e2, used)),
            pool_id,
        },
        SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2,
            pool_id,
        } => SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2: Box::new(remove_unused_variables(*e2, used)),
            pool_id,
        },
        SSAExpression::Noop => SSAExpression::Noop,
        SSAExpression::Return { val, pool_id } => SSAExpression::Return { val, pool_id },
        SSAExpression::Block(b, pid) => {
            SSAExpression::Block(Box::new(remove_unused_variables(*b, used)), pid)
        }
        SSAExpression::Yield { val, pool_id } => SSAExpression::Yield { val, pool_id },
        SSAExpression::ForLoop {
            iv,
            from,
            to,
            block,
            parallel,
            e2,
            step,
            pool_id,
        } => SSAExpression::ForLoop {
            iv,
            from,
            to,
            step,
            block: Box::new(remove_unused_variables(*block, used)),
            parallel,
            e2: Box::new(remove_unused_variables(*e2, used)),
            pool_id,
        },
        SSAExpression::WhileLoop {
            cond_expr,
            cond,
            block,
            e2,
            pool_id,
        } => SSAExpression::WhileLoop {
            cond_expr: Box::new(remove_unused_variables(*cond_expr, used)),
            cond,
            block: Box::new(remove_unused_variables(*block, used)),
            e2: Box::new(remove_unused_variables(*e2, used)),
            pool_id,
        },
    }
}

fn mir_val_variable_fold(val: SSAValue, env: &mut HashMap<String, SSAValue>) -> SSAValue {
    match val.fcopy() {
        SSAValue::VariableReference(r, _) => match env.get(&r) {
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
        SSAValue::Integer {
            value,
            width,
            pool_id,
        } => SSAValue::Integer {
            value,
            width,
            pool_id,
        },
        SSAValue::Float {
            value,
            width,
            pool_id,
        } => SSAValue::Float {
            value,
            width,
            pool_id,
        },
        SSAValue::Bool(b, p) => SSAValue::Bool(b, p),
        SSAValue::FunctionCall {
            name,
            parameters,
            pool_id,
        } => SSAValue::FunctionCall {
            name,
            parameters: parameters
                .into_iter()
                .map(|x| mir_val_variable_fold(x, env))
                .collect(),
            pool_id,
        },
        SSAValue::Nothing => SSAValue::Nothing,
        SSAValue::Tensor(v, p) => SSAValue::Tensor(
            v.into_iter()
                .map(|x| mir_val_variable_fold(x, env))
                .collect(),
            p,
        ),
        SSAValue::ConditionalBlock {
            if_block,
            else_block,
            pool_id,
        } => {
            // let ev1: HashMap<String, SSAValue> =
            //     env.iter().map(|(x, y)| (x.clone(), y.fcopy())).collect();
            // let ev2: HashMap<String, SSAValue> =
            //     env.iter().map(|(x, y)| (x.clone(), y.fcopy())).collect();

            let (op_if_block, mut env) = mir_variable_fold(*if_block.block.block, env.clone());
            let op_if = SSAConditionalBlock {
                condition: Box::new(mir_val_variable_fold(*if_block.condition, &mut env)),
                block: SSALabeledBlock {
                    label: if_block.block.label,
                    block: Box::new(op_if_block),
                },
            };

            let (op_else_block, _env) = mir_variable_fold(*else_block.block, env);
            let op_else = SSALabeledBlock {
                label: else_block.label,
                block: Box::new(op_else_block),
            };

            SSAValue::ConditionalBlock {
                if_block: op_if,
                else_block: op_else,
                pool_id,
            }
        }
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
            pool_id,
        } => {
            let optimised_val = mir_val_variable_fold(e1, &mut env);
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
                    pool_id,
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
            pool_id,
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
                    pool_id,
                },
                env,
            )
        }
        SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2,
            pool_id,
        } => {
            let (op, env) = mir_variable_fold(*e2, env);
            (
                SSAExpression::FuncForwardDecl {
                    name,
                    args,
                    ret_type,
                    e2: Box::new(op),
                    pool_id,
                },
                env,
            )
        }
        SSAExpression::Noop => (SSAExpression::Noop, env),
        SSAExpression::Return { val, pool_id } => (
            SSAExpression::Return {
                val: mir_val_variable_fold(val, &mut env),
                pool_id,
            },
            env,
        ),
        SSAExpression::Block(b, _) => mir_variable_fold(*b, env),
        SSAExpression::Yield { val, pool_id } => (
            SSAExpression::Yield {
                val: mir_val_variable_fold(val, &mut env),
                pool_id,
            },
            env,
        ),
        SSAExpression::ForLoop {
            iv,
            from,
            to,
            block,
            parallel,
            e2,
            step,
            pool_id,
        } => {
            let env_cpy1 = env.iter().map(|(n, v)| (n.clone(), v.fcopy()));
            let env_cpy2 = env.iter().map(|(n, v)| (n.clone(), v.fcopy()));

            let (op_block, _env) = mir_variable_fold(*block, env_cpy1.collect());
            // COPY ENV HERE
            let (op_e2, env_rst) = mir_variable_fold(*e2, env_cpy2.collect());

            (
                SSAExpression::ForLoop {
                    iv,
                    from: mir_val_variable_fold(from, &mut env),
                    to: mir_val_variable_fold(to, &mut env),
                    block: Box::new(op_block),
                    parallel,
                    e2: Box::new(op_e2),
                    step,
                    pool_id,
                },
                env_rst,
            )
        }
        SSAExpression::WhileLoop {
            cond_expr,
            cond,
            block,
            e2,
            pool_id,
        } => {
            let env_cpy1 = env.iter().map(|(n, v)| (n.clone(), v.fcopy()));
            let env_cpy2 = env.iter().map(|(n, v)| (n.clone(), v.fcopy()));
            let env_cpy3 = env.iter().map(|(n, v)| (n.clone(), v.fcopy()));

            let (op_cond, _env) = mir_variable_fold(*cond_expr, env_cpy1.collect());
            let (op_block, _env) = mir_variable_fold(*block, env_cpy2.collect());
            let (op_rst, _env) = mir_variable_fold(*e2, env_cpy3.collect());

            (
                SSAExpression::WhileLoop {
                    cond_expr: Box::new(op_cond),
                    cond: mir_val_variable_fold(cond, &mut env),
                    block: Box::new(op_block),
                    e2: Box::new(op_rst),
                    pool_id: pool_id,
                },
                _env,
            )
        }
    }
}
