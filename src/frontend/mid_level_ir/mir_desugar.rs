//===----------------------------------------------------------------------===//
///
/// Desugaring involves lexical scoping and giving each variable 
/// reassingment a fresh name. 
/// 
/// Lexical scoping involves giving varibles in different scopes 
/// which can access the variables in outer scopes a fresh name 
/// 
///     e.g 
///         let x = 1; 
///         if something 
///             let x = 2; 
///             print(x)
/// 
///     will be converted to
///         let x = 1; 
///         %label if something 
///             let x.label = 2; 
///             print(x.label)
/// 
/// giving each variable assignment a fresh name is to preserve SSA 
/// for example 
/// 
///     let x = 1; 
///     print(x)
///     x = 2; 
///     print(x)
/// 
/// the above is not SSA. to convert it into SSA, it must be rewritten to 
/// 
///     let x.0 = 1; 
///     print(x.0)
///     let x.1 = 2; 
///     print(x.1)
/// 
/// All references to x now have to be rewritten to ensure that they refere
/// to the correct x
///
//===----------------------------------------------------------------------===//


use std::collections::{HashMap, HashSet};

use super::{
    mir_ast_types::{Phi, SSAConditionalBlock, SSAExpression, SSALabeledBlock, SSAValue},
    parsers::generate_label_name,
};

fn scoped_rename(existing_name: &str, scoped_name: &Vec<String>) -> String {
    let sn = scoped_name.join(".");
    format!("{sn}.{existing_name}")
}


/*
    ========================================
            Variable reassignment 
    ========================================

*/
pub fn rename_variable_reassignment_value(
    val: SSAValue,
    tracker: &mut HashMap<String, i32>,
) -> SSAValue {
    match val {
        SSAValue::VariableReference(v, pid) => SSAValue::VariableReference(
            match tracker.get(&v) {
                Some(x) => format!("{v}.{x}"),
                None => format!("{v}.0"),
            },
            pid,
        ),
        SSAValue::Phi(p) => SSAValue::Phi(
            p.into_iter()
                .map(|x| Phi {
                    branch_name: x.branch_name,
                    value: rename_variable_reassignment_value(x.value, tracker),
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
        SSAValue::Operation {
            lhs: _,
            op: _,
            rhs: _,
            pool_id: _,
        } => todo!(),
        SSAValue::FunctionCall {
            name,
            parameters,
            pool_id,
        } => SSAValue::FunctionCall {
            name,
            parameters: parameters
                .into_iter()
                .map(|x| rename_variable_reassignment_value(x, tracker))
                .collect(),
            pool_id,
        },
        SSAValue::Nothing => SSAValue::Nothing,
        SSAValue::Tensor(v, pid) => SSAValue::Tensor(
            v.into_iter()
                .map(|x| rename_variable_reassignment_value(x, tracker))
                .collect(),
            pid,
        ),
        SSAValue::ConditionalBlock {
            if_block,
            else_block,
            pool_id,
        } => {
            let inner_if_block = SSALabeledBlock {
                label: if_block.block.label,
                block: Box::new(rename_variable_reassignment(*if_block.block.block, tracker)),
            };
            let if_block = SSAConditionalBlock {
                condition: Box::new(rename_variable_reassignment_value(
                    *if_block.condition,
                    tracker,
                )),
                block: inner_if_block,
            };
            let else_block = SSALabeledBlock {
                label: else_block.label,
                block: Box::new(rename_variable_reassignment(*else_block.block, tracker)),
            };

            SSAValue::ConditionalBlock {
                if_block,
                else_block,
                pool_id,
            }
        }
        SSAValue::Cast { value, to, pool_id } => SSAValue::Cast {
            value: Box::new(rename_variable_reassignment_value(*value, tracker)),
            to,
            pool_id,
        },
    }
}

pub fn rename_variable_reassignment(
    expr: SSAExpression,
    tracker: &mut HashMap<String, i32>,
) -> SSAExpression {
    match expr {
        SSAExpression::VariableDecl {
            name,
            vtype,
            e1,
            e2,
            pool_id,
        } => {
            let e1 = rename_variable_reassignment_value(e1, tracker);
            let counter = *tracker.get(&name).unwrap_or(&-1) + 1;
            tracker.insert(name.clone(), counter);

            SSAExpression::VariableDecl {
                name: format!("{name}.{counter}"),
                vtype,
                e1,
                e2: Box::new(rename_variable_reassignment(*e2, tracker)),
                pool_id,
            }
        }
        SSAExpression::FuncDecl {
            name,
            args,
            ret_type,
            block,
            e2,
            pool_id,
        } => {
            args.iter().for_each(|(name, _)| {
                tracker.insert(name.clone(), 0);
            });

            SSAExpression::FuncDecl {
                name,
                args: args
                    .into_iter()
                    .map(|(n, tpe)| (format!("{n}.{}", tracker.get(&n).unwrap()), tpe))
                    .collect(),
                ret_type,
                block: Box::new(rename_variable_reassignment(*block, tracker)),
                e2: Box::new(rename_variable_reassignment(*e2, tracker)),
                pool_id,
            }
        }
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
            e2: Box::new(rename_variable_reassignment(*e2, tracker)),
            pool_id,
        },
        SSAExpression::Noop => SSAExpression::Noop,
        SSAExpression::Return { val, pool_id } => SSAExpression::Return {
            val: rename_variable_reassignment_value(val, tracker),
            pool_id,
        },
        SSAExpression::Block(b, pid) => {
            SSAExpression::Block(Box::new(rename_variable_reassignment(*b, tracker)), pid)
        }
        SSAExpression::Yield { val, pool_id } => SSAExpression::Yield {
            val: rename_variable_reassignment_value(val, tracker),
            pool_id,
        },
        SSAExpression::ForLoop {
            iv,
            from,
            to,
            block,
            parallel,
            e2,
            pool_id,
            step,
        } => {
            let from = rename_variable_reassignment_value(from, tracker);
            let to = rename_variable_reassignment_value(to, tracker);
            let counter = *tracker.get(&iv).unwrap_or(&-1) + 1;
            tracker.insert(iv.clone(), counter);
            let new_block = rename_variable_reassignment(*block, tracker);
            tracker.remove(&iv);

            SSAExpression::ForLoop {
                iv: format!("{iv}.0"),
                from,
                to,
                block: Box::new(new_block),
                parallel,
                e2: Box::new(rename_variable_reassignment(*e2, tracker)),
                pool_id,
                step,
            }
        }
        SSAExpression::WhileLoop {
            cond,
            block,
            e2,
            pool_id,
            cond_expr,
        } => SSAExpression::WhileLoop {
            cond: rename_variable_reassignment_value(cond, tracker),
            block: Box::new(rename_variable_reassignment(*block, tracker)),
            e2: Box::new(rename_variable_reassignment(*e2, tracker)),
            cond_expr: Box::new(rename_variable_reassignment(*cond_expr, tracker)),
            pool_id,
        },
    }
}



/*
    ========================================
            Lexical scoping 
    ========================================

*/
pub fn rename_variables_value(
    value: SSAValue,
    mut scoped_name: Vec<String>,
    used_vars: &mut HashSet<String>,
) -> SSAValue {
    match value {
        SSAValue::VariableReference(name, pid) => {
            let saved_scoped_name = scoped_name.clone();
            loop {
                if scoped_name.is_empty() {
                    // unreachable!("Variable {name} not found in any scope")
                    scoped_name = saved_scoped_name;
                    break;
                }
                let new_scoped_name = scoped_rename(&name, &scoped_name);
                if used_vars.contains(&new_scoped_name) {
                    break;
                } else {
                    scoped_name.pop();
                }
            }

            SSAValue::VariableReference(scoped_rename(&name, &scoped_name), pid)
        }
        SSAValue::Phi(p) => SSAValue::Phi(
            p.into_iter()
                .map(|x| {
                    let mut new_scope_name = scoped_name.clone();
                    new_scope_name.push(x.branch_name);
                    Phi {
                        branch_name: scoped_rename(new_scope_name.last().unwrap(), &scoped_name),
                        value: rename_variables_value(x.value, new_scope_name, used_vars),
                    }
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
        SSAValue::Operation {
            lhs: _,
            op: _,
            rhs: _,
            pool_id: _,
        } => todo!(),
        SSAValue::FunctionCall {
            name,
            parameters,
            pool_id,
        } => SSAValue::FunctionCall {
            name,
            parameters: parameters
                .into_iter()
                .map(|v| rename_variables_value(v, scoped_name.clone(), used_vars))
                .collect(),
            pool_id,
        },
        SSAValue::Nothing => SSAValue::Nothing,
        SSAValue::Tensor(v, p) => SSAValue::Tensor(
            v.into_iter()
                .map(|v| rename_variables_value(v, scoped_name.clone(), used_vars))
                .collect(),
            p,
        ),
        SSAValue::ConditionalBlock {
            if_block,
            else_block,
            pool_id,
        } => {
            scoped_name.push(if_block.block.label.clone());
            let if_block_expr =
                rename_variables(*if_block.block.block, scoped_name.clone(), used_vars);
            let if_block_label = scoped_name.pop().unwrap();
            scoped_name.push(else_block.label);

            let else_block_expr =
                rename_variables(*else_block.block, scoped_name.clone(), used_vars);
            let else_block_label = scoped_name.pop().unwrap();

            SSAValue::ConditionalBlock {
                if_block: SSAConditionalBlock {
                    condition: Box::new(rename_variables_value(
                        *if_block.condition,
                        scoped_name.clone(),
                        used_vars,
                    )),
                    block: SSALabeledBlock {
                        label: scoped_rename(&if_block_label, &scoped_name),
                        block: Box::new(if_block_expr),
                    },
                },
                else_block: SSALabeledBlock {
                    label: scoped_rename(&else_block_label, &scoped_name),
                    block: Box::new(else_block_expr),
                },
                pool_id,
            }
        }
        SSAValue::Cast { value, to, pool_id } => SSAValue::Cast {
            value: Box::new(rename_variables_value(*value, scoped_name, used_vars)),
            to,
            pool_id,
        },
    }
}

// Rename based on scope
pub fn rename_variables(
    expression: SSAExpression,
    mut scoped_name: Vec<String>,
    used_vars: &mut HashSet<String>,
) -> SSAExpression {
    match expression {
        SSAExpression::VariableDecl {
            name,
            vtype,
            e1,
            e2,
            pool_id,
        } => {
            let new_name = scoped_rename(&name, &scoped_name);
            used_vars.insert(new_name);
            SSAExpression::VariableDecl {
                name: scoped_rename(&name, &scoped_name),
                vtype,
                e1: rename_variables_value(e1, scoped_name.clone(), used_vars),
                e2: Box::new(rename_variables(*e2, scoped_name, used_vars)),
                pool_id,
            }
        }
        SSAExpression::FuncDecl {
            name,
            args,
            ret_type,
            block,
            e2,
            pool_id,
        } => {
            scoped_name.push(name.clone());
            let args: Vec<(String, crate::frontend::high_level_ir::ast_types::Type)> = args
                .into_iter()
                .map(|(name, tpe)| (scoped_rename(&name, &scoped_name), tpe))
                .collect();

            args.iter().for_each(|(name, _)| {
                used_vars.insert(name.clone());
            });
            let block = rename_variables(*block, scoped_name.clone(), used_vars);
            args.iter().for_each(|(name, _)| {
                used_vars.remove(name);
            });
            scoped_name.pop();

            SSAExpression::FuncDecl {
                block: Box::new(block),
                name,
                args,
                e2: Box::new(rename_variables(*e2, scoped_name, used_vars)),
                ret_type,
                pool_id,
            }
        }
        SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2,
            pool_id,
        } => SSAExpression::FuncForwardDecl {
            name,
            args: args
                .into_iter()
                .map(|(name, tpe)| (scoped_rename(&name, &scoped_name), tpe))
                .collect(),
            e2: Box::new(rename_variables(*e2, scoped_name, used_vars)),
            ret_type,
            pool_id,
        },
        SSAExpression::Noop => SSAExpression::Noop,
        SSAExpression::Return { val, pool_id } => SSAExpression::Return {
            val: rename_variables_value(val, scoped_name, used_vars),
            pool_id,
        },
        SSAExpression::Block(b, p) => {
            SSAExpression::Block(Box::new(rename_variables(*b, scoped_name, used_vars)), p)
        }
        SSAExpression::Yield { val, pool_id } => SSAExpression::Yield {
            val: rename_variables_value(val, scoped_name, used_vars),
            pool_id,
        },
        SSAExpression::ForLoop {
            iv,
            from,
            to,
            block,
            parallel,
            e2,
            pool_id,
            step,
        } => {
            let from = rename_variables_value(from, scoped_name.clone(), used_vars);
            let to = rename_variables_value(to, scoped_name.clone(), used_vars);
            let e2 = Box::new(rename_variables(*e2, scoped_name.clone(), used_vars));
            scoped_name.push(generate_label_name());
            let new_iv = scoped_rename(&iv, &scoped_name);
            used_vars.insert(new_iv.clone());
            let block = Box::new(rename_variables(*block, scoped_name.clone(), used_vars));
            SSAExpression::ForLoop {
                iv: new_iv,
                from,
                to,
                block,
                parallel,
                e2,
                pool_id,
                step,
            }
        }
        SSAExpression::WhileLoop {
            cond,
            block,
            e2,
            pool_id,
            cond_expr,
        } => {
            let e2 = Box::new(rename_variables(*e2, scoped_name.clone(), used_vars));
            let cond_expr = Box::new(rename_variables(*cond_expr, scoped_name.clone(), used_vars));
            scoped_name.push(generate_label_name());
            let block = Box::new(rename_variables(*block, scoped_name.clone(), used_vars));
            SSAExpression::WhileLoop {
                cond: rename_variables_value(cond, scoped_name, used_vars),
                block,
                e2,
                pool_id,
                cond_expr,
            }
        }
    }
}
