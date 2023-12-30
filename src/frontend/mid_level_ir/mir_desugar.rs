use std::collections::HashSet;

use super::mir_ast_types::{Phi, SSAConditionalBlock, SSAExpression, SSALabeledBlock, SSAValue};

fn scoped_rename(existing_name: &str, scoped_name: &Vec<String>) -> String {
    let sn = scoped_name.join(".");
    format!("{sn}.{existing_name}")
}

pub fn rename_variables_value(value: SSAValue, mut scoped_name: Vec<String>, used_vars: &HashSet<String>) -> SSAValue {
    match value {
        SSAValue::VariableReference(name) => {
            let saved_scoped_name = scoped_name.clone();
            loop {
                println!("{scoped_name:#?}");
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


            SSAValue::VariableReference(scoped_rename(&name, &scoped_name))
        }
        SSAValue::Phi(p) => SSAValue::Phi(
            p.into_iter()
                .map(|x| {
                    let mut new_scope_name = scoped_name.clone();
                    new_scope_name.push(x.branch_name);
                    Phi {
                        branch_name: scoped_rename(&new_scope_name.last().unwrap(), &scoped_name),
                        value: rename_variables_value(x.value, new_scope_name, used_vars),
                    }
                })
                .collect(),
        ),
        SSAValue::Integer(i) => SSAValue::Integer(i),
        SSAValue::Float(f) => SSAValue::Float(f),
        SSAValue::Bool(b) => SSAValue::Bool(b),
        SSAValue::Operation {
            lhs: _,
            op: _,
            rhs: _,
        } => todo!(),
        SSAValue::FunctionCall { name, parameters } => SSAValue::FunctionCall {
            name,
            parameters: parameters
                .into_iter()
                .map(|v| rename_variables_value(v, scoped_name.clone(), used_vars))
                .collect(),
        },
        SSAValue::Nothing => SSAValue::Nothing,
    }
}

pub fn rename_variables(
    expression: SSAExpression,
    mut scoped_name: Vec<String>,
    mut used_vars: HashSet<String>,
) -> SSAExpression {
    match expression {
        SSAExpression::VariableDecl {
            name,
            vtype,
            e1,
            e2,
        } => {
            let new_name = scoped_rename(&name, &scoped_name);
            used_vars.insert(new_name);
            SSAExpression::VariableDecl {
                name: scoped_rename(&name, &scoped_name),
                vtype,
                e1: rename_variables_value(e1, scoped_name.clone(), &used_vars),
                e2: Box::new(rename_variables(*e2, scoped_name, used_vars)),
            }
        }
        SSAExpression::FuncDecl {
            name,
            args,
            ret_type,
            block,
            e2,
        } => {
            scoped_name.push(name.clone());
            let args: Vec<(String, crate::frontend::high_level_ir::ast_types::Type)> = args
                .into_iter()
                .map(|(name, tpe)| (scoped_rename(&name, &scoped_name), tpe))
                .collect();

            args.iter().for_each(|(name, _)| {
                used_vars.insert(name.clone());
            });
            let block = rename_variables(*block, scoped_name.clone(), used_vars.clone());
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
            }
        }
        SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2,
        } => SSAExpression::FuncForwardDecl {
            name,
            args: args
                .into_iter()
                .map(|(name, tpe)| (scoped_rename(&name, &scoped_name), tpe))
                .collect(),
            e2: Box::new(rename_variables(*e2, scoped_name, used_vars)),
            ret_type,
        },
        SSAExpression::Noop => SSAExpression::Noop,
        SSAExpression::Return { val } => SSAExpression::Return {
            val: rename_variables_value(val, scoped_name, &used_vars),
        },
        SSAExpression::Block(b) => {
            SSAExpression::Block(Box::new(rename_variables(*b, scoped_name, used_vars)))
        }
        SSAExpression::ConditionalBlock {
            if_block,
            else_block,
            e2,
        } => {
            scoped_name.push(if_block.block.label.clone());
            let if_block_expr = rename_variables(*if_block.block.block, scoped_name.clone(), used_vars.clone());
            let if_block_label = scoped_name.pop().unwrap();
            scoped_name.push(else_block.label);

            let else_block_expr = rename_variables(*else_block.block, scoped_name.clone(), used_vars.clone());
            let else_block_label = scoped_name.pop().unwrap();

            SSAExpression::ConditionalBlock {
                if_block: SSAConditionalBlock {
                    condition: rename_variables_value(if_block.condition, scoped_name.clone(), &used_vars),
                    block: SSALabeledBlock {
                        label: scoped_rename(&if_block_label, &scoped_name),
                        block: Box::new(if_block_expr),
                    },
                },
                else_block: SSALabeledBlock {
                    label: scoped_rename(&else_block_label, &scoped_name),
                    block: Box::new(else_block_expr),
                },
                e2: Box::new(rename_variables(*e2, scoped_name, used_vars)),
            }
        }
    }
}
