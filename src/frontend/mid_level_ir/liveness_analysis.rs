use super::mir_ast_types::{SSAExpression, SSAValue};

use crate::frontend::mid_level_ir::parsers::generate_register_name;

fn requires_tracking(val: &SSAValue) -> bool {
    match val {
        SSAValue::Tensor(_, _) => true,
        _ => false
    }
}

fn get_alive_vars(val: &SSAValue) -> Vec<String> {
    match val {
        SSAValue::VariableReference(n, _) => vec![n.clone()],
        SSAValue::Phi(_) => todo!(),
        SSAValue::ConditionalBlock {
            if_block: _,
            else_block: _,
            pool_id: _,
        } => vec![],
        SSAValue::Integer {
            value: _,
            width: _,
            pool_id: _,
        } => vec![],
        SSAValue::Float {
            value: _,
            width: _,
            pool_id: _,
        } => vec![],
        SSAValue::Bool(_, _) => vec![],
        SSAValue::Operation {
            lhs: _,
            op: _,
            rhs: _,
            pool_id: _,
        } => todo!(),
        SSAValue::FunctionCall {
            name: _,
            parameters: _,
            pool_id: _,
        } => todo!(),
        SSAValue::Nothing => vec![],
        SSAValue::Tensor(_, _) => vec![],
        SSAValue::Cast {
            value,
            to: _,
            pool_id: _,
        } => get_alive_vars(value),
    }
}

fn drop_dead_vars(mut vars: Vec<String>, e2: SSAExpression) -> SSAExpression {
    if vars.len() == 0 {
        e2
    } else {
        let cvar = vars.pop().unwrap();

        SSAExpression::VariableDecl {
            name: generate_register_name(),
            vtype: None,
            e1: SSAValue::FunctionCall {
                name: "@drop".into(),
                parameters: vec![SSAValue::VariableReference(cvar, 0)],
                pool_id: 0,
            },
            e2: Box::new(drop_dead_vars(vars, e2)),
            pool_id: 0,
        }
    }
}

pub fn unalive_vars(blk: SSAExpression, mut alive_vars: Vec<String>) -> SSAExpression {
    match blk {
        SSAExpression::VariableDecl {
            name,
            vtype,
            e1,
            e2,
            pool_id,
        } => {
            if requires_tracking(&e1) {
                alive_vars.push(name.clone());
            }
            

            let new_cont = unalive_vars(*e2, alive_vars);
            SSAExpression::VariableDecl {
                name,
                vtype,
                e1,
                e2: Box::new(new_cont),
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
            let new_block = unalive_vars(*block, vec![]);
            let new_cont = unalive_vars(*e2, alive_vars);

            SSAExpression::FuncDecl {
                name,
                args,
                ret_type,
                block: Box::new(new_block),
                e2: Box::new(new_cont),
                pool_id,
            }
        }
        SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2,
            pool_id,
        } => {
            let new_cont = unalive_vars(*e2, alive_vars);

            SSAExpression::FuncForwardDecl {
                name,
                args,
                ret_type,
                e2: Box::new(new_cont),
                pool_id,
            }
        }
        SSAExpression::Noop => SSAExpression::Noop,
        SSAExpression::Block(b, pid) => {
            let new_block = unalive_vars(*b, vec![]);

            SSAExpression::Block(Box::new(new_block), pid)
        }
        // Anything returned by return and yield will not die
        SSAExpression::Return { val, pool_id } => {
            let alive = get_alive_vars(&val);
            let to_die: Vec<String> = alive_vars
                .into_iter()
                .filter(|x| !alive.contains(x))
                .collect();

            drop_dead_vars(to_die, SSAExpression::Return { val, pool_id })
        }
        SSAExpression::Yield { val, pool_id } => {
            let alive = get_alive_vars(&val);
            let to_die: Vec<String> = alive_vars
                .into_iter()
                .filter(|x| alive.contains(x))
                .collect();
            drop_dead_vars(to_die, SSAExpression::Yield { val, pool_id })
        }
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
            let new_block = unalive_vars(*block, vec![iv.clone()]);
            let new_cont = unalive_vars(*e2, alive_vars);

            SSAExpression::ForLoop {
                iv,
                from,
                to,
                block: Box::new(new_block),
                parallel,
                e2: Box::new(new_cont),
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
            let new_cont = unalive_vars(*e2, alive_vars.clone());
            let new_cond = unalive_vars(*cond_expr, alive_vars.clone());
            let new_block = unalive_vars(*block, alive_vars);
            SSAExpression::WhileLoop {
                cond,
                cond_expr: Box::new(new_cond),
                block: Box::new(new_block),
                e2: Box::new(new_cont),
                pool_id,
            }
        }
    }
}
