

use super::mir_ast_types::{SSAExpression, SSAValue};

use crate::frontend::{
    mid_level_ir::parsers::generate_register_name,
    type_system::{
        context::Context,
        tir_types::{MonoType, TIRType},
        type_engine::instantiate,
    },
};

fn get_alive_vars(val: &SSAValue) -> Vec<String> {
    match val {
        SSAValue::VariableReference(n) => vec![n.clone()],
        SSAValue::Phi(_) => todo!(),
        SSAValue::ConditionalBlock {
            if_block: _,
            else_block: _,
        } => vec![],
        SSAValue::Integer(_) => vec![],
        SSAValue::Float(_) => vec![],
        SSAValue::Bool(_) => vec![],
        SSAValue::Operation {
            lhs: _,
            op: _,
            rhs: _,
        } => todo!(),
        SSAValue::FunctionCall {
            name: _,
            parameters: _,
        } => todo!(),
        SSAValue::Nothing => vec![],
        SSAValue::Tensor(_) => vec![],
    }
}

fn drop_dead_vars(mut vars: Vec<String>, e2: SSAExpression, context: &Context) -> SSAExpression {
    if vars.len() == 0 {
        e2
    } else {
        let cvar = vars.pop().unwrap();
        let tpe = context
            .get_type_for_name(&cvar)
            .expect(&format!("Can't drop unknown type for {cvar}"))
            .first()
            .unwrap();

        // let TIRType::MonoType(tpe) = tpe else {
        //     unreachable!("Can't work with polytypes")
        // };
        let tpe = match tpe.clone() {
            TIRType::MonoType(m) => m,
            TIRType::PolyType(p) => instantiate(TIRType::PolyType(p)),
            TIRType::ForwardDecleration(_) => todo!(),
        };

        let MonoType::Application {
            c,
            dimensions,
            types: _,
        } = tpe
        else {
            unreachable!("Untyped drop unavailable");
        };
        let drop_extension = format!("{}{}", if dimensions.is_none() { "" } else { "tensor" }, c);

        SSAExpression::VariableDecl {
            name: generate_register_name(),
            vtype: None,
            e1: SSAValue::FunctionCall {
                name: format!("@drop.{drop_extension}"),
                parameters: vec![SSAValue::VariableReference(cvar)],
            },
            e2: Box::new(drop_dead_vars(vars, e2, context)),
        }
    }
}

pub fn unalive_vars(
    blk: SSAExpression,
    mut alive_vars: Vec<String>,
    ctx: &Context,
) -> SSAExpression {
    match blk {
        SSAExpression::VariableDecl {
            name,
            vtype,
            e1,
            e2,
        } => {
            alive_vars.push(name.clone());

            let new_cont = unalive_vars(*e2, alive_vars, ctx);
            SSAExpression::VariableDecl {
                name,
                vtype,
                e1,
                e2: Box::new(new_cont),
            }
        }
        SSAExpression::FuncDecl {
            name,
            args,
            ret_type,
            block,
            e2,
        } => {
            let new_block = unalive_vars(*block, vec![], ctx);
            let new_cont = unalive_vars(*e2, alive_vars, ctx);

            SSAExpression::FuncDecl {
                name,
                args,
                ret_type,
                block: Box::new(new_block),
                e2: Box::new(new_cont),
            }
        }
        SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2,
        } => {
            let new_cont = unalive_vars(*e2, alive_vars, ctx);

            SSAExpression::FuncForwardDecl {
                name,
                args,
                ret_type,
                e2: Box::new(new_cont),
            }
        }
        SSAExpression::Noop => SSAExpression::Noop,
        SSAExpression::Block(b) => {
            let new_block = unalive_vars(*b, vec![], ctx);

            SSAExpression::Block(Box::new(new_block))
        }
        // Anything returned by return and yield will not die
        SSAExpression::Return { val } => {
            let alive = get_alive_vars(&val);
            println!("{alive_vars:?}");
            let to_die: Vec<String> = alive_vars
                .into_iter()
                .filter(|x| !alive.contains(x))
                .collect();
            println!("{alive:?}");
            println!("{to_die:?}\n");

            drop_dead_vars(to_die, SSAExpression::Return { val }, ctx)
        }
        SSAExpression::Yield { val } => {
            let alive = get_alive_vars(&val);
            let to_die: Vec<String> = alive_vars
                .into_iter()
                .filter(|x| alive.contains(x))
                .collect();
            drop_dead_vars(to_die, SSAExpression::Yield { val }, ctx)
        }
        SSAExpression::ForLoop {
            iv,
            from,
            to,
            block,
            e2,
        } => {
            let new_block = unalive_vars(*block, vec![iv.clone()], ctx);
            let new_cont = unalive_vars(*e2, alive_vars, ctx);

            SSAExpression::ForLoop {
                iv,
                from,
                to,
                block: Box::new(new_block),
                e2: Box::new(new_cont),
            }
        }
    }
}
