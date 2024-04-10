use crate::frontend::{
    error::PoolID,
    high_level_ir::ast_types::{FailureCopy, Type},
    mid_level_ir::mir_ast_types::{SSAExpression, SSAValue},
};

use super::{
    context::Context,
    tir_ast_expressions::{TIRExpression, TIRPhi},
    tir_types::{MonoType, TIRType},
};

pub fn transform_mir_value_to_tir(mir: SSAValue, ctx: Context) -> (TIRExpression, Context) {
    match mir {
        SSAValue::Bool(b, p) => (TIRExpression::Bool(b, p), ctx),
        SSAValue::VariableReference(r, p) => (
            TIRExpression::VariableReference {
                name: r,
                pool_id: p,
            },
            ctx,
        ),
        SSAValue::Integer {
            value,
            width,
            pool_id,
        } => (TIRExpression::Integer(value, width, pool_id), ctx),
        SSAValue::Float {
            value,
            width,
            pool_id,
        } => (TIRExpression::Float(value, width, pool_id), ctx),
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
        } => {
            if parameters.len() == 1 {
                let (xp, ctx) = transform_mir_value_to_tir(parameters[0].fcopy(), ctx);

                (
                    TIRExpression::FunctionCall {
                        e1: Box::new(TIRExpression::VariableReference { name, pool_id }),
                        e2: Box::new(xp),
                        pool_id,
                    },
                    ctx,
                )
            } else if (parameters.len()) == 0 {
                (
                    TIRExpression::FunctionCall {
                        e1: Box::new(TIRExpression::VariableReference { name, pool_id }),
                        e2: Box::new(TIRExpression::Void),
                        pool_id,
                    },
                    ctx,
                )
            } else {
                let (xp, ctx) = transform_mir_value_to_tir(
                    SSAValue::FunctionCall {
                        name,
                        parameters: parameters[..parameters.len() - 1]
                            .iter()
                            .map(|x| x.fcopy())
                            .collect(),
                        pool_id,
                    },
                    ctx,
                );

                let (xp2, ctx2) =
                    transform_mir_value_to_tir(parameters.last().unwrap().fcopy(), ctx);

                (
                    TIRExpression::FunctionCall {
                        e1: Box::new(xp),
                        e2: Box::new(xp2),
                        pool_id,
                    },
                    ctx2,
                )
            }
        }
        SSAValue::Nothing => (TIRExpression::Void, ctx),
        SSAValue::Phi(p) => {
            let phis = p.into_iter().map(|x| {
                let (tir, _) = transform_mir_value_to_tir(x.value, ctx.clone());
                TIRPhi {
                    branch_name: x.branch_name,
                    value: tir,
                }
            });

            (TIRExpression::Phi(phis.collect()), ctx)
        }
        SSAValue::Tensor(v, p) => {
            let vals = v
                .into_iter()
                .map(|x| transform_mir_value_to_tir(x, ctx.clone()).0)
                .collect();

            (TIRExpression::Tensor(vals, p), ctx)
        }
        SSAValue::ConditionalBlock {
            if_block,
            else_block,
            pool_id,
        } => {
            let (condition, ctx) = transform_mir_value_to_tir(*if_block.condition, ctx);
            let (tir_if_block, ctx) = transform_mir_to_tir(*if_block.block.block, ctx);
            let (tir_else_block, ctx) = transform_mir_to_tir(*else_block.block, ctx);

            (
                TIRExpression::Conditional {
                    condition: Box::new(condition),
                    if_block: (if_block.block.label, Box::new(tir_if_block)),
                    else_block: (else_block.label, Box::new(tir_else_block)),
                    pool_id,
                },
                ctx,
            )
        }
        SSAValue::Cast { value, to, pool_id } => {
            let (tpe, ctx) = transform_mir_value_to_tir(*value, ctx);

            (
                TIRExpression::Cast {
                    from: Box::new(tpe),
                    to_type: to.to_tir_type(),
                    pool_id,
                },
                ctx,
            )
        }
    }
}

pub fn get_typename(tpe: Type) -> String {
    tpe.to_string()
}

// pub fn convert_hir_to_tir_type(tpe: Type) -> TIRType {
//     TIRType::MonoType(MonoType::Application {
//         c: get_typename(tpe),
//         types: vec![],
//     })
// }

pub fn transform_mir_function_decl_to_tir(
    args: Vec<(String, Type)>,
    block: Box<SSAExpression>,
    ret_type_hint: Option<TIRType>,
    ctx: Context,
    pool_id: PoolID,
) -> (TIRExpression, Context) {
    if args.len() == 1 {
        let (xp, ctx) = transform_mir_to_tir(SSAExpression::Block(block, pool_id), ctx);
        (
            TIRExpression::FunctionDefinition {
                arg_name: args[0].0.clone(),
                arg_type_hint: Some(TIRType::MonoType(args[0].1.fcopy().to_tir_type())),
                ret_type_hint,
                e1: Box::new(xp),
                pool_id,
            },
            ctx,
        )
    } else if args.is_empty() {
        let (xp, ctx) = transform_mir_to_tir(SSAExpression::Block(block, pool_id), ctx);
        (
            TIRExpression::FunctionDefinition {
                arg_name: "a".into(),
                arg_type_hint: None,
                ret_type_hint,
                e1: Box::new(xp),
                pool_id,
            },
            ctx,
        )
    } else {
        let (xp, ctx) = transform_mir_function_decl_to_tir(
            args[1..]
                .iter()
                .map(|(x, y)| (x.clone(), y.fcopy()))
                .collect(),
            block,
            ret_type_hint.clone(),
            ctx,
            pool_id,
        );

        (
            TIRExpression::FunctionDefinition {
                arg_name: args[0].0.clone(),
                arg_type_hint: Some(TIRType::MonoType(args[0].1.fcopy().to_tir_type())),
                e1: Box::new(xp),
                ret_type_hint,
                pool_id,
            },
            ctx,
        )
    }
}

pub fn transform_mir_function_forward_decl_to_tir(
    args: Vec<Type>,
    ret_type: Option<Type>,
) -> MonoType {
    match args.as_slice() {
        [tpe] => {
            let arg_type = tpe.to_tir_type();

            let return_type = match ret_type {
                Some(dt) => dt.to_tir_type(),
                None => MonoType::Application {
                    c: "void".into(),
                    dimensions: Some(vec![1]),
                    types: vec![],
                },
            };
            MonoType::Application {
                c: "->".into(),
                dimensions: None,
                types: vec![arg_type, return_type],
            }
        }
        [tpe, rest @ ..] => {
            let arg_type = tpe.to_tir_type();
            let rest: Vec<Type> = rest.iter().map(|arg| arg.fcopy()).collect();
            let rest_decl = transform_mir_function_forward_decl_to_tir(rest, ret_type);

            MonoType::Application {
                c: "->".into(),
                dimensions: None,
                types: vec![arg_type, rest_decl],
            }
        }
        [] => {
            let return_type = match ret_type {
                Some(dt) => dt.to_tir_type(),
                None => MonoType::Application {
                    c: "void".into(),
                    dimensions: Some(vec![1]),
                    types: vec![],
                },
            };

            MonoType::Application {
                c: "->".into(),
                dimensions: None,
                types: vec![MonoType::Variable("*".into()), return_type],
            }
        }
    }
}

pub fn transform_mir_to_tir(mir: SSAExpression, ctx: Context) -> (TIRExpression, Context) {
    match mir {
        SSAExpression::VariableDecl {
            name,
            vtype,
            e1,
            e2,
            pool_id,
        } => {
            let (xp, ctx) = transform_mir_value_to_tir(e1, ctx);
            let (xp2, ctx) = transform_mir_to_tir(*e2, ctx);

            (
                TIRExpression::VariableDecl {
                    name,
                    type_hint: vtype,
                    e1: Box::new(xp),
                    e2: Box::new(xp2),
                    pool_id,
                },
                ctx,
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
            let tpe = TIRType::MonoType(ret_type.clone().unwrap().to_tir_type());
            let (e1xp, ctx) =
                transform_mir_function_decl_to_tir(args, block, Some(tpe), ctx, pool_id);
            let (e2xp, ctx) = transform_mir_to_tir(*e2, ctx);

            (
                TIRExpression::VariableDecl {
                    name,
                    type_hint: None,
                    e1: Box::new(e1xp),
                    e2: Box::new(e2xp),
                    pool_id,
                },
                ctx,
            )
        }
        SSAExpression::Noop => (TIRExpression::Void, ctx),
        SSAExpression::Return { val, pool_id: _ } => transform_mir_value_to_tir(val, ctx),
        SSAExpression::Block(eb, _) => transform_mir_to_tir(*eb, ctx),
        SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2,
            pool_id: _,
        } => {
            // let (e1xp, ctx) = transform_mir_function_decl_to_tir(args, block, ctx);
            // let (e2xp, ctx) = transform_mir_to_tir(*e2, ctx);
            let mut new_ctx = ctx;
            let tpe = transform_mir_function_forward_decl_to_tir(
                args.into_iter().map(|(_, a)| a).collect(),
                ret_type,
            );

            new_ctx.add_type_for_name(name, TIRType::ForwardDecleration(tpe));
            let (e2tir, e2ctx) = transform_mir_to_tir(*e2, new_ctx);

            (e2tir, e2ctx)
        }
        SSAExpression::Yield { val, pool_id: _ } => transform_mir_value_to_tir(val, ctx),
        SSAExpression::ForLoop {
            iv: _,
            from,
            to,
            block: _,
            parallel: _,
            e2,
            pool_id,
            step: _,
        } => {
            // don't convert for loop as it does not have a type
            (
                TIRExpression::ForLoop {
                    from: Box::new(transform_mir_value_to_tir(from, ctx.clone()).0),
                    to: Box::new(transform_mir_value_to_tir(to, ctx.clone()).0),
                    e2: Box::new(transform_mir_to_tir(*e2, ctx.clone()).0),
                    pool_id,
                },
                ctx,
            )
        }
        SSAExpression::WhileLoop {
            cond,
            block,
            e2,
            pool_id,
            cond_expr,
        } => {
            let (cond_expr, ctx) = transform_mir_to_tir(*cond_expr, ctx);
            let (cont, ctx) = transform_mir_to_tir(*e2, ctx);
            let (cond, ctx) = transform_mir_value_to_tir(cond, ctx);
            let (block, ctx) = transform_mir_to_tir(*block, ctx);
            (
                TIRExpression::WhileLoop {
                    condition: Box::new(cond),
                    block: Box::new(block),
                    pool_id,
                    e2: Box::new(cont),
                    cond_expr: Box::new(cond_expr),
                },
                ctx,
            )
        }
    }
}
