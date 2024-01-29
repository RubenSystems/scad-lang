use crate::frontend::{
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
        SSAValue::Bool(b) => (TIRExpression::Bool(b), ctx),
        SSAValue::VariableReference(r) => (TIRExpression::VariableReference { name: r }, ctx),
        SSAValue::Integer(i) => (TIRExpression::Integer(i), ctx),
        SSAValue::Float(f) => (TIRExpression::Float(f), ctx),
        SSAValue::Operation {
            lhs: _,
            op: _,
            rhs: _,
        } => todo!(),
        SSAValue::FunctionCall { name, parameters } => {
            if parameters.len() == 1 {
                let (xp, ctx) = transform_mir_value_to_tir(parameters[0].fcopy(), ctx);

                (
                    TIRExpression::FunctionCall {
                        e1: Box::new(TIRExpression::VariableReference { name }),
                        e2: Box::new(xp),
                    },
                    ctx,
                )
            } else if (parameters.len()) == 0 {
                (
                    TIRExpression::FunctionCall {
                        e1: Box::new(TIRExpression::VariableReference { name }),
                        e2: Box::new(TIRExpression::Void),
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
                    },
                    ctx,
                );

                let (xp2, ctx2) =
                    transform_mir_value_to_tir(parameters.last().unwrap().fcopy(), ctx);

                (
                    TIRExpression::FunctionCall {
                        e1: Box::new(xp),
                        e2: Box::new(xp2),
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
        SSAValue::Array(v) => {
            let vals = v
                .into_iter()
                .map(|x| transform_mir_value_to_tir(x, ctx.clone()).0)
                .collect();

            (TIRExpression::Array(vals), ctx)
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
) -> (TIRExpression, Context) {
    if args.len() == 1 {
        let (xp, ctx) = transform_mir_to_tir(SSAExpression::Block(block), ctx);
        (
            TIRExpression::FunctionDefinition {
                arg_name: args[0].0.clone(),
                arg_type_hint: Some(TIRType::MonoType(args[0].1.fcopy().to_tir_type())),
                ret_type_hint,
                e1: Box::new(xp),
            },
            ctx,
        )
    } else if args.is_empty() {
        let (xp, ctx) = transform_mir_to_tir(SSAExpression::Block(block), ctx);
        (
            TIRExpression::FunctionDefinition {
                arg_name: "a".into(),
                arg_type_hint: None,
                ret_type_hint,
                e1: Box::new(xp),
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
        );

        (
            TIRExpression::FunctionDefinition {
                arg_name: args[0].0.clone(),
                arg_type_hint: Some(TIRType::MonoType(args[0].1.fcopy().to_tir_type())),
                e1: Box::new(xp),
                ret_type_hint,
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
        } => {
            let (xp, ctx) = transform_mir_value_to_tir(e1, ctx);
            let (xp2, ctx) = transform_mir_to_tir(*e2, ctx);

            (
                TIRExpression::VariableDecl {
                    name,
                    type_hint: vtype,
                    e1: Box::new(xp),
                    e2: Box::new(xp2),
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
        } => {
            let tpe = TIRType::MonoType(ret_type.clone().unwrap().to_tir_type());
            let (e1xp, ctx) = transform_mir_function_decl_to_tir(args, block, Some(tpe), ctx);
            let (e2xp, ctx) = transform_mir_to_tir(*e2, ctx);

            (
                TIRExpression::VariableDecl {
                    name,
                    type_hint: ret_type,
                    e1: Box::new(e1xp),
                    e2: Box::new(e2xp),
                },
                ctx,
            )
        }
        SSAExpression::Noop => (TIRExpression::Void, ctx),
        SSAExpression::Return { val } => transform_mir_value_to_tir(val, ctx),
        SSAExpression::Block(eb) => transform_mir_to_tir(*eb, ctx),
        SSAExpression::ConditionalBlock {
            if_block,
            else_block,
            e2,
        } => {
            let (condition, ctx) = transform_mir_value_to_tir(if_block.condition, ctx);
            let (tir_if_block, ctx) = transform_mir_to_tir(*if_block.block.block, ctx);
            let (tir_else_block, ctx) = transform_mir_to_tir(*else_block.block, ctx);
            let (e2, ctx) = transform_mir_to_tir(*e2, ctx);

            (
                TIRExpression::Conditional {
                    condition: Box::new(condition),
                    if_block: (if_block.block.label, Box::new(tir_if_block)),
                    else_block: (else_block.label, Box::new(tir_else_block)),
                    e1: Box::new(e2),
                },
                ctx,
            )
        }
        SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2,
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
    }
}
