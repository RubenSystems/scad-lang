// given an HIR expression, return a type

// define TIR(tm) (typed intermediate repr)

use std::collections::{HashMap, HashSet};

use crate::frontend::{
    high_level_ir::ast_types::{FailureCopy, Type},
    mid_level_ir::mir_ast_types::{SSAExpression, SSAValue},
};

use super::{
    context::Context,
    substitution::Substitution,
    tir_ast_expressions::{TIRExpression, TIRPhi},
    tir_types::{generate_type_name, MonoType, PolyType, TIRType},
    traits::{FreeVarsGettable, Instantiatable},
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
                        e1: Box::new(TIRExpression::VariableReference { name: name.clone() }),
                        e2: Box::new(xp),
                    },
                    ctx,
                )
            } else {
                let (xp, ctx) = transform_mir_value_to_tir(
                    SSAValue::FunctionCall {
                        name: name.clone(),
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
    }
}

pub fn transform_mir_function_decl_to_tir(
    args: Vec<(String, Type)>,
    block: Box<SSAExpression>,
    ctx: Context,
) -> (TIRExpression, Context) {
    if args.len() == 1 {
        let (xp, ctx) = transform_mir_to_tir(SSAExpression::Block(block), ctx);
        (
            TIRExpression::FunctionDefinition {
                arg_name: args[0].0.clone(),
                e1: Box::new(xp),
            },
            ctx,
        )
    } else if args.is_empty() {
        let (xp, ctx) = transform_mir_to_tir(SSAExpression::Block(block), ctx);
        (
            TIRExpression::FunctionDefinition {
                arg_name: "a".into(),
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
            ctx,
        );

        (
            TIRExpression::FunctionDefinition {
                arg_name: args[0].0.clone(),
                e1: Box::new(xp),
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
            let arg_type = if let TIRType::MonoType(m) = tpe.to_tir_type() {
                m
            } else {
                unreachable!("how did you do that")
            };
            let return_type = match ret_type {
                Some(dt) => {
                    if let TIRType::MonoType(m) = dt.to_tir_type() {
                        m
                    } else {
                        unreachable!("how did you do that")
                    }
                }
                None => MonoType::Application {
                    c: "void".into(),
                    types: vec![],
                },
            };
            MonoType::Application {
                c: "->".into(),
                types: vec![arg_type, return_type],
            }
        }
        [tpe, rest @ ..] => {
            let arg_type = if let TIRType::MonoType(m) = tpe.to_tir_type() {
                m
            } else {
                unreachable!("how did you do that")
            };
            let rest: Vec<Type> = rest.iter().map(|arg| arg.fcopy()).collect();
            let rest_decl = transform_mir_function_forward_decl_to_tir(rest, ret_type);

            MonoType::Application {
                c: "->".into(),
                types: vec![arg_type, rest_decl],
            }
        }
        [] => {
            let return_type = match ret_type {
                Some(dt) => {
                    if let TIRType::MonoType(m) = dt.to_tir_type() {
                        m
                    } else {
                        unreachable!("how did you do that")
                    }
                }
                None => MonoType::Application {
                    c: "void".into(),
                    types: vec![],
                },
            };

            MonoType::Application {
                c: "->".into(),
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
            ret_type: _,
            block,
            e2,
        } => {
            let (e1xp, ctx) = transform_mir_function_decl_to_tir(args, block, ctx);
            let (e2xp, ctx) = transform_mir_to_tir(*e2, ctx);

            (
                TIRExpression::VariableDecl {
                    name,
                    type_hint: None,
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
            let (if_block, ctx) = transform_mir_to_tir(*if_block.block.block, ctx);
            let (else_block, ctx) = transform_mir_to_tir(*else_block.block, ctx);
            let (e2, ctx) = transform_mir_to_tir(*e2, ctx);

            (
                TIRExpression::Conditional {
                    condition: Box::new(condition),
                    if_block: Box::new(if_block),
                    else_block: Box::new(else_block),
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

pub fn w_algo(context: Context, exp: &TIRExpression) -> (Substitution, MonoType, Context) {
    match exp {
        TIRExpression::Integer(_) => (
            Substitution::new(),
            MonoType::Application {
                c: "i32".into(),
                types: vec![],
            },
            context,
        ),
        TIRExpression::Bool(_) => (
            Substitution::new(),
            MonoType::Application {
                c: "bool".into(),
                types: vec![],
            },
            context,
        ),
        TIRExpression::Void => (
            Substitution::new(),
            MonoType::Variable("void".into()),
            context,
        ),
        TIRExpression::Float(_) => (
            Substitution::new(),
            MonoType::Application {
                c: "f32".into(),
                types: vec![],
            },
            context,
        ),
        TIRExpression::VariableReference { name } => {
            let Some(tpe) = context.get_type_for_name(name) else {
                unreachable!("Undefined variable reference {name} - epic fail")
            };

            (Substitution::new(), instantiate(tpe.clone()), context)
        }
        TIRExpression::VariableDecl {
            name,
            type_hint,
            e1,
            e2,
        } => {
            let (s1, t1, mut context) = w_algo(context, e1);

            if let Some(x) = context.get_type_for_name(name) {
                match x {
                    TIRType::ForwardDecleration(_) => {
                        context.remove_type_for_name(name);
                    },
                    TIRType::MonoType(m) => {
                        let unification = unify(m, &t1);

                        let Ok(sub) = unification else {
                            unreachable!("butthead you tried to trick me but i am smarter then you");
                        };
                        let substituted_val = sub.substitute(&TIRType::MonoType(t1.clone()));
                        context.add_type_for_name(name.clone(), substituted_val);

                    }
                    _  => unreachable!("whoopsies: Attempting to reassign {name} to type: \n\n{t1:#?}\n\n when it already exists as \n\n{x:#?}")
                }
            }

            if let Some(t) = type_hint {
                let tir_t = match t.to_tir_type() {
                    TIRType::MonoType(x) => x,
                    _ => unreachable!("How did you not typehint a monotype lol?"),
                };

                if tir_t != t1 {
                    unreachable!("skill issue: attempting to assign expression of type: \n\n{t1:#?} to variable of specified_type \n\n{tir_t:#?}")
                }
            }

            let mut sub_context = context.applying_substitution(&s1).clone();
            if !sub_context.has_type_for_name(name) {
                sub_context.add_type_for_name(
                    name.clone(),
                    s1.substitute(&TIRType::PolyType(generalise(&context, t1))),
                );
            }

            let (s2, t2, sub_context) = w_algo(sub_context, e2);

            (s2.merge(&s1), t2, sub_context)
        }
        TIRExpression::FunctionCall { e1, e2 } => {
            let (s1, t1, context) = w_algo(context, e1);
            let (s2, t2, context) = w_algo(context.applying_substitution(&s1), e2);
            let b = generate_type_name();

            let s3 = unify(
                &s2.substitute_mono(&t1),
                &MonoType::Application {
                    c: "->".into(),
                    types: vec![t2, MonoType::Variable(b.clone())],
                },
            );

            let s3u = s3.unwrap();

            (
                s3u.merge(&s2.merge(&s1)),
                s3u.substitute_mono(&MonoType::Variable(b)),
                context,
            )
        }
        TIRExpression::FunctionDefinition { arg_name: name, e1 } => {
            let new_type = generate_type_name();
            let tir_new_type = MonoType::Variable(new_type);

            let mut new_context = context.clone();
            new_context.add_type_for_name(name.into(), TIRType::MonoType(tir_new_type.clone()));
            let (sub, tpe, new_context) = w_algo(new_context, e1);

            let x = sub.substitute_mono(&MonoType::Application {
                c: "->".into(),
                types: vec![tir_new_type, tpe],
            });

            (sub, x, new_context)
        }
        TIRExpression::Conditional {
            condition,
            if_block,
            else_block,
            e1,
        } => {
            // 1 type conditon ensure bool
            let boolean = MonoType::Application {
                c: "bool".into(),
                types: vec![],
            };
            let (_, cond_mt, ctx) = w_algo(context, condition);
            if cond_mt != boolean {
                unreachable!("Condition for if statement is not a boolean!");
            }
            let (_, if_mt, if_ctx) = w_algo(ctx.clone(), if_block);
            let (_, else_mt, ctx) = w_algo(if_ctx, else_block);

            if if_mt != else_mt {
                unreachable!("If and else branches must have the same type!");
            }

            let (e2_sub, e2_mt, e2_ctx) = w_algo(ctx, e1);
            (e2_sub, e2_mt, e2_ctx)
        }
        TIRExpression::Phi(p) => {
            let types: Vec<TIRType> = p
                .iter()
                .map(|phi| w_algo(context.clone(), &phi.value).1)
                .map(TIRType::MonoType)
                .collect();
            if !types.iter().all(|x| *x == types[0]) {
                unreachable!("you issue: All branches on phi node are not equal and so you have done something wrong");
            }

            let ret = match &types[0] {
                TIRType::MonoType(mt) => mt.clone(),
                TIRType::PolyType(_) => instantiate(types[0].clone()),
                TIRType::ForwardDecleration(_) => todo!(),
            };

            (Substitution::new(), ret, context)
        }
    }
}

fn generalise(ctx: &Context, tpe: MonoType) -> PolyType {
    let free_variables = diff(ctx.free_vars(), tpe.free_vars());
    let mut base_polytype = PolyType::MonoType(tpe);
    free_variables.iter().for_each(|fv| {
        base_polytype = PolyType::TypeQuantifier {
            alpha: fv.clone(),
            sigma: Box::new(base_polytype.clone()),
        }
    });

    base_polytype
}

fn contains(v: &MonoType, tpe: &String) -> bool {
    match v {
        MonoType::Variable(v) => *v == *tpe,
        MonoType::Application { c: _, types } => {
            let inner_contains: Vec<bool> = types
                .iter()
                .map(|x| contains(x, tpe))
                .filter(|x| *x)
                .collect();
            !inner_contains.is_empty()
        }
    }
}

#[derive(Debug)]
enum UnificationError {
    InfiniteUnification,
    UnifyingDifferentFunctions,
    UnifyingSameFunctionDifferentArgCount,
}

fn unify(t1: &MonoType, t2: &MonoType) -> Result<Substitution, UnificationError> {
    if let (MonoType::Variable(a), MonoType::Variable(b)) = (&t1, &t2) {
        if a == b {
            return Ok(Substitution::new());
        }
    };
    if let MonoType::Variable(a) = &t1 {
        if contains(t2, a) {
            return Err(UnificationError::InfiniteUnification);
        };
        let mut new_sub = Substitution::new();
        new_sub.add_sub(a.clone(), t2.clone());
        return Ok(new_sub);
    }

    if let MonoType::Variable(_) = &t2 {
        return unify(t2, t1);
    };

    if let (
        MonoType::Application { c: c1, types: t1 },
        MonoType::Application { c: c2, types: t2 },
    ) = (t1, t2)
    {
        if c1 != c2 {
            return Err(UnificationError::UnifyingDifferentFunctions);
        }
        if t1.len() != t2.len() {
            return Err(UnificationError::UnifyingSameFunctionDifferentArgCount);
        }

        let mut s = Substitution::new();
        for (a, b) in t1.iter().zip(t2.iter()) {
            s = s.merge(&unify(&s.substitute_mono(a), &s.substitute_mono(b))?)
        }
        return Ok(s);
    }

    todo!()
}

fn diff(a: Vec<String>, b: Vec<String>) -> Vec<String> {
    let vars: HashSet<_> = a.into_iter().collect();
    b.into_iter().filter(|x| !vars.contains(x)).collect()
}

fn instantiate(tpe: TIRType) -> MonoType {
    let mut map = HashMap::new();
    match tpe {
        TIRType::MonoType(m) => m.instantiate(&mut map),
        TIRType::PolyType(p) => p.instantiate(&mut map),
        TIRType::ForwardDecleration(fd) => instantiate(TIRType::MonoType(fd)),
    }
}
