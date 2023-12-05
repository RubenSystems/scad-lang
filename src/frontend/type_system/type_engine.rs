// given an HIR expression, return a type

// define TIR(tm) (typed intermediate repr)

use std::collections::{HashMap, HashSet};

use crate::frontend::{
    high_level_ir::ast_types::{FailureCopy, Type},
    mid_level_ir::mir_ast_types::{SSAExpression, SSAValue},
};

use super::{
    context::{Context, self},
    substitution::Substitution,
    tir_ast_expressions::TIRExpression,
    tir_types::{generate_type_name, MonoType, PolyType, TIRType},
    traits::{FreeVarsGettable, Instantiatable},
};

pub fn transform_mir_value_to_tir(mir: SSAValue, ctx: Context) -> (TIRExpression, Context) {
    match mir {
        SSAValue::RegisterReference(r) => (TIRExpression::VariableReference { name: r }, ctx),
        SSAValue::VariableDereference(r) => (TIRExpression::VariableReference { name: r }, ctx),
        SSAValue::Integer(_) => (TIRExpression::Integer, ctx),
        SSAValue::Float(_) => (TIRExpression::Float, ctx),
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
        SSAValue::Nothing => (TIRExpression::Integer, ctx),
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
    } else if args.len() == 0 {
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

pub fn transform_mir_to_tir(mir: SSAExpression, ctx: Context) -> (TIRExpression, Context) {
    match mir {
        SSAExpression::RegisterDecl {
            name,
            vtype,
            e1,
            e2,
        } => {
            let (xp, ctx) = transform_mir_value_to_tir(e1, ctx);
            let (xp2, ctx) = transform_mir_to_tir(*e2, ctx);

            (
                TIRExpression::VariableDecl {
                    name: name,
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
        SSAExpression::Noop => (TIRExpression::Integer, ctx),
        SSAExpression::Return { val } => transform_mir_value_to_tir(val, ctx),
        SSAExpression::VariableReference {
            name: _,
            tmp_name: _,
            e2: _,
        } => todo!(),
        SSAExpression::Block(eb) => transform_mir_to_tir(*eb, ctx),
        SSAExpression::ConditionalBlock {
            conditionals: _,
            else_block: _,
        } => todo!(),
        SSAExpression::Conditional(_) => todo!(),
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
    }
}

pub fn w_algo(context: Context, exp: &TIRExpression) -> (Substitution, MonoType, Context) {
    match exp {
        TIRExpression::Integer => (Substitution::new(), MonoType::Variable("any_int".into()), context),
        TIRExpression::Float => (Substitution::new(), MonoType::Variable("any_float".into()), context),
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
            let (s1, t1, context) = w_algo(context, &e1);
            match context.get_type_for_name(name) {
                Some(x) => unreachable!("whoopsies: Attempting to reassign {name} to type: {t1:?} when it already exists as {x:?}"),
                None => {},
            }

            if let Some(t) = type_hint {
                let tir_t = match t.to_tir_type() {
                    TIRType::MonoType(x) => x,
                    TIRType::PolyType(_) => unreachable!("POLY TYPES ARE NOT USERSPECIFIABLE YET!"),
                };

                if tir_t != t1 {
                    unreachable!("skill issue: attempting to assign expression of type: {t1:?} to variable of specified_type {tir_t:?}")
                }
            }

            let mut sub_context = context.applying_substitution(&s1).clone();

            sub_context.add_type_for_name(
                name.clone(),
                s1.substitute(&TIRType::PolyType(generalise(&context, t1))),
            );
            let (s2, t2, sub_context) = w_algo(sub_context, &e2);

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
            (
                s3.merge(&s2.merge(&s1)),
                s3.substitute_mono(&MonoType::Variable(b)),
                context
            )
        }
        TIRExpression::FunctionDefinition { arg_name: name, e1 } => {
            let new_type = generate_type_name();
            let tir_new_type = MonoType::Variable(new_type);

            let mut new_context = context.clone();
            new_context.add_type_for_name(name.into(), TIRType::MonoType(tir_new_type.clone()));
            let (sub, tpe, new_context) = w_algo(new_context, e1);

            (
                sub.clone(),
                sub.substitute_mono(&MonoType::Application {
                    c: "->".into(),
                    types: vec![tir_new_type, tpe],
                }),
                new_context
            )
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

fn unify(t1: &MonoType, t2: &MonoType) -> Substitution {
    if let (MonoType::Variable(a), MonoType::Variable(b)) = (&t1, &t2) {
        if a == b {
            return Substitution::new();
        }
    };
    if let MonoType::Variable(a) = &t1 {
        if contains(&t2, a) {
            unreachable!("infiniteee waaaaaaa")
        };
        let mut new_sub = Substitution::new();
        new_sub.add_sub(a.clone(), t2.clone());
        return new_sub;
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
            unreachable!("Can't unify different functions")
        }
        if t1.len() != t2.len() {
            unreachable!("Can't unify functions with different numbers of args")
        }

        let mut s = Substitution::new();
        t1.iter()
            .zip(t2.iter())
            .for_each(|(a, b)| s = s.merge(&unify(&s.substitute_mono(a), &s.substitute_mono(b))));
        return s;
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
    }
}
