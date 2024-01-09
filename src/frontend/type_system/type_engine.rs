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



#[derive(Debug)]
pub enum WAlgoError {

}

pub fn w_algo(
    context: Context,
    req_type: Option<TIRType>,
    exp: &TIRExpression,
) -> Result<(Substitution, MonoType, Context), WAlgoError> {
    match exp {
        TIRExpression::Integer(_) => Ok((
            Substitution::new(),
            MonoType::Application {
                c: "i32".into(),
                types: vec![],
            },
            context,
        )),
        TIRExpression::Bool(_) => Ok((
            Substitution::new(),
            MonoType::Application {
                c: "bool".into(),
                types: vec![],
            },
            context,
        )),
        TIRExpression::Void => Ok((
            Substitution::new(),
            MonoType::Application {
                c: "void".into(),
                types: vec![],
            },
            context,
        )),
        TIRExpression::Float(_) => Ok((
            Substitution::new(),
            MonoType::Application {
                c: "f32".into(),
                types: vec![],
            },
            context,
        )),
        TIRExpression::VariableReference { name } => {
            let Some(tpe) = context.get_type_for_name(name) else {
                unreachable!("Undefined variable reference {name} - epic fail")
            };

            // TODO: make it so polymorphic types are allowed
            let tpe = match tpe.len() {
                0 => unreachable!("Undefined variable reference {name} - epic fail"),
                _ => &tpe[0],
            };

            Ok((Substitution::new(), instantiate(tpe.clone()), context))
        }
        TIRExpression::VariableDecl {
            name,
            type_hint,
            e1,
            e2,
        } => {
            let Ok((s1, t1, mut context)) = w_algo(
                context,
                type_hint
                    .as_ref()
                    .map(|x| Some(TIRType::MonoType(x.to_tir_type())))
                    .unwrap_or(req_type.clone()),
                e1,
            ) else {
                unreachable!("HERE");
            };

            if let Some(x) = context.get_type_for_name(name) {
                // TODO: make it so polymorphic types are allowed
                match x.first().unwrap() {
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
                let tir_t = t.to_tir_type();

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

            let Ok((s2, t2, sub_context)) = w_algo(sub_context, req_type, e2) else { todo!() };

            Ok((s2.merge(&s1), t2, sub_context))
        }
        TIRExpression::FunctionCall { e1, e2 } => {
            let Ok((s1, t1, context)) = w_algo(context, req_type.clone(), e1) else { todo!() };
            let Ok((s2, t2, context)) = w_algo(context.applying_substitution(&s1), req_type, e2) else { todo!() };
            let b = generate_type_name();

            let s3 = unify(
                &s2.substitute_mono(&t1),
                &MonoType::Application {
                    c: "->".into(),
                    types: vec![t2, MonoType::Variable(b.clone())],
                },
            );

            let s3u = s3.unwrap();

            Ok((
                s3u.merge(&s2.merge(&s1)),
                s3u.substitute_mono(&MonoType::Variable(b)),
                context,
            ))
        }
        TIRExpression::FunctionDefinition {
            arg_name: name,
            e1,
            arg_type_hint,
        } => {
            let new_type = generate_type_name();
            let tir_new_type = MonoType::Variable(new_type);

            let mut new_context = context.clone();
            new_context.add_type_for_name(name.into(), TIRType::MonoType(tir_new_type.clone()));
            let Ok((sub, tpe, new_context)) = w_algo(new_context, req_type, e1) else { todo!() };

            let x = sub.substitute_mono(&MonoType::Application {
                c: "->".into(),
                types: vec![tir_new_type, tpe],
            });

            Ok((sub, x, new_context))
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
            let Ok((_, cond_mt, ctx)) = w_algo(context, req_type.clone(), condition) else { todo!() };
            if cond_mt != boolean {
                unreachable!("Condition for if statement is not a boolean!");
            }
            let Ok((_, if_mt, if_ctx)) = w_algo(ctx.clone(), req_type.clone(), &if_block.1) else { todo!() };
            let Ok((_, else_mt, ctx)) = w_algo(if_ctx, req_type.clone(), &else_block.1) else { todo!() };

            if if_mt != else_mt {
                unreachable!("If and else branches must have the same type!");
            }

            let Ok((e2_sub, e2_mt, e2_ctx)) = w_algo(ctx, req_type, e1) else { todo!() };
            Ok((e2_sub, e2_mt, e2_ctx))
        }
        TIRExpression::Phi(p) => {
            let types: Vec<TIRType> = p
                .iter()
                .map(|phi| w_algo(context.clone(), req_type.clone(), &phi.value).unwrap().1)
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

            Ok((Substitution::new(), ret, context))
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

pub fn instantiate(tpe: TIRType) -> MonoType {
    let mut map = HashMap::new();
    match tpe {
        TIRType::MonoType(m) => m.instantiate(&mut map),
        TIRType::PolyType(p) => p.instantiate(&mut map),
        TIRType::ForwardDecleration(fd) => instantiate(TIRType::MonoType(fd)),
    }
}
