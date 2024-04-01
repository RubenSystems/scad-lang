// given an HIR expression, return a type

// define TIR(tm) (typed intermediate repr)

use std::collections::{HashMap, HashSet};

use crate::frontend::{
    error::{ErrorPool, ErrorType, SCADError},
    high_level_ir::ast_types::IntegerWidth,
};

use super::{
    context::Context,
    substitution::Substitution,
    tir_ast_expressions::TIRExpression,
    tir_types::{generate_type_name, MonoType, PolyType, TIRType},
    traits::{FreeVarsGettable, Instantiatable},
};

#[derive(Debug)]
pub enum WAlgoError {
    UnificationError(UnificationError),
}

pub struct WAlgoInfo<'a> {
    pub retry_count: usize,
    pub req_type: Option<TIRType>,
    pub pool: &'a ErrorPool,
}

pub fn w_algo(
    context: Context,
    info: WAlgoInfo,
    exp: &TIRExpression,
) -> Result<(Substitution, MonoType, Context), SCADError> {
    match exp {
        TIRExpression::Integer(_, width, _) => Ok((
            Substitution::new(),
            MonoType::Application {
                c: match width {
                    IntegerWidth::IndexType => "ii".into(),
                    IntegerWidth::Variable(v) => format!("i{v}"),
                },
                dimensions: None,
                types: vec![],
            },
            context,
        )),
        TIRExpression::Bool(_, _) => Ok((
            Substitution::new(),
            MonoType::Application {
                c: "bool".into(),
                dimensions: None,
                types: vec![],
            },
            context,
        )),
        TIRExpression::Void => Ok((
            Substitution::new(),
            MonoType::Application {
                c: "void".into(),
                dimensions: None,
                types: vec![],
            },
            context,
        )),
        TIRExpression::Float(_, width, _p) => Ok((
            Substitution::new(),
            MonoType::Application {
                c: format!("f{width}"),
                dimensions: None,
                types: vec![],
            },
            context,
        )),
        TIRExpression::VariableReference { name, pool_id } => {
            let Some(tpe) = context.get_type_for_name(name) else {
                return Err(SCADError::from_pid(
                    ErrorType::UndefinedVariableReference(name.clone()),
                    *pool_id,
                    info.pool,
                ));
            };

            

            let tpe = match tpe.get(info.retry_count).clone() {
                Some(a) => a,
                None => {
                    return Err(SCADError::from_pid(
                        ErrorType::UndefinedVariableReference(name.clone()),
                        *pool_id,
                        info.pool,
                    ))
                }
            };

            Ok((Substitution::new(), instantiate(tpe.clone()), context))
        }
        TIRExpression::VariableDecl {
            name,
            type_hint,
            e1,
            e2,
            pool_id,
        } => {
            let (s1, t1, mut context) = w_algo(
                context,
                WAlgoInfo {
                    retry_count: info.retry_count,
                    pool: info.pool,
                    req_type: type_hint
                        .as_ref()
                        .map(|x| Some(TIRType::MonoType(x.to_tir_type())))
                        .unwrap_or(info.req_type.clone()),
                },
                e1,
            )?;

            if let Some(x) = context.get_type_for_name(name) {
                // TODO: make it so polymorphic types are allowed
                match x.first().unwrap() {
                    TIRType::ForwardDecleration(_) => {
                        // context.remove_type_for_name(name);
                    }
                    TIRType::MonoType(m) => {
                        let unification = unify(m, &t1).map_err(|_| {
                            SCADError::from_pid(
                                ErrorType::CannotTypeExpression,
                                *pool_id,
                                info.pool,
                            )
                        })?;

                        let substituted_val =
                            unification.substitute(&TIRType::MonoType(t1.clone()));
                        context.add_type_for_name(name.clone(), substituted_val);
                    }
                    _ => {
                        return Err(SCADError::from_pid(
                            ErrorType::UnsupportedVariableReassignment,
                            *pool_id,
                            info.pool,
                        ))
                    }
                }
            }

            //VARIBLE TYPE CHECKING!!!
            // if let Some(t) = type_hint {
            //     let tir_t = t.to_tir_type();

            //     if tir_t != t1 {
            //         unreachable!("skill issue: attempting to assign expression of type: \n\n{t1:#?} to variable of specified_type \n\n{tir_t:#?}")
            //     }
            // }

            let mut sub_context = context.applying_substitution(&s1).clone();
            if !sub_context.has_type_for_name(name) {
                sub_context.add_type_for_name(
                    name.clone(),
                    s1.substitute(&TIRType::PolyType(generalise(&context, t1))),
                );
            }

            let (s2, t2, sub_context) = w_algo(
                sub_context,
                WAlgoInfo {
                    retry_count: info.retry_count,
                    req_type: info.req_type,
                    pool: info.pool,
                },
                e2,
            )?;

            Ok((s2.merge(&s1), t2, sub_context))
        }
        TIRExpression::FunctionCall { e1, e2, pool_id: _ } => {
            let mut retry_count = 0;
            loop {
                let (s1, t1, context) = w_algo(
                    context.clone(),
                    WAlgoInfo {
                        retry_count,
                        req_type: info.req_type.clone(),
                        pool: info.pool,
                    },
                    e1,
                )?;
                let (s2, t2, context) = w_algo(
                    context.applying_substitution(&s1),
                    WAlgoInfo {
                        retry_count: info.retry_count,
                        req_type: info.req_type.clone(),
                        pool: info.pool,
                    },
                    e2,
                )?;

                let b = generate_type_name();
                let s3 = unify(
                    &s2.substitute_mono(&t1),
                    &MonoType::Application {
                        c: "->".into(),
                        dimensions: None,
                        types: vec![t2.clone(), MonoType::Variable(b.clone())],
                    },
                );

                match s3 {
                    Ok(s3u) => {
                        return Ok((
                            s3u.merge(&s2.merge(&s1)),
                            s3u.substitute_mono(&MonoType::Variable(b)),
                            context,
                        ))
                    }
                    Err(_e) => retry_count += 1,
                };
            }
        }
        TIRExpression::FunctionDefinition {
            arg_name: name,
            e1,
            ret_type_hint,
            arg_type_hint,
            pool_id,
        } => {
            let new_type = generate_type_name();
            let tir_new_type = match arg_type_hint {
                Some(s) => {
                    let TIRType::MonoType(a) = s.clone() else {
                        return Err(SCADError::from_pid(
                            ErrorType::UnableToTypeFunctionArguement,
                            *pool_id,
                            info.pool,
                        ));
                    };
                    a
                }
                None => MonoType::Variable(new_type),
            };

            let mut new_context = context.clone();
            new_context.add_type_for_name(name.into(), TIRType::MonoType(tir_new_type.clone()));
            let (sub, tpe, new_context) = w_algo(
                new_context,
                WAlgoInfo {
                    retry_count: info.retry_count,
                    req_type: info.req_type,
                    pool: info.pool,
                },
                e1,
            )?;

            let x = sub.substitute_mono(&MonoType::Application {
                c: "->".into(),
                dimensions: None,
                types: vec![tir_new_type, tpe],
            });

            if let Some(hnt) = ret_type_hint {
                let TIRType::MonoType(_a) = hnt else {
                    return Err(SCADError::from_pid(
                        ErrorType::CannotCheckReturnType,
                        *pool_id,
                        info.pool,
                    ));
                };

                // let rettype = get_rettype_of_application(x.clone());
                // if *a != rettype {
                //     println!("x: {a:#?}");
                //     println!("rettype: {rettype:#?}");

                //     // println!("==\nretfail {:#?} \n\n{:#?}\n==", a,  get_rettype_of_application(x.clone()));
                //     unreachable!("Skill issue: Function defines different type to decleration")
                // }
            }

            Ok((sub, x, new_context))
        }
        TIRExpression::Conditional {
            condition,
            if_block,
            else_block,
            pool_id,
        } => {
            // 1 type conditon ensure bool

            let Ok((_, cond_mt, ctx)) = w_algo(
                context,
                WAlgoInfo {
                    retry_count: info.retry_count,
                    req_type: info.req_type.clone(),
                    pool: info.pool,
                },
                condition,
            ) else {
                todo!()
            };

            // check to see if all condiitons are a bool
            match cond_mt {
                MonoType::Variable(_) => {
                    return Err(SCADError::from_pid(
                        ErrorType::CannotTypeCheckConditionalCondition,
                        *pool_id,
                        info.pool,
                    ))
                }
                MonoType::Application {
                    c,
                    dimensions: _,
                    types: _,
                } if c == "i1" => {}
                MonoType::Application {
                    c: _,
                    dimensions: _,
                    types: _,
                } => {
                    return Err(SCADError::from_pid(
                        ErrorType::ConditionalDoesNotHaveBooleanCondition,
                        *pool_id,
                        info.pool,
                    ))
                }
            };

            let (if_sub, if_mt, if_ctx) = w_algo(
                ctx.clone(),
                WAlgoInfo {
                    retry_count: info.retry_count,
                    req_type: info.req_type.clone(),
                    pool: info.pool,
                },
                &if_block.1,
            )?;
            let Ok((else_sub, else_mt, ctx)) = w_algo(
                if_ctx,
                WAlgoInfo {
                    retry_count: info.retry_count,
                    pool: info.pool,
                    req_type: info.req_type.clone(),
                },
                &else_block.1,
            ) else {
                todo!()
            };

            if if_mt != else_mt {
                return Err(SCADError::from_pid(
                    ErrorType::MultipleBranchTypesInConditional,
                    *pool_id,
                    info.pool,
                ));
            }

            // let Ok((e2_sub, e2_mt, e2_ctx)) = w_algo(
            //     ctx,
            //     WAlgoInfo {
            //         retry_count: info.retry_count,
            //         req_type: info.req_type,
            //     },
            //     e1,
            // ) else {
            //     todo!()
            // };
            Ok((if_sub.merge(&else_sub), if_mt, ctx))
        }
        TIRExpression::Phi(_) => todo!(),
        TIRExpression::Tensor(v, p) => {
            let types: Vec<TIRType> = v
                .iter()
                .map(|val| {
                    w_algo(
                        context.clone(),
                        WAlgoInfo {
                            retry_count: info.retry_count,
                            req_type: info.req_type.clone(),
                            pool: info.pool,
                        },
                        &val,
                    )
                    .unwrap()
                    .1
                })
                .map(TIRType::MonoType)
                .collect();
            if !types.iter().all(|x| *x == types[0]) {
                return Err(SCADError::from_pid(
                    ErrorType::MultipleTypesInVector,
                    *p,
                    info.pool,
                ));
            }

            let ret = cvt_scalar_to_vector(
                v.len() as u32,
                match &types[0] {
                    TIRType::MonoType(mt) => mt.clone(),
                    TIRType::PolyType(_) => instantiate(types[0].clone()),
                    TIRType::ForwardDecleration(_) => todo!(),
                },
            );

            Ok((Substitution::new(), ret, context))
        }
        TIRExpression::Cast {
            from: _,
            to_type,
            pool_id: _,
        } => Ok((Substitution::new(), to_type.clone(), context)),
        TIRExpression::WhileLoop {
            condition,
            block,
            e2,
            pool_id,
            cond_expr,
        } => {
            let (_, _cond_expr_mt, cond_ctx) = w_algo(
                context.clone(),
                WAlgoInfo {
                    retry_count: info.retry_count,
                    req_type: info.req_type.clone(),
                    pool: info.pool,
                },
                &cond_expr,
            )?;

            let (_, cond_mt, ctx) = w_algo(
                cond_ctx,
                WAlgoInfo {
                    retry_count: info.retry_count,
                    req_type: info.req_type.clone(),
                    pool: info.pool,
                },
                condition,
            )?;

            match cond_mt {
                MonoType::Variable(_) => {
                    return Err(SCADError::from_pid(
                        ErrorType::CannotTypeCheckConditionalCondition,
                        *pool_id,
                        info.pool,
                    ))
                }
                MonoType::Application {
                    c,
                    dimensions: _,
                    types: _,
                } if c == "i1" => {}
                MonoType::Application {
                    c: _,
                    dimensions: _,
                    types: _,
                } => {
                    return Err(SCADError::from_pid(
                        ErrorType::ConditionalDoesNotHaveBooleanCondition,
                        *pool_id,
                        info.pool,
                    ))
                }
            };
            let (_blk_sub, _blk_mt, _blk_ctx) = w_algo(
                ctx.clone(),
                WAlgoInfo {
                    retry_count: info.retry_count,
                    req_type: info.req_type.clone(),
                    pool: info.pool,
                },
                &block,
            )?;

            let (s2, t2, contex) = w_algo(
                context,
                WAlgoInfo {
                    retry_count: info.retry_count,
                    req_type: info.req_type.clone(),
                    pool: info.pool,
                },
                e2,
            )?;

            Ok((s2, t2, contex))
        }
        TIRExpression::ForLoop {
            from,
            to,
            e2,
            pool_id,
        } => {
            let (_, from_mt, ctx) = w_algo(
                context.clone(),
                WAlgoInfo {
                    retry_count: info.retry_count,
                    req_type: info.req_type.clone(),
                    pool: info.pool,
                },
                from,
            )?;

            let (_, to_mt, _ctx) = w_algo(
                ctx,
                WAlgoInfo {
                    retry_count: info.retry_count,
                    req_type: info.req_type.clone(),
                    pool: info.pool,
                },
                to,
            )?;

            match (from_mt, to_mt) {
                (
                    MonoType::Application {
                        c,
                        dimensions: _,
                        types: _,
                    },
                    MonoType::Application {
                        c: c1,
                        dimensions: _,
                        types: _,
                    },
                ) if c == "ii" && c1 == "ii" => {}
                _ => {
                    return Err(SCADError::from_pid(
                        ErrorType::ForLoopInductionVariablesMustBeIndexType,
                        *pool_id,
                        info.pool,
                    ))
                }
            };

            let (s1, rest_mt, ctx) = w_algo(
                context,
                WAlgoInfo {
                    retry_count: info.retry_count,
                    req_type: info.req_type.clone(),
                    pool: info.pool,
                },
                e2,
            )?;

            Ok((s1, rest_mt, ctx))
        }
    }
}

fn get_rettype_of_application(app: MonoType) -> MonoType {
    println!("{app:#?}");

    match app {
        MonoType::Variable(_) => unreachable!(),
        MonoType::Application {
            c,
            dimensions,
            types,
        } => {
            if types.len() == 0 {
                MonoType::Application {
                    c,
                    dimensions,
                    types,
                }
            } else {
                get_rettype_of_application(types.last().unwrap().clone())
            }
        }
    }
}

fn cvt_scalar_to_vector(size: u32, mt: MonoType) -> MonoType {
    match mt {
        MonoType::Variable(v) => MonoType::Variable(v),
        MonoType::Application {
            c,
            dimensions,
            types,
        } => {
            if types.len() == 0 {
                MonoType::Application {
                    c,
                    dimensions: Some(vec![size]),
                    types,
                }
            } else {
                MonoType::Application {
                    c,
                    dimensions,
                    types,
                }
            }
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
        MonoType::Application {
            c: _,
            dimensions: _,
            types,
        } => {
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
pub enum UnificationError {
    InfiniteUnification,
    UnifyingDifferentFunctions,
    UnifyingSameFunctionDifferentArgCount,
    IncorrectDimensions,
}

fn unify(t1: &MonoType, t2: &MonoType) -> Result<Substitution, UnificationError> {
    // println!("----UNIFYING-----\n\n{t1:#?}\n\n{t2:#?}\n---------");
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
        MonoType::Application {
            c: c1,
            dimensions: d1,
            types: t1,
        },
        MonoType::Application {
            c: c2,
            dimensions: d2,
            types: t2,
        },
    ) = (t1, t2)
    {
        if c1 != c2 {
            return Err(UnificationError::UnifyingDifferentFunctions);
        }
        if t1.len() != t2.len() {
            return Err(UnificationError::UnifyingSameFunctionDifferentArgCount);
        }
        if d1 != d2 {
            return Err(UnificationError::IncorrectDimensions);
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
