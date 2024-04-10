//===----------------------------------------------------------------------===// 
///
/// Hindley milner typing system for SCaD 
/// 
/// This takes in the UTLCIR (untyped lambda calculus intermediate repr) and
/// generates type information which can be fused by the fusing layer of the 
/// compiler to generate the LIR 
/// 
//===----------------------------------------------------------------------===//


// given an HIR expression, return a type

// define TIR(tm) (typed intermediate repr)

use std::collections::{HashMap, HashSet};

use crate::{
    core::typedefs::create_types_for_core,
    frontend::{
        error::{ErrorPool, ErrorType, SCADError},
        high_level_ir::ast_types::{FailureCopy, IntegerWidth},
        mid_level_ir::mir_ast_types::SSAExpression,
    },
};

use super::{
    context::Context,
    mir_to_tir::transform_mir_to_tir,
    substitution::Substitution,
    tir_ast_expressions::TIRExpression,
    tir_types::{generate_type_name, MonoType, PolyType, TIRType},
    traits::{FreeVarsGettable, Instantiatable},
};

pub fn extract_type_information(
    code: &SSAExpression,
    location_pool: &ErrorPool,
) -> Result<Context, SCADError> {
    let consumable_context = create_types_for_core();

    let (tir, ctx) = transform_mir_to_tir(code.fcopy(), consumable_context);

    let (_, _, ctx) = w_algo(
        ctx,
        WAlgoInfo {
            retry_count: 0,
            req_type: None,
            pool: &location_pool,
        },
        &tir,
    )?;

    Ok(ctx)
}

const MAX_HINDLEY_RETRYS: usize = 10000;

#[derive(Debug)]
pub enum WAlgoError {
    UnificationError(UnificationError),
}

pub struct WAlgoInfo<'a> {
    pub retry_count: usize,
    pub req_type: Option<TIRType>,
    pub pool: &'a ErrorPool,
}


/*
    The w algorithm 

    an explination for this algorithm as well as the typing rules 
    can be seen in the report created about this programming language 
*/
fn w_algo(
    context: Context,
    info: WAlgoInfo,
    exp: &TIRExpression,
) -> Result<(Substitution, MonoType, Context), SCADError> {
    match exp {

        /*
            Rules for an integer 
            if the width is defined in the program, the type is 
            known. 

            Else it should be a generic integer and inferred using 
            annotations or its interactions with functions etc 
         */
        TIRExpression::Integer(_, width, _) => {
            if let Some(width) = width {
                Ok((
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
                ))
            } else {
                Ok((
                    Substitution::new(),
                    MonoType::Variable(generate_type_name()),
                    context,
                ))
            }
        }
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

        /*
            Rules for a float 
            if the width is defined in the program, the type is 
            known. 

            Else it should be a generic float and inferred using 
            annotations or its interactions with functions etc 
         */
        TIRExpression::Float(_, width, _p) => {
            if let Some(width) = width {
                Ok((
                    Substitution::new(),
                    MonoType::Application {
                        c: format!("f{width}"),
                        dimensions: None,
                        types: vec![],
                    },
                    context,
                ))
            } else {
                Ok((
                    Substitution::new(),
                    MonoType::Variable(generate_type_name()),
                    context,
                ))
            }
        }
        /*
            A variable is a funcition or variable 

            this can be extracted from the typing environment. If it is not 
            in either, yeet
         */
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

        /*
            Declare a variable. As discussed above, a variable is a function 
            or a variable (recursive definition). 

            First, the expression you are trying to assign to the variable must be 
            typed. This is done recursivley. 

            Next, checks are performed if the variable exists in the function env already to
            ensure its not an invalid reassignment 

            If a type hint is provided, it is used to push down information in the event that 
            the type cannot be inferred (e.g in the case of a generic numeric)

            after, substitutions are applied so the context reflects the new types. 

         */
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

            if let Some(th) = type_hint {
                let tir_type = th.to_tir_type();
                context.add_type_for_name(name.clone(), TIRType::MonoType(tir_type));
            }

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

        /*
            Typing rules for a funciton call

            The first function 'cell' is typed. In lambda calculus, functions are one 
            arguement and one return type. A function with multiple args must return a functioon
            which can then has more args. This process must be typed
         */
        TIRExpression::FunctionCall { e1, e2, pool_id } => {
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
                if retry_count > MAX_HINDLEY_RETRYS {
                    return Err(SCADError::from_pid(
                        ErrorType::CannotFindFunctionWithMatchingArguementTypes,
                        *pool_id,
                        info.pool,
                    ));
                }
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
                let TIRType::MonoType(a) = hnt else {
                    return Err(SCADError::from_pid(
                        ErrorType::CannotCheckReturnType,
                        *pool_id,
                        info.pool,
                    ));
                };

                let rettype = get_rettype_of_application(x.clone());
                if rettype.is_some() && *a != rettype.unwrap() {
                    return Err(SCADError::from_pid(
                        ErrorType::IncorrectFunctionReturnType,
                        *pool_id,
                        info.pool,
                    ));
                }
            }

            Ok((sub, x, new_context))
        }

        /*
            Rules for typing a conditonal 
                Both branches must not have divergent types
                The type for a conditional is equal to the type of each branch 
         */
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

            Ok((if_sub.merge(&else_sub), if_mt, ctx))
        }

        /*
            Phi nodes are depreciated in SCaD 0.1.3
         */
        TIRExpression::Phi(_) => todo!(),
        /*
            Rules for typing a tensor 

            All elements must be the same type. 

            The type is n x type where n is the size
         */
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
        /*
            Type a cast (e.g return the type you are casting to)
         */
        TIRExpression::Cast {
            from: _,
            to_type,
            pool_id: _,
        } => Ok((Substitution::new(), to_type.clone(), context)),

        /*
            While loops do not have a return type as they are statements 

            The condition must be typed to ensure it is a bool
         */
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

        /*
            For loops must be typed to ensre the loop bounds are 
            index types
         */
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

/*
    Get the return type of a function.
*/
pub fn get_rettype_of_application(app: MonoType) -> Option<MonoType> {
    match app {
        MonoType::Variable(_) => None,
        MonoType::Application {
            c,
            dimensions,
            types,
        } => {
            if types.len() == 0 {
                Some(MonoType::Application {
                    c,
                    dimensions,
                    types,
                })
            } else {
                get_rettype_of_application(types.last().unwrap().clone())
            }
        }
    }
}

/*
    Create a vector type from a scalar type and size
*/
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

/*
    Generalise by converting to a polytype
*/
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


/*
    Attempt to peform a unification on two monotypes. 

    If unification is possible, return a general substitution which wen applied 
    to a context converts both monotypes to the same monotype
*/
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
