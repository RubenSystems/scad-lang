use crate::frontend::{
    error::{ErrorPool, ErrorType, SCADError},
    high_level_ir::ast_types::IntegerWidth,
    mid_level_ir::mir_ast_types::{SSAExpression, SSAValue},
    type_system::{
        context::Context,
        tir_types::{MonoType, TIRType},
        type_engine::{get_rettype_of_application, instantiate},
    },
};

use super::ffi_types::FFIHirValueTag;
use super::ffi_types::{
    convert_monotype_to_ffi, ExpressionUnion, FFIApplication, FFIExpressionBlock, FFIHIRCast,
    FFIHIRConditional, FFIHIRExpr, FFIHIRFloat, FFIHIRForLoop, FFIHIRForwardFunctionDecl,
    FFIHIRFunctionCall, FFIHIRFunctionDecl, FFIHIRInteger, FFIHIRReturn, FFIHIRTag, FFIHIRTensor,
    FFIHIRValue, FFIHIRVariableDecl, FFIHIRVariableReference, FFIHIRWhile, FFIHIRYield, FFIString,
    FFIType, ValueUnion,
};
use crate::frontend::high_level_ir::ast_types::FailureCopy;

fn flatten_applications(tpe: MonoType) -> FFIType {
    let MonoType::Application {
        c,
        dimensions,
        types,
    } = tpe
    else {
        unreachable!("Unable to type value")
    };
    let mut flattened = Vec::new();
    collapse_application(
        MonoType::Application {
            c: c.clone(),
            dimensions: dimensions.clone(),
            types: types.clone(),
        },
        &mut flattened,
    );

    let apps: Vec<FFIApplication> = flattened
        .into_iter()
        .map(|x| convert_monotype_to_ffi(x))
        .collect();

    let apps_len = apps.len();
    let apps_ptr = apps.as_ptr();
    std::mem::forget(apps);

    FFIType {
        size: apps_len,
        applications: apps_ptr,
    }
}

fn collapse_application(app: MonoType, collapsed_val: &mut Vec<MonoType>) -> bool {
    let MonoType::Application {
        c,
        dimensions,
        types,
    } = app
    else {
        return false;
    };

    if c == "->" {
        // its not a leaf
        collapse_application(types.first().unwrap().clone(), collapsed_val)
            && collapse_application(types.last().unwrap().clone(), collapsed_val)
    } else {
        collapsed_val.push(MonoType::Application {
            c,
            dimensions,
            types,
        });
        true
    }
}

pub fn convert_type_to_ffi(tpe: TIRType) -> FFIType {
    match tpe.clone() {
        TIRType::MonoType(mt) => flatten_applications(mt),
        TIRType::PolyType(_) => flatten_applications(instantiate(tpe)),
        TIRType::ForwardDecleration(_) => todo!(),
    }
}

pub fn ffi_ssa_val(
    val: SSAValue,
    current_render_width: &Result<u32, SCADError>,
    fname: &str,
    context: &Context,
    pool: &ErrorPool,
) -> Result<FFIHIRValue, SCADError> {
    match val.fcopy() {
        SSAValue::VariableReference(v, _) => Ok(FFIHIRValue {
            tag: FFIHirValueTag::VariableReference,
            value: ValueUnion {
                variable_reference: FFIHIRVariableReference {
                    name: FFIString::from_string(v),
                },
            },
        }),
        SSAValue::Phi(_) => todo!(),
        SSAValue::Integer {
            value,
            width,
            pool_id: _,
        } => Ok(FFIHIRValue {
            tag: FFIHirValueTag::Integer,
            value: ValueUnion {
                integer: FFIHIRInteger {
                    value: value as usize,
                    width: if let Some(width) = width {
                        match width {
                            // This is not very clever and will blow up in ur face at some point in time
                            IntegerWidth::IndexType => 1000,
                            IntegerWidth::Variable(v) => v,
                        }
                    } else {
                        match current_render_width {
                            Ok(o) => *o,
                            Err(e) => return Err(e.clone()),
                        }
                    },
                },
            },
        }),
        SSAValue::Float {
            value,
            width,
            pool_id: _,
        } => Ok(FFIHIRValue {
            tag: FFIHirValueTag::Float,
            value: ValueUnion {
                float: FFIHIRFloat {
                    value,
                    width: if let Some(w) = width {
                        w
                    } else {
                        match current_render_width {
                            Ok(o) => *o,
                            Err(e) => return Err(e.clone()),
                        }
                    },
                },
            },
        }),
        SSAValue::Bool(b, _) => Ok(FFIHIRValue {
            tag: FFIHirValueTag::Boolean,
            value: ValueUnion {
                boolean: if b { 1 } else { 0 },
            },
        }),
        SSAValue::Operation {
            lhs: _,
            op: _,
            rhs: _,
            pool_id: _,
        } => todo!(),
        SSAValue::FunctionCall {
            name,
            parameters,
            pool_id: _,
        } => {
            let mut param_transform = vec![];
            for i in parameters {
                param_transform.push(ffi_ssa_val(
                    i,
                    current_render_width,
                    fname.clone(),
                    context,
                    pool,
                )?)
            }
            let len = param_transform.len();
            let ptr = param_transform.as_ptr();
            std::mem::forget(param_transform);
            Ok(FFIHIRValue {
                tag: FFIHirValueTag::FunctionCall,
                value: ValueUnion {
                    function_call: FFIHIRFunctionCall {
                        func_name: FFIString::from_string(name),
                        params: ptr,
                        param_len: len,
                    },
                },
            })
        }
        SSAValue::Nothing => todo!(),
        SSAValue::Tensor(v, _) => {
            let mut arr = vec![];
            for i in v {
                arr.push(ffi_ssa_val(
                    i,
                    current_render_width,
                    fname.clone(),
                    context,
                    pool,
                )?)
            }
            let arr_ptr = arr.as_ptr();
            let arr_len = arr.len();
            std::mem::forget(arr);
            Ok(FFIHIRValue {
                tag: FFIHirValueTag::Tensor,
                value: ValueUnion {
                    tensor: FFIHIRTensor {
                        vals: arr_ptr,
                        size: arr_len,
                    },
                },
            })
        }
        SSAValue::ConditionalBlock {
            if_block,
            else_block,
            pool_id: _,
        } => Ok(FFIHIRValue {
            tag: FFIHirValueTag::Conditional,
            value: ValueUnion {
                conditional: FFIHIRConditional {
                    if_arm: FFIExpressionBlock {
                        condition: Box::into_raw(Box::new(ffi_ssa_val(
                            *if_block.condition,
                            current_render_width,
                            fname,
                            context,
                            pool,
                        )?)),
                        block: Box::into_raw(Box::new(ffi_ssa_expr(
                            *if_block.block.block,
                            fname,
                            context,
                            pool,
                        )?)),
                    },
                    else_arm: Box::into_raw(Box::new(ffi_ssa_expr(
                        *else_block.block,
                        fname,
                        context,
                        pool,
                    )?)),
                },
            },
        }),
        SSAValue::Cast {
            value,
            to,
            pool_id: _,
        } => Ok(FFIHIRValue {
            tag: FFIHirValueTag::Cast,
            value: ValueUnion {
                cast: FFIHIRCast {
                    expr: Box::into_raw(Box::new(ffi_ssa_val(
                        *value,
                        current_render_width,
                        fname,
                        context,
                        pool,
                    )?)),
                    to: unsafe {
                        *convert_type_to_ffi(TIRType::MonoType(to.to_tir_type())).applications
                    },
                },
            },
        }),
    }
}

pub fn ffi_ssa_expr(
    expr: SSAExpression,
    fname: &str,
    context: &Context,
    pool: &ErrorPool,
) -> Result<FFIHIRExpr, SCADError> {
    match expr.fcopy() {
        SSAExpression::VariableDecl {
            name,
            vtype: _,
            e1,
            e2,
            pool_id,
        } => Ok(FFIHIRExpr {
            tag: FFIHIRTag::VariableDecl,
            value: ExpressionUnion {
                variable_decl: FFIHIRVariableDecl {
                    e1: ffi_ssa_val(
                        e1,
                        &get_width_for_type_from_context(&name, context, pool_id, pool),
                        fname,
                        context,
                        pool,
                    )?,
                    name: FFIString::from_string(name),
                    e2: Box::into_raw(Box::new(ffi_ssa_expr(*e2, fname, context, pool)?)),
                },
            },
        }),
        SSAExpression::FuncDecl {
            name,
            args,
            ret_type: _,
            block,
            e2,
            pool_id: _,
        } => {
            let a: Vec<FFIString> = args
                .into_iter()
                .map(|x| FFIString::from_string(x.0))
                .collect();
            let a_len = a.len();
            let a_ptr = a.as_ptr();
            std::mem::forget(a);

            Ok(FFIHIRExpr {
                tag: FFIHIRTag::FunctionDecl,
                value: ExpressionUnion {
                    function_decl: FFIHIRFunctionDecl {
                        block: Box::into_raw(Box::new(ffi_ssa_expr(*block, &name, context, pool)?)),
                        name: FFIString::from_string(name),
                        e2: Box::into_raw(Box::new(ffi_ssa_expr(*e2, fname, context, pool)?)),
                        args: a_ptr,
                        arg_len: a_len,
                    },
                },
            })
        }
        SSAExpression::FuncForwardDecl {
            name,
            args: _,
            ret_type: _,
            e2,
            pool_id: _,
        } => Ok(FFIHIRExpr {
            tag: FFIHIRTag::ForwardFunctionDecl,
            value: ExpressionUnion {
                forward_function_decl: FFIHIRForwardFunctionDecl {
                    name: FFIString::from_string(name),
                    e2: Box::into_raw(Box::new(ffi_ssa_expr(*e2, fname, context, pool)?)),
                },
            },
        }),
        SSAExpression::Noop => Ok(FFIHIRExpr {
            tag: FFIHIRTag::Noop,
            value: ExpressionUnion { noop: 0 },
        }),
        SSAExpression::Return { val, pool_id } => Ok(FFIHIRExpr {
            tag: FFIHIRTag::Return,
            value: ExpressionUnion {
                ret: FFIHIRReturn {
                    res: ffi_ssa_val(
                        val,
                        &get_width_for_function_from_context(&fname, context, pool_id, pool),
                        fname,
                        context,
                        pool,
                    )?,
                },
            },
        }),
        SSAExpression::Block(b, _) => ffi_ssa_expr(*b, fname, context, pool),
        SSAExpression::Yield { val, pool_id } => Ok(FFIHIRExpr {
            tag: FFIHIRTag::Yield,
            value: ExpressionUnion {
                yld: FFIHIRYield {
                    res: ffi_ssa_val(
                        val,
                        &get_width_for_function_from_context(&fname, context, pool_id, pool),
                        fname,
                        context,
                        pool,
                    )?,
                },
            },
        }),
        SSAExpression::ForLoop {
            iv,
            from,
            to,
            block,
            parallel,
            e2,
            pool_id: _,
            step,
        } => Ok(FFIHIRExpr {
            tag: FFIHIRTag::ForLoop,
            value: ExpressionUnion {
                floop: FFIHIRForLoop {
                    iv: FFIString::from_string(iv),
                    start: ffi_ssa_val(from, &Ok(1000), fname, context, pool)?,
                    end: ffi_ssa_val(to, &Ok(1000), fname, context, pool)?,
                    block: Box::into_raw(Box::new(ffi_ssa_expr(*block, fname, context, pool)?)),
                    parallel,
                    e2: Box::into_raw(Box::new(ffi_ssa_expr(*e2, fname, context, pool)?)),
                    step: ffi_ssa_val(step, &Ok(1000), fname, context, pool)?,
                },
            },
        }),
        SSAExpression::WhileLoop {
            cond,
            block,
            e2,
            pool_id: _,
            cond_expr,
        } => Ok(FFIHIRExpr {
            tag: FFIHIRTag::WhileLoop,
            value: ExpressionUnion {
                whl: FFIHIRWhile {
                    condition: ffi_ssa_val(cond, &Ok(1), fname, context, pool)?,
                    block: Box::into_raw(Box::new(ffi_ssa_expr(*block, fname, context, pool)?)),
                    e2: Box::into_raw(Box::new(ffi_ssa_expr(*e2, fname, context, pool)?)),
                    cond_expr: Box::into_raw(Box::new(ffi_ssa_expr(
                        *cond_expr, fname, context, pool,
                    )?)),
                },
            },
        }),
    }
}

fn get_width_for_type_from_context(
    type_name: &str,
    context: &Context,
    pid: usize,
    pool: &ErrorPool,
) -> Result<u32, SCADError> {
    let entry = match context.get_type_for_name(type_name) {
        Some(e) => e,
        None => {
            return Err(SCADError::from_pid(
                ErrorType::CannotTypeExpression,
                pid,
                pool,
            ));
        }
    };
    println!("{type_name}");
    get_width_for_tir_type(&entry[0], pid, pool)
}

fn get_width_for_tir_type(tpe: &TIRType, pid: usize, pool: &ErrorPool) -> Result<u32, SCADError> {
    let mt = instantiate(tpe.clone());
    println!("mt");
    let MonoType::Application {
        c,
        dimensions,
        types,
    } = mt
    else {
        // todo!();

        return Err(SCADError::from_pid(
            ErrorType::CannotTypeExpression,
            pid,
            pool,
        ));
    };

    Ok(width_conversion(&c))
}

fn get_width_for_function_from_context(
    type_name: &str,
    context: &Context,
    pid: usize,
    pool: &ErrorPool,
) -> Result<u32, SCADError> {
    let entry = match context.get_type_for_name(type_name) {
        Some(e) => e,
        None => {
            return Err(SCADError::from_pid(
                ErrorType::CannotTypeExpression,
                pid,
                pool,
            ));
        }
    };

    get_width_for_tir_function_type(&entry[0], pid, pool)
}

fn get_width_for_tir_function_type(
    tpe: &TIRType,
    pid: usize,
    pool: &ErrorPool,
) -> Result<u32, SCADError> {
    let mt = instantiate(tpe.clone());

    let ret = get_rettype_of_application(mt.clone()).unwrap();
    let MonoType::Application {
        c,
        dimensions,
        types,
    } = ret
    else {
        return Err(SCADError::from_pid(
            ErrorType::CannotTypeExpression,
            pid,
            pool,
        ));
    };
    Ok(width_conversion(&c))
}

fn width_conversion(name: &str) -> u32 {
    match name {
        "i1" => 1,
        "i8" => 8,
        "i16" => 16,
        "i32" => 32,
        "i64" => 64,
        "f32" => 32,
        "f64" => 64,
        "ii" => 1,
        _ => unreachable!("undefined type"),
    }
}
