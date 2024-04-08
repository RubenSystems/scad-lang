use crate::frontend::{
    high_level_ir::ast_types::IntegerWidth,
    mid_level_ir::mir_ast_types::{SSAExpression, SSAValue},
    type_system::{
        tir_types::{MonoType, TIRType},
        type_engine::instantiate,
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

pub fn ffi_ssa_val(val: std::mem::ManuallyDrop<SSAValue>) -> FFIHIRValue {
    match val.fcopy() {
        SSAValue::VariableReference(v, _) => FFIHIRValue {
            tag: FFIHirValueTag::VariableReference,
            value: ValueUnion {
                variable_reference: FFIHIRVariableReference {
                    name: FFIString::from_string(v),
                },
            },
        },
        SSAValue::Phi(_) => todo!(),
        SSAValue::Integer {
            value,
            width,
            pool_id: _,
        } => FFIHIRValue {
            tag: FFIHirValueTag::Integer,
            value: ValueUnion {
                integer: FFIHIRInteger {
                    value: value as usize,
                    width: match width {
                        // This is not very clever and will blow up in ur face at some point in time
                        IntegerWidth::IndexType => 1000,
                        IntegerWidth::Variable(v) => v,
                    },
                },
            },
        },
        SSAValue::Float {
            value,
            width,
            pool_id: _,
        } => FFIHIRValue {
            tag: FFIHirValueTag::Float,
            value: ValueUnion {
                float: FFIHIRFloat { value, width },
            },
        },
        SSAValue::Bool(b, _) => FFIHIRValue {
            tag: FFIHirValueTag::Boolean,
            value: ValueUnion {
                boolean: if b { 1 } else { 0 },
            },
        },
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
            let param_transform: Vec<FFIHIRValue> = parameters
                .into_iter()
                .map(|x| ffi_ssa_val(std::mem::ManuallyDrop::new(x)))
                .collect();
            let len = param_transform.len();
            let ptr = param_transform.as_ptr();
            std::mem::forget(param_transform);
            FFIHIRValue {
                tag: FFIHirValueTag::FunctionCall,
                value: ValueUnion {
                    function_call: FFIHIRFunctionCall {
                        func_name: FFIString::from_string(name),
                        params: ptr,
                        param_len: len,
                    },
                },
            }
        }
        SSAValue::Nothing => todo!(),
        SSAValue::Tensor(v, _) => {
            let arr: Vec<FFIHIRValue> = v
                .into_iter()
                .map(|x| ffi_ssa_val(std::mem::ManuallyDrop::new(x)))
                .collect();
            let arr_ptr = arr.as_ptr();
            let arr_len = arr.len();
            std::mem::forget(arr);
            FFIHIRValue {
                tag: FFIHirValueTag::Tensor,
                value: ValueUnion {
                    tensor: FFIHIRTensor {
                        vals: arr_ptr,
                        size: arr_len,
                    },
                },
            }
        }
        SSAValue::ConditionalBlock {
            if_block,
            else_block,
            pool_id: _,
        } => FFIHIRValue {
            tag: FFIHirValueTag::Conditional,
            value: ValueUnion {
                conditional: FFIHIRConditional {
                    if_arm: FFIExpressionBlock {
                        condition: Box::into_raw(Box::new(ffi_ssa_val(
                            std::mem::ManuallyDrop::new(*if_block.condition),
                        ))),
                        block: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(
                            *if_block.block.block,
                        )))),
                    },
                    else_arm: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(
                        *else_block.block,
                    )))),
                },
            },
        },
        SSAValue::Cast {
            value,
            to,
            pool_id: _,
        } => FFIHIRValue {
            tag: FFIHirValueTag::Cast,
            value: ValueUnion {
                cast: FFIHIRCast {
                    expr: Box::into_raw(Box::new(ffi_ssa_val(std::mem::ManuallyDrop::new(*value)))),
                    to: unsafe {
                        *convert_type_to_ffi(TIRType::MonoType(to.to_tir_type())).applications
                    },
                },
            },
        },
    }
}

pub fn ffi_ssa_expr(expr: std::mem::ManuallyDrop<SSAExpression>) -> FFIHIRExpr {
    match expr.fcopy() {
        SSAExpression::VariableDecl {
            name,
            vtype: _,
            e1,
            e2,
            pool_id: _,
        } => FFIHIRExpr {
            tag: FFIHIRTag::VariableDecl,
            value: ExpressionUnion {
                variable_decl: FFIHIRVariableDecl {
                    name: FFIString::from_string(name),
                    e1: ffi_ssa_val(std::mem::ManuallyDrop::new(e1)),
                    e2: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(*e2)))),
                },
            },
        },
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

            FFIHIRExpr {
                tag: FFIHIRTag::FunctionDecl,
                value: ExpressionUnion {
                    function_decl: FFIHIRFunctionDecl {
                        name: FFIString::from_string(name),
                        block: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(
                            *block,
                        )))),
                        e2: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(*e2)))),
                        args: a_ptr,
                        arg_len: a_len,
                    },
                },
            }
        }
        SSAExpression::FuncForwardDecl {
            name,
            args: _,
            ret_type: _,
            e2,
            pool_id: _,
        } => FFIHIRExpr {
            tag: FFIHIRTag::ForwardFunctionDecl,
            value: ExpressionUnion {
                forward_function_decl: FFIHIRForwardFunctionDecl {
                    name: FFIString::from_string(name),
                    e2: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(*e2)))),
                },
            },
        },
        SSAExpression::Noop => FFIHIRExpr {
            tag: FFIHIRTag::Noop,
            value: ExpressionUnion { noop: 0 },
        },
        SSAExpression::Return { val, pool_id: _ } => FFIHIRExpr {
            tag: FFIHIRTag::Return,
            value: ExpressionUnion {
                ret: FFIHIRReturn {
                    res: ffi_ssa_val(std::mem::ManuallyDrop::new(val)),
                },
            },
        },
        SSAExpression::Block(b, _) => ffi_ssa_expr(std::mem::ManuallyDrop::new(*b)),
        SSAExpression::Yield { val, pool_id: _ } => FFIHIRExpr {
            tag: FFIHIRTag::Yield,
            value: ExpressionUnion {
                yld: FFIHIRYield {
                    res: ffi_ssa_val(std::mem::ManuallyDrop::new(val)),
                },
            },
        },
        SSAExpression::ForLoop {
            iv,
            from,
            to,
            block,
            parallel,
            e2,
            pool_id: _,
            step,
        } => FFIHIRExpr {
            tag: FFIHIRTag::ForLoop,
            value: ExpressionUnion {
                floop: FFIHIRForLoop {
                    iv: FFIString::from_string(iv),
                    start: ffi_ssa_val(std::mem::ManuallyDrop::new(from)),
                    end: ffi_ssa_val(std::mem::ManuallyDrop::new(to)),
                    block: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(
                        *block,
                    )))),
                    parallel,
                    e2: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(*e2)))),
                    step: ffi_ssa_val(std::mem::ManuallyDrop::new(step)),
                },
            },
        },
        SSAExpression::WhileLoop {
            cond,
            block,
            e2,
            pool_id: _,
            cond_expr,
        } => FFIHIRExpr {
            tag: FFIHIRTag::WhileLoop,
            value: ExpressionUnion {
                whl: FFIHIRWhile {
                    condition: ffi_ssa_val(std::mem::ManuallyDrop::new(cond)),
                    block: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(
                        *block,
                    )))),
                    e2: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(*e2)))),
                    cond_expr: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(
                        *cond_expr,
                    )))),
                },
            },
        },
    }
}
