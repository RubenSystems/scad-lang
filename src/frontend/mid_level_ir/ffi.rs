use crate::frontend::high_level_ir::ast_types::FailureCopy;
use std::os::raw::c_char;
use super::mir_ast_types::{SSAExpression, SSAValue};

#[repr(C)]
pub enum FFIHIRTag {
    VariableDecl = 0,
    Noop = 1,
    FunctionDecl = 2,
    ForwardFunctionDecl = 3,
    Return = 4,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRVariableDecl {
    name: *const c_char,
    e1: FFIHIRValue,
    e2: *const FFIHIRExpr,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRFunctionDecl {
    name: *const c_char,
    block: *const FFIHIRExpr,
    e2: *const FFIHIRExpr,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRReturn {
    res: FFIHIRValue,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRForwardFunctionDecl {
    name: *const c_char,
    e2: *const FFIHIRExpr,
}

#[repr(C)]
pub union ExpressionUnion {
    variable_decl: FFIHIRVariableDecl,
    function_decl: FFIHIRFunctionDecl,
    forward_function_decl: FFIHIRForwardFunctionDecl,
    noop: u8,
    ret: FFIHIRReturn,
}

#[repr(C)]
pub struct FFIHIRExpr {
    tag: FFIHIRTag,
    value: ExpressionUnion,
}

pub fn ffi_ssa_expr(expr: std::mem::ManuallyDrop<SSAExpression>) -> FFIHIRExpr {
    match expr.fcopy() {
        SSAExpression::VariableDecl {
            name,
            vtype: _,
            e1,
            e2,
        } => FFIHIRExpr {
            tag: FFIHIRTag::VariableDecl,
            value: ExpressionUnion {
                variable_decl: FFIHIRVariableDecl {
                    name: std::ffi::CString::new(name).unwrap().into_raw(),
                    e1: ffi_ssa_val(e1),
                    e2: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(*e2)))),
                },
            },
        },
        SSAExpression::FuncDecl {
            name,
            args: _,
            ret_type: _,
            block,
            e2,
        } => FFIHIRExpr {
            tag: FFIHIRTag::FunctionDecl,
            value: ExpressionUnion {
                function_decl: FFIHIRFunctionDecl {
                    name: std::ffi::CString::new(name).unwrap().into_raw(),
                    block: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(*block)))),
                    e2: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(*e2)))),
                },
            },
        },
        SSAExpression::FuncForwardDecl {
            name,
            args: _,
            ret_type: _,
            e2,
        } => FFIHIRExpr {
            tag: FFIHIRTag::ForwardFunctionDecl,
            value: ExpressionUnion {
                forward_function_decl: FFIHIRForwardFunctionDecl {
                    name: std::ffi::CString::new(name).unwrap().into_raw(),
                    e2: Box::into_raw(Box::new(ffi_ssa_expr(std::mem::ManuallyDrop::new(*e2)))),
                },
            },
        },
        SSAExpression::Noop => FFIHIRExpr {
            tag: FFIHIRTag::Noop,
            value: ExpressionUnion { noop: 0 },
        },
        SSAExpression::Return { val } => FFIHIRExpr {
            tag: FFIHIRTag::Return,
            value: ExpressionUnion {
                ret: FFIHIRReturn {
                    res: ffi_ssa_val(val),
                },
            },
        },
        SSAExpression::Block(_) => todo!(),
        SSAExpression::ConditionalBlock {
            if_block: _,
            else_block: _,
            e2: _,
        } => todo!(),
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRArray {
    pub vals: *const FFIHIRValue,
    pub size: usize,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRInteger {
    pub value: usize,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union ValueUnion {
    array: FFIHIRArray,
    integer: FFIHIRInteger,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub enum FFIHirValueTag {
    Array,
    Integer,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRValue {
    tag: FFIHirValueTag,
    value: ValueUnion,
}

pub fn ffi_ssa_val(val: SSAValue) -> FFIHIRValue {
    match val {
        SSAValue::VariableReference(_) => todo!(),
        SSAValue::Phi(_) => todo!(),
        SSAValue::Integer(i) => FFIHIRValue {
            tag: FFIHirValueTag::Integer,
            value: ValueUnion {
                integer: FFIHIRInteger { value: i as usize },
            },
        },
        SSAValue::Float(_) => todo!(),
        SSAValue::Bool(_) => todo!(),
        SSAValue::Operation {
            lhs: _,
            op: _,
            rhs: _,
        } => todo!(),
        SSAValue::FunctionCall {
            name: _,
            parameters: _,
        } => todo!(),
        SSAValue::Nothing => todo!(),
        SSAValue::Array(v) => {
            let arr: Vec<FFIHIRValue> = v.into_iter().map(|x| ffi_ssa_val(x)).collect();
            FFIHIRValue {
                tag: FFIHirValueTag::Array,
                value: ValueUnion {
                    array: FFIHIRArray {
                        vals: arr.as_ptr(),
                        size: arr.len(),
                    },
                },
            }
        }
    }
}
