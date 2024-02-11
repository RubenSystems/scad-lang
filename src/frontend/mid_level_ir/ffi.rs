use super::mir_ast_types::{SSAExpression, SSAValue};
use crate::frontend::high_level_ir::ast_types::FailureCopy;
use std::os::raw::c_char;

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
pub struct FFIString {
    data: *const c_char,
    length: usize,
}

impl FFIString {
    fn from_string(string: String) -> Self {
        Self {
            length: string.len(),
            data: std::ffi::CString::new(string).unwrap().into_raw(),
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRVariableDecl {
    name: FFIString,
    e1: FFIHIRValue,
    e2: *const FFIHIRExpr,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRFunctionDecl {
    name: FFIString,
    block: *const FFIHIRExpr,
    args: *const FFIString,
    arg_len: usize,
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
    name: FFIString,
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
                    name: FFIString::from_string(name),
                    e1: ffi_ssa_val(e1),
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
pub struct FFIHIRVariableReference {
    pub name: FFIString,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRFunctionCall {
    pub func_name: FFIString,
    pub params: *const FFIHIRValue,
    pub param_len: usize,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union ValueUnion {
    array: FFIHIRArray,
    integer: FFIHIRInteger,
    variable_reference: FFIHIRVariableReference,
    function_call: FFIHIRFunctionCall,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub enum FFIHirValueTag {
    Array = 0,
    Integer = 1,
    VariableReference = 2,
    FunctionCall = 3,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRValue {
    tag: FFIHirValueTag,
    value: ValueUnion,
}

pub fn ffi_ssa_val(val: SSAValue) -> FFIHIRValue {
    match val {
        SSAValue::VariableReference(v) => {
            println!("{v}");
            FFIHIRValue {
                tag: FFIHirValueTag::VariableReference,
                value: ValueUnion {
                    variable_reference: FFIHIRVariableReference {
                        name: FFIString::from_string(v),
                    },
                },
            }
        }
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
        SSAValue::FunctionCall { name, parameters } => {
            let param_transform: Vec<FFIHIRValue> =
                parameters.into_iter().map(|x| ffi_ssa_val(x)).collect();
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
        SSAValue::Array(v) => {
            let arr: Vec<FFIHIRValue> = v.into_iter().map(|x| ffi_ssa_val(x)).collect();
            let arr_ptr = arr.as_ptr();
            let arr_len = arr.len();
            std::mem::forget(arr);
            FFIHIRValue {
                tag: FFIHirValueTag::Array,
                value: ValueUnion {
                    array: FFIHIRArray {
                        vals: arr_ptr,
                        size: arr_len,
                    },
                },
            }
        }
    }
}
