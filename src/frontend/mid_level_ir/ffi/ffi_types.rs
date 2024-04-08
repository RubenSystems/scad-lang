use crate::frontend::{
    type_system::{
        tir_types::{MonoType},
    },
};
use std::{os::raw::c_char, ptr::null};

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Location {
    user: FFIString,
    line: usize,
    column: usize,
}

impl Location {
    pub fn new(user: String, line: usize, column: usize) -> Self {
        Self {
            user: FFIString::from_string(user),
            line,
            column,
        }
    }
}

// Types
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FFIApplication {
    pub c: FFIString,
    pub dimensions: *const u32,
    pub dimensions_count: usize,
}

#[repr(C)]
#[derive(Debug)]
pub struct FFIType {
    pub size: usize,
    pub applications: *const FFIApplication,
}

pub fn convert_monotype_to_ffi(tpe: MonoType) -> FFIApplication {
    let MonoType::Application {
        c,
        dimensions,
        types: _,
    } = tpe
    else {
        unreachable!("Unable to type value")
    };

    let (dlen, dptr) = if let Some(d) = dimensions {
        let dims_len = d.len();
        let dims_ptr = d.as_ptr();
        std::mem::forget(d);
        (dims_len, dims_ptr)
    } else {
        (0, null())
    };

    FFIApplication {
        c: FFIString::from_string(c),
        dimensions: dptr,
        dimensions_count: dlen,
    }
}

#[repr(C)]
pub enum FFIHIRTag {
    VariableDecl = 0,
    Noop = 1,
    FunctionDecl = 2,
    ForwardFunctionDecl = 3,
    Return = 4,
    Yield = 5,
    ForLoop = 6,
    WhileLoop = 7,
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]

pub struct FFIString {
    data: *const c_char,
    length: usize,
}

impl FFIString {
    pub fn from_string(string: String) -> Self {
        Self {
            length: string.len(),
            data: std::ffi::CString::new(string).unwrap().into_raw(),
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRVariableDecl {
    pub name: FFIString,
    pub e1: FFIHIRValue,
    pub e2: *const FFIHIRExpr,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRFunctionDecl {
    pub name: FFIString,
    pub block: *const FFIHIRExpr,
    pub args: *const FFIString,
    pub arg_len: usize,
    pub e2: *const FFIHIRExpr,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRForLoop {
    pub iv: FFIString,
    pub start: FFIHIRValue,
    pub end: FFIHIRValue,
    pub step: FFIHIRValue,
    pub block: *const FFIHIRExpr,
    pub parallel: bool,
    pub e2: *const FFIHIRExpr,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRReturn {
    pub res: FFIHIRValue,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRYield {
    pub res: FFIHIRValue,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRForwardFunctionDecl {
    pub name: FFIString,
    pub e2: *const FFIHIRExpr,
}

#[repr(C)]
pub union ExpressionUnion {
    pub variable_decl: FFIHIRVariableDecl,
    pub function_decl: FFIHIRFunctionDecl,
    pub forward_function_decl: FFIHIRForwardFunctionDecl,
    pub noop: u8,
    pub ret: FFIHIRReturn,
    pub yld: FFIHIRYield,
    pub floop: FFIHIRForLoop,
    pub whl: FFIHIRWhile,
}

#[repr(C)]
pub struct FFIHIRExpr {
    pub tag: FFIHIRTag,
    pub value: ExpressionUnion,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRConditional {
    pub if_arm: FFIExpressionBlock,
    pub else_arm: *const FFIHIRExpr,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIExpressionBlock {
    pub condition: *const FFIHIRValue,
    pub block: *const FFIHIRExpr,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRWhile {
    pub condition: FFIHIRValue,
    pub cond_expr: *const FFIHIRExpr,
    pub block: *const FFIHIRExpr,
    pub e2: *const FFIHIRExpr,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRTensor {
    pub vals: *const FFIHIRValue,
    pub size: usize,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRInteger {
    pub value: usize,
    pub width: u32,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRFloat {
    pub value: f64,
    pub width: u32,
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
pub struct FFIHIRCast {
    pub expr: *const FFIHIRValue,
    pub to: FFIApplication,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union ValueUnion {
    pub tensor: FFIHIRTensor,
    pub integer: FFIHIRInteger,
    pub variable_reference: FFIHIRVariableReference,
    pub function_call: FFIHIRFunctionCall,
    pub boolean: u8,
    pub conditional: FFIHIRConditional,
    pub float: FFIHIRFloat,
    pub cast: FFIHIRCast,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub enum FFIHirValueTag {
    Tensor = 0,
    Integer = 1,
    VariableReference = 2,
    FunctionCall = 3,
    Boolean = 4,
    Conditional = 5,
    Float = 6,
    Cast = 7,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRValue {
    pub tag: FFIHirValueTag,
    pub value: ValueUnion,
}
