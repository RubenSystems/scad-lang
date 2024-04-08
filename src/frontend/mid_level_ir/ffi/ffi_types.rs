use crate::frontend::{
    high_level_ir::ast_types::{FailureCopy, IntegerWidth},
    type_system::{
        context::Context,
        tir_types::{MonoType, TIRType},
        type_engine::instantiate,
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
pub struct FFIHIRForLoop {
    iv: FFIString,
    start: FFIHIRValue,
    end: FFIHIRValue,
    step: FFIHIRValue,
    block: *const FFIHIRExpr,
    parallel: bool,
    e2: *const FFIHIRExpr,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRReturn {
    res: FFIHIRValue,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRYield {
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
    yld: FFIHIRYield,
    floop: FFIHIRForLoop,
    whl: FFIHIRWhile,
}

#[repr(C)]
pub struct FFIHIRExpr {
    tag: FFIHIRTag,
    value: ExpressionUnion,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRConditional {
    if_arm: FFIExpressionBlock,
    else_arm: *const FFIHIRExpr,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIExpressionBlock {
    condition: *const FFIHIRValue,
    block: *const FFIHIRExpr,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRWhile {
    condition: FFIHIRValue,
    cond_expr: *const FFIHIRExpr,
    block: *const FFIHIRExpr,
    e2: *const FFIHIRExpr,
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
    expr: *const FFIHIRValue,
    to: FFIApplication,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union ValueUnion {
    tensor: FFIHIRTensor,
    integer: FFIHIRInteger,
    variable_reference: FFIHIRVariableReference,
    function_call: FFIHIRFunctionCall,
    boolean: u8,
    conditional: FFIHIRConditional,
    float: FFIHIRFloat,
    cast: FFIHIRCast,
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
    tag: FFIHirValueTag,
    value: ValueUnion,
}

