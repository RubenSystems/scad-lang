use super::mir_ast_types::{SSAExpression, SSAValue};

#[repr(C)]
pub enum FFIHIRTag {
    VariableDecl,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FFIHIRVariableDecl {
    name: *const u8,
}

#[repr(C)]
pub union ExpressionUnion {
    variable_decl: FFIHIRVariableDecl,
}

#[repr(C)]
pub struct FFIHIRExpr {
    tag: FFIHIRTag,
    value: ExpressionUnion,
}

pub fn ffi_ssa_expr(expr: SSAExpression) -> FFIHIRExpr {
    match expr {
        SSAExpression::VariableDecl {
            name,
            vtype,
            e1,
            e2,
        } => FFIHIRExpr {
            tag: FFIHIRTag::VariableDecl,
            value: ExpressionUnion {
                variable_decl: FFIHIRVariableDecl {
                    name: name.as_ptr(),
                },
            },
        },
        SSAExpression::FuncDecl {
            name,
            args,
            ret_type,
            block,
            e2,
        } => todo!(),
        SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2,
        } => todo!(),
        SSAExpression::Noop => todo!(),
        SSAExpression::Return { val } => todo!(),
        SSAExpression::Block(_) => todo!(),
        SSAExpression::ConditionalBlock {
            if_block,
            else_block,
            e2,
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
pub union ValueUnion {
    array: FFIHIRArray,
    integer: FFIHIRInteger,
}

#[repr(C)]
pub enum FFIHirValueTag {
    Array,
    Integer,
}

#[repr(C)]
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
        SSAValue::Operation { lhs, op, rhs } => todo!(),
        SSAValue::FunctionCall { name, parameters } => todo!(),
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
