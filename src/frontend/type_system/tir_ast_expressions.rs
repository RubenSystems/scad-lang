use super::tir_types::{MonoType, TIRType};
use crate::frontend::high_level_ir::ast_types::{IntegerWidth, Type};

#[derive(Debug)]
pub struct TIRPhi {
    pub branch_name: String,
    pub value: TIRExpression,
}

#[derive(Debug)]
pub enum TIRExpression {
    Integer(i128, IntegerWidth),
    Float(f64, u32),
    Bool(bool),
    Void,
    Tensor(Vec<TIRExpression>),
    Phi(Vec<TIRPhi>),
    VariableReference {
        name: String,
    },
    VariableDecl {
        name: String,
        type_hint: Option<Type>,
        e1: Box<TIRExpression>,
        e2: Box<TIRExpression>,
    },
    FunctionCall {
        e1: Box<TIRExpression>,
        e2: Box<TIRExpression>,
    },
    FunctionDefinition {
        arg_name: String,
        arg_type_hint: Option<TIRType>,
        ret_type_hint: Option<TIRType>,
        e1: Box<TIRExpression>,
    },
    Conditional {
        condition: Box<TIRExpression>,
        if_block: (String, Box<TIRExpression>), //Name, Expr
        else_block: (String, Box<TIRExpression>),
    },
    Cast {
        from: Box<TIRExpression>,
        to_type: MonoType,
    },
}
