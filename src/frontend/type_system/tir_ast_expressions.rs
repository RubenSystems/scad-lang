use super::tir_types::{MonoType, TIRType};
use crate::frontend::{
    error::PoolID,
    high_level_ir::ast_types::{IntegerWidth, Type},
};

#[derive(Debug)]
pub struct TIRPhi {
    pub branch_name: String,
    pub value: TIRExpression,
}

#[derive(Debug)]
pub enum TIRExpression {
    Integer(i128, IntegerWidth, PoolID),
    Float(f64, u32, PoolID),
    Bool(bool, PoolID),
    Void,
    Tensor(Vec<TIRExpression>, PoolID),
    Phi(Vec<TIRPhi>),
    VariableReference {
        name: String,
        pool_id: PoolID,
    },
    VariableDecl {
        name: String,
        type_hint: Option<Type>,
        e1: Box<TIRExpression>,
        e2: Box<TIRExpression>,
        pool_id: PoolID,
    },
    FunctionCall {
        e1: Box<TIRExpression>,
        e2: Box<TIRExpression>,
        pool_id: PoolID,
    },
    FunctionDefinition {
        arg_name: String,
        arg_type_hint: Option<TIRType>,
        ret_type_hint: Option<TIRType>,
        e1: Box<TIRExpression>,
        pool_id: PoolID,
    },
    Conditional {
        condition: Box<TIRExpression>,
        if_block: (String, Box<TIRExpression>), //Name, Expr
        else_block: (String, Box<TIRExpression>),
        pool_id: PoolID,
    },
    WhileLoop {
        condition: Box<TIRExpression>,
        block: Box<TIRExpression>,
        e2: Box<TIRExpression>,
        pool_id: PoolID,
    },
    Cast {
        from: Box<TIRExpression>,
        to_type: MonoType,
        pool_id: PoolID,
    },
}
