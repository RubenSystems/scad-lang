use crate::frontend::high_level_ir::ast_types::Type;

#[derive(Debug)]
pub struct TIRPhi {
    pub branch_name: String,
    pub value: TIRExpression,
}

#[derive(Debug)]
pub enum TIRExpression {
    Integer(i128),
    Bool(bool),
    Void,
    Phi(Vec<TIRPhi>),
    Float(f64),
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
        e1: Box<TIRExpression>,
    },
    Conditional {
        condition: Box<TIRExpression>,
        if_block: (String, Box<TIRExpression>), //Name, Expr
        else_block: (String, Box<TIRExpression>),
        e1: Box<TIRExpression>,
    },
}
