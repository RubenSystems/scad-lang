use crate::frontend::high_level_ir::ast_types::Type;

#[derive(Debug)]
pub enum TIRExpression {
    Integer,
    Float,
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
}
