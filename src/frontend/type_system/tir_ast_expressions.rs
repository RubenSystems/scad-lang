pub enum TIRExpression {
    VariableReference {
        name: String,
    },
    VariableDecl {
        name: String,
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
