use crate::frontend::type_system::{
    context::Context, tir_ast_expressions::TIRExpression, type_engine::w_algo,
};

trait LLVMCodegeneratable {
    fn generate_llvm(&self) -> String;
}

impl LLVMCodegeneratable for TIRExpression {
    fn generate_llvm(&self) -> String {
        let (_, _, ctx) = w_algo(Context::new(), self);

        match self {
            TIRExpression::Integer(i) => i.to_string(),
            TIRExpression::Bool(b) if *b => "1".into(),
            TIRExpression::Bool(_) => "0".into(),
            TIRExpression::Void => "".into(),
            TIRExpression::Phi(_) => todo!(),
            TIRExpression::Float(_) => todo!(),
            TIRExpression::VariableReference { name } => todo!(),
            TIRExpression::VariableDecl {
                name,
                type_hint,
                e1,
                e2,
            } => todo!(),
            TIRExpression::FunctionCall { e1, e2 } => todo!(),
            TIRExpression::FunctionDefinition { arg_name, e1 } => todo!(),
            TIRExpression::Conditional {
                condition,
                if_block,
                else_block,
                e1,
            } => todo!(),
        }
    }
}
