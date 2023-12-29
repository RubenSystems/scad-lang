use crate::frontend::type_system::{
    context::Context, tir_ast_expressions::TIRExpression, type_engine::w_algo,
};

trait LLVMCodegeneratable {
    fn generate_llvm(&self) -> String;
}

impl LLVMCodegeneratable for TIRExpression {
    fn generate_llvm(&self) -> String {
        let (_, _, _ctx) = w_algo(Context::new(), self);

        match self {
            TIRExpression::Integer(i) => i.to_string(),
            TIRExpression::Bool(b) if *b => "1".into(),
            TIRExpression::Bool(_) => "0".into(),
            TIRExpression::Void => "".into(),
            TIRExpression::Phi(_) => todo!(),
            TIRExpression::Float(_) => todo!(),
            TIRExpression::VariableReference { name: _ } => todo!(),
            TIRExpression::VariableDecl {
                name: _,
                type_hint: _,
                e1: _,
                e2: _,
            } => todo!(),
            TIRExpression::FunctionCall { e1: _, e2: _ } => todo!(),
            TIRExpression::FunctionDefinition { arg_name: _, e1: _ } => todo!(),
            TIRExpression::Conditional {
                condition: _,
                if_block: _,
                else_block: _,
                e1: _,
            } => todo!(),
        }
    }
}
