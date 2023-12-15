use crate::frontend::mid_level_ir::parsers::generate_register_name;

use super::{
    mir_ast_types::{SSAExpression, SSAValue},
    parsers::op_to_llvm,
};

impl SSAValue {
    pub fn to_llvm_ir(&self) -> String {
        match self {
            SSAValue::RegisterReference(name) => format!("%{name}"),
            SSAValue::Integer(i) => i.to_string(),
            SSAValue::Float(f) => f.to_string(),
            SSAValue::Operation { lhs, op, rhs } => format!(
                "{} i32 {}, {}",
                op_to_llvm(op),
                lhs.to_llvm_ir(),
                rhs.to_llvm_ir()
            ),
            SSAValue::VariableDereference(name) => format!("%{name}"),
            SSAValue::Nothing => "".into(),
            SSAValue::FunctionCall { name, parameters } => todo!(),
            SSAValue::Bool(_) => todo!(),
            SSAValue::Phi(_) => todo!(),
        }
    }
}

impl SSAExpression {
    pub fn to_llvm_ir(&self, end_conditional_block: Option<String>, is_last_block: bool) -> String {
        match self {
            SSAExpression::VariableDecl {
                name,
                vtype: _,
                e1,
                e2,
            } => {
                format!(
                    "%{name} = alloca i32, align 4\nstore i32 {}, ptr %{name}, align 4\n{}",
                    e1.to_llvm_ir(),
                    e2.to_llvm_ir(end_conditional_block, is_last_block)
                )
            }
            SSAExpression::ConstDecl {
                name,
                vtype: _,
                e1,
                e2,
            } => {
                format!(
                    "%{name} = alloca i32, align 4\nstore i32 {}, ptr %{name}, align 4\n{}",
                    e1.to_llvm_ir(),
                    e2.to_llvm_ir(end_conditional_block, is_last_block)
                )
            }
            SSAExpression::RegisterDecl {
                name,
                vtype: _,
                e1,
                e2,
            } => {
                format!(
                    "%{name} = {} \n{}",
                    e1.to_llvm_ir(),
                    e2.to_llvm_ir(end_conditional_block, is_last_block)
                )
            }
            SSAExpression::Noop => "".into(),
            SSAExpression::FuncDecl {
                name,
                args,
                ret_type,
                block,
                e2,
            } => {
                let arg_defs: Vec<String> = args
                    .iter()
                    .map(|(name, _tpe)| format!("i32 %{name}.arg"))
                    .collect();

                let arg_derefs : Vec<String> = args
				.iter()
				.map(|(name, _tpe)| format!("%{name} = alloca i32, align 4\nstore i32 %{name}.arg, ptr %{name}, align 4"))
				.collect();

                let statements: String =
                    block.to_llvm_ir(end_conditional_block.clone(), is_last_block);

                format!(
                    "define i32 @{name}({}) {{\n{}\n{}\n}}",
                    arg_defs.join(","),
                    arg_derefs.join("\n"),
                    statements
                )
            }
            SSAExpression::Return { val } => {
                format!("ret i32 {}", val.to_llvm_ir())
            }
            SSAExpression::VariableReference { name, tmp_name, e2 } => format!(
                "%{tmp_name} = load i32, ptr %{name}, align 4\n{}",
                e2.to_llvm_ir(end_conditional_block, is_last_block)
            ),
            SSAExpression::Block(b) => b.to_llvm_ir(end_conditional_block.clone(), is_last_block),
            SSAExpression::Conditional(c) => todo!(),
            SSAExpression::ConditionalBlock {
                if_block,
                else_block,
                e2,
            } => todo!(),
            SSAExpression::FuncForwardDecl {
                name,
                args,
                ret_type,
                e2,
            } => todo!(),
        }
    }
}
