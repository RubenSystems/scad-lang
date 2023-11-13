use std::fmt::format;

use crate::frontend::{
    high_level_ir::ast_types::FailureCopy, mid_level_ir::parsers::generate_register_name,
};

use super::{
    mir_ast_types::{SSAConditionalBlock, SSAExpression, SSAValue},
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
            SSAValue::FunctionCall { name, parameters } => {
                let parsed_params: Vec<String> = parameters
                    .iter()
                    .map(|x| format!("i32 noundef {}", x.to_llvm_ir()))
                    .collect();
                format!("call i32 @{name}({})", parsed_params.join(","))
            }
        }
    }
}

impl SSAExpression {
    pub fn to_llvm_ir(&self) -> String {
        match self {
            SSAExpression::VariableDecl { name, e1, e2 } => {
                format!(
                    "%{name} = alloca i32, align 4\nstore i32 {}, ptr %{name}, align 4\n{}",
                    e1.to_llvm_ir(),
                    e2.to_llvm_ir()
                )
            }
            SSAExpression::ConstDecl { name, e1, e2 } => {
                format!(
                    "%{name} = alloca i32, align 4\nstore i32 {}, ptr %{name}, align 4\n{}",
                    e1.to_llvm_ir(),
                    e2.to_llvm_ir()
                )
            }
            SSAExpression::RegisterDecl { name, e1, e2 } => {
                format!("%{name} = {} \n{}", e1.to_llvm_ir(), e2.to_llvm_ir())
            }
            SSAExpression::Noop => "".into(),
            SSAExpression::FuncDecl { name, args, block } => {
                let arg_defs: Vec<String> = args
                    .iter()
                    .map(|(name, _tpe)| format!("i32 %{name}.arg"))
                    .collect();

                let arg_derefs : Vec<String> = args
				.iter()
				.map(|(name, _tpe)| format!("%{name} = alloca i32, align 4\nstore i32 %{name}.arg, ptr %{name}, align 4"))
				.collect();

                let statements: Vec<String> = block.iter().map(|s| s.to_llvm_ir()).collect();

                format!(
                    "define i32 @{name}({}) {{\n{}\n{}\n}}",
                    arg_defs.join(","),
                    arg_derefs.join("\n"),
                    statements.join("\n")
                )
            }
            SSAExpression::Return { val } => {
                format!("ret i32 {}", val.to_llvm_ir())
            }
            SSAExpression::VariableReference { name, tmp_name, e2 } => format!(
                "%{tmp_name} = load i32, ptr %{name}, align 4\n{}",
                e2.to_llvm_ir()
            ),
            SSAExpression::Block(b) => {
                let statements: Vec<String> = b.iter().map(|s| s.to_llvm_ir()).collect();
                statements.join("\n")
            }
            SSAExpression::ConditionalBlock { if_block, e2 } => {
                let start_label = generate_register_name();
                let done_label = generate_register_name(); 
                render_if_statement(if_block.fcopy(), e2.fcopy(), start_label, done_label)
            }
        }
    }
}

fn render_if_statement(
    blocks: SSAConditionalBlock,
    e2: SSAExpression,
    this_label: String,
    done_label: String,
) -> String {
    let next_label = generate_register_name();

    let next = match e2 {
        SSAExpression::ConditionalBlock { if_block, e2 } => render_if_statement(*if_block, *e2, next_label.clone(), done_label.clone()),
        a => format!(r#"
            {done_label}:
            {}
        "#,
        a.to_llvm_ir())
    };

    format!(
        r#"
        br i1 {}, label %{this_label}, label %{next_label}
        {this_label}:
        {}
        br label %{done_label}
        {next_label}:
        {}
        "#,
        blocks.condition.to_llvm_ir(),
        blocks.block.to_llvm_ir(),
        next
    )
}
