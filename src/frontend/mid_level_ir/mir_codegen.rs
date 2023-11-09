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
                    .map(|(name, tpe)| format!("i32 %{name}.arg"))
                    .collect();

                let arg_derefs : Vec<String> = args
				.iter()
				.map(|(name, tpe)| format!("%{name} = alloca i32, align 4\nstore i32 %{name}.arg, ptr %{name}, align 4"))
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
        }
    }
}
