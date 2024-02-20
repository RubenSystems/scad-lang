
// Not in use anymore


// impl SSAValue {
//     pub fn to_llvm_ir(&self) -> String {
//         match self {
//             _ => todo!()
//         }
//     }
// }

// impl SSAExpression {
//     pub fn to_llvm_ir(&self, end_conditional_block: Option<String>, is_last_block: bool) -> String {
//         match self {
//             SSAExpression::VariableDecl {
//                 name,
//                 vtype: _,
//                 e1,
//                 e2,
//             } => {
//                 format!(
//                     "%{name} = {} \n{}",
//                     e1.to_llvm_ir(),
//                     e2.to_llvm_ir(end_conditional_block, is_last_block)
//                 )
//             }
//             SSAExpression::Noop => "".into(),
//             SSAExpression::FuncDecl {
//                 name,
//                 args,
//                 ret_type: _,
//                 block,
//                 e2: _,
//             } => {
//                 let arg_defs: Vec<String> = args
//                     .iter()
//                     .map(|(name, _tpe)| format!("i32 %{name}.arg"))
//                     .collect();

//                 let arg_derefs : Vec<String> = args
// 				.iter()
// 				.map(|(name, _tpe)| format!("%{name} = alloca i32, align 4\nstore i32 %{name}.arg, ptr %{name}, align 4"))
// 				.collect();

//                 let statements: String =
//                     block.to_llvm_ir(end_conditional_block.clone(), is_last_block);

//                 format!(
//                     "define i32 @{name}({}) {{\n{}\n{}\n}}",
//                     arg_defs.join(","),
//                     arg_derefs.join("\n"),
//                     statements
//                 )
//             }
//             SSAExpression::Return { val } => {
//                 format!("ret i32 {}", val.to_llvm_ir())
//             }
//             SSAExpression::Block(b) => b.to_llvm_ir(end_conditional_block.clone(), is_last_block),
//             SSAExpression::FuncForwardDecl {
//                 name: _,
//                 args: _,
//                 ret_type: _,
//                 e2: _,
//             } => todo!(),
//         }
//     }
// }
