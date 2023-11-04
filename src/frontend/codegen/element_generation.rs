use crate::frontend::parser::ast_types::{
    Block, CharArray, Expression, Float, Identifier, InfixOperation, Integer, Statement, Type,
    VariableDecl,
};

use super::generatable::Generatable;

static mut CURRENT_TMP_INDEX: u128 = 0;

fn generate_register_name() -> String {
    let val = unsafe { CURRENT_TMP_INDEX };
    unsafe { CURRENT_TMP_INDEX += 1 };
    format!("tmp{val}")
}

#[derive(Debug)]
pub enum SSAStatement {}

// SSA Definitions
#[derive(Debug)]
pub enum SSAExpression {
    RegisterDecl {
        name: String,
        e1: SSAValue,
        e2: Box<SSAExpression>,
    },
    VariableDecl {
        name: String,
        e1: SSAValue,
        e2: Box<SSAExpression>,
    },
    ConstDecl {
        name: String,
        e1: SSAValue,
        e2: Box<SSAExpression>,
    },
    FuncDecl {
        name: String,
        args: Vec<(String, Type)>,
        block: Vec<SSAExpression>,
    },
    Noop,
    Return {
        val: SSAValue,
    },
    VariableReference {
        name: String,
        tmp_name: String,
        e2: Box<SSAExpression>,
    },
}

#[derive(Debug)]
pub enum SSAValue {
    RegisterReference(String),
    VariableDereference(String),
    Integer(i128),
    Float(f64),
    Operation {
        lhs: Box<SSAValue>,
        op: String,
        rhs: Box<SSAValue>,
    },
}

fn op_to_llvm(op: &String) -> String {
    match op.as_str() {
        "+" => "add".into(),
        "*" => "mul".into(),
        "-" => "sub".into(),
        "/" => "div".into(),
        _ => todo!(),
    }
}

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
                    .map(|(name, tpe)| format!("i32 %{name}"))
                    .collect();
                let statements: Vec<String> = block.iter().map(|s| s.to_llvm_ir()).collect();

                format!(
                    "define i32 @{name}({}) {{\n{}\n}}",
                    arg_defs.join(","),
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
        }
    }
}

pub fn expression_ssa_transformation(
    exp: Expression,
    k: Box<dyn FnOnce(SSAValue) -> SSAExpression>,
) -> SSAExpression {
    match exp {
        Expression::InfixOperation(e) => {
            let op = e.op.0.clone();

            let k1 = |y1| {
                let k2 = |y2| {
                    let tmp_name = generate_register_name();
                    let operation = SSAValue::Operation {
                        lhs: Box::new(y1),
                        op: op,
                        rhs: Box::new(y2),
                    };
                    SSAExpression::RegisterDecl {
                        name: tmp_name.clone(),
                        e1: operation,
                        e2: Box::new(k(SSAValue::RegisterReference(tmp_name))),
                    }
                };
                expression_ssa_transformation(*e.lhs, Box::new(k2))
            };
            expression_ssa_transformation(*e.rhs, Box::new(k1))
        }
        Expression::Float(f) => k(SSAValue::Float(f.0)),
        Expression::Integer(i) => k(SSAValue::Integer(i.0)),
        Expression::CharArray(_) => todo!(),
        Expression::Identifier(x) => {
            let id = x.0;
            let tmp_name = generate_register_name();
            SSAExpression::VariableReference {
                name: id,
                tmp_name: tmp_name.clone(),
                e2: Box::new(k(SSAValue::VariableDereference(tmp_name))),
            }
        }
        Expression::FunctionCall(f) => todo!(),
        Expression::IfControlFlow {
            if_block,
            else_ifs,
            else_block,
        } => todo!(),
    }
}

fn parse_block(blk: Block) -> Vec<SSAExpression> {
    let length = blk.statements.len();
    let mut block_statements: Vec<SSAExpression> = blk
        .statements
        .into_iter()
        .enumerate()
        .map(|(idx, statement)| {
            if idx == length - 1 {
                match statement {
                    Statement::Expression(e) => expression_ssa_transformation(
                        e,
                        Box::new(|e| SSAExpression::Return { val: e }),
                    ),
                    _ => statement_ssa_translation(statement, Box::new(|_| SSAExpression::Noop)),
                }
            } else {
                statement_ssa_translation(statement, Box::new(|_| SSAExpression::Noop))
            }
        })
        .collect();

    // if let Statement::Expression(exp) = blk.statements[blk.statements.len() - 1] {
    // 	let ret = expression_ssa_transformation(exp, Box::new(|e| SSAExpression::Return { variable_name: e.to_llvm_ir() } ));
    // 	block_statements.push(ret);
    // } else {
    // 	let end = statement_cps_translation(blk.statements[blk.statements.len() - 1], Box::new(|e| SSAExpression::Noop ));
    // 	block_statements.push(end);
    // }

    block_statements
}

pub fn statement_ssa_translation(
    statement: Statement,
    k: Box<dyn FnOnce(SSAValue) -> SSAExpression>,
) -> SSAExpression {
    match statement {
        Statement::ConstDecl(c) => expression_ssa_transformation(
            c.expression,
            Box::new(|val| SSAExpression::ConstDecl {
                name: c.identifier.0,
                e1: val,
                e2: Box::new(SSAExpression::Noop),
            }),
        ),
        Statement::VariableDecl(v) => expression_ssa_transformation(
            v.expression,
            Box::new(|val| SSAExpression::VariableDecl {
                name: v.identifier.0,
                e1: val,
                e2: Box::new(SSAExpression::Noop),
            }),
        ),
        Statement::Loop(_) => todo!(),
        Statement::Expression(exp) => {
            expression_ssa_transformation(exp, Box::new(|_| SSAExpression::Noop))
        }
        Statement::FunctionDefinition(f) => SSAExpression::FuncDecl {
            name: f.identifier.0,
            args: f.args.into_iter().map(|e| (e.0 .0, e.1)).collect(),
            block: parse_block(f.block),
        },
    }
}
