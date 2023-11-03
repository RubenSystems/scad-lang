use crate::frontend::parser::ast_types::{
    CharArray, Expression, Float, InfixOperation, Integer, Statement, VariableDecl,
};

use super::generatable::Generatable;

static mut CURRENT_TMP_INDEX: u128 = 0;

fn generate_register_name() -> String {
    let val = unsafe { CURRENT_TMP_INDEX };
    unsafe { CURRENT_TMP_INDEX += 1 };
    format!("tmp{val}")
}

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
    Noop,
}

#[derive(Debug)]
pub enum SSAValue {
    RegisterReference(String),
	VariableReference(String),
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
            SSAValue::VariableReference(_) => todo!(),
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
        Expression::Identifier(_) => todo!(),
        Expression::IfControlFlow {
            if_block,
            else_ifs,
            else_block,
        } => todo!(),
    }
}

pub fn statement_cps_translation(
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
    }
}
