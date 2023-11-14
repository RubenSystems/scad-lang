use crate::frontend::high_level_ir::ast_types::{FailureCopy, Type};

#[derive(Debug)]
pub struct SSAConditionalBlock {
    pub condition: SSAValue,
    pub block: Vec<SSAExpression>,
}

impl FailureCopy for SSAConditionalBlock {
    fn fcopy(&self) -> Self {
        SSAConditionalBlock {
            condition: self.condition.fcopy(),
            block: self.block.iter().map(|x| x.fcopy()).collect(),
        }
    }
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
    Block(Vec<SSAExpression>),
    ConditionalBlock {
        conditionals: Vec<SSAExpression>,
        else_block: Option<Vec<SSAExpression>>,
    },
    Conditional(SSAConditionalBlock),
}

impl FailureCopy for SSAExpression {
    fn fcopy(&self) -> Self {
        match self {
            SSAExpression::RegisterDecl { name, e1, e2 } => SSAExpression::RegisterDecl {
                name: name.clone(),
                e1: e1.fcopy(),
                e2: Box::new(e2.fcopy()),
            },
            SSAExpression::VariableDecl { name, e1, e2 } => SSAExpression::VariableDecl {
                name: name.clone(),
                e1: e1.fcopy(),
                e2: Box::new(e2.fcopy()),
            },
            SSAExpression::ConstDecl { name, e1, e2 } => SSAExpression::ConstDecl {
                name: name.clone(),
                e1: e1.fcopy(),
                e2: Box::new(e2.fcopy()),
            },
            SSAExpression::FuncDecl {
                name: _,
                args: _,
                block: _,
            } => todo!(),
            SSAExpression::Noop => SSAExpression::Noop,
            SSAExpression::Return { val: _ } => todo!(),
            SSAExpression::VariableReference { name, tmp_name, e2 } => {
                SSAExpression::VariableReference {
                    name: name.clone(),
                    tmp_name: tmp_name.clone(),
                    e2: Box::new(e2.fcopy()),
                }
            }
            SSAExpression::Block(b) => SSAExpression::Block(b.iter().map(|x| x.fcopy()).collect()),
            SSAExpression::ConditionalBlock { conditionals, else_block } => todo!(),
            SSAExpression::Conditional(c) => SSAExpression::Conditional(c.fcopy())
            // SSAExpression::ConditionalBlock { if_block, e2 } => SSAExpression::ConditionalBlock {
            //     if_block: Box::new(if_block.fcopy()),
            //     e2: Box::new(e2.fcopy()),
            // },

        }
    }
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
    FunctionCall {
        name: String,
        parameters: Vec<SSAValue>,
    },
}

impl FailureCopy for SSAValue {
    fn fcopy(&self) -> SSAValue {
        match self {
            SSAValue::RegisterReference(r) => SSAValue::RegisterReference(r.clone()),
            SSAValue::VariableDereference(v) => SSAValue::VariableDereference(v.clone()),
            SSAValue::Integer(i) => SSAValue::Integer(*i),
            SSAValue::Float(f) => SSAValue::Float(*f),
            SSAValue::Operation { lhs, op, rhs: _ } => SSAValue::Operation {
                lhs: Box::new(lhs.fcopy()),
                op: op.clone(),
                rhs: Box::new(lhs.fcopy()),
            },
            SSAValue::FunctionCall { name, parameters } => SSAValue::FunctionCall {
                name: name.clone(),
                parameters: parameters.iter().map(|x| x.fcopy()).collect(),
            },
        }
    }
}
