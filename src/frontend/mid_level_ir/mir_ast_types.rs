use crate::frontend::high_level_ir::ast_types::{FailureCopy, Type};

#[derive(Debug)]
pub struct SSAConditionalBlock {
    pub condition: SSAValue,
    pub block: Box<SSAExpression>,
}

impl FailureCopy for SSAConditionalBlock {
    fn fcopy(&self) -> Self {
        SSAConditionalBlock {
            condition: self.condition.fcopy(),
            block: Box::new(self.block.fcopy()),
        }
    }
}

// SSA Definitions
#[derive(Debug)]
pub enum SSAExpression {
    RegisterDecl {
        name: String,
        vtype: Option<Type>,
        e1: SSAValue,
        e2: Box<SSAExpression>,
    },
    VariableDecl {
        name: String,
        vtype: Option<Type>,
        e1: SSAValue,
        e2: Box<SSAExpression>,
    },
    ConstDecl {
        name: String,
        vtype: Option<Type>,
        e1: SSAValue,
        e2: Box<SSAExpression>,
    },
    FuncDecl {
        name: String,
        args: Vec<(String, Type)>,
        ret_type: Option<Type>,
        block: Box<SSAExpression>,
        e2: Box<SSAExpression>,
    },
    FuncForwardDecl {
        name: String,
        args: Vec<(String, Type)>,
        ret_type: Option<Type>,
        e2: Box<SSAExpression>,
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
    Block(Box<SSAExpression>),
    ConditionalBlock {
        conditionals: Vec<SSAExpression>,
        else_block: Option<Vec<SSAExpression>>,
    },
    Conditional(SSAConditionalBlock),
}

impl FailureCopy for SSAExpression {
    fn fcopy(&self) -> Self {
        match self {
            SSAExpression::RegisterDecl {
                name,
                vtype,
                e1,
                e2,
            } => Self::RegisterDecl {
                name: name.clone(),
                vtype: vtype.as_ref().map(|x| x.fcopy()),
                e1: e1.fcopy(),
                e2: Box::new(e2.fcopy()),
            },
            SSAExpression::VariableDecl {
                name: _,
                vtype: _,
                e1: _,
                e2: _,
            } => todo!(),
            SSAExpression::ConstDecl {
                name: _,
                vtype: _,
                e1: _,
                e2: _,
            } => todo!(),
            SSAExpression::FuncDecl {
                name,
                args,
                ret_type,
                block,
                e2,
            } => Self::FuncDecl {
                name: name.clone(),
                args: args.iter().map(|(n, t)| (n.clone(), t.fcopy())).collect(),
                ret_type: ret_type.as_ref().map(|x| x.fcopy()),
                block: Box::new(block.fcopy()),
                e2: Box::new(e2.fcopy()),
            },
            SSAExpression::Noop => SSAExpression::Noop,
            SSAExpression::Return { val } => SSAExpression::Return { val: val.fcopy() },
            SSAExpression::VariableReference { name, tmp_name, e2 } => {
                SSAExpression::VariableReference {
                    name: name.clone(),
                    tmp_name: tmp_name.clone(),
                    e2: Box::new(e2.fcopy()),
                }
            }
            SSAExpression::Block(b) => SSAExpression::Block(Box::new(b.fcopy())),
            SSAExpression::ConditionalBlock {
                conditionals: _,
                else_block: _,
            } => todo!(),
            SSAExpression::Conditional(c) => SSAExpression::Conditional(c.fcopy()),
            SSAExpression::FuncForwardDecl {
                name,
                args,
                ret_type,
                e2,
            } => Self::FuncForwardDecl {
                name: name.clone(),
                args: args.iter().map(|(n, t)| (n.clone(), t.fcopy())).collect(),
                ret_type: ret_type.as_ref().map(|x| x.fcopy()),
                e2: Box::new(e2.fcopy()),
            },
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
    Nothing,
}

impl FailureCopy for SSAValue {
    fn fcopy(&self) -> SSAValue {
        match self {
            SSAValue::Nothing => SSAValue::Nothing,
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
