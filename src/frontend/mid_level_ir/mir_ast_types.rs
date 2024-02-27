use crate::frontend::high_level_ir::ast_types::{FailureCopy, IntegerWidth, Type};

#[derive(Debug, Clone)]
pub struct SSALabeledBlock {
    pub label: String,
    pub block: Box<SSAExpression>,
}

impl FailureCopy for SSALabeledBlock {
    fn fcopy(&self) -> Self {
        SSALabeledBlock {
            label: self.label.clone(),
            block: Box::new(self.block.fcopy()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SSAConditionalBlock {
    pub condition: Box<SSAValue>,
    pub block: SSALabeledBlock,
}

impl FailureCopy for SSAConditionalBlock {
    fn fcopy(&self) -> Self {
        SSAConditionalBlock {
            condition: Box::new(self.condition.fcopy()),
            block: self.block.fcopy(),
        }
    }
}

// SSA Definitions
#[derive(Debug, Clone)]
#[repr(C)]
pub enum SSAExpression {
    VariableDecl {
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
    Yield {
        val: SSAValue,
    },
    ForLoop {
        iv: String,
        from: SSAValue,
        to: SSAValue,
        block: Box<SSAExpression>,
        parallel: bool,
        e2: Box<SSAExpression>,
    },

    Block(Box<SSAExpression>),
}

impl FailureCopy for SSAExpression {
    fn fcopy(&self) -> Self {
        match self {
            SSAExpression::VariableDecl {
                name,
                vtype,
                e1,
                e2,
            } => Self::VariableDecl {
                name: name.clone(),
                vtype: vtype.as_ref().map(|x| x.fcopy()),
                e1: e1.fcopy(),
                e2: Box::new(e2.fcopy()),
            },
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
            SSAExpression::Yield { val } => SSAExpression::Yield { val: val.fcopy() },
            SSAExpression::Block(b) => SSAExpression::Block(Box::new(b.fcopy())),
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
            SSAExpression::ForLoop {
                iv,
                from,
                to,
                block,
                parallel,
                e2,
            } => SSAExpression::ForLoop {
                iv: iv.clone(),
                from: from.fcopy(),
                to: to.fcopy(),
                block: Box::new(block.fcopy()),
                parallel: *parallel,
                e2: Box::new(e2.fcopy()),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Phi {
    pub branch_name: String,
    pub value: SSAValue,
}

#[derive(Debug, Clone)]
#[repr(C)]

pub enum SSAValue {
    VariableReference(String),
    Phi(Vec<Phi>),
    ConditionalBlock {
        if_block: SSAConditionalBlock,
        else_block: SSALabeledBlock,
    },
    Cast {
        value: Box<SSAValue>,
        to: Type,
    },
    Integer {
        value: i128,
        width: IntegerWidth,
    },
    Float {
        value: f64,
        width: u32,
    },
    Bool(bool),
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
    Tensor(Vec<SSAValue>),
}

impl FailureCopy for SSAValue {
    fn fcopy(&self) -> SSAValue {
        match self {
            SSAValue::Tensor(v) => SSAValue::Tensor(v.iter().map(|x| x.fcopy()).collect()),
            SSAValue::Nothing => SSAValue::Nothing,
            SSAValue::VariableReference(r) => SSAValue::VariableReference(r.clone()),
            SSAValue::Integer { value, width } => SSAValue::Integer {
                value: *value,
                width: *width,
            },
            SSAValue::Float { value, width } => SSAValue::Float {
                value: *value,
                width: *width,
            },
            SSAValue::Operation { lhs, op, rhs: _ } => SSAValue::Operation {
                lhs: Box::new(lhs.fcopy()),
                op: op.clone(),
                rhs: Box::new(lhs.fcopy()),
            },
            SSAValue::FunctionCall { name, parameters } => SSAValue::FunctionCall {
                name: name.clone(),
                parameters: parameters.iter().map(|x| x.fcopy()).collect(),
            },
            SSAValue::Bool(b) => SSAValue::Bool(*b),
            SSAValue::Phi(v) => SSAValue::Phi(v.clone()),
            SSAValue::ConditionalBlock {
                if_block,
                else_block,
            } => SSAValue::ConditionalBlock {
                if_block: if_block.fcopy(),
                else_block: else_block.fcopy(),
            },
            SSAValue::Cast { value, to } => SSAValue::Cast {
                value: Box::new(value.fcopy()),
                to: to.fcopy(),
            },
        }
    }
}
