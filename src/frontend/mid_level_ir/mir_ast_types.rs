//===----------------------------------------------------------------------===//
///
/// This file defines the types for the mid level SSA representation 
/// it is represented as a tree with a continuation
/// 
///                                 function
///                                     conditional
///                                         expr
///                                             cont
///                                     else
///                                         expr
///                                             cont
///                                     cont
///                                 cont
/// 
/// the continuation defines the next 'thing' in the program
///
//===----------------------------------------------------------------------===//


use crate::frontend::{
    error::PoolID,
    high_level_ir::ast_types::{FailureCopy, IntegerWidth, Type},
};

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
        pool_id: PoolID,
    },
    FuncDecl {
        name: String,
        args: Vec<(String, Type)>,
        ret_type: Option<Type>,
        block: Box<SSAExpression>,
        e2: Box<SSAExpression>,
        pool_id: PoolID,
    },
    FuncForwardDecl {
        name: String,
        args: Vec<(String, Type)>,
        ret_type: Option<Type>,
        e2: Box<SSAExpression>,
        pool_id: PoolID,
    },
    Noop,
    Return {
        val: SSAValue,
        pool_id: PoolID,
    },
    Yield {
        val: SSAValue,
        pool_id: PoolID,
    },
    ForLoop {
        iv: String,
        from: SSAValue,
        to: SSAValue,
        step: SSAValue,
        block: Box<SSAExpression>,
        parallel: bool,
        e2: Box<SSAExpression>,
        pool_id: PoolID,
    },
    WhileLoop {
        cond_expr: Box<SSAExpression>,
        cond: SSAValue,
        block: Box<SSAExpression>,
        e2: Box<SSAExpression>,
        pool_id: PoolID,
    },
    Block(Box<SSAExpression>, PoolID),
}

impl FailureCopy for SSAExpression {
    fn fcopy(&self) -> Self {
        match self {
            SSAExpression::VariableDecl {
                name,
                vtype,
                e1,
                e2,
                pool_id,
            } => Self::VariableDecl {
                name: name.clone(),
                vtype: vtype.as_ref().map(|x| x.fcopy()),
                e1: e1.fcopy(),
                e2: Box::new(e2.fcopy()),
                pool_id: *pool_id,
            },
            SSAExpression::FuncDecl {
                name,
                args,
                ret_type,
                block,
                e2,
                pool_id,
            } => Self::FuncDecl {
                name: name.clone(),
                args: args.iter().map(|(n, t)| (n.clone(), t.fcopy())).collect(),
                ret_type: ret_type.as_ref().map(|x| x.fcopy()),
                block: Box::new(block.fcopy()),
                e2: Box::new(e2.fcopy()),
                pool_id: *pool_id,
            },
            SSAExpression::Noop => SSAExpression::Noop,
            SSAExpression::Return { val, pool_id } => SSAExpression::Return {
                val: val.fcopy(),
                pool_id: *pool_id,
            },
            SSAExpression::Yield { val, pool_id } => SSAExpression::Yield {
                val: val.fcopy(),
                pool_id: *pool_id,
            },
            SSAExpression::Block(b, pool_id) => SSAExpression::Block(Box::new(b.fcopy()), *pool_id),
            SSAExpression::FuncForwardDecl {
                name,
                args,
                ret_type,
                e2,
                pool_id,
            } => Self::FuncForwardDecl {
                name: name.clone(),
                args: args.iter().map(|(n, t)| (n.clone(), t.fcopy())).collect(),
                ret_type: ret_type.as_ref().map(|x| x.fcopy()),
                e2: Box::new(e2.fcopy()),
                pool_id: *pool_id,
            },
            SSAExpression::ForLoop {
                iv,
                from,
                to,
                block,
                parallel,
                e2,
                pool_id,
                step,
            } => SSAExpression::ForLoop {
                iv: iv.clone(),
                from: from.fcopy(),
                to: to.fcopy(),
                block: Box::new(block.fcopy()),
                parallel: *parallel,
                e2: Box::new(e2.fcopy()),
                pool_id: *pool_id,
                step: step.fcopy(),
            },
            SSAExpression::WhileLoop {
                cond,
                block,
                e2,
                pool_id,
                cond_expr,
            } => SSAExpression::WhileLoop {
                cond: cond.fcopy(),
                block: Box::new(block.fcopy()),
                e2: Box::new(e2.fcopy()),
                cond_expr: Box::new(cond_expr.fcopy()),
                pool_id: *pool_id,
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
    VariableReference(String, PoolID),
    Phi(Vec<Phi>),
    ConditionalBlock {
        if_block: SSAConditionalBlock,
        else_block: SSALabeledBlock,
        pool_id: PoolID,
    },
    Cast {
        value: Box<SSAValue>,
        to: Type,
        pool_id: PoolID,
    },
    Integer {
        value: i128,
        width: Option<IntegerWidth>,
        pool_id: PoolID,
    },
    Float {
        value: f64,
        width: Option<u32>,
        pool_id: PoolID,
    },
    Bool(bool, PoolID),
    Operation {
        lhs: Box<SSAValue>,
        op: String,
        rhs: Box<SSAValue>,
        pool_id: PoolID,
    },
    FunctionCall {
        name: String,
        parameters: Vec<SSAValue>,
        pool_id: PoolID,
    },
    Nothing,
    Tensor(Vec<SSAValue>, PoolID),
}

/*
    Define the copying process for an SSA vlue
*/
impl FailureCopy for SSAValue {
    fn fcopy(&self) -> SSAValue {
        match self {
            SSAValue::Tensor(v, pid) => {
                SSAValue::Tensor(v.iter().map(|x| x.fcopy()).collect(), *pid)
            }
            SSAValue::Nothing => SSAValue::Nothing,
            SSAValue::VariableReference(r, pid) => SSAValue::VariableReference(r.clone(), *pid),
            SSAValue::Integer {
                value,
                width,
                pool_id,
            } => SSAValue::Integer {
                value: *value,
                width: *width,
                pool_id: *pool_id,
            },
            SSAValue::Float {
                value,
                width,
                pool_id,
            } => SSAValue::Float {
                value: *value,
                width: *width,
                pool_id: *pool_id,
            },
            SSAValue::Operation {
                lhs,
                op,
                rhs: _,
                pool_id,
            } => SSAValue::Operation {
                lhs: Box::new(lhs.fcopy()),
                op: op.clone(),
                rhs: Box::new(lhs.fcopy()),
                pool_id: *pool_id,
            },
            SSAValue::FunctionCall {
                name,
                parameters,
                pool_id,
            } => SSAValue::FunctionCall {
                name: name.clone(),
                parameters: parameters.iter().map(|x| x.fcopy()).collect(),
                pool_id: *pool_id,
            },
            SSAValue::Bool(b, pid) => SSAValue::Bool(*b, *pid),
            SSAValue::Phi(v) => SSAValue::Phi(v.clone()),
            SSAValue::ConditionalBlock {
                if_block,
                else_block,
                pool_id,
            } => SSAValue::ConditionalBlock {
                if_block: if_block.fcopy(),
                else_block: else_block.fcopy(),
                pool_id: *pool_id,
            },
            SSAValue::Cast { value, to, pool_id } => SSAValue::Cast {
                value: Box::new(value.fcopy()),
                to: to.fcopy(),
                pool_id: *pool_id,
            },
        }
    }
}
