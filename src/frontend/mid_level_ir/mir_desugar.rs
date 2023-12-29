use super::mir_ast_types::{Phi, SSAConditionalBlock, SSAExpression, SSALabeledBlock, SSAValue};

fn scoped_rename(existing_name: &str, scoped_name: &str) -> String {
    format!("{scoped_name}.{existing_name}")
}

pub fn rename_variables_value(value: SSAValue, scoped_name: &str) -> SSAValue {
    match value {
        SSAValue::VariableReference(name) => {
            SSAValue::VariableReference(scoped_rename(&name, scoped_name))
        }
        SSAValue::Phi(p) => SSAValue::Phi(
            p.into_iter()
                .map(|x| Phi {
                    branch_name: scoped_rename(&x.branch_name, scoped_name),
                    value: rename_variables_value(
                        x.value,
                        &scoped_rename(&x.branch_name, scoped_name),
                    ),
                })
                .collect(),
        ),
        SSAValue::Integer(i) => SSAValue::Integer(i),
        SSAValue::Float(f) => SSAValue::Float(f),
        SSAValue::Bool(b) => SSAValue::Bool(b),
        SSAValue::Operation {
            lhs: _,
            op: _,
            rhs: _,
        } => todo!(),
        SSAValue::FunctionCall { name, parameters } => SSAValue::FunctionCall {
            name,
            parameters: parameters
                .into_iter()
                .map(|v| rename_variables_value(v, scoped_name))
                .collect(),
        },
        SSAValue::Nothing => SSAValue::Nothing,
    }
}

pub fn rename_variables(expression: SSAExpression, scoped_name: &str) -> SSAExpression {
    match expression {
        SSAExpression::VariableDecl {
            name,
            vtype,
            e1,
            e2,
        } => SSAExpression::VariableDecl {
            name: scoped_rename(&name, scoped_name),
            vtype,
            e1,
            e2: Box::new(rename_variables(*e2, scoped_name)),
        },
        SSAExpression::FuncDecl {
            name,
            args,
            ret_type,
            block,
            e2,
        } => SSAExpression::FuncDecl {
            block: Box::new(rename_variables(*block, &format!("{scoped_name}.{name}"))),
            name,
            args: args
                .into_iter()
                .map(|(name, tpe)| (scoped_rename(&name, scoped_name), tpe))
                .collect(),
            e2: Box::new(rename_variables(*e2, scoped_name)),
            ret_type,
        },
        SSAExpression::FuncForwardDecl {
            name,
            args,
            ret_type,
            e2,
        } => SSAExpression::FuncForwardDecl {
            name,
            args: args
                .into_iter()
                .map(|(name, tpe)| (scoped_rename(&name, scoped_name), tpe))
                .collect(),
            e2: Box::new(rename_variables(*e2, scoped_name)),
            ret_type,
        },
        SSAExpression::Noop => SSAExpression::Noop,
        SSAExpression::Return { val } => SSAExpression::Return {
            val: rename_variables_value(val, scoped_name),
        },
        SSAExpression::Block(b) => {
            SSAExpression::Block(Box::new(rename_variables(*b, scoped_name)))
        }
        SSAExpression::ConditionalBlock {
            if_block,
            else_block,
            e2,
        } => SSAExpression::ConditionalBlock {
            if_block: SSAConditionalBlock {
                condition: rename_variables_value(if_block.condition, scoped_name),
                block: SSALabeledBlock {
                    label: scoped_rename(&if_block.block.label, scoped_name),
                    block: Box::new(rename_variables(
                        *if_block.block.block,
                        &scoped_rename(&if_block.block.label, scoped_name),
                    )),
                },
            },
            else_block: SSALabeledBlock {
                label: scoped_rename(&else_block.label, scoped_name),
                block: Box::new(rename_variables(
                    *else_block.block,
                    &scoped_rename(&else_block.label, scoped_name),
                )),
            },
            e2: Box::new(rename_variables(*e2, scoped_name)),
        },
    }
}
