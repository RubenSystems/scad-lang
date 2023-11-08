use crate::frontend::parser::ast_types::{Expression, FailureCopy, Statement};

use super::{
    mir_ast_types::{SSAExpression, SSAValue},
    parsers::{generate_register_name, parse_function_block},
};

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
        Expression::Block(b) => todo!(),
        Expression::FunctionCall(f) => {
            fn aux(
                args: Vec<Expression>,
                vals: Vec<SSAValue>,
                function_name: String,
                k: Box<dyn FnOnce(SSAValue) -> SSAExpression>,
            ) -> SSAExpression {
                match args.as_slice() {
                    [] => {
                        let tmp_name = generate_register_name();

                        SSAExpression::RegisterDecl {
                            name: tmp_name.clone(),
                            e1: SSAValue::FunctionCall {
                                name: function_name,
                                parameters: vals.iter().map(|x| x.fcopy()).collect(),
                            },
                            e2: Box::new(k(SSAValue::RegisterReference(tmp_name))),
                        }
                    }
                    [head, rest @ ..] => {
                        let rest_copy: Vec<Expression> = rest.iter().map(|v| v.fcopy()).collect();
                        let mut val_copy: Vec<SSAValue> = vals.iter().map(|v| v.fcopy()).collect();

                        expression_ssa_transformation(
                            head.fcopy(),
                            Box::new(|x| {
                                val_copy.push(x);
                                aux(rest_copy, val_copy, function_name, k)
                            }),
                        )
                    }
                }
            }

            aux(
                f.args.iter().map(|(_, exp)| exp.fcopy()).collect(),
                vec![],
                f.identifier.0,
                k,
            )
        }
        Expression::IfControlFlow {
            if_block,
            else_ifs,
            else_block,
        } => todo!(),
    }
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
            block: parse_function_block(f.block),
        },
    }
}
