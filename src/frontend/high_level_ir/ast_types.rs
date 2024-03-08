//===- ast_types.rs - Abstract Syntax tree -----*- rust -*-===//
//
// Part of the SCaD Compiler,
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the Abstract Syntax Tree (AST) for SCaD.
/// Thispub  file contains data structures representing different elements
/// of the abstract syntax tree for the custom language.
///
//===----------------------------------------------------------------------===//

// Definition of terminal symbols representing basic data types and identifiers
use crate::frontend::{error::PoolID, type_system::tir_types::MonoType};

// When you are a failure, you copy. When you are a succes, you move
pub trait FailureCopy {
    fn fcopy(&self) -> Self;
}

#[derive(Debug, Clone, Copy)]
pub enum IntegerWidth {
    IndexType,
    Variable(u32),
}

#[derive(Debug)]
pub struct Integer {
    pub value: i128,
    pub width: IntegerWidth,
}

#[derive(Debug)]
pub struct Float {
    pub value: f64,
    pub width: u32,
} // Terminal symbol for floating-point values

#[derive(Debug)]
pub struct CharArray(pub String); // Terminal symbol for character arrays

#[derive(Debug)]
pub struct InfixOperator(pub String); // Terminal symbol for character arrays

#[derive(Debug)]
pub struct Identifier(pub String); // Terminal symbol for identifiers

#[derive(Debug)]
pub struct VariableName(pub String);

#[derive(Debug)]
pub struct FunctionName(pub String);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeName(pub String);

// Definition of types within the language, including arrays and simple types
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub dimensions: Vec<u32>,
    pub subtype: TypeName, // Array type with its subtype (element type)
}

impl Type {
    pub fn to_string(&self) -> String {
        let dimension_string: Vec<String> = self.dimensions.iter().map(|x| x.to_string()).collect();
        let fstring = format!("{}x{}", dimension_string.join("x"), self.subtype.0);
        println!("{fstring}");
        fstring
    }

    pub fn to_tir_type(&self) -> MonoType {
        MonoType::Application {
            c: self.subtype.0.clone(),
            dimensions: if self.dimensions.is_empty() {
                None
            } else {
                Some(self.dimensions.clone())
            },
            types: vec![],
        }
    }
}

impl FailureCopy for Type {
    fn fcopy(&self) -> Self {
        self.clone()
    }
}

// Definition of a block within the language, containing statements and an optional expression
#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>, // List of statements within the block
}

impl FailureCopy for Block {
    fn fcopy(&self) -> Self {
        Block {
            statements: self.statements.iter().map(|e| e.fcopy()).collect(),
        }
    }
}

// Definition of a block within the language, containing statements and an optional expression
#[derive(Debug)]
pub struct ExpressionBlock {
    pub statements: Vec<Statement>, // List of statements within the block
    pub expression: Box<Expression>,
}

#[derive(Debug)]
pub struct StatementBlock {
    pub statements: Vec<Statement>, // List of statements within the block
}

impl FailureCopy for ExpressionBlock {
    fn fcopy(&self) -> Self {
        ExpressionBlock {
            statements: self.statements.iter().map(|e| e.fcopy()).collect(),
            expression: Box::new(self.expression.fcopy()),
        }
    }
}

impl FailureCopy for StatementBlock {
    fn fcopy(&self) -> Self {
        Self {
            statements: self.statements.iter().map(|e| e.fcopy()).collect(),
        }
    }
}

// Definition of constant declarations in the language
#[derive(Debug)]
pub struct ConstDecl {
    pub identifier: VariableName, // Identifier for the constant
    pub subtype: Type,            // Type of the constant
    pub expression: Expression,   // Expression representing the constant's value
}

#[derive(Debug)]
pub struct VariableDecl {
    pub identifier: VariableName, // Identifier for the constant
    pub subtype: Option<Type>,    // Type of the constant
    pub expression: Expression,   // Expression representing the constant's value
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub identifier: FunctionName,
    pub args: Vec<(Identifier, Type)>,
    pub return_type: Type,
    pub block: ExpressionBlock,
}

#[derive(Debug)]
pub struct ProcedureDefinition {
    pub identifier: FunctionName,
    pub args: Vec<(Identifier, Type)>,
    pub block: Block,
}

#[derive(Debug)]
pub struct FunctionDecleration {
    pub identifier: FunctionName,
    pub args: Vec<(Identifier, Type)>,
    pub return_type: Type,
}

#[derive(Debug)]
pub struct ProcedureDecleration {
    pub identifier: FunctionName,
    pub args: Vec<(Identifier, Type)>,
}

#[derive(Debug)]
pub struct FunctionCall {
    pub identifier: FunctionName,
    pub args: Vec<(Identifier, Expression)>,
}

// Definition of a conditional block in the language
#[derive(Debug)]
pub struct ConditionalExpressionBlock {
    pub condition: Expression,  // Condition for the block (boolean expression)
    pub block: ExpressionBlock, // Block of statements to be executed if the condition is met
}

impl FailureCopy for ConditionalExpressionBlock {
    fn fcopy(&self) -> Self {
        ConditionalExpressionBlock {
            condition: self.condition.fcopy(),
            block: self.block.fcopy(),
        }
    }
}

#[derive(Debug)]
pub struct ForLoop {
    pub variable: Identifier, // Condition for the block (boolean expression)
    pub from: usize,
    pub to: usize,
    pub step: usize,
    pub unroll: usize,
    pub block: StatementBlock, // Block of statements to be executed if the condition is met
    pub parallel: bool,
}

impl FailureCopy for ForLoop {
    fn fcopy(&self) -> Self {
        Self {
            variable: Identifier(self.variable.0.clone()),
            from: self.from,
            to: self.to,
            block: self.block.fcopy(),
            parallel: self.parallel,
            step: self.step,
            unroll: self.unroll,
        }
    }
}

#[derive(Debug)]
pub struct WhileLoop {
    pub condition: Expression, // Condition for the block (boolean expression)
    pub block: StatementBlock, // Block of statements to be executed if the condition is met
}

impl FailureCopy for WhileLoop {
    fn fcopy(&self) -> Self {
        Self {
            condition: self.condition.fcopy(),
            block: self.block.fcopy(),
        }
    }
}

#[derive(Debug)]
pub struct ConditionalStatementBlock {
    pub condition: Expression, // Condition for the block (boolean expression)
    pub block: Block,          // Block of statements to be executed if the condition is met
}

impl FailureCopy for ConditionalStatementBlock {
    fn fcopy(&self) -> Self {
        ConditionalStatementBlock {
            condition: self.condition.fcopy(),
            block: self.block.fcopy(),
        }
    }
}

#[derive(Debug)]
pub struct InfixOperation {
    pub lhs: Box<Expression>,
    pub op: InfixOperator,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct Cast {
    pub expr: Box<Expression>,
    pub to_type: Type,
}

// Enumeration of major building blocks of the language expressions
#[derive(Debug)]
pub enum Expression {
    InfixOperation(InfixOperation, PoolID), // Boolean expressions
    Float(Float, PoolID),                   // Floating-point value
    Bool(bool, PoolID),
    Integer(Integer, PoolID),       // Integer values
    CharArray(CharArray, PoolID),   // Character arrays
    Identifier(Identifier, PoolID), // Identifiers
    ConditionalExpressionControlFlowControl {
        if_blocks: Vec<ConditionalExpressionBlock>, // List of else-if condition blocks
        else_block: Box<ExpressionBlock>,           // Optional else block if no conditions are met
        pool_id: PoolID,
    },
    FunctionCall(FunctionCall, PoolID),
    Block(ExpressionBlock, PoolID),
    Tensor(Vec<Expression>, PoolID),
    Cast(Cast, PoolID),
}

impl FailureCopy for Expression {
    fn fcopy(&self) -> Expression {
        match self {
            Expression::InfixOperation(i, pid) => Expression::InfixOperation(
                InfixOperation {
                    lhs: Box::new(i.lhs.fcopy()),
                    op: InfixOperator(i.op.0.clone()),
                    rhs: Box::new(i.rhs.fcopy()),
                },
                *pid,
            ),
            Expression::Float(f, pid) => Expression::Float(
                Float {
                    value: f.value,
                    width: f.width,
                },
                *pid,
            ),
            Expression::Integer(i, pid) => Expression::Integer(
                Integer {
                    value: i.value,
                    width: i.width,
                },
                *pid,
            ),
            Expression::CharArray(_, _) => todo!(),
            Expression::Identifier(i, pid) => Expression::Identifier(Identifier(i.0.clone()), *pid),
            Expression::ConditionalExpressionControlFlowControl {
                if_blocks,
                else_block,
                pool_id,
            } => Expression::ConditionalExpressionControlFlowControl {
                if_blocks: if_blocks.iter().map(|x| x.fcopy()).collect(),
                else_block: Box::new(else_block.fcopy()),
                pool_id: *pool_id,
            },
            Expression::FunctionCall(f, pid) => Expression::FunctionCall(
                FunctionCall {
                    identifier: FunctionName(f.identifier.0.clone()),
                    args: f
                        .args
                        .iter()
                        .map(|(id, exp)| (Identifier(id.0.clone()), exp.fcopy()))
                        .collect(),
                },
                *pid,
            ),
            Expression::Block(b, pid) => Expression::Block(b.fcopy(), *pid),
            Expression::Bool(b, pid) => Expression::Bool(*b, *pid),
            Expression::Tensor(a, pid) => {
                Expression::Tensor(a.iter().map(|x| x.fcopy()).collect(), *pid)
            }
            Expression::Cast(c, pid) => Expression::Cast(
                Cast {
                    expr: Box::new(c.expr.fcopy()),
                    to_type: c.to_type.fcopy(),
                },
                *pid,
            ),
        }
    }
}

// Enumeration of different types of statements in the language
#[derive(Debug)]
pub enum Statement {
    ConstDecl(ConstDecl, PoolID),       // Constant declaration statement
    VariableDecl(VariableDecl, PoolID), // Constant declaration statement
    Expression(Expression, PoolID),     // Expression statement
    FunctionDefinition(FunctionDefinition, PoolID),
    ProcedureDefinition(ProcedureDefinition, PoolID),
    ForLoop(ForLoop, PoolID),
    WhileLoop(WhileLoop, PoolID),
    FunctionDecleration(FunctionDecleration, PoolID),
    ProcedureDecleration(ProcedureDecleration, PoolID),
    Block(Block, PoolID),
}

impl FailureCopy for Statement {
    fn fcopy(&self) -> Self {
        match self {
            Self::ConstDecl(c, pid) => Statement::ConstDecl(
                ConstDecl {
                    identifier: VariableName(c.identifier.0.clone()),
                    subtype: c.subtype.fcopy(),
                    expression: c.expression.fcopy(),
                },
                *pid,
            ),
            Self::VariableDecl(v, pid) => Statement::VariableDecl(
                VariableDecl {
                    identifier: VariableName(v.identifier.0.clone()),
                    subtype: v.subtype.as_ref().map(|x| x.fcopy()),
                    expression: v.expression.fcopy(),
                },
                *pid,
            ),
            Self::Expression(e, pid) => Statement::Expression(e.fcopy(), *pid),
            Self::FunctionDefinition(f, pid) => {
                let ret_type = f.return_type.fcopy();

                Self::FunctionDefinition(
                    FunctionDefinition {
                        identifier: FunctionName(f.identifier.0.clone()),
                        args: f
                            .args
                            .iter()
                            .map(|(id, tpe)| (Identifier(id.0.clone()), tpe.fcopy()))
                            .collect(),
                        return_type: ret_type,
                        block: f.block.fcopy(),
                    },
                    *pid,
                )
            }
            Self::ProcedureDefinition(_p, _pid) => todo!(),
            Statement::Block(b, pid) => Statement::Block(
                Block {
                    statements: b.statements.iter().map(|s| s.fcopy()).collect(),
                },
                *pid,
            ),
            Statement::FunctionDecleration(f, pid) => {
                let ret_type = f.return_type.fcopy();

                Self::FunctionDecleration(
                    FunctionDecleration {
                        identifier: FunctionName(f.identifier.0.clone()),
                        args: f
                            .args
                            .iter()
                            .map(|(id, tpe)| (Identifier(id.0.clone()), tpe.fcopy()))
                            .collect(),
                        return_type: ret_type,
                    },
                    *pid,
                )
            }
            Statement::ProcedureDecleration(_, _) => todo!(),
            Statement::ForLoop(f, pid) => Statement::ForLoop(f.fcopy(), *pid),
            Statement::WhileLoop(w, pid) => Statement::WhileLoop(w.fcopy(), *pid),
        }
    }
}
