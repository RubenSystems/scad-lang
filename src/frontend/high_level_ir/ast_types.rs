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

// When you are a failure, you copy. When you are a succes, you move
pub trait FailureCopy {
    fn fcopy(&self) -> Self;
}

#[derive(Debug)]
pub struct Integer(pub i128); // Terminal symbol for integer values

#[derive(Debug)]
pub struct Float(pub f64); // Terminal symbol for floating-point values

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

#[derive(Debug)]
pub struct TypeName(pub String);

// Definition of types within the language, including arrays and simple types
#[derive(Debug)]
pub enum Type {
    Array {
        subtype: Box<Type>, // Array type with its subtype (element type)
        size: usize,        // Size of the array
    },
    SimpleType {
        identifier: TypeName, // Simple type identified by a name (identifier)
    },
}

impl FailureCopy for Type {
    fn fcopy(&self) -> Self {
        match self {
            Type::Array { subtype, size } => Type::Array {
                subtype: Box::new(subtype.fcopy()),
                size: *size,
            },
            Type::SimpleType { identifier } => Type::SimpleType {
                identifier: TypeName(identifier.0.clone()),
            },
        }
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

impl FailureCopy for ExpressionBlock {
    fn fcopy(&self) -> Self {
        ExpressionBlock {
            statements: self.statements.iter().map(|e| e.fcopy()).collect(),
            expression: Box::new(self.expression.fcopy()),
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
    pub subtype: Type,            // Type of the constant
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

// Enumeration of major building blocks of the language expressions
#[derive(Debug)]
pub enum Expression {
    InfixOperation(InfixOperation), // Boolean expressions
    Float(Float),                   // Floating-point values
    Integer(Integer),               // Integer values
    CharArray(CharArray),           // Character arrays
    Identifier(Identifier),         // Identifiers
    ConditionalExpressionControlFlowControl {
        if_blocks: Vec<ConditionalExpressionBlock>, // List of else-if condition blocks
        else_block: Option<Box<ExpressionBlock>>,   // Optional else block if no conditions are met
    },
    FunctionCall(FunctionCall),
    Block(Block),
}

impl FailureCopy for Expression {
    fn fcopy(&self) -> Expression {
        match self {
            Expression::InfixOperation(i) => Expression::InfixOperation(InfixOperation {
                lhs: Box::new(i.lhs.fcopy()),
                op: InfixOperator(i.op.0.clone()),
                rhs: Box::new(i.rhs.fcopy()),
            }),
            Expression::Float(f) => Expression::Float(Float(f.0)),
            Expression::Integer(i) => Expression::Integer(Integer(i.0)),
            Expression::CharArray(_) => todo!(),
            Expression::Identifier(i) => Expression::Identifier(Identifier(i.0.clone())),
            Expression::ConditionalExpressionControlFlowControl {
                if_blocks: _,
                else_block: _,
            } => todo!(),
            Expression::FunctionCall(f) => Expression::FunctionCall(FunctionCall {
                identifier: FunctionName(f.identifier.0.clone()),
                args: f
                    .args
                    .iter()
                    .map(|(id, exp)| (Identifier(id.0.clone()), exp.fcopy()))
                    .collect(),
            }),
            Expression::Block(b) => Expression::Block(b.fcopy()),
        }
    }
}

// Enumeration of different types of statements in the language
#[derive(Debug)]
pub enum Statement {
    ConstDecl(ConstDecl),       // Constant declaration statement
    VariableDecl(VariableDecl), // Constant declaration statement
    Expression(Expression),     // Expression statement
    ConditionalStatementControlFlow {
        if_blocks: Vec<ConditionalStatementBlock>, // List of else-if condition blocks
        else_block: Option<Box<Block>>,            // Optional else block if no conditions are met
    },
    FunctionDefinition(FunctionDefinition),
    ProcedureDefinition(ProcedureDefinition),
}

impl FailureCopy for Statement {
    fn fcopy(&self) -> Self {
        match self {
            Self::ConstDecl(c) => Statement::ConstDecl(ConstDecl {
                identifier: VariableName(c.identifier.0.clone()),
                subtype: c.subtype.fcopy(),
                expression: c.expression.fcopy(),
            }),
            Self::VariableDecl(v) => Statement::VariableDecl(VariableDecl {
                identifier: VariableName(v.identifier.0.clone()),
                subtype: v.subtype.fcopy(),
                expression: v.expression.fcopy(),
            }),
            Self::Expression(e) => Statement::Expression(e.fcopy()),
            Self::ConditionalStatementControlFlow {
                if_blocks: _,
                else_block: _,
            } => todo!(),
            Self::FunctionDefinition(f) => {
                let ret_type = f.return_type.fcopy();

                Self::FunctionDefinition(FunctionDefinition {
                    identifier: FunctionName(f.identifier.0.clone()),
                    args: f
                        .args
                        .iter()
                        .map(|(id, tpe)| (Identifier(id.0.clone()), tpe.fcopy()))
                        .collect(),
                    return_type: ret_type,
                    block: f.block.fcopy(),
                })
            }
            Self::ProcedureDefinition(p) => Self::ProcedureDefinition(ProcedureDefinition {
                identifier: FunctionName(p.identifier.0.clone()),
                args: p
                    .args
                    .iter()
                    .map(|(id, tpe)| (Identifier(id.0.clone()), tpe.fcopy()))
                    .collect(),
                block: p.block.fcopy(),
            }),
        }
    }
}
