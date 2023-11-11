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
    pub return_type: Option<Type>,
    pub block: Block,
}

#[derive(Debug)]
pub struct FunctionCall {
    pub identifier: FunctionName,
    pub args: Vec<(Identifier, Expression)>,
}

// Definition of a conditional block in the language
#[derive(Debug)]
pub struct ConditionalBlock {
    pub condition: Expression, // Condition for the block (boolean expression)
    pub block: Block,          // Block of statements to be executed if the condition is met
}

#[derive(Debug)]
pub enum Loop {
    Conditional(ConditionalLoop),
    NonconditionalLoop(NonconditionalLoop),
}

#[derive(Debug)]
pub struct NonconditionalLoop {
    pub block: Block,
}

#[derive(Debug)]
pub struct ConditionalLoop {
    pub block: Block,
    pub conditional: Expression,
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
    IfControlFlow {
        if_block: Box<ConditionalBlock>, // If condition block
        else_ifs: Vec<ConditionalBlock>, // List of else-if condition blocks
        else_block: Option<Box<Block>>,  // Optional else block if no conditions are met
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
            Expression::IfControlFlow {
                if_block: _,
                else_ifs: _,
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
    Loop(Loop),
    Expression(Expression), // Expression statement
    FunctionDefinition(FunctionDefinition),
}

impl FailureCopy for Statement {
    fn fcopy(&self) -> Self {
        match self {
            Statement::ConstDecl(c) => Statement::ConstDecl(ConstDecl {
                identifier: VariableName(c.identifier.0.clone()),
                subtype: c.subtype.fcopy(),
                expression: c.expression.fcopy(),
            }),
            Statement::VariableDecl(v) => Statement::VariableDecl(VariableDecl {
                identifier: VariableName(v.identifier.0.clone()),
                subtype: v.subtype.fcopy(),
                expression: v.expression.fcopy(),
            }),
            Statement::Loop(_) => todo!(),
            Statement::Expression(e) => Statement::Expression(e.fcopy()),
            Statement::FunctionDefinition(f) => {
                let ret_type = match &f.return_type {
                    Some(r) => Some(r.fcopy()),
                    _ => None,
                };

                Statement::FunctionDefinition(FunctionDefinition {
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
        }
    }
}
