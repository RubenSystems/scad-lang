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

// Definition of a block within the language, containing statements and an optional expression
#[derive(Debug)]
pub struct Block {
    statements: Vec<Statement>,     // List of statements within the block
    expression: Option<Expression>, // Optional expression at the end of the block
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

// Definition of a conditional block in the language
#[derive(Debug)]
pub struct ConditionalBlock {
    condition: Expression, // Condition for the block (boolean expression)
    block: Block,          // Block of statements to be executed if the condition is met
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
}

// Enumeration of different types of statements in the language
#[derive(Debug)]
pub enum Statement {
    ConstDecl(ConstDecl),       // Constant declaration statement
    VariableDecl(VariableDecl), // Constant declaration statement
    Loop(Loop),
    Expression(Expression), // Expression statement
}
