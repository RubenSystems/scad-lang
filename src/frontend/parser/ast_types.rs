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
pub struct Integer(i128); // Terminal symbol for integer values
pub struct Float(f64); // Terminal symbol for floating-point values
pub struct CharArray(String); // Terminal symbol for character arrays
pub struct Identifier(String); // Terminal symbol for identifiers

// Definition of types within the language, including arrays and simple types
pub enum Type {
    Array {
        subtype: Box<Type>, // Array type with its subtype (element type)
        size: usize // Size of the array
    },
    SimpleType {
        identifier: String // Simple type identified by a name (identifier)
    }
}

// Definition of a block within the language, containing statements and an optional expression
pub struct Block {
    statements: Vec<Statement>, // List of statements within the block
    expression: Option<Expression> // Optional expression at the end of the block
}

// Definition of constant declarations in the language
pub struct ConstDecl {
    mutable: bool, // Indicates if the constant is mutable
    identifier: String, // Identifier for the constant
    subtype: Type, // Type of the constant
    expression: Expression // Expression representing the constant's value
}

// Definition of a conditional block in the language
pub struct ConditionalBlock {
    condition: BooleanExpression, // Condition for the block (boolean expression)
    block: Block // Block of statements to be executed if the condition is met
}

// Enumeration of numeric operations supported in the language
enum NumericOp {
    Add, // Addition operation
    Subtract, // Subtraction operation
    Multiply, // Multiplication operation
    Divide // Division operation
}

// Enumeration of different representations for numeric values
pub enum NumericRepr {
    Int(Integer), // Representation of an integer value
    Float(Float), // Representation of a floating-point value
    NumericExpression(NumericExpression), // Representation of a complex numeric expression
    Identifier(Identifier) // Representation of an identifier referring to a numeric value
}

// Definition of a single atomic numeric expression
pub struct NumericAtom {
    unary_minus: bool, // Indicates if the numeric value is negated
    numeric_expression: NumericRepr // The numeric representation
}

// Definition of a numeric expression consisting of an atomic value and a series of right-hand side operands
pub struct NumericExpression {
    atom: Box<NumericAtom>, // Atomic numeric value
    rhss: Vec<NumericAtom> // List of right-hand side operands
}

// Enumeration of logical 'AND' and 'OR' operators
pub enum BooleanJoins {
    And, // Logical 'AND' operator
    Or // Logical 'OR' operator
}

// Enumeration of binary boolean operations for comparisons
pub enum BinaryBooleanOp {
    LessThanEqual, // Less than or equal comparison
    GreatherThanEqual, // Greater than or equal comparison
    GreaterThan, // Greater than comparison
    LessThan, // Less than comparison
    Equality // Equality comparison
}

// Enumeration of different comparators in binary boolean expressions
pub enum BinaryBooleanComparitor {
    Int(Integer), // Comparator representing an integer value
    Float(Float), // Comparator representing a floating-point value
    Identifier(Identifier) // Comparator representing an identifier referring to a boolean value
}

// Enumeration of representations for boolean values and expressions
pub enum BooleanRepr {
    Int(Integer), // Representation of an integer value as a boolean
    Float(Float), // Representation of a float value as a boolean
    True, // Boolean 'true' value
    False, // Boolean 'false' value
    Identifier(Identifier), // Representation of an identifier referring to a boolean value
    BinaryBooleanExpression {
        lhs: BinaryBooleanComparitor, // Left-hand side comparator in the binary boolean expression
        op: BinaryBooleanOp, // Operator for the comparison
        rhs: BinaryBooleanComparitor // Right-hand side comparator in the binary boolean expression
    }
}

// Definition of a single atomic boolean expression
pub struct BooleanAtom {
    not: bool, // Indicates if the boolean value is negated
    repr: BooleanRepr // The boolean representation
}

// Definition of a boolean expression consisting of an atomic value and a series of right-hand side atoms
pub struct BooleanExpression {
    repr: BooleanAtom, // Atomic boolean value
    rhss: Vec<(BooleanJoins, BooleanAtom)> // List of right-hand side atoms combined with boolean joins
}

// Enumeration of major building blocks of the language expressions
pub enum Expression {
    BooleanExpression(BooleanExpression), // Boolean expressions
    NumericExpression(NumericExpression), // Numeric expressions
    Float(Float), // Floating-point values
    Integer(Integer), // Integer values
    CharArray(CharArray), // Character arrays
    Identifier(Identifier) // Identifiers
}

// Enumeration of different types of statements in the language
pub enum Statement {
    ConstDecl(ConstDecl), // Constant declaration statement
    IfControlFlow {
        if_block: ConditionalBlock, // If condition block
        else_ifs: Vec<ConditionalBlock>, // List of else-if condition blocks
        else_block: Option<Block> // Optional else block if no conditions are met
    },
    Loop {
        condition: Option<BooleanExpression>, // Optional condition for loop execution
        block: Block // Block of statements within the loop
    },
    Expression(Expression) // Expression statement
}