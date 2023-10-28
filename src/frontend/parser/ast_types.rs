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


// Terminals
pub struct TrueT;
pub struct FalseT;
pub struct Integer(i128);
pub struct Float(f64);

pub struct CharArray(String);
pub struct Identifier(String);


pub enum Type {
	Array {
		subtype: Box<Type>,
		size: usize
	}, 
	SimpleType {
		identifier: String
	}
}

pub struct Block {
	statements: Vec<Statement>,
	expression: Option<Expression>
}

pub struct ConstDecl {
	mutable: bool, 
	identifier: String, 
	subtype: Type, 
	expression: Expression
}

pub struct ConditionalBlock {
	condition: BooleanExpression, 
	block: Block
}

// Numerics
enum NumericOp {
	Add,
	Subtract,
	Multiply, 
	Divide
}

pub enum NumericRepr {
	Int(Integer),
	Float(Float),
	NumericExpression(NumericExpression),
	Identifier(Identifier)
}

pub struct NumericAtom {
	unary_minus: bool, 
	numeric_expression: NumericRepr
}

pub struct NumericExpression {
	atom: Box<NumericAtom>, 
	rhss: Vec<NumericAtom>
}

// Booleans
pub enum BooleanJoins {
	And, 
	Or
}

pub enum BinaryBooleanOp {
	LessThanEqual, 
	GreatherThanEqual, 
	GreaterThan, 
	LessThan, 
	Equality
}

pub enum BinaryBooleanComparitor {
	Int(Integer),
	Float(Float),
	Identifier(Identifier)
}

pub enum BooleanRepr {
	Int(Integer),
	Float(Float),
	True, 
	False,
	Identifier(Identifier),
	BinaryBooleanExpression {
		lhs: BinaryBooleanComparitor, 
		op: BinaryBooleanOp, 
		rhs: BinaryBooleanComparitor
	}
}

pub struct BooleanAtom {
	not: bool, 
	repr: BooleanRepr
}

pub struct BooleanExpression {
	repr: BooleanAtom, 
	rhss: Vec<(BooleanJoins, BooleanAtom)>
}

// major building blocks

pub enum Expression {
	BooleanExpression(BooleanExpression),
	NumericExpression(NumericExpression), 
	Float(Float), 
	Integer(Integer),
	CharArray(CharArray),
	Identifier(Identifier)
}

pub enum Statement {
	ConstDecl(ConstDecl),
	IfControlFlow {
		if_block: ConditionalBlock,
		else_ifs: Vec<ConditionalBlock>,
		else_block: Option<Block>
	}, 
	Loop { 
		condition: Option<BooleanExpression>, 
		block: Block
	}, 
	Expression(Expression)
}
