// AST for Types
enum Type {
    ArrayType { element_type: Box<Type>, size: i64 },
    SimpleType(String), // Identifier
}

// AST for Variable Declarations
enum VariableDeclaration {
    Constant {
        mutable: bool,
        identifier: String,
        var_type: Type,
        expression: Expression,
    },
}

// AST for Blocks
enum Block {
    Statements(Vec<Statement>),
    StatementAndExpression(Vec<Statement>, Expression),
}

// AST for If Statements
enum IfStatement {
    If { condition: BooleanExpression, body: Block },
    ElseIf { condition: BooleanExpression, body: Block },
    Else { body: Block },
}

// AST for Loops
enum Loop {
    ConditionallyInfiniteLoop { condition: Option<BooleanExpression>, body: Block },
}

// AST for Numerics
enum NumericExpression {
    AtomicNumeric { value: Numeric },
    UnaryOperation { op: String, operand: Box<NumericExpression> },
    BinaryOperation {
        op: String,
        left: Box<NumericExpression>,
        right: Box<NumericExpression>,
    },
}

enum Numeric {
    Float(f64),
    Integer(i64),
    Variable(String), // Identifier
}

// AST for Booleans
enum BooleanExpression {
    AtomicBoolean { value: Boolean },
    UnaryBooleanOperation { op: String, operand: Box<BooleanExpression> },
    BinaryBooleanOperation {
        op: String,
        left: Box<BooleanExpression>,
        right: Box<BooleanExpression>,
    },
}

enum Boolean {
    BooleanLiteral(bool),
    Variable(String), // Identifier
    NumericExpression(NumericExpression),
}

// AST for Major Building Blocks
enum Expression {
    BooleanExpr(BooleanExpression),
    NumericExpr(NumericExpression),
    Float(f64),
    Integer(i64),
    CharArray(String),
    Identifier(String),
}

enum Statement {
    ConstantDeclaration(VariableDeclaration),
    IfControlFlow(IfStatement),
    Loop(Loop),
    Expr(Expression),
}

// AST for Program
struct Program {
    statements: Vec<Statement>,
}
