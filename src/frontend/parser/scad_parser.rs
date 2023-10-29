use pest::{
    iterators::Pairs,
    pratt_parser::{Assoc, Op, PrattParser},
};
use pest_derive::Parser;

use super::ast_types::{
    ConstDecl, Expression, Float, Identifier, Integer, NumericExpression, NumericOp, Statement,
    Type, TypeName, VariableName,
};

// use super::ast_types::{Numeric, Expression};

#[derive(Parser)]
#[grammar = "frontend/parser/scad.pest"]
pub struct SCADParser;

pub struct ParserToAST {
    parser: PrattParser<Rule>,
}

impl ParserToAST {
    pub fn new() -> Self {
        Self {
            parser: PrattParser::new()
                .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::subtract, Assoc::Left))
                .op(Op::infix(Rule::multiply, Assoc::Left) | Op::infix(Rule::divide, Assoc::Left))
                .op(Op::prefix(Rule::unary_minus)),
        }
    }
}

impl ParserToAST {
    fn parse_type(&self, tpe: pest::iterators::Pair<'_, Rule>) -> Type {
        match tpe.as_rule() {
            Rule::r#type => self.parse_type(tpe),
            Rule::simple_type => Type::SimpleType {
                identifier: TypeName(tpe.as_str().into()),
            },
            Rule::array_type => {
                println!("GOT A TYPE {tpe:?}");
                let mut p = tpe.into_inner();
                let typename = self.parse_type(p.next().unwrap().into());
                let size = p.next().unwrap().as_str().parse::<usize>().unwrap();
                Type::Array {
                    subtype: Box::new(typename),
                    size: size,
                }
            }
            _ => unreachable!("PANICCCCCCCC invalid type"),
        }
    }

    pub fn parse(&self, rules: Pairs<Rule>) -> Statement {
        self.parser
            .map_primary(
                |primary: pest::iterators::Pair<'_, Rule>| match primary.as_rule() {
                    Rule::integer => Statement::Expression(Expression::Integer(Integer(
                        primary.as_str().parse::<i128>().unwrap(),
                    ))),
                    Rule::float => Statement::Expression(Expression::Float(Float(
                        primary.as_str().parse::<f64>().unwrap(),
                    ))),
                    // Rule::char_array => todo!(),
                    // Rule::identifier => todo!(),
                    // Rule::boolean_t => todo!(),

                    // Rule::mutable_modifier => todo!(),
                    Rule::const_decl => {
                        let mut p = primary.into_inner();
                        let identifier = VariableName(p.next().unwrap().as_str().into());
                        let stype_unparsed = p.next().unwrap();
                        let parsed_stype = self.parse_type(stype_unparsed);

                        let Statement::Expression(expression) =
                            self.parse(p.next().unwrap().into_inner())
                        else {
                            unreachable!("NOT AN EXPRESSION!");
                        };

                        Statement::ConstDecl(ConstDecl {
                            identifier: identifier,
                            subtype: parsed_stype,
                            expression: expression,
                        })
                    }
                    // Rule::block => todo!(),
                    // Rule::if_control_flow => todo!(),
                    // Rule::if_block => todo!(),
                    // Rule::else_if_block => todo!(),
                    // Rule::else_block => todo!(),
                    // Rule::r#loop => todo!(),
                    // Rule::numeric_op => todo!(),
                    // Rule::add => todo!(),
                    // Rule::subtract => todo!(),
                    // Rule::multiply => todo!(),
                    // Rule::divide => todo!(),
                    // Rule::unary_minus => todo!(),
                    // Rule::numeric => todo!(),
                    Rule::numeric_atom => self.parse(primary.into_inner()),
                    Rule::numeric_expression => self.parse(primary.into_inner()),
                    // Rule::unary_boolean_op => todo!(),
                    // Rule::not => todo!(),
                    // Rule::boolean_joins => todo!(),
                    // Rule::and => todo!(),
                    // Rule::or => todo!(),
                    // Rule::binary_boolean_op => todo!(),
                    // Rule::greater_than => todo!(),
                    // Rule::less_than => todo!(),
                    // Rule::equality => todo!(),
                    // Rule::less_than_equal => todo!(),
                    // Rule::greather_than_equal => todo!(),
                    // Rule::binary_boolean_comparitors => todo!(),
                    // Rule::binary_boolean_expression => todo!(),
                    // Rule::boolean => todo!(),
                    // Rule::boolean_atom => todo!(),
                    // Rule::boolean_expression => todo!(),
                    Rule::expression => self.parse(primary.into_inner()),
                    Rule::statement => self.parse(primary.into_inner()),
                    Rule::statements => self.parse(primary.into_inner()),
                    // Rule::program => todo!(),
                    rule => {
                        eprintln!("{}", primary);
                        unreachable!("Expr::parse expected atom, found {:?}", rule);
                    }
                },
            )
            // .map_prefix(|op, rhs| match op.as_rule() {
            //     Rule::unary_minus  => -rhs,
            //     _          => unreachable!(),
            // })
            .map_infix(|lhs, op, rhs| match op.as_rule() {
                Rule::add => {
                    let (Statement::Expression(lhs), Statement::Expression(rhs)) = (lhs, rhs)
                    else {
                        unreachable!();
                    };

                    Statement::Expression(Expression::NumericExpression(NumericExpression {
                        lhs: Box::new(lhs),
                        op: NumericOp::Add,
                        rhs: Box::new(rhs),
                    }))
                }
                Rule::subtract => {
                    let (Statement::Expression(lhs), Statement::Expression(rhs)) = (lhs, rhs)
                    else {
                        unreachable!();
                    };

                    Statement::Expression(Expression::NumericExpression(NumericExpression {
                        lhs: Box::new(lhs),
                        op: NumericOp::Subtract,
                        rhs: Box::new(rhs),
                    }))
                }
                Rule::multiply => {
                    let (Statement::Expression(lhs), Statement::Expression(rhs)) = (lhs, rhs)
                    else {
                        unreachable!();
                    };

                    Statement::Expression(Expression::NumericExpression(NumericExpression {
                        lhs: Box::new(lhs),
                        op: NumericOp::Subtract,
                        rhs: Box::new(rhs),
                    }))
                }
                Rule::divide => {
                    let (Statement::Expression(lhs), Statement::Expression(rhs)) = (lhs, rhs)
                    else {
                        unreachable!();
                    };

                    Statement::Expression(Expression::NumericExpression(NumericExpression {
                        lhs: Box::new(lhs),
                        op: NumericOp::Divide,
                        rhs: Box::new(rhs),
                    }))
                }
                rule => unreachable!("Can't parse infix operator {:?}", rule),
            })
            .parse(rules)
    }
}
