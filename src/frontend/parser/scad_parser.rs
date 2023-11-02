use pest::{
    iterators::Pairs,
    pratt_parser::{Assoc, Op, PrattParser},
};
use pest_derive::Parser;

use super::ast_types::{
    ConstDecl, Expression, Float, InfixOperation, InfixOperator, Integer, Statement, Type,
    TypeName, VariableDecl, VariableName,
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
                .op(Op::infix(Rule::greater, Assoc::Left)
                    | Op::infix(Rule::greater_equal, Assoc::Left))
                .op(Op::infix(Rule::equal, Assoc::Left)
                    | Op::infix(Rule::greater_equal, Assoc::Left))
                .op(Op::infix(Rule::and, Assoc::Left) | Op::infix(Rule::or, Assoc::Left))
                .op(Op::prefix(Rule::infix_operator)),
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
                    Rule::var_decl => {
                        let mut p = primary.into_inner();
                        let identifier = VariableName(p.next().unwrap().as_str().into());
                        let stype_unparsed = p.next().unwrap();
                        let parsed_stype = self.parse_type(stype_unparsed);

                        let Statement::Expression(expression) =
                            self.parse(p.next().unwrap().into_inner())
                        else {
                            unreachable!("NOT AN EXPRESSION!");
                        };

                        Statement::VariableDecl(VariableDecl {
                            identifier: identifier,
                            subtype: parsed_stype,
                            expression: expression,
                        })
                    }
                    Rule::infix_operation => self.parse(primary.into_inner()),
                    Rule::binary_infix_operation => self.parse(primary.into_inner()),

                    // Rule::r#loop => {

                    // }
                    // Rule::numeric_atom => self.parse(primary.into_inner()),
                    // Rule::numeric_expression => self.parse(primary.into_inner()),
                    Rule::expression => self.parse(primary.into_inner()),
                    Rule::statement => self.parse(primary.into_inner()),
                    Rule::statements => self.parse(primary.into_inner()),

                    rule => {
                        eprintln!("{}", primary);
                        unreachable!("Expr::parse expected atom, found {:?}", rule);
                    }
                },
            )
            .map_infix(|lhs, op, rhs| match op.as_rule() {
                // Rule::infix_operation => {
                //     let (Statement::Expression(lhs), Statement::Expression(rhs)) = (lhs, rhs)
                //     else {
                //         unreachable!();
                //     };

                //     Statement::Expression(Expression::InfixOperation(InfixOperation {
                //         lhs: Box::new(lhs),
                //         op: InfixOperator(op.as_str().into()),
                //         rhs: Box::new(rhs),
                //     }))
                // }
                rule => {
                    let (Statement::Expression(lhs), Statement::Expression(rhs)) = (lhs, rhs)
                    else {
                        unreachable!();
                    };

                    Statement::Expression(Expression::InfixOperation(InfixOperation {
                        lhs: Box::new(lhs),
                        op: InfixOperator(op.as_str().into()),
                        rhs: Box::new(rhs),
                    }))
                } // rule => unreachable!("Can't parse infix operator {:?}", rule),
            })
            .parse(rules)
    }
}
