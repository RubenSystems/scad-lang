use pest::{
    iterators::Pairs,
    pratt_parser::{Assoc, Op, PrattParser},
};
use pest_derive::Parser;

use crate::frontend::layer1::ast_types::{Block, FunctionDefinition, FunctionName};

use super::ast_types::{
    ConstDecl, Expression, Float, FunctionCall, Identifier, InfixOperation, InfixOperator, Integer,
    Statement, Type, TypeName, VariableDecl, VariableName,
};

// use super::ast_types::{Numeric, Expression};

#[derive(Parser)]
#[grammar = "frontend/layer1/scad.pest"]
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

    fn parse_function_def_arg(&self, arg: pest::iterators::Pair<'_, Rule>) -> (Identifier, Type) {
        let mut id = arg.into_inner();
        let identifier = Identifier(id.next().unwrap().as_str().into());
        let tpe = self.parse_type(id.next().unwrap());
        (identifier, tpe)
    }

    fn parse_function_def_args(
        &self,
        tpe: pest::iterators::Pair<'_, Rule>,
    ) -> Vec<(Identifier, Type)> {
        tpe.into_inner()
            .map(|x| self.parse_function_def_arg(x))
            .collect()
    }

    fn parse_function_call_arg(
        &self,
        arg: pest::iterators::Pair<'_, Rule>,
    ) -> (Identifier, Expression) {
        let mut id = arg.into_inner();
        let identifier = Identifier(id.next().unwrap().as_str().into());
        let exp = self.parse(id.next().unwrap().into_inner());
        if let Statement::Expression(e) = exp {
            (identifier, e)
        } else {
            unreachable!("NOT AN EXPRESSION WAAAAA")
        }
    }

    fn parse_function_call_args(
        &self,
        tpe: pest::iterators::Pair<'_, Rule>,
    ) -> Vec<(Identifier, Expression)> {
        tpe.into_inner()
            .map(|x| self.parse_function_call_arg(x))
            .collect()
    }

    fn parse_block(&self, blk: pest::iterators::Pair<'_, Rule>) -> Vec<Statement> {
        blk.into_inner()
            .map(|x| self.parse(x.into_inner()))
            .collect()
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
                    Rule::identifier => Statement::Expression(Expression::Identifier(Identifier(
                        primary.as_str().into(),
                    ))),
                    Rule::function_call => {
                        let mut it: Pairs<'_, Rule> = primary.into_inner();
                        let identifier = FunctionName(it.next().unwrap().as_str().into());
                        let args = self.parse_function_call_args(it.next().unwrap());

                        Statement::Expression(Expression::FunctionCall(FunctionCall {
                            identifier,
                            args,
                        }))
                    }
                    Rule::function_definition => {
                        // on

                        let mut it: Pairs<'_, Rule> = primary.into_inner();
                        let name: String = it.next().unwrap().as_str().into();
                        let mut nxt = it.next().unwrap();
                        let args = match nxt.as_rule() {
                            Rule::function_def_params => {
                                let res = Some(self.parse_function_def_args(nxt));
                                nxt = it.next().unwrap();
                                res
                            }
                            _ => None,
                        }
                        .unwrap_or(vec![]);

                        let return_type = match nxt.as_rule() {
                            Rule::r#type | Rule::simple_type | Rule::array_type => {
                                let res = Some(self.parse_type(nxt));
                                nxt = it.next().unwrap();
                                res
                            }
                            _ => None,
                        };

                        let def = FunctionDefinition {
                            identifier: FunctionName(name),
                            args: args,
                            return_type: return_type,
                            block: Block {
                                statements: self.parse_block(nxt),
                            },
                        };

                        Statement::FunctionDefinition(def)
                    }
                    Rule::block | Rule::expression_block | Rule::statement_block => {
                        Statement::Expression(Expression::Block(Block {
                            statements: self.parse_block(primary.into_inner().next().unwrap()),
                        }))
                    }
                    Rule::expression => self.parse(primary.into_inner()),
                    Rule::statement => self.parse(primary.into_inner()),
                    Rule::statements => self.parse(primary.into_inner()),
                    rule => {
                        eprintln!("{}", primary);
                        unreachable!("Expr::parse expected atom, found {:?}", rule);
                    }
                },
            )
            .map_infix(|lhs, op, rhs| {
                let (Statement::Expression(lhs), Statement::Expression(rhs)) = (lhs, rhs) else {
                    unreachable!();
                };

                Statement::Expression(Expression::InfixOperation(InfixOperation {
                    lhs: Box::new(lhs),
                    op: InfixOperator(op.as_str().into()),
                    rhs: Box::new(rhs),
                }))
            })
            .parse(rules)
    }
}
