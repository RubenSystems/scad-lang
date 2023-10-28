use pest::{pratt_parser::{PrattParser, Assoc, Op}, iterators::Pairs};
use pest_derive::Parser;

// use super::ast_types::{Numeric, Expression};

#[derive(Parser)]
#[grammar = "frontend/parser/scad.pest"]
pub struct SCADParser;


pub struct ParserToAST {
	parser: PrattParser<Rule>
}

impl ParserToAST {
	pub fn new() -> Self {
		Self {
			parser: PrattParser::new()
				.op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::subtract, Assoc::Left))
				.op(Op::infix(Rule::multiply, Assoc::Left) | Op::infix(Rule::divide, Assoc::Left))
				.op(Op::prefix(Rule::unary_minus))
		}
	}
}
/* 
impl ParserToAST {
	pub fn parse(&self, rules: Pairs<Rule>) { 

		self.parser
        .map_primary(|primary| match primary.as_rule() {
            Rule::integer => Numeric::Integer(primary.as_str().parse::<i128>().unwrap()),
            Rule::float => Numeric::Float(primary.as_str().parse::<f64>().unwrap()),
            Rule::char_array => Expression::CharArray(primary.as_str()),
            Rule::identifier => todo!(),
            Rule::boolean_t => todo!(),
            Rule::r#type => todo!(),
            Rule::array_type => todo!(),
            Rule::simple_type => todo!(),
            Rule::mutable_modifier => todo!(),
            Rule::const_decl => todo!(),
            Rule::block => todo!(),
            Rule::if_control_flow => todo!(),
            Rule::if_block => todo!(),
            Rule::else_if_block => todo!(),
            Rule::else_block => todo!(),
            Rule::r#loop => todo!(),
            Rule::numeric_op => todo!(),
            Rule::add => todo!(),
            Rule::subtract => todo!(),
            Rule::multiply => todo!(),
            Rule::divide => todo!(),
            Rule::unary_minus => todo!(),
            Rule::numeric => todo!(),
            Rule::numeric_atom => todo!(),
            Rule::numeric_expression => todo!(),
            Rule::unary_boolean_op => todo!(),
            Rule::not => todo!(),
            Rule::boolean_joins => todo!(),
            Rule::and => todo!(),
            Rule::or => todo!(),
            Rule::binary_boolean_op => todo!(),
            Rule::greater_than => todo!(),
            Rule::less_than => todo!(),
            Rule::equality => todo!(),
            Rule::less_than_equal => todo!(),
            Rule::greather_than_equal => todo!(),
            Rule::binary_boolean_comparitors => todo!(),
            Rule::binary_boolean_expression => todo!(),
            Rule::boolean => todo!(),
            Rule::boolean_atom => todo!(),
            Rule::boolean_expression => todo!(),
            Rule::expression => todo!(),
            Rule::statement => todo!(),
            Rule::statements => todo!(),
            Rule::program => todo!(),
            // Rule::int  => primary.as_str().parse().unwrap(),
            // Rule::expr => parse_expr(primary.into_inner(), pratt), // from "(" ~ expr ~ ")"
            _          => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::unary_minus  => -rhs,
            _          => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::add  => lhs + rhs,
            Rule::sub  => lhs - rhs,
            Rule::mul  => lhs * rhs,
            Rule::div  => lhs / rhs,
            Rule::pow  => (1..rhs+1).map(|_| lhs).product(),
            _          => unreachable!(),
        })
        .parse(pairs);

	}
}

*/