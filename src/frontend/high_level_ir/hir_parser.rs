use lazy_static::lazy_static;
use pest::{
    iterators::Pairs,
    pratt_parser::{Assoc, Op, PrattParser},
};
use pest_derive::Parser;

use crate::frontend::high_level_ir::ast_types::{Block, FunctionDefinition, FunctionName};

use super::ast_types::{
    ConditionalExpressionBlock, ConstDecl, Expression, ExpressionBlock, Float, ForLoop,
    FunctionCall, FunctionDecleration, Identifier, Integer, IntegerWidth, ProcedureDefinition,
    Statement, StatementBlock, Type, TypeName, VariableDecl, VariableName,
};

// use super::ast_types::{Numeric, Expression};

#[derive(Parser)]
#[grammar = "frontend/high_level_ir/scad.pest"]
pub struct SCADParser;

pub struct HIRParser {
    parser: PrattParser<Rule>,
}

impl HIRParser {
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

impl Default for HIRParser {
    fn default() -> Self {
        Self::new()
    }
}

lazy_static! {
    /// This is an example for using doc comment attributes
    static ref PARSER: HIRParser = HIRParser::new();

}

fn parse_type(tpe: pest::iterators::Pair<'_, Rule>) -> Type {
    match tpe.as_rule() {
        Rule::r#type => {
            let subtype = tpe
                .clone()
                .into_inner()
                .last()
                .unwrap()
                .as_str()
                .to_string();
            let dimensions: Vec<u32> = tpe
                .into_inner()
                .filter_map(|x| {
                    if let Rule::integer = x.as_rule() {
                        x.as_str().parse::<u32>().map(|x| Some(x)).unwrap_or(None)
                    } else {
                        None
                    }
                })
                .collect();

            Type {
                dimensions,
                subtype: TypeName(subtype),
            }
        }
        _ => unreachable!("PANICCCCCCCC invalid type"),
    }
}

fn parse_function_def_arg(arg: pest::iterators::Pair<'_, Rule>) -> (Identifier, Type) {
    let mut id = arg.into_inner();
    let identifier = Identifier(id.next().unwrap().as_str().into());
    let tpe = parse_type(id.next().unwrap());
    (identifier, tpe)
}

fn parse_function_def_args(tpe: pest::iterators::Pair<'_, Rule>) -> Vec<(Identifier, Type)> {
    tpe.into_inner().map(parse_function_def_arg).collect()
}

fn parse_function_call_arg(arg: pest::iterators::Pair<'_, Rule>) -> (Identifier, Expression) {
    let mut id = arg.into_inner();
    let identifier = Identifier(id.next().unwrap().as_str().into());
    let exp = parse(id.next().unwrap().into_inner());
    if let Statement::Expression(e) = exp {
        (identifier, e)
    } else {
        unreachable!("NOT AN EXPRESSION WAAAAA")
    }
}

fn parse_for_loop(lp: pest::iterators::Pair<'_, Rule>) -> Statement {
    let mut it = lp.into_inner();
    let identifier = Identifier(it.next().unwrap().as_str().to_string());
    let from = it.next().unwrap().as_str().parse::<usize>().unwrap();
    let to = it.next().unwrap().as_str().parse::<usize>().unwrap();
    let block = parse_statement_block(it.next().unwrap());

    Statement::ForLoop(ForLoop {
        variable: identifier,
        from,
        to,
        block,
    })
}

fn parse_function_call_args(tpe: pest::iterators::Pair<'_, Rule>) -> Vec<(Identifier, Expression)> {
    tpe.into_inner().map(parse_function_call_arg).collect()
}

fn parse_block(blk: pest::iterators::Pair<'_, Rule>) -> Block {
    let statements = blk.into_inner().map(|x| parse(x.into_inner())).collect();

    Block { statements }
}

fn parse_statement_block(blk: pest::iterators::Pair<'_, Rule>) -> StatementBlock {
    let statements = blk.into_inner().map(|x| parse(x.into_inner())).collect();

    StatementBlock { statements }
}

fn parse_expression_block(blk: pest::iterators::Pair<'_, Rule>) -> ExpressionBlock {
    let mut statements: Vec<Statement> = blk.into_inner().map(|x| parse(x.into_inner())).collect();
    let expression = Box::new(match statements.pop() {
        Some(e) => {
            let Statement::Expression(e) = e else {
                unreachable!("Last statement does not eval to expression")
            };
            e
        }
        None => unreachable!("Can't have expression block with no expression at the end!"),
    });

    ExpressionBlock {
        statements,
        expression,
    }
}

// fn parse_conditional_block(blk: pest::iterators::Pair<'_, Rule>) -> ConditionalStatementBlock {
//     let mut unparsed_it = blk.into_inner();
//     let a = unparsed_it.next().unwrap().into_inner();
//     let b = unparsed_it.next().unwrap();
//     let parsed_exp = parse(a);
//     let block = parse_block(b);

//     let Statement::Expression(e) = parsed_exp else {
//         unreachable!("Not expressions")
//     };

//     ConditionalStatementBlock {
//         condition: e,
//         block,
//     }
// }

fn parse_expression_conditional_block(
    blk: pest::iterators::Pair<'_, Rule>,
) -> ConditionalExpressionBlock {
    let mut unparsed_it = blk.into_inner();
    let a = unparsed_it.next().unwrap().into_inner();
    let b = unparsed_it.next().unwrap();
    let parsed_exp = parse(a);
    let block = parse_expression_block(b);

    let Statement::Expression(e) = parsed_exp else {
        unreachable!("Not expressions")
    };

    ConditionalExpressionBlock {
        condition: e,
        block,
    }
}

pub fn parse_pair(primary: pest::iterators::Pair<'_, Rule>) -> Statement {
    match primary.as_rule() {
        Rule::integer_literal => {
            let mut p = primary.into_inner();
            // unreachable!("{}", p);
            let value = p.next().unwrap().as_str().parse::<i128>().unwrap();
            let width = match p.next() {
                Some(a) => IntegerWidth::Variable(a.as_str().parse::<u32>().unwrap()),
                None => IntegerWidth::IndexType,
            };

            Statement::Expression(Expression::Integer(Integer { value, width }))
        }
        Rule::float_literal => {
            let mut p = primary.into_inner();
            let value = p.next().unwrap().as_str().parse::<f64>().unwrap();
            let width = p.next().unwrap().as_str().parse::<u32>().unwrap();
            Statement::Expression(Expression::Float(Float { value, width }))
        }
        Rule::const_decl => {
            let mut p = primary.into_inner();
            let identifier = VariableName(p.next().unwrap().as_str().into());
            let stype_unparsed = p.next().unwrap();
            let parsed_stype = parse_type(stype_unparsed);

            let Statement::Expression(expression) = parse(p.next().unwrap().into_inner()) else {
                unreachable!("NOT AN EXPRESSION!");
            };

            Statement::ConstDecl(ConstDecl {
                identifier,
                subtype: parsed_stype,
                expression,
            })
        }
        Rule::for_loop => parse_for_loop(primary),
        Rule::tensor => Statement::Expression(Expression::Tensor(
            primary
                .into_inner()
                .map(|x| {
                    let Statement::Expression(e) = parse_pair(x) else {
                        unreachable!("You can't do that bro");
                    };
                    e
                })
                .collect(),
        )),
        Rule::var_decl => {
            let mut p = primary.into_inner();
            let identifier = VariableName(p.next().unwrap().as_str().into());
            let stype_unparsed = p.next().unwrap();
            let parsed_stype = parse_type(stype_unparsed);

            let Statement::Expression(expression) = parse(p.next().unwrap().into_inner()) else {
                unreachable!("NOT AN EXPRESSION!");
            };

            Statement::VariableDecl(VariableDecl {
                identifier,
                subtype: Some(parsed_stype),
                expression,
            })
        }

        Rule::var_reassignment => {
            let mut p = primary.into_inner();
            let identifier = VariableName(p.next().unwrap().as_str().trim().into());
            let Statement::Expression(expression) = parse(p.next().unwrap().into_inner()) else {
                unreachable!("NOT AN EXPRESSION!");
            };
            Statement::VariableDecl(VariableDecl {
                identifier,
                subtype: None,
                expression,
            })
        }
        Rule::infix_operation => parse(primary.into_inner()),
        Rule::binary_infix_operation => parse(primary.into_inner()),
        Rule::identifier => Statement::Expression(Expression::Identifier(Identifier(
            primary.as_str().trim().into(),
        ))),
        Rule::function_call => {
            let mut it: Pairs<'_, Rule> = primary.into_inner();
            let identifier = FunctionName(it.next().unwrap().as_str().into());

            let args = match it.next() {
                Some(x) => parse_function_call_args(x),
                _ => vec![],
            };

            Statement::Expression(Expression::FunctionCall(FunctionCall { identifier, args }))
        }
        Rule::function_definition => {
            // on

            let mut it: Pairs<'_, Rule> = primary.into_inner();
            let name: String = it.next().unwrap().as_str().into();
            let mut nxt = it.next().unwrap();
            let args = match nxt.as_rule() {
                Rule::function_def_params => {
                    let res = Some(parse_function_def_args(nxt));
                    nxt = it.next().unwrap();
                    res
                }
                _ => None,
            }
            .unwrap_or(vec![]);

            let return_type = parse_type(nxt);
            nxt = it.next().unwrap();

            let def = FunctionDefinition {
                identifier: FunctionName(name),
                args,
                return_type,
                block: parse_expression_block(nxt),
            };

            Statement::FunctionDefinition(def)
        }
        Rule::procedure_definition => {
            // on

            let mut it: Pairs<'_, Rule> = primary.into_inner();
            let name: String = it.next().unwrap().as_str().into();
            let mut nxt = it.next().unwrap();
            let args = match nxt.as_rule() {
                Rule::function_def_params => {
                    let res = Some(parse_function_def_args(nxt));
                    nxt = it.next().unwrap();
                    res
                }
                _ => None,
            }
            .unwrap_or(vec![]);

            let def = ProcedureDefinition {
                identifier: FunctionName(name),
                args,
                block: parse_block(nxt),
            };

            Statement::ProcedureDefinition(def)
        }
        Rule::forward_function_decleration => {
            let mut it: Pairs<'_, Rule> = primary.into_inner();
            let name: String = it.next().unwrap().as_str().into();
            let mut nxt = it.next().unwrap();
            let args = match nxt.as_rule() {
                Rule::function_def_params => {
                    let res = Some(parse_function_def_args(nxt));
                    nxt = it.next().unwrap();
                    res
                }
                _ => None,
            }
            .unwrap_or(vec![]);

            let return_type = parse_type(nxt);

            let def = FunctionDecleration {
                identifier: FunctionName(name),
                args,
                return_type,
            };

            Statement::FunctionDecleration(def)
        }
        Rule::if_expression_block => {
            todo!()

            // let mut it = primary.into_inner();
            // let cond_block = parse_expression_conditional_block(it.next().unwrap());
            // todo!()

            // let mut if_blocks: Vec<ConditionalExpressionBlock> = vec![];
            // let else_block: Option<Box<ExpressionBlock>> = None;
            // println!("{:#?}", primary.clone().into_inner());
            // primary.into_inner().for_each(|c| match c.as_rule() {
            //     Rule::if_expression_block if if_blocks.is_empty() => {
            //         let cond_block = parse_expression_conditional_block(c);
            //         if_blocks.push(cond_block)
            //     }
            //     Rule::else_if_expression_block => {
            //         let cond_block = parse_expression_conditional_block(c);
            //         if_blocks.push(cond_block);
            //     }
            //     Rule::else_expression_block => todo!(),
            //     Rule::if_expression_block if !if_blocks.is_empty() => {
            //         unreachable!("CAN'T HAVE MORE THAN ONE IF BLOCK!!!")
            //     }
            //     r => unreachable!("{r:#?} TRYING TO DO SOMETHING WEIRD!"),
            // });
            // Statement::Expression(Expression::ConditionalExpressionControlFlowControl {
            //     if_blocks,
            //     else_block: else_block.unwrap_or_else(|| {
            //         unreachable!("Expression if must be paired with else!")
            //     }),
            // })
        }

        Rule::statement | Rule::top_level_statement | Rule::expression => {
            parse(primary.into_inner())
        }
        Rule::if_expression_control_flow => {
            let it = primary.into_inner();
            let mut if_blocks = Vec::new();
            let mut else_block = None;
            for m in it {
                match m.as_rule() {
                    Rule::if_expression_block | Rule::else_if_expression_block => {
                        if_blocks.push(parse_expression_conditional_block(m))
                    }
                    Rule::else_expression_block => {
                        else_block = Some(parse_expression_block(m));
                    }
                    x => todo!("rule {x:?}"),
                };
            }

            Statement::Expression(Expression::ConditionalExpressionControlFlowControl {
                if_blocks,
                else_block: Box::new(else_block.unwrap()),
            })
        }
        Rule::boolean_t => Statement::Expression(Expression::Bool(if primary.as_str() == "true" {
            true
        } else if primary.as_str() == "false" {
            false
        } else {
            unreachable!("Undefined boolean")
        })),
        rule => {
            eprintln!("{}", primary);
            unreachable!("Expr::parse expected atom, found {:?}", rule);
        }
    }
}

pub fn parse(rules: Pairs<Rule>) -> Statement {
    PARSER
        .parser
        .map_primary(|p| parse_pair(p))
        .map_infix(|lhs, _, rhs| {
            let (Statement::Expression(lhs), Statement::Expression(rhs)) = (lhs, rhs) else {
                unreachable!();
            };

            let func_call = FunctionCall {
                identifier: FunctionName("scad.inbuilts.add".into()),
                args: vec![(Identifier("a".into()), lhs), (Identifier("a".into()), rhs)],
            };

            Statement::Expression(Expression::FunctionCall(func_call))
        })
        .parse(rules)
}
