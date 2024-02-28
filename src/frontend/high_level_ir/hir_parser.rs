use std::collections::HashMap;

use lazy_static::lazy_static;
use pest::{
    iterators::Pairs,
    pratt_parser::{Assoc, Op, PrattParser},
};
use pest_derive::Parser;

use crate::frontend::{
    error::{ErrorPool, PoolID},
    high_level_ir::ast_types::{Block, FunctionDefinition, FunctionName},
    mid_level_ir::ffi::Location,
};

use super::ast_types::{
    Cast, ConditionalExpressionBlock, ConstDecl, Expression, ExpressionBlock, Float, ForLoop,
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

fn parse_type(tpe: pest::iterators::Pair<'_, Rule>, loc_pool: &mut ErrorPool) -> Type {
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

fn parse_function_def_arg(
    arg: pest::iterators::Pair<'_, Rule>,

    loc_pool: &mut ErrorPool,
) -> (Identifier, Type) {
    let mut id = arg.into_inner();
    let identifier = Identifier(id.next().unwrap().as_str().into());
    let tpe = parse_type(id.next().unwrap(), loc_pool);
    (identifier, tpe)
}

fn parse_function_def_args(
    tpe: pest::iterators::Pair<'_, Rule>,

    loc_pool: &mut ErrorPool,
) -> Vec<(Identifier, Type)> {
    tpe.into_inner()
        .map(|x| parse_function_def_arg(x, loc_pool))
        .collect()
}

fn parse_function_call_arg(
    arg: pest::iterators::Pair<'_, Rule>,

    loc_pool: &mut ErrorPool,
) -> (Identifier, Expression) {
    // let location =
    let pid = loc_pool.insert(&arg);
    let mut id = arg.into_inner();
    let identifier = Identifier(id.next().unwrap().as_str().into());
    let exp = parse(id.next().unwrap().into_inner(), loc_pool);
    if let Statement::Expression(e, pid) = exp {
        (identifier, e)
    } else {
        unreachable!("NOT AN EXPRESSION WAAAAA")
    }
}

fn parse_for_loop(
    lp: pest::iterators::Pair<'_, Rule>,
    parallel: bool,

    loc_pool: &mut ErrorPool,
) -> Statement {
    let pid = loc_pool.insert(&lp);
    let mut it = lp.into_inner();
    let identifier = Identifier(it.next().unwrap().as_str().to_string());
    let from = it.next().unwrap().as_str().parse::<usize>().unwrap();
    let to = it.next().unwrap().as_str().parse::<usize>().unwrap();
    let block = parse_statement_block(it.next().unwrap(), loc_pool);
    Statement::ForLoop(
        ForLoop {
            variable: identifier,
            from,
            to,
            block,
            parallel,
        },
        pid,
    )
}

fn parse_function_call_args(
    tpe: pest::iterators::Pair<'_, Rule>,

    loc_pool: &mut ErrorPool,
) -> Vec<(Identifier, Expression)> {
    tpe.into_inner()
        .map(|x| parse_function_call_arg(x, loc_pool))
        .collect()
}

fn parse_block(blk: pest::iterators::Pair<'_, Rule>, loc_pool: &mut ErrorPool) -> Block {
    let statements = blk
        .into_inner()
        .map(|x| parse(x.into_inner(), loc_pool))
        .collect();

    Block { statements }
}

fn parse_statement_block(
    blk: pest::iterators::Pair<'_, Rule>,

    loc_pool: &mut ErrorPool,
) -> StatementBlock {
    let statements = blk
        .into_inner()
        .map(|x| parse(x.into_inner(), loc_pool))
        .collect();

    StatementBlock { statements }
}

fn parse_expression_block(
    blk: pest::iterators::Pair<'_, Rule>,

    loc_pool: &mut ErrorPool,
) -> ExpressionBlock {
    let mut statements: Vec<Statement> = blk
        .into_inner()
        .map(|x| parse(x.into_inner(), loc_pool))
        .collect();
    let expression = Box::new(match statements.pop() {
        Some(e) => {
            let Statement::Expression(e, _) = e else {
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

    loc_pool: &mut ErrorPool,
) -> ConditionalExpressionBlock {
    let mut unparsed_it = blk.into_inner();
    let a = unparsed_it.next().unwrap().into_inner();
    let b = unparsed_it.next().unwrap();
    let parsed_exp = parse(a, loc_pool);
    let block = parse_expression_block(b, loc_pool);

    let Statement::Expression(e, _) = parsed_exp else {
        unreachable!("Not expressions")
    };

    ConditionalExpressionBlock {
        condition: e,
        block,
    }
}

pub fn parse_pair(primary: pest::iterators::Pair<'_, Rule>, loc_pool: &mut ErrorPool) -> Statement {
    match primary.as_rule() {
        Rule::integer_literal => {
            let pid = loc_pool.insert(&primary);
            let mut p = primary.into_inner();
            // unreachable!("{}", p);
            let value = p.next().unwrap().as_str().parse::<i128>().unwrap();
            let width = match p.next() {
                Some(a) => IntegerWidth::Variable(a.as_str().parse::<u32>().unwrap()),
                None => IntegerWidth::IndexType,
            };
            Statement::Expression(Expression::Integer(Integer { value, width }, pid), pid)
        }
        Rule::float_literal => {
            let pid = loc_pool.insert(&primary);
            let mut p = primary.into_inner();
            let value = p.next().unwrap().as_str().parse::<f64>().unwrap();
            let width = p.next().unwrap().as_str().parse::<u32>().unwrap();
            Statement::Expression(Expression::Float(Float { value, width }, pid), pid)
        }
        Rule::cast => {
            let pid = loc_pool.insert(&primary);
            let mut p: Pairs<'_, Rule> = primary.into_inner();
            let Statement::Expression(e, _) = parse_pair(p.next().unwrap(), loc_pool) else {
                unreachable!("bad")
            };
            let tpe = parse_type(p.next().unwrap(), loc_pool);
            Statement::Expression(
                Expression::Cast(
                    Cast {
                        expr: Box::new(e),
                        to_type: tpe,
                    },
                    pid,
                ),
                pid,
            )
        }
        Rule::const_decl => {
            let pid = loc_pool.insert(&primary);
            let mut p = primary.into_inner();
            let identifier = VariableName(p.next().unwrap().as_str().into());
            let stype_unparsed = p.next().unwrap();
            let parsed_stype = parse_type(stype_unparsed, loc_pool);

            let Statement::Expression(expression, _) =
                parse(p.next().unwrap().into_inner(), loc_pool)
            else {
                unreachable!("NOT AN EXPRESSION!");
            };
            Statement::ConstDecl(
                ConstDecl {
                    identifier,
                    subtype: parsed_stype,
                    expression,
                },
                pid,
            )
        }
        Rule::for_loop => parse_for_loop(primary, false, loc_pool),
        Rule::parallel_loop => parse_for_loop(primary, true, loc_pool),
        Rule::tensor => {
            let loc = loc_pool.insert(&primary);
            Statement::Expression(
                Expression::Tensor(
                    primary
                        .into_inner()
                        .map(|x| {
                            let Statement::Expression(e, _) = parse_pair(x, loc_pool) else {
                                unreachable!("You can't do that bro");
                            };
                            e
                        })
                        .collect(),
                    loc,
                ),
                loc,
            )
        }
        Rule::var_decl => {
            let loc = loc_pool.insert(&primary);
            let mut p = primary.into_inner();
            let identifier = VariableName(p.next().unwrap().as_str().into());
            let stype_unparsed = p.next().unwrap();
            let parsed_stype = parse_type(stype_unparsed, loc_pool);

            let Statement::Expression(expression, _) =
                parse(p.next().unwrap().into_inner(), loc_pool)
            else {
                unreachable!("NOT AN EXPRESSION!");
            };

            Statement::VariableDecl(
                VariableDecl {
                    identifier,
                    subtype: Some(parsed_stype),
                    expression,
                },
                loc,
            )
        }

        Rule::var_reassignment => {
            let loc = loc_pool.insert(&primary);
            let mut p = primary.into_inner();
            let identifier = VariableName(p.next().unwrap().as_str().trim().into());
            let Statement::Expression(expression, _) =
                parse(p.next().unwrap().into_inner(), loc_pool)
            else {
                unreachable!("NOT AN EXPRESSION!");
            };
            Statement::VariableDecl(
                VariableDecl {
                    identifier,
                    subtype: None,
                    expression,
                },
                loc,
            )
        }
        Rule::infix_operation => parse(primary.into_inner(), loc_pool),
        Rule::binary_infix_operation => parse(primary.into_inner(), loc_pool),
        Rule::identifier => {
            let loc = loc_pool.insert(&primary);
            Statement::Expression(
                Expression::Identifier(Identifier(primary.as_str().trim().into()), loc),
                loc,
            )
        }
        Rule::function_call => {
            let loc = loc_pool.insert(&primary);
            let mut it: Pairs<'_, Rule> = primary.into_inner();
            let identifier = FunctionName(it.next().unwrap().as_str().into());

            let args = match it.next() {
                Some(x) => parse_function_call_args(x, loc_pool),
                _ => vec![],
            };
            Statement::Expression(
                Expression::FunctionCall(FunctionCall { identifier, args }, loc),
                loc,
            )
        }
        Rule::function_definition => {
            // on
            let loc = loc_pool.insert(&primary);
            let mut it: Pairs<'_, Rule> = primary.into_inner();
            let name: String = it.next().unwrap().as_str().into();
            let mut nxt = it.next().unwrap();
            let args = match nxt.as_rule() {
                Rule::function_def_params => {
                    let res = Some(parse_function_def_args(nxt, loc_pool));
                    nxt = it.next().unwrap();
                    res
                }
                _ => None,
            }
            .unwrap_or(vec![]);

            let return_type = parse_type(nxt, loc_pool);
            nxt = it.next().unwrap();

            let def = FunctionDefinition {
                identifier: FunctionName(name),
                args,
                return_type,
                block: parse_expression_block(nxt, loc_pool),
            };

            Statement::FunctionDefinition(def, loc)
        }
        Rule::procedure_definition => {
            // on
            let loc = loc_pool.insert(&primary);
            let mut it: Pairs<'_, Rule> = primary.into_inner();
            let name: String = it.next().unwrap().as_str().into();
            let mut nxt = it.next().unwrap();
            let args = match nxt.as_rule() {
                Rule::function_def_params => {
                    let res = Some(parse_function_def_args(nxt, loc_pool));
                    nxt = it.next().unwrap();
                    res
                }
                _ => None,
            }
            .unwrap_or(vec![]);

            let def = ProcedureDefinition {
                identifier: FunctionName(name),
                args,
                block: parse_block(nxt, loc_pool),
            };

            Statement::ProcedureDefinition(def, loc)
        }
        Rule::forward_function_decleration => {
            let loc = loc_pool.insert(&primary);
            let mut it: Pairs<'_, Rule> = primary.into_inner();
            let name: String = it.next().unwrap().as_str().into();
            let mut nxt = it.next().unwrap();
            let args = match nxt.as_rule() {
                Rule::function_def_params => {
                    let res = Some(parse_function_def_args(nxt, loc_pool));
                    nxt = it.next().unwrap();
                    res
                }
                _ => None,
            }
            .unwrap_or(vec![]);

            let return_type = parse_type(nxt, loc_pool);

            let def = FunctionDecleration {
                identifier: FunctionName(name),
                args,
                return_type,
            };

            Statement::FunctionDecleration(def, loc)
        }
        Rule::if_expression_block => {
            todo!()
        }
        Rule::statement | Rule::top_level_statement | Rule::expression => {
            parse(primary.into_inner(), loc_pool)
        }
        Rule::if_expression_control_flow => {
            let pid = loc_pool.insert(&primary);
            let it = primary.into_inner();
            let mut if_blocks = Vec::new();
            let mut else_block = None;
            for m in it {
                match m.as_rule() {
                    Rule::if_expression_block | Rule::else_if_expression_block => {
                        if_blocks.push(parse_expression_conditional_block(m, loc_pool))
                    }
                    Rule::else_expression_block => {
                        else_block = Some(parse_expression_block(m, loc_pool));
                    }
                    x => todo!("rule {x:?}"),
                };
            }
            Statement::Expression(
                Expression::ConditionalExpressionControlFlowControl {
                    if_blocks,
                    else_block: Box::new(else_block.unwrap()),
                    pool_id: pid,
                },
                pid,
            )
        }
        Rule::boolean_t => {
            let pid = loc_pool.insert(&primary);
            Statement::Expression(
                Expression::Bool(
                    if primary.as_str() == "true" {
                        true
                    } else if primary.as_str() == "false" {
                        false
                    } else {
                        unreachable!("Undefined boolean")
                    },
                    pid,
                ),
                pid,
            )
        }
        rule => {
            eprintln!("{}", primary);
            unreachable!("Expr::parse expected atom, found {:?}", rule);
        }
    }
}

pub fn parse(rules: Pairs<Rule>, loc_pool: &mut ErrorPool) -> Statement {
    PARSER
        .parser
        .map_primary(|p| parse_pair(p, loc_pool))
        .map_infix(|lhs, _, rhs| {
            let (Statement::Expression(lhs, _), Statement::Expression(rhs, _)) = (lhs, rhs) else {
                unreachable!();
            };

            let func_call = FunctionCall {
                identifier: FunctionName("scad.inbuilts.add".into()),
                args: vec![(Identifier("a".into()), lhs), (Identifier("a".into()), rhs)],
            };

            // Statement::Expression(Expression::FunctionCall(func_call))
            todo!()
        })
        .parse(rules)
}
