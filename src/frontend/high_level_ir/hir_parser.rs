use lazy_static::lazy_static;
use pest::{
    iterators::Pairs,
    pratt_parser::{Assoc, Op, PrattParser},
};
use pest_derive::Parser;

use crate::frontend::{
    error::{ErrorPool, ErrorType, SCADError},
    high_level_ir::ast_types::{Block, FunctionDefinition, FunctionName},
};

use super::ast_types::{
    Cast, ConditionalExpressionBlock, ConstDecl, Expression, ExpressionBlock, Float, ForLoop,
    FunctionCall, FunctionDecleration, Identifier, Integer, IntegerWidth, ProcedureDefinition,
    Statement, StatementBlock, Type, TypeName, VariableDecl, VariableName, WhileLoop,
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

fn parse_type(
    tpe: pest::iterators::Pair<'_, Rule>,
    _loc_pool: &mut ErrorPool,
) -> Result<Type, SCADError> {
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

            Ok(Type {
                dimensions,
                subtype: TypeName(subtype),
            })
        }
        _ => Err(SCADError::from_pair(ErrorType::InvalidType, &tpe)),
    }
}

fn parse_function_def_arg(
    arg: pest::iterators::Pair<'_, Rule>,

    loc_pool: &mut ErrorPool,
) -> Result<(Identifier, Type), SCADError> {
    let mut id = arg.into_inner();
    let identifier = Identifier(id.next().unwrap().as_str().into());
    let tpe = parse_type(id.next().unwrap(), loc_pool)?;
    Ok((identifier, tpe))
}

fn parse_function_def_args(
    tpe: pest::iterators::Pair<'_, Rule>,

    loc_pool: &mut ErrorPool,
) -> Result<Vec<(Identifier, Type)>, SCADError> {
    tpe.into_inner()
        .map(|x| parse_function_def_arg(x, loc_pool))
        .collect()
}

fn parse_function_call_arg(
    arg: pest::iterators::Pair<'_, Rule>,
    loc_pool: &mut ErrorPool,
) -> Result<(Identifier, Expression), SCADError> {
    // let location =
    let _pid = loc_pool.insert(&arg);
    let mut id = arg.clone().into_inner();
    let identifier = Identifier(id.next().unwrap().as_str().into());
    let exp = parse(id.next().unwrap().into_inner(), loc_pool);
    if let Statement::Expression(e, _pid) = exp? {
        Ok((identifier, e))
    } else {
        Err(SCADError::from_pair(ErrorType::InvalidType, &arg))
    }
}

fn parse_for_loop(
    lp: pest::iterators::Pair<'_, Rule>,
    parallel: bool,

    loc_pool: &mut ErrorPool,
) -> Result<Statement, SCADError> {
    let unroll_error = SCADError::from_pair(ErrorType::InvalidUnroll, &lp);
    let pid = loc_pool.insert(&lp);
    let mut it = lp.into_inner();
    let identifier = Identifier(it.next().unwrap().as_str().to_string());
    let from = it.next().unwrap().as_str().parse::<usize>().unwrap();
    let to = it.next().unwrap().as_str().parse::<usize>().unwrap();
    let mut nxt = it.next().unwrap();
    let mut step = None;
    let mut unroll = None;

    match nxt.as_rule() {
        Rule::step => {
            step = Some(nxt.into_inner().as_str().parse::<usize>().unwrap());
            nxt = it.next().unwrap()
        }
        _ => {}
    }

    match nxt.as_rule() {
        Rule::unroll => {
            let factor = nxt
                .into_inner()
                .as_str()
                .parse::<usize>()
                .map_err(|_| unroll_error)?;

            unroll = Some(factor);

            nxt = it.next().unwrap()
        }
        _ => {}
    }

    let block = parse_statement_block(nxt, loc_pool)?;

    // let (step, block) = match nxt.as_rule() {
    //     Rule::statement_block => (1_usize, parse_statement_block(nxt, loc_pool)?),
    //     _ => (
    //         nxt.as_str().parse::<usize>().unwrap(),

    //         parse_statement_block(it.next().unwrap(), loc_pool)?,
    //     ),
    // };
    Ok(Statement::ForLoop(
        ForLoop {
            variable: identifier,
            from,
            to,
            block,
            parallel,
            step: step.unwrap_or(1),
            unroll: unroll.unwrap_or(1),
        },
        pid,
    ))
}

fn parse_function_call_args(
    tpe: pest::iterators::Pair<'_, Rule>,

    loc_pool: &mut ErrorPool,
) -> Result<Vec<(Identifier, Expression)>, SCADError> {
    tpe.into_inner()
        .map(|x| parse_function_call_arg(x, loc_pool))
        .collect()
}

fn parse_block(
    blk: pest::iterators::Pair<'_, Rule>,
    loc_pool: &mut ErrorPool,
) -> Result<Block, SCADError> {
    let mut statements = vec![];
    for x in blk.into_inner() {
        statements.push(parse(x.into_inner(), loc_pool)?)
    }

    Ok(Block { statements })
}

fn parse_statement_block(
    blk: pest::iterators::Pair<'_, Rule>,

    loc_pool: &mut ErrorPool,
) -> Result<StatementBlock, SCADError> {
    let mut statements = vec![];
    for x in blk.into_inner() {
        statements.push(parse(x.into_inner(), loc_pool)?)
    }

    Ok(StatementBlock { statements })
}

fn parse_expression_block(
    blk: pest::iterators::Pair<'_, Rule>,

    loc_pool: &mut ErrorPool,
) -> Result<ExpressionBlock, SCADError> {
    let mut statements = vec![];
    for x in blk.into_inner() {
        statements.push(parse(x.into_inner(), loc_pool)?)
    }
    let expression = Box::new(match statements.pop() {
        Some(e) => {
            let Statement::Expression(e, _) = e else {
                // no need to error handle because parser will catch
                unreachable!("Last statement does not eval to expression")
            };
            e
        }
        // no need to error handle as parser will catch
        None => unreachable!("Can't have expression block with no expression at the end!"),
    });

    Ok(ExpressionBlock {
        statements,
        expression,
    })
}

fn parse_expression_conditional_block(
    blk: pest::iterators::Pair<'_, Rule>,

    loc_pool: &mut ErrorPool,
) -> Result<ConditionalExpressionBlock, SCADError> {
    let mut unparsed_it = blk.into_inner();
    let a = unparsed_it.next().unwrap().into_inner();
    let b = unparsed_it.next().unwrap();
    let parsed_exp = parse(a, loc_pool);
    let block = parse_expression_block(b, loc_pool)?;

    let Statement::Expression(e, _) = parsed_exp? else {
        unreachable!("Not expressions")
    };

    Ok(ConditionalExpressionBlock {
        condition: e,
        block,
    })
}

pub fn parse_pair(
    primary: pest::iterators::Pair<'_, Rule>,
    loc_pool: &mut ErrorPool,
) -> Result<Statement, SCADError> {
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
            Ok(Statement::Expression(
                Expression::Integer(Integer { value, width }, pid),
                pid,
            ))
        }
        Rule::float_literal => {
            let pid = loc_pool.insert(&primary);
            let mut p = primary.into_inner();
            let value = p.next().unwrap().as_str().parse::<f64>().unwrap();
            let width = p.next().unwrap().as_str().parse::<u32>().unwrap();
            Ok(Statement::Expression(
                Expression::Float(Float { value, width }, pid),
                pid,
            ))
        }
        Rule::cast => {
            let pid = loc_pool.insert(&primary);
            let mut p: Pairs<'_, Rule> = primary.into_inner();
            let Statement::Expression(e, _) = parse_pair(p.next().unwrap(), loc_pool)? else {
                unreachable!("bad cast")
            };
            let tpe = parse_type(p.next().unwrap(), loc_pool);
            Ok(Statement::Expression(
                Expression::Cast(
                    Cast {
                        expr: Box::new(e),
                        to_type: tpe?,
                    },
                    pid,
                ),
                pid,
            ))
        }
        Rule::for_loop => parse_for_loop(primary, false, loc_pool),
        Rule::parallel_loop => parse_for_loop(primary, true, loc_pool),
        Rule::tensor => {
            let loc = loc_pool.insert(&primary);

            let mut values = vec![];
            for x in primary.into_inner() {
                let Statement::Expression(e, _) = parse_pair(x, loc_pool)? else {
                    unreachable!("uncaught error from parser")
                };
                values.push(e)
            }
            Ok(Statement::Expression(Expression::Tensor(values, loc), loc))
        }
        Rule::var_decl => {
            let loc = loc_pool.insert(&primary);
            let mut p = primary.into_inner();
            let identifier = VariableName(p.next().unwrap().as_str().trim().into());
            let stype_unparsed = p.next().unwrap();
            let mut parsed_stype = None;
            let mut used = false;
            match stype_unparsed.as_rule() {
                Rule::r#type => {
                    parsed_stype = Some(parse_type(stype_unparsed.clone(), loc_pool)?);
                    used = true
                }
                _ => {}
            };

            let Statement::Expression(expression, _) = parse(
                if !used {
                    stype_unparsed.into_inner()
                } else {
                    p.next().unwrap().into_inner()
                },
                loc_pool,
            )?
            else {
                unreachable!("uncaught error from parser")
            };

            Ok(Statement::VariableDecl(
                VariableDecl {
                    identifier,
                    subtype: parsed_stype,
                    expression,
                },
                loc,
            ))
        }

        Rule::var_reassignment => {
            let loc = loc_pool.insert(&primary);
            let mut p = primary.into_inner();
            let identifier = VariableName(p.next().unwrap().as_str().trim().into());
            let Statement::Expression(expression, _) =
                parse(p.next().unwrap().into_inner(), loc_pool)?
            else {
                unreachable!("uncaught error from parser")
            };
            Ok(Statement::VariableDecl(
                VariableDecl {
                    identifier,
                    subtype: None,
                    expression,
                },
                loc,
            ))
        }
        Rule::infix_operation => parse(primary.into_inner(), loc_pool),
        Rule::binary_infix_operation => parse(primary.into_inner(), loc_pool),
        Rule::identifier => {
            let loc = loc_pool.insert(&primary);
            Ok(Statement::Expression(
                Expression::Identifier(Identifier(primary.as_str().trim().into()), loc),
                loc,
            ))
        }
        Rule::function_call => {
            let loc = loc_pool.insert(&primary);
            let mut it: Pairs<'_, Rule> = primary.into_inner();
            let identifier = FunctionName(it.next().unwrap().as_str().into());

            let args = match it.next() {
                Some(x) => parse_function_call_args(x, loc_pool)?,
                _ => vec![],
            };
            Ok(Statement::Expression(
                Expression::FunctionCall(FunctionCall { identifier, args }, loc),
                loc,
            ))
        }
        Rule::function_definition => {
            // on
            let loc = loc_pool.insert(&primary);
            let mut it: Pairs<'_, Rule> = primary.into_inner();
            let name: String = it.next().unwrap().as_str().into();
            let mut nxt = it.next().unwrap();
            let args = match nxt.as_rule() {
                Rule::function_def_params => {
                    let res = Some(parse_function_def_args(nxt, loc_pool)?);
                    nxt = it.next().unwrap();
                    res
                }
                _ => None,
            }
            .unwrap_or(vec![]);

            let return_type = parse_type(nxt, loc_pool)?;
            nxt = it.next().unwrap();

            let def = FunctionDefinition {
                identifier: FunctionName(name),
                args,
                return_type,
                block: parse_expression_block(nxt, loc_pool)?,
            };

            Ok(Statement::FunctionDefinition(def, loc))
        }
        Rule::procedure_definition => todo!(),
        Rule::forward_function_decleration => {
            let loc = loc_pool.insert(&primary);
            let mut it: Pairs<'_, Rule> = primary.into_inner();
            let name: String = it.next().unwrap().as_str().into();
            let mut nxt = it.next().unwrap();
            let args = match nxt.as_rule() {
                Rule::function_def_params => {
                    let res = Some(parse_function_def_args(nxt, loc_pool)?);
                    nxt = it.next().unwrap();
                    res
                }
                _ => None,
            }
            .unwrap_or(vec![]);

            let return_type = parse_type(nxt, loc_pool)?;

            let def = FunctionDecleration {
                identifier: FunctionName(name),
                args,
                return_type,
            };

            Ok(Statement::FunctionDecleration(def, loc))
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
                        if_blocks.push(parse_expression_conditional_block(m, loc_pool)?)
                    }
                    Rule::else_expression_block => {
                        else_block = Some(parse_expression_block(m, loc_pool)?);
                    }
                    x => todo!("rule {x:?}"),
                };
            }
            Ok(Statement::Expression(
                Expression::ConditionalExpressionControlFlowControl {
                    if_blocks,
                    else_block: Box::new(else_block.unwrap()),
                    pool_id: pid,
                },
                pid,
            ))
        }
        Rule::while_loop => {
            let pid = loc_pool.insert(&primary);
            let mut it = primary.into_inner();
            let Statement::Expression(cond, _) = parse(it.next().unwrap().into_inner(), loc_pool)?
            else {
                unreachable!("this should never happen")
            };

            Ok(Statement::WhileLoop(
                WhileLoop {
                    condition: cond,
                    block: parse_statement_block(it.next().unwrap(), loc_pool)?,
                },
                pid,
            ))
        }
        Rule::boolean_t => {
            let pid = loc_pool.insert(&primary);
            Ok(Statement::Expression(
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
            ))
        }
        _ => Err(SCADError::from_pair(ErrorType::InvalidInput, &primary)),
    }
}

pub fn parse(rules: Pairs<Rule>, loc_pool: &mut ErrorPool) -> Result<Statement, SCADError> {
    let x = PARSER
        .parser
        .map_primary(|p| parse_pair(p, loc_pool))
        .map_infix(|lhs, _, rhs| {
            let (Statement::Expression(lhs, _), Statement::Expression(rhs, _)) = (lhs?, rhs?)
            else {
                unreachable!();
            };

            let _func_call = FunctionCall {
                identifier: FunctionName("scad.inbuilts.add".into()),
                args: vec![(Identifier("a".into()), lhs), (Identifier("a".into()), rhs)],
            };

            // Statement::Expression(Expression::FunctionCall(func_call))
            todo!()
        })
        .parse(rules)?;

    Ok(x)
}
