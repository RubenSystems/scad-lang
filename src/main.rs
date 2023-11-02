pub mod frontend;

use crate::frontend::codegen::element_generation::{
    expression_ssa_transformation, statement_cps_translation, SSAExpression,
};
use crate::frontend::codegen::generatable::Generatable;
use crate::frontend::parser::ast_types::Statement;
use crate::frontend::parser::scad_parser::{ParserToAST, SCADParser};
use frontend::parser::scad_parser::Rule;
use pest::Parser;

fn main() {
    let prog = r#"
    let m : u32 = (900 + 300) * (200 * 9);
    "#;

    let mut x = SCADParser::parse(Rule::program, prog).unwrap();
    // println!("{:#?}", x);
    let p = ParserToAST::new();

    let m = p.parse(x.next().unwrap().into_inner());

    println!("{:#?}", m);

    let x = statement_cps_translation(m, Box::new(|_| SSAExpression::Noop));

    println!("==\n{:#?}", x);
    println!("==\n{}", x.to_llvm_ir());
}
