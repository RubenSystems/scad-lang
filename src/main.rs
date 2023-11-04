pub mod frontend;

use crate::frontend::codegen::element_generation::{
    expression_ssa_transformation, statement_ssa_translation, SSAExpression,
};
use crate::frontend::codegen::generatable::Generatable;
use crate::frontend::parser::ast_types::Statement;
use crate::frontend::parser::scad_parser::{ParserToAST, SCADParser};
use frontend::parser::scad_parser::Rule;
use pest::Parser;

fn main() {
    /*


    */
    let prog = r#"


    fn main() {
        let mut add_res : u32 = (18 * 4) + 3; 
        add_res
    };
    "#;

    let mut parsed_result = SCADParser::parse(Rule::program, prog).unwrap();
    // println!("{:#?}", x);
    let parser = ParserToAST::new();

    let code: Vec<String> = parsed_result
        .flat_map(|pair| {
            if let Rule::EOI = pair.as_rule() {
                None
            } else {
                Some(parser.parse(pair.into_inner()))
            }
        })
        .map(|s| statement_ssa_translation(s, Box::new(|_| SSAExpression::Noop)))
        .map(|ssa| ssa.to_llvm_ir())
        .collect();

    println!("{}", code.join("\n"))
    // let m = p.parse(x.next().unwrap().into_inner());

    // println!("{:#?}", m);

    // let x = statement_ssa_translation(m, Box::new(|_| SSAExpression::Noop));

    // println!("==\n{:#?}", x);
    // println!("==\n{}", x.to_llvm_ir());
}
