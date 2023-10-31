use frontend::parser::scad_parser::Rule;
// use pest::iterators::Pairs;
// use pest::pratt_parser::PrattParser;
use pest::Parser;

pub mod frontend;

use crate::frontend::parser::scad_parser::{ParserToAST, SCADParser};

fn main() {
    let prog = r#"
    let mut x : u32 = ((2 + 2) >= 2) && (4 == 2); 
    "#;

    let mut x = SCADParser::parse(Rule::program, prog).unwrap();
    println!("{:#?}", x);
    let p = ParserToAST::new();

    let m = p.parse(x.next().unwrap().into_inner());

    println!("{:#?}", m);
}
