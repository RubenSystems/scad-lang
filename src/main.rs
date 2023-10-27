use frontend::parser::{
    scad_parser::Rule, 
};
// use pest::iterators::Pairs;
// use pest::pratt_parser::PrattParser;
use pest::Parser;

pub mod frontend;

use crate::frontend::parser::{
    scad_parser::SCADParser,
};




fn main() {
    let prog = r#"
    (2 >= 4) && (3 >= (8 + 2 * 4));
    "#;

    let x = SCADParser::parse(Rule::program, prog).unwrap();
    println!("{:?}", x);
}
