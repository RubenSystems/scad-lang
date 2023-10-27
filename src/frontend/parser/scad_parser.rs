use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "frontend/parser/scad.pest"]
pub struct SCADParser;
