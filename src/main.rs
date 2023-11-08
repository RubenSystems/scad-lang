pub mod frontend;

use crate::frontend::parser::scad_parser::{ParserToAST, SCADParser};
use frontend::codegen::mir_ast_types::SSAExpression;
use frontend::codegen::mir_translators::statement_ssa_translation;
use frontend::parser::scad_parser::Rule;
use pest::Parser;
use std::fs::File;
use std::io::Write;

use std::process::Command;

fn main() -> std::io::Result<()> {
    /*
    struct Player {
        age: i32,
        x_pos: i32,
        y_pos: i32
    }

    layout PlayerLayout: Player = {age}, {x_pos, y_pos}

    struct LargePlayerObject {
        let players : [Player: PlayerLayout; 32] = [Player::default(); 32];
    }

        fn jeff(a: i32, b: i32) i32 {
        let some_value: i32 = a + 4;
        some_value + 10
    };

    [alloc]
    fn something() i32 {
        alloc(100 * sizeof(i32))[0]
    }

    let number: i32 = alloc(malloc) {
        something()
    }

    fn main() {

        let mut value_of_four: i32 = jeff(a: 3 + 4, b: 3);
        let mut value_of_three: i32 = value_of_four;
        let mut value_of_two: i32 = 2;
        print_int(thing_to_print: 4 + 3 + 2);

        value_of_four + value_of_three * value_of_two
    };


    */
    let prog = r#"

        fn add_two_numbers(a: i32, b: i32) i32 {a + b};


        fn main() {

            let adder : i32 = {100 + 200};
            print_int(thing: add_two_numbers(a: adder, b: 10));
        };

    "#;

    let parsed_result = SCADParser::parse(Rule::program, prog).unwrap();
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

    let tmp_builtins = r#"
        declare i32 @printf(i8*, ...)
        
        @.str_nl = private constant [2 x i8] c"\0A\00"
        @.str_star = private constant [2 x i8] c"*\00"
        @.str_space = private constant [2 x i8] c" \00"
        
        define void @new_line() #0 {
        %t0 = getelementptr [2 x i8], [2 x i8]* @.str_nl, i32 0, i32 0
        %1 = call i32 (i8*, ...) @printf(i8* %t0)
        ret void
        }
        
        define void @print_star() #0 {
        %t0 = getelementptr [2 x i8], [2 x i8]* @.str_star, i32 0, i32 0
        %1 = call i32 (i8*, ...) @printf(i8* %t0)
        ret void
        }
        
        define void @print_space() #0 {
        %t0 = getelementptr [2 x i8], [2 x i8]* @.str_space, i32 0, i32 0
        %1 = call i32 (i8*, ...) @printf(i8* %t0)
        ret void
        }
        
        define void @skip() #0 {
        ret void
        }
        
        @.str = private constant [4 x i8] c"%d\0A\00"
        
        define void @print_int(i32 %x) {
        %t0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
        call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
        ret void
        }
    "#;

    let path = "test.ll";
    let mut output = File::create(path)?;
    write!(output, "{tmp_builtins}\n{}", code.join("\n"))?;

    // opt -O3 -S in_file.ll > out_file.ll
    let _ = Command::new("sh")
        .arg("-c")
        .arg("/opt/homebrew/opt/llvm/bin/opt -O3 -S test.ll > test_opt.ll")
        .output()
        .expect("Failed to execute command");

    let _ = Command::new("sh")
        .arg("-c")
        .arg("/opt/homebrew/opt/llvm/bin/llc -filetype=obj test_opt.ll")
        .output()
        .expect("Failed to execute command");

    let _ = Command::new("sh")
        .arg("-c")
        .arg("clang test_opt.o -o program")
        .output()
        .expect("Failed to execute command");

    Ok(())
}
