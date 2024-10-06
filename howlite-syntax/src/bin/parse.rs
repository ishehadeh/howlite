use ast::{AstNode, AstNodeData};
use howlite_syntax::{ast, parse, token_epp, tree::TreeBuilder};
use std::{error::Error, io::Read};

fn main() {
    let input = std::env::args().nth(1).expect("USAGE: parse [FILE]");
    let file_data = {
        let mut s = String::new();
        std::fs::File::open(input)
            .expect("failed to open file")
            .read_to_string(&mut s)
            .expect("failed to read file");
        s
    };

    let lexerdef = howlite_syntax::lexerdef();
    let lexer = lexerdef.lexer(&file_data);
    let tree_builder: TreeBuilder<_> = TreeBuilder::default();
    let (root, errs) = howlite_syntax::parse(&lexer, &tree_builder);
    let tree = tree_builder.finalize();

    println!("Parse Errors:");
    for e in errs {
        println!("{}", e.pp(&lexer, &howlite_syntax::token_epp));
    }
    println!("Tree Root = {:?}", root);
    println!("Tree = {:#?}", tree);
}
