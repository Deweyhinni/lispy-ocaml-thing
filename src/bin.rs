use std::{fs::File, io::Read};

use silly_list_lib::{ast::SyntaxTree, tokenizer::TokenList};

pub fn main() {
    let mut file = File::open("example_code.slylsp").unwrap();
    let mut code = String::new();
    file.read_to_string(&mut code).unwrap();
    let tokens = TokenList::generate(code);
    println!("{:?}", tokens);
    let tree = SyntaxTree::generate(tokens.unwrap()).unwrap();
    println!("{:#?}", tree);
}
