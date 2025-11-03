#![allow(unused_imports)]
use std::{
    fs::{self, File},
    io::Read,
};

use silly_lisp_lib::{
    ast::SyntaxTree, codegen::rust_generator::RustGenerator, tokenizer::TokenList,
};

pub fn main() {
    let mut file = File::open("example_code.slylsp").unwrap();
    let mut code = String::new();
    file.read_to_string(&mut code).unwrap();
    let tokens = TokenList::generate(code);
    println!("{:?}", tokens);
    let tree = SyntaxTree::generate(tokens.unwrap()).unwrap();
    println!("{:#?}", tree);
    let rust_code = RustGenerator::new(tree).generate().unwrap();
    println!("{:#?}", rust_code);
    fs::write("out.rs", rust_code).unwrap();
}
