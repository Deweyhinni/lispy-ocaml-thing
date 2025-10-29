use std::{
    fs::{self, File},
    io::Read,
};

use silly_list_lib::{
    ast::SyntaxTree, codegen::rust_generator::RustGenerator, tokenizer::TokenList,
};

pub fn main() {
    // let mut file = File::open("example_code.slylsp").unwrap();
    // let mut code = String::new();
    // file.read_to_string(&mut code).unwrap();
    let code = String::from(
        r#"
        (let meow (m : String) = (m))

        (let meows = (["meow" "mrow" "mjá" "ニャー"]))

        (let floats = ([3.14 2.72 6.9 3.141592653589]))

        (let nums = ([2001 1984 2000 1 0 -1 -154 3 1024]))

        (let curry_str_concat = (fn (s1 : String) -> (fn (s2 : String) -> (+ s1 s2))))

        (let meow_concat = (curry_str_concat "meow"))

        (let meow_lol = (meow_concat "lol"))

        (let always_true = let t = true in (if (= t true) then "true" else "false"))

        (let bigger (x : Int) (y : Int) = (if (> x y) then x else y))

        (let equals (x : Int) (y : Int) = (if (= x y) then "equal" else "not"))

        (let () = (print (meow_lol)))
        "#,
    );
    let tokens = TokenList::generate(code);
    println!("{:?}", tokens);
    let tree = SyntaxTree::generate(tokens.unwrap()).unwrap();
    println!("{:#?}", tree);
    let rust_code = RustGenerator::new(tree).generate().unwrap();
    println!("{:#?}", rust_code);
    fs::write("out.rs", rust_code).unwrap();
}
