#![allow(unused_imports)]
use std::{
    fs::{self, File},
    io::Read,
};

use silly_lisp_lib::{
    ast::SyntaxTree, codegen::rust_generator::RustGenerator, tokenizer::TokenList,
};

pub fn main() {
    // let mut file = File::open("example_code.slylsp").unwrap();
    // let mut code = String::new();
    // file.read_to_string(&mut code).unwrap();
    let code = String::from(
        r#"
        (let print s = (extern Unit))
        (let int_of_float (f : Float) = (extern Int))
        (let float_of_int (i : Int) = (extern Float))

        (let meow (m : String) = (m))

        (let meows = (["meow" "mrow" "mjá" "ニャー"]))

        (let floats = ([3.14 2.72 6.9 3.141592653589]))

        (let nums = ([2001 1984 2000 1 0 -1 -154 3 1024]))

        (let curry_str_concat = (fn (s1 : String) -> (fn (s2 : String) -> (+ s1 s2))))

        (let meow_concat = (curry_str_concat "meow"))

        (let meow_lol = (meow_concat "lol"))

        (let always_true = let t = true in (if (= t true) then "true" else "false"))

        (let bigger (x : Int) (y : Int) = (if (> x y) then x else y))

        (let equals (x : Int) (y : Int) = (if (= x y) then "equal" else "does not equal"))

        (let kitty = (fn (s : String) -> (+ (+ "kitty says: " s) "!")))

        (let factorial (n : Int) = 
          (if (= n 0) 
            then 1 
            else (* n (factorial (- n 1)))))

        (
        let func_var = 
            let add_1 = (fn (x : Int) -> (+ x 1))
            in (add_1 2)
        )

        (let max_of_three (a : Int) (b : Int) (c : Int) = 
          (if (> a b) then (if (> a c) then a else c) else (if (> b c) then b else c)))

        (let var_ref_list = 
            let a = 10 
            let b = 15 
            let c = 20 
            in ([a b c a b c])
        )

        (let string_list =
            let a = "hello"
            let b = "there"
            let c = "lol"
            in ([a b c])
        )

        (let list_list = 
            let a = ([1 2 3])
            let b = ([4 5 6])
            let c = ([7 8 9])
            in ([a b c])
        )

        (let func_param (f1 : (Int -> Float)) (f2 : ((Int Int -> Int) -> Int)) = (f1 4))

        (let func_param_2 (f1 : (Int -> Float)) (f2 : (Int Int -> Int)) = (f1 (f2 3 2)))

        (let arrow_func_call = ((fn (x : Int) -> (+ x 2)) 4))

        (let convert_test = let a = 31.4 in (int_of_float a))

        (let () = (print (+ "15 factorial is: " (factorial 15))))
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
