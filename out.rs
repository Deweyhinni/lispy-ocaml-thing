#![allow(unused_braces)]
use std::rc::Rc;
pub fn print(s: String) -> () { () }
pub fn meow(m: String) -> String { { m.clone() } }
pub fn meows() -> String { { Rc::new(vec![String::from("meow"),
String::from("mrow"),
String::from("mjá"),
String::from("ニャー")]) } }
pub fn floats() -> f64 { { Rc::new(vec![3.14_f64,
2.72_f64,
6.9_f64,
3.141592653589_f64]) } }
pub fn nums() -> i64 { { Rc::new(vec![2001_i64,
1984_i64,
2000_i64,
1_i64,
0_i64,
-1_i64,
-154_i64,
3_i64,
1024_i64]) } }
pub fn curry_str_concat() -> Box<dyn Fn(String) -> Box<dyn Fn(String) -> String>> { Box::new(move |s1: String| -> Box<dyn Fn(String) -> String> { Box::new(move |s2: String| -> String { format!("{}{}", { s1.clone() }, { s2.clone() }) }) }) }
pub fn meow_concat() -> Box<dyn Fn(String) -> String> { { curry_str_concat()(String::from("meow")) } }
pub fn always_true() -> String { {let t = true;
{ if { ({ t }) == (true) } { String::from("true") } else { String::from("false") } }} }
pub fn main() -> () { println!("{:?}", (meow(String::from("lol")))) }