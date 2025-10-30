#![allow(unused_braces)]
use std::rc::Rc;
pub fn print(s: String) -> () { () }
pub fn meow(m: String) -> String { { m.clone() } }
pub fn meows() -> Rc<Vec<String>> { { Rc::new(vec![String::from("meow"),
String::from("mrow"),
String::from("mjá"),
String::from("ニャー")]) } }
pub fn floats() -> Rc<Vec<f64>> { { Rc::new(vec![3.14_f64,
2.72_f64,
6.9_f64,
3.141592653589_f64]) } }
pub fn nums() -> Rc<Vec<i64>> { { Rc::new(vec![2001_i64,
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
pub fn meow_lol() -> String { { meow_concat()(String::from("lol")) } }
pub fn always_true() -> String { {let t = true;
{ if { ({ t }) == (true) } { String::from("true") } else { String::from("false") } }} }
pub fn bigger(x: i64, y: i64) -> i64 { { if { ({ x }) > ({ y }) } { { x } } else { { y } } } }
pub fn equals(x: i64, y: i64) -> String { { if { ({ x }) == ({ y }) } { String::from("equal") } else { String::from("does not equal") } } }
pub fn kitty() -> Box<dyn Fn(String) -> String> { Box::new(move |s: String| -> String { format!("{}{}", format!("{}{}", String::from("kitty says: "), { s.clone() }), String::from("!")) }) }
pub fn factorial(n: i64) -> i64 { { if { ({ n }) == (0_i64) } { 1_i64 } else { { ({ n }) * (factorial({ ({ n }) - (1_i64) })) } } } }
pub fn func_var() -> i64 { {let add_1 = Box::new(move |x: i64| -> i64 { { ({ x }) + (1_i64) } });
add_1(2_i64)} }
pub fn max_of_three(a: i64, b: i64, c: i64) -> i64 { { if { ({ a }) > ({ b }) } { { if { ({ a }) > ({ c }) } { { a } } else { { c } } } } else { { if { ({ b }) > ({ c }) } { { b } } else { { c } } } } } }
pub fn main() -> () { println!("{:?}", (format!("{}{}", String::from("15 factorial is: "), factorial(15_i64)))) }