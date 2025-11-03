#![allow(unused_braces)]
use std::rc::Rc;
pub fn int_of_float(f: f64) -> i64 {f as i64}
pub fn float_of_int(i: i64) -> f64 {i as f64}
pub fn meow(m: String) -> String { format!("{}{}", { m.clone() }, String::from(" meow")) }
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
pub fn var_ref_list() -> Rc<Vec<i64>> { {let a = 10_i64;
let b = 15_i64;
let c = 20_i64;
{ Rc::new(vec![{ a },
{ b },
{ c },
{ a },
{ b },
{ c }]) }} }
pub fn string_list() -> Rc<Vec<String>> { {let a = String::from("hello");
let b = String::from("there");
let c = String::from("lol");
{ Rc::new(vec![{ a.clone() },
{ b.clone() },
{ c.clone() }]) }} }
pub fn list_list() -> Rc<Vec<Rc<Vec<i64>>>> { {let a = { Rc::new(vec![1_i64,
2_i64,
3_i64]) };
let b = { Rc::new(vec![4_i64,
5_i64,
6_i64]) };
let c = { Rc::new(vec![7_i64,
8_i64,
9_i64]) };
{ Rc::new(vec![{ Rc::clone(&a) },
{ Rc::clone(&b) },
{ Rc::clone(&c) }]) }} }
pub fn fibonacci(n: i64) -> i64 { { if { ({ n }) == (0_i64) } { 0_i64 } else { { if { ({ n }) == (1_i64) } { 1_i64 } else { { (fibonacci({ ({ n }) - (1_i64) })) + (fibonacci({ ({ n }) - (2_i64) })) } } } } } }
pub fn fib_list() -> Rc<Vec<i64>> { { Rc::new(vec![fibonacci(0_i64),
fibonacci(1_i64),
fibonacci(2_i64),
fibonacci(3_i64),
fibonacci(4_i64),
fibonacci(5_i64),
fibonacci(6_i64),
fibonacci(7_i64),
fibonacci(8_i64),
fibonacci(9_i64),
fibonacci(10_i64)]) } }
pub fn func_param(f1: Box<dyn Fn(i64) -> f64>, f2: Box<dyn Fn(Box<dyn Fn(i64, i64) -> i64>) -> i64>) -> f64 { f1(4_i64) }
pub fn func_param_2(f1: Box<dyn Fn(i64) -> f64>, f2: Box<dyn Fn(i64, i64) -> i64>) -> f64 { f1(f2(3_i64, 2_i64)) }
pub fn func_param_call() -> f64 { func_param_2(Box::new(move |x: i64| -> f64 { float_of_int({ x }) }), Box::new(move |i: i64, j: i64| -> i64 { { ({ i }) + ({ j }) } })) }
pub fn arrow_func_call() -> i64 { { Box::new(move |x: i64| -> i64 { { ({ x }) + (2_i64) } })(4_i64) } }
pub fn arrow_add() -> Box<dyn Fn(i64) -> Box<dyn Fn(i64) -> i64>> { Box::new(move |x: i64| -> Box<dyn Fn(i64) -> i64> { Box::new(move |y: i64| -> i64 { { ({ x }) + ({ y }) } }) }) }
pub fn add_3() -> Box<dyn Fn(i64) -> i64> { { arrow_add()(3_i64) } }
pub fn convert_test() -> i64 { {let a = 31.4_f64;
int_of_float({ a })} }
pub fn main() -> () { { println!("{:?}", format!("{}{}", String::from("3 + 3 = "), { add_3()(3_i64) })) };{ println!("{:?}", fib_list()) };{ println!("{:?}", format!("{}{}", String::from("func param call: "), func_param_call())) };{ println!("{:?}", format!("{}{}", String::from("15 factorial is: "), factorial(15_i64))) } }
