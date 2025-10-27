use std::rc::Rc;
pub fn meow(m: Rc<String>) -> Rc<String> { {{ Rc::clone(&m) }} }
pub fn meows() -> Rc<Vec<Rc<String>>> { {{ Rc::new(vec![{Rc::new(String::from("meow"))},
{Rc::new(String::from("mrow"))},
{Rc::new(String::from("mjá"))},
{Rc::new(String::from("ニャー"))}]) }} }
pub fn floats() -> Rc<Vec<Rc<f64>>> { {{ Rc::new(vec![{Rc::new(3.14_f64)},
{Rc::new(2.72_f64)},
{Rc::new(6.9_f64)},
{Rc::new(3.141592653589_f64)}]) }} }
pub fn nums() -> Rc<Vec<Rc<i64>>> { {{ Rc::new(vec![{Rc::new(2001_i64)},
{Rc::new(1984_i64)},
{Rc::new(2000_i64)},
{Rc::new(1_i64)},
{Rc::new(0_i64)},
{Rc::new(-1_i64)},
{Rc::new(-154_i64)},
{Rc::new(3_i64)},
{Rc::new(1024_i64)}]) }} }
pub fn main() -> () { {println!("{:?}", ({meow({Rc::new(String::from("meow"))})}))} }