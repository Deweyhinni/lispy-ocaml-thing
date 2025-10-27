use std::rc::Rc;
pub fn meow(m: Rc<String>) -> Rc<String> { {{ Rc::clone(&m) }} }
pub fn meows() -> Rc<String> { {{ Rc::new(vec![{Rc::new(String::from("meow"))},
{Rc::new(String::from("mrow"))},
{Rc::new(String::from("mjá"))},
{Rc::new(String::from("ニャー"))}]) }} }
pub fn floats() -> Rc<f64> { {{ Rc::new(vec![{3.14},
{2.72},
{6.9},
{3.141592653589}]) }} }
pub fn nums() -> Rc<i64> { {{ Rc::new(vec![{2001},
{1984},
{2000},
{1},
{0},
{-1},
{-154},
{3},
{1024}]) }} }
pub fn main() -> () { {println!("{:?}", ({meow({Rc::new(String::from("meow"))})}))} }