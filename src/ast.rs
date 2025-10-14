use anyhow::Context;
use serde::{Deserialize, Serialize};
use std::{cmp::PartialEq, thread::current};

use crate::tokenizer::{self, Keyword, Operator, Token, TokenList};

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct SyntaxTree {
    nodes: Vec<Expression>,
}

impl SyntaxTree {
    pub fn generate(code: TokenList) -> Self {
        let nodes: Vec<Expression> = Vec::new();

        Self { nodes }
    }

    fn item_from_tokens(tokens: Vec<Token>) -> anyhow::Result<Item> {
        match &tokens[..] {
            [
                Token::Keyword(Keyword::Let),
                Token::Identifier(fn_name),
                rest @ ..,
            ] => {
                if let Some(eq_pos) = rest
                    .iter()
                    .position(|t| t == &Token::Operator(Operator::Eq))
                {
                    let mut params: Vec<tokenizer::Token> = Vec::new();
                    for t in rest[..eq_pos].iter() {
                        if let Token::Identifier(_) = t {
                            params.push(t.clone());
                        } else {
                            return Err(anyhow::anyhow!(
                                "function decleration params contain non-identifier"
                            ));
                        }
                    }
                }
                todo!()
            }

            _ => todo!(),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Item {
    Declaration(Declaration),
    Expression(Expression),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Declaration {
    Var(Identifier),
    Func(Identifier),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Expression {
    local_vars: Vec<Identifier>,
    expression_body: ExpressionBody,
}

impl Expression {
    fn from_tokens(tokens: &[Token]) -> anyhow::Result<Self> {
        let mut vars: Vec<Token> = Vec::new();
        if let Some(Token::Keyword(Keyword::Let)) = tokens.get(0) {
            let expression_vars = match tokens {
                [start @ .., Token::Keyword(Keyword::In)] => {
                    let mut vars: Vec<Identifier> = Vec::new();
                    let vars_tokens = split_with_prefix(start, &Token::Keyword(Keyword::Let));
                    for vts in vars_tokens {
                        let var = Self::var_from_tokens(&vts[..]).map_err(|e| {
                            e.context("unable to get variable in expression definition")
                        })?;
                        vars.push(var);
                    }

                    vars
                }
                _ => todo!(),
            };

            let expression_body = match tokens {
                [
                    Token::Keyword(Keyword::In),
                    Token::LParen,
                    body @ ..,
                    Token::RParen,
                ] => ExpressionBody::from_tokens(body)?,
                _ => todo!(),
            };

            Ok(Self {
                local_vars: expression_vars,
                expression_body,
            })
        } else if let Some(Token::LParen) = tokens.get(0) {
            let expression_body = match tokens {
                [Token::LParen, body @ .., Token::RParen] => ExpressionBody::from_tokens(body)?,
                _ => todo!(),
            };

            Ok(Self {
                local_vars: Vec::new(),
                expression_body,
            })
        } else {
            return Err(anyhow::anyhow!("{tokens:#?} is not an expression"));
        }
    }

    fn var_from_tokens(tokens: &[Token]) -> anyhow::Result<Identifier> {
        match tokens {
            [
                Token::Keyword(Keyword::Let),
                Token::Identifier(name),
                Token::Operator(Operator::Eq),
                expression @ ..,
                Token::RParen,
            ] => {
                let expr = Self::from_tokens(expression)
                    .map_err(|e| e.context("unable to get expression in variable declaration"))?;
                Ok(Identifier::Var {
                    name: name.clone(),
                    value: expr,
                })
            }

            _ => Err(anyhow::anyhow!("{tokens:#?} is not a variable declaration")),
        }
    }
}

#[cfg(test)]
mod expression_tests {
    use crate::{
        ast::{Expression, ExpressionBody, Identifier, Literal, Type, TypeValue},
        tokenizer,
    };

    #[test]
    fn test_var_from_tokens() {
        let tokens = crate::tokenizer::TokenList::generate("let meow = (\"meow\")".into()).unwrap();
        let var = crate::ast::Expression::var_from_tokens(&tokens.tokens()).unwrap();

        println!("{:#?}", var);

        assert_eq!(
            var,
            Identifier::Var {
                name: "meow".into(),
                value: Expression {
                    local_vars: Vec::new(),
                    expression_body: ExpressionBody::Literal(Box::new(Literal {
                        typ: Type::String,
                        value: TypeValue::String("meow".into())
                    }))
                }
            }
        )
    }

    #[test]
    fn test_literal_expression() {
        let tokens = tokenizer::TokenList::generate("(\"meow\")".into()).unwrap();
        let expr = Expression::from_tokens(&tokens.tokens()).unwrap();

        println!("{:#?}", expr);

        assert_eq!(
            expr,
            Expression {
                local_vars: Vec::new(),
                expression_body: ExpressionBody::Literal(Box::new(Literal {
                    typ: Type::String,
                    value: TypeValue::String("meow".into())
                })),
            }
        )
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum ExpressionBody {
    Operation(Box<Operation>),
    FuncCall {
        func: Box<Identifier>,
        params: Vec<Expression>,
    },
    Literal(Box<Literal>),
    Identifier(Box<Identifier>),
    Expression(Box<Expression>),
    Unit,
}

impl ExpressionBody {
    fn from_tokens(tokens: &[Token]) -> anyhow::Result<ExpressionBody> {
        if let Ok(expr) = Expression::from_tokens(tokens) {
            Ok(Self::Expression(Box::new(expr)))
        } else if tokens.len() == 1 {
            if let Some(l) = Literal::from_token(
                tokens
                    .get(0)
                    .expect("length checked but still unable to get token"),
            ) {
                Ok(Self::Literal(Box::new(l)))
            } else {
                todo!("identifier expression")
            }
        } else {
            Err(anyhow::anyhow!(
                "cannot create expression body from {tokens:?}"
            ))
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Identifier {
    Func { name: String, value: Func },
    FuncParam { name: String, typ: Type },
    Var { name: String, value: Expression },
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Func {
    params: Vec<Identifier>,
    body: Expression,
    ret: Type,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Operation {
    Add { lhs: Expression, rhs: Expression },
    Sub { lhs: Expression, rhs: Expression },
    Mul { lhs: Expression, rhs: Expression },
    Div { lhs: Expression, rhs: Expression },
    Assign { lhs: Identifier, rhs: Expression },
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    List,
    Bool,
    Unit,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Literal {
    typ: Type,
    value: TypeValue,
}

impl Literal {
    fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Literal(l) => match l {
                tokenizer::Literal::String(s) => Some(Self {
                    typ: Type::String,
                    value: TypeValue::String(s.clone()),
                }),
                tokenizer::Literal::Int(i) => Some(Self {
                    typ: Type::Int,
                    value: TypeValue::Int(*i),
                }),
                tokenizer::Literal::Float(f) => Some(Self {
                    typ: Type::Float,
                    value: TypeValue::Float(*f),
                }),
                tokenizer::Literal::Bool(b) => Some(Self {
                    typ: Type::Bool,
                    value: TypeValue::Bool(*b),
                }),
                tokenizer::Literal::Unit => Some(Self {
                    typ: Type::Unit,
                    value: TypeValue::Unit,
                }),
            },
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum TypeValue {
    Int(i64),
    Float(f64),
    String(String),
    List(Vec<Expression>),
    Bool(bool),
    Unit,
}

// utils ///////////////

fn split_with_prefix<T: Clone + PartialEq>(list: &[T], splitter: &T) -> Vec<Vec<T>> {
    let mut result: Vec<Vec<T>> = Vec::new();
    let mut current: Vec<T> = Vec::new();
    for item in list {
        if item == splitter && current.is_empty() {
            current.push(item.clone());
        } else if item == splitter {
            result.push(current.clone());
            current.clear();
            current.push(item.clone());
        } else {
            current.push(item.clone());
        }
    }

    result.push(current.clone());

    result
}

#[test]
fn test_split_with_prefix() {
    let s: Vec<char> = ":idk:lol:meow".chars().collect();
    let split = split_with_prefix(&s[..], &':');

    assert_eq!(
        split,
        vec![
            vec![':', 'i', 'd', 'k'],
            vec![':', 'l', 'o', 'l'],
            vec![':', 'm', 'e', 'o', 'w']
        ]
    );
}
