use anyhow::Context;
use serde::{Deserialize, Serialize};
use std::{cmp::PartialEq, env::ArgsOs, iter, thread::current};

use crate::tokenizer::{self, Keyword, Operator, Token, TokenList};

mod tests;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct SyntaxTree {
    items: Vec<Item>,
}

impl SyntaxTree {
    pub fn generate(code: TokenList) -> anyhow::Result<Self> {
        let tokens = code.tokens();
        let body_indices: Vec<(usize, usize)> = {
            let mut bodies = Vec::new();
            let mut parens = Vec::new();
            for (i, t) in tokens.iter().enumerate() {
                if t == &Token::LParen {
                    parens.push(i);
                } else if t == &Token::RParen {
                    if parens.len() == 1 {
                        bodies.push((parens.pop().expect("length checked but unable to pop"), i));
                    } else if parens.is_empty() {
                        return Err(anyhow::anyhow!("parenthesis are not balanced"));
                    } else {
                        parens
                            .pop()
                            .expect("pop returned None even though length checked");
                    }
                }
            }

            bodies
        };

        let mut items = Vec::new();

        for (i, (_oidx, cidx)) in body_indices.iter().enumerate() {
            if i == 0 {
                items.push(Self::item_from_tokens(&tokens[1..=(*cidx - 1)], &vec![])?);
            } else {
                let prev_idx = body_indices[i - 1].1;
                let idents: Vec<Identifier> = items
                    .iter()
                    .filter_map(|it| match it {
                        Item::Declaration(Declaration::Func(ident)) => Some(ident.clone()),
                    })
                    .collect();
                items.push(Self::item_from_tokens(
                    &tokens[(prev_idx + 2)..=(*cidx - 1)],
                    &idents,
                )?);
            }
        }

        Ok(Self { items })
    }

    fn item_from_tokens(tokens: &[Token], idents: &[Identifier]) -> anyhow::Result<Item> {
        match &tokens[..] {
            [
                Token::Keyword(Keyword::Let),
                Token::Literal(tokenizer::Literal::Unit),
                Token::Operator(Operator::Eq),
                rest @ ..,
            ] => {
                let expr = Expression::from_tokens(rest, idents)
                    .map_err(|e| e.context("unable to get expression when defining function"))?;

                let f = Func {
                    params: Vec::new(),
                    body: expr,
                    ret: Some(Type::Unit),
                };

                let fn_ident = Identifier::FuncDef {
                    name: "unit".to_string(),
                    value: f,
                };

                let decl = Declaration::Func(fn_ident);

                Ok(Item::Declaration(decl))
            }
            [
                Token::Keyword(Keyword::Let),
                Token::Identifier(fn_name),
                rest @ ..,
            ] => {
                if let Some(eq_pos) = rest
                    .iter()
                    .position(|t| t == &Token::Operator(Operator::Eq))
                {
                    let mut params: Vec<Identifier> = Vec::new();
                    for t in rest[..eq_pos].iter() {
                        if let Token::Identifier(name) = t {
                            params.push(Identifier::FuncParam {
                                name: name.to_string(),
                                typ: None,
                            });
                        } else {
                            return Err(anyhow::anyhow!(
                                "function declaration params contain non-identifier: {t:?}"
                            ));
                        }
                    }

                    let expr =
                        Expression::from_tokens(&rest[(eq_pos + 1)..], idents).map_err(|e| {
                            e.context("unable to get expression when defining function")
                        })?;
                    let typ = expr.ret_type.clone();

                    let f = Func {
                        params,
                        body: expr,
                        ret: typ,
                    };

                    let fn_ident = Identifier::FuncDef {
                        name: fn_name.clone(),
                        value: f,
                    };

                    let decl = Declaration::Func(fn_ident);

                    Ok(Item::Declaration(decl))
                } else {
                    Err(anyhow::anyhow!("could not create declaration"))
                }
            }

            t => return Err(anyhow::anyhow!("cannot create item from {t:?}")),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Item {
    Declaration(Declaration),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Declaration {
    Func(Identifier),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Expression {
    pub(crate) local_vars: Vec<Identifier>,
    pub(crate) expression_body: ExpressionBody,
    pub(crate) ret_type: Option<Type>,
}

impl Expression {
    /// creates an expression from tokens, either with local variables or just a basic expression
    fn from_tokens(tokens: &[Token], idents: &[Identifier]) -> anyhow::Result<Self> {
        if let Some(Token::Keyword(Keyword::Let)) = tokens.get(0) {
            let in_pos = tokens
                .iter()
                .position(|t| t == &Token::Keyword(Keyword::In))
                .ok_or(anyhow::anyhow!("no 'in' token after let declaration"))?;

            let expression_vars = {
                let mut vars: Vec<Identifier> = Vec::new();
                let vars_tokens =
                    split_with_prefix(&tokens[..in_pos], &Token::Keyword(Keyword::Let));
                for vts in vars_tokens {
                    let var = Self::var_from_tokens(&vts[..], idents).map_err(|e| {
                        e.context("unable to get variable in expression definition")
                    })?;
                    vars.push(var);
                }

                vars
            };

            let idents = {
                let mut idents = idents.to_vec();
                idents.append(&mut expression_vars.clone());
                idents
            };

            let (expression_body, typ) = match &tokens[in_pos..] {
                [
                    Token::Keyword(Keyword::In),
                    Token::LParen,
                    body @ ..,
                    Token::RParen,
                ] => ExpressionBody::from_tokens(body, &idents)?,
                _ => todo!(),
            };

            Ok(Self {
                local_vars: expression_vars,
                expression_body,
                ret_type: typ,
            })
        } else if let Some(Token::LParen) = tokens.get(0) {
            let (expression_body, typ) = match tokens {
                [Token::LParen, body @ .., Token::RParen] => {
                    ExpressionBody::from_tokens(body, idents)?
                }
                _ => todo!(),
            };

            Ok(Self {
                local_vars: Vec::new(),
                expression_body,
                ret_type: typ,
            })
        } else if tokens.len() == 1 {
            match tokens {
                [Token::Literal(literal)] => {
                    let lit = Literal::from_tokenizer_literal(literal);
                    let typ = lit.typ.clone();
                    Ok(Self {
                        local_vars: Vec::new(),
                        expression_body: ExpressionBody::Literal(Box::new(lit)),
                        ret_type: Some(typ),
                    })
                }
                [Token::Identifier(ident)] => Ok({
                    let var_type = {
                        if let Some(Some(typ)) = idents.iter().find_map(|id| match id {
                            Identifier::FuncDef {
                                name,
                                value:
                                    Func {
                                        params,
                                        body: _,
                                        ret,
                                    },
                            } => {
                                if name == ident && params.is_empty() {
                                    Some(ret)
                                } else {
                                    None
                                }
                            }
                            Identifier::VarDef { name, value } => {
                                if name == ident {
                                    Some(&value.ret_type)
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        }) {
                            Some(typ.clone())
                        } else {
                            None
                        }
                    };
                    Self {
                        local_vars: Vec::new(),
                        expression_body: ExpressionBody::VarRef(VarRef {
                            name: ident.clone(),
                        }),
                        ret_type: var_type,
                    }
                }),
                _ => Err(anyhow::anyhow!(
                    "cannot create expression from token {tokens:?}"
                )),
            }
        } else {
            return Err(anyhow::anyhow!("{tokens:#?} is not an expression"));
        }
    }

    /// creates a variable definition from tokens representing an expression variable definition,
    /// this works on one definition at a time and can't have the 'in' token at the end
    fn var_from_tokens(tokens: &[Token], idents: &[Identifier]) -> anyhow::Result<Identifier> {
        match tokens {
            [
                Token::Keyword(Keyword::Let),
                Token::Identifier(name),
                Token::Operator(Operator::Eq),
                expression @ ..,
            ] => {
                let expr = Self::from_tokens(expression, idents)
                    .map_err(|e| e.context("unable to get expression in variable declaration"))?;
                Ok(Identifier::VarDef {
                    name: name.clone(),
                    value: expr,
                })
            }

            _ => Err(anyhow::anyhow!("{tokens:#?} is not a variable declaration")),
        }
    }

    /// finds and creates multiple expressions as a list for when you have multiple expressions in
    /// a row in operations or function calls.
    /// must only include valid expressions seperated by spaces
    fn multiple_from_tokens(tokens: &[Token], idents: &[Identifier]) -> anyhow::Result<Vec<Self>> {
        // finds the position of expression bodies
        let body_indices: Vec<(usize, usize)> = {
            let mut bodies = Vec::new();
            let mut parens = Vec::new();
            for (i, t) in tokens.iter().enumerate() {
                if t == &Token::LParen {
                    parens.push(i);
                } else if t == &Token::RParen {
                    if parens.len() == 1 {
                        bodies.push((parens.pop().expect("length checked but unable to pop"), i));
                    } else if parens.is_empty() {
                        return Err(anyhow::anyhow!("parenthesis are not balanced"));
                    } else {
                        parens
                            .pop()
                            .expect("pop returned None even though length checked");
                    }
                }
            }

            bodies
        };

        let mut expressions = Vec::new();
        if body_indices.is_empty() {
            for t in tokens {
                expressions.push(Expression::from_tokens(&vec![t.clone()][..], idents)?);
            }
        } else {
            for (i, (_oidx, cidx)) in body_indices.iter().enumerate() {
                if i == 0 {
                    expressions.push(Expression::from_tokens(&tokens[0..=*cidx], idents)?);
                } else {
                    let prev_idx = body_indices[i - 1].1;
                    expressions.push(Expression::from_tokens(
                        &tokens[(prev_idx + 1)..=*cidx],
                        idents,
                    )?);
                }
            }
        }

        Ok(expressions)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum ExpressionBody {
    Operation(Box<Operation>),
    FuncCall(Box<FuncCall>),
    Literal(Box<Literal>),
    VarRef(VarRef),
    Expression(Box<Expression>),
    List(Vec<Expression>),
    Func(Box<Func>),
}

impl ExpressionBody {
    /// creates an expression body from tokens representing an expression body inside parenthesis
    /// but the parenthesis must not be included
    fn from_tokens(
        tokens: &[Token],
        idents: &[Identifier],
    ) -> anyhow::Result<(ExpressionBody, Option<Type>)> {
        if tokens.len() == 1 {
            if let Some(l) = Literal::from_token(
                tokens
                    .get(0)
                    .expect("length checked but still unable to get token"),
            ) {
                Ok((Self::Literal(Box::new(l)), None))
            } else if let Token::Identifier(name) = tokens
                .get(0)
                .expect("length checked but still unable to get token")
            {
                Ok((Self::VarRef(VarRef { name: name.clone() }), None))
            } else {
                Err(anyhow::anyhow!(
                    "non- literal or identifier single token expression body"
                ))
            }
        } else if let Ok(expr) = Expression::from_tokens(tokens, idents) {
            let typ = expr.ret_type.clone();
            Ok((Self::Expression(Box::new(expr)), typ))
        } else if tokens.is_empty() {
            Ok((
                Self::Literal(Box::new(Literal {
                    typ: Type::Unit,
                    value: TypeValue::Unit,
                })),
                Some(Type::Unit),
            ))
        } else if let Ok(operation) = Operation::from_tokens(tokens, idents) {
            let typ = {
                match operation {
                    Operation::Eq { lhs: _, rhs: _ } => Some(Type::Bool),

                    Operation::Add { ref lhs, ref rhs }
                    | Operation::Sub { ref lhs, ref rhs }
                    | Operation::Mul { ref lhs, ref rhs }
                    | Operation::Div { ref lhs, ref rhs } => {
                        match (lhs.ret_type.clone(), rhs.ret_type.clone()) {
                            (Some(Type::Int), Some(Type::Int)) => Some(Type::Int),
                            (Some(Type::Float), Some(Type::Float)) => Some(Type::Float),
                            (Some(Type::Float), Some(Type::Int)) => Some(Type::Float),
                            (Some(Type::Int), Some(Type::Float)) => Some(Type::Float),
                            _ => None,
                        }
                    }
                }
            };

            Ok((Self::Operation(Box::new(operation)), typ))
        } else if let Ok(list) = Self::list_from_tokens(tokens, idents) {
            Ok((list, None))
        } else if let Ok(func) = Self::func_from_tokens(tokens, idents) {
            let typ = match func {
                Self::Func(ref f) => f.ret.clone(),
                _ => unreachable!("matching on a expression body that should always be a func"),
            };
            Ok((func, typ))
        } else {
            match tokens {
                [Token::Identifier(ident), rest @ ..] => {
                    let param_expressions = Expression::multiple_from_tokens(rest, idents)?;

                    let func_type = {
                        if let Some(Some(typ)) = idents.iter().find_map(|id| match id {
                            Identifier::FuncDef {
                                name,
                                value:
                                    Func {
                                        params: _,
                                        body: _,
                                        ret,
                                    },
                            } => {
                                if name == ident {
                                    Some(ret)
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        }) {
                            Some(typ.clone())
                        } else {
                            None
                        }
                    };

                    Ok((
                        Self::FuncCall(Box::new(FuncCall {
                            name: ident.clone(),
                            params: param_expressions,
                        })),
                        func_type,
                    ))
                }
                _ => Err(anyhow::anyhow!(
                    "cannot create expression body from {tokens:?}"
                )),
            }
        }
    }

    /// creates a list expression body from bracket enclosed sets of tokens representing
    /// expressions
    fn list_from_tokens(tokens: &[Token], idents: &[Identifier]) -> anyhow::Result<Self> {
        match tokens {
            [Token::LBracket, middle @ .., Token::RBracket] => {
                let expressions = Expression::multiple_from_tokens(middle, idents)?;

                Ok(Self::List(expressions))
            }
            t => Err(anyhow::anyhow!("could not create list from tokens {t:?}")),
        }
    }

    /// creates a fn expression body from tokens representing an anonymous function with some
    /// params and an expression
    fn func_from_tokens(tokens: &[Token], idents: &[Identifier]) -> anyhow::Result<Self> {
        match tokens {
            [Token::Keyword(Keyword::Fn), rest @ ..] => {
                if let Some(arrow_pos) = rest.iter().position(|t| t == &Token::Arrow) {
                    let params = {
                        let mut params = Vec::new();
                        for t in &rest[..arrow_pos] {
                            if let Token::Identifier(n) = t {
                                params.push(Identifier::FuncParam {
                                    name: n.to_string(),
                                    typ: None,
                                });
                            } else {
                                return Err(anyhow::anyhow!(
                                    "non-identifier in fn definition: {t:?}"
                                ));
                            }
                        }

                        params
                    };

                    let idents = {
                        let mut idents = idents.to_vec();
                        idents.append(&mut params.clone());
                        idents
                    };

                    let expr = Expression::from_tokens(&rest[(arrow_pos + 1)..], &idents)?;
                    let typ = expr.ret_type.clone();

                    Ok(Self::Func(Box::new(Func {
                        params,
                        body: expr,
                        ret: typ,
                    })))
                } else {
                    Err(anyhow::anyhow!("no arrow in fn definition"))
                }
            }
            t => Err(anyhow::anyhow!("cannot create func from {t:?}")),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Identifier {
    FuncDef { name: String, value: Func },
    FuncParam { name: String, typ: Option<Type> },
    VarDef { name: String, value: Expression },
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct FuncCall {
    pub(crate) name: String,
    pub(crate) params: Vec<Expression>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct VarRef {
    pub(crate) name: String,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Func {
    pub(crate) params: Vec<Identifier>,
    pub(crate) body: Expression,
    pub(crate) ret: Option<Type>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Operation {
    Add { lhs: Expression, rhs: Expression },
    Sub { lhs: Expression, rhs: Expression },
    Mul { lhs: Expression, rhs: Expression },
    Div { lhs: Expression, rhs: Expression },
    Eq { lhs: Expression, rhs: Expression },
}

impl Operation {
    fn from_tokens(tokens: &[Token], idents: &[Identifier]) -> anyhow::Result<Self> {
        match tokens {
            [Token::Operator(o), Token::Literal(lhs), Token::Literal(rhs)] => {
                let lhs_expr = {
                    let lit = Literal::from_tokenizer_literal(lhs);
                    let typ = lit.typ.clone();
                    Expression {
                        local_vars: Vec::new(),
                        expression_body: ExpressionBody::Literal(Box::new(lit)),
                        ret_type: Some(typ),
                    }
                };

                let rhs_expr = {
                    let lit = Literal::from_tokenizer_literal(rhs);
                    let typ = lit.typ.clone();
                    Expression {
                        local_vars: Vec::new(),
                        expression_body: ExpressionBody::Literal(Box::new(lit)),
                        ret_type: Some(typ),
                    }
                };

                Ok(match o {
                    Operator::Add => Self::Add {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    },
                    Operator::Sub => Self::Sub {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    },
                    Operator::Mul => Self::Mul {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    },
                    Operator::Div => Self::Div {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    },
                    Operator::Eq => Self::Eq {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    },
                })
            }
            [Token::Operator(o), rest @ ..] => {
                let expressions = Expression::multiple_from_tokens(rest, idents)?;
                if expressions.len() == 2 {
                    Ok(match o {
                        Operator::Add => Self::Add {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        },
                        Operator::Sub => Self::Sub {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        },
                        Operator::Mul => Self::Mul {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        },
                        Operator::Div => Self::Div {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        },
                        Operator::Eq => Self::Eq {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        },
                    })
                } else {
                    Err(anyhow::anyhow!(
                        "number of expressions does not match operator"
                    ))
                }
            }

            _ => Err(anyhow::anyhow!("no operator at the start of expression")),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Unit,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Literal {
    pub(crate) typ: Type,
    pub(crate) value: TypeValue,
}

impl Literal {
    fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Literal(l) => Some(Self::from_tokenizer_literal(l)),
            _ => None,
        }
    }

    fn from_tokenizer_literal(literal: &tokenizer::Literal) -> Self {
        match literal {
            tokenizer::Literal::String(s) => Self {
                typ: Type::String,
                value: TypeValue::String(s.clone()),
            },
            tokenizer::Literal::Int(i) => Self {
                typ: Type::Int,
                value: TypeValue::Int(*i),
            },
            tokenizer::Literal::Float(f) => Self {
                typ: Type::Float,
                value: TypeValue::Float(*f),
            },
            tokenizer::Literal::Bool(b) => Self {
                typ: Type::Bool,
                value: TypeValue::Bool(*b),
            },
            tokenizer::Literal::Unit => Self {
                typ: Type::Unit,
                value: TypeValue::Unit,
            },
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum TypeValue {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Unit,
}

// utils ///////////////

/// splits a list by some prefix, including that prefix
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
