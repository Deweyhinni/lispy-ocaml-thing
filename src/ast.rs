use anyhow::Context;
use serde::{Deserialize, Serialize};
use std::{cmp::PartialEq, error::Error, fmt::Display, iter};

use crate::tokenizer::{self, Keyword, Operator, Token, TokenList};

mod tests;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct SyntaxTree {
    pub items: Vec<Item>,
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
                let expr = Expression::from_tokens(rest, idents)?;

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
                    let params = Self::params_from_tokens(&rest[..eq_pos])?;

                    let idents = {
                        let mut idents = idents.to_vec();
                        idents.append(&mut params.clone());
                        idents
                    };

                    let expr = Expression::from_tokens(&rest[(eq_pos + 1)..], &idents)?;
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

    fn params_from_tokens(tokens: &[Token]) -> Result<Vec<Identifier>, ParseError> {
        let param_indices: Vec<(usize, usize)> = {
            let mut params = Vec::new();
            let mut parens = Vec::new();
            for (i, t) in tokens.iter().enumerate() {
                if t == &Token::LParen {
                    parens.push(i);
                } else if t == &Token::RParen {
                    if parens.len() == 1 {
                        params.push((parens.pop().expect("length checked but unable to pop"), i));
                    } else if parens.is_empty() {
                        return Err(ParseError::ParseFailed(format!(
                            "parenthesis are not balanced in: {:?}",
                            tokens
                        )));
                    } else {
                        parens
                            .pop()
                            .expect("pop returned None even though length checked");
                    }
                }
            }

            params
        };

        if param_indices.is_empty() {
            let mut params = Vec::new();
            for t in tokens {
                if let Token::Identifier(ident) = t {
                    params.push(Identifier::FuncParam {
                        name: ident.clone(),
                        typ: None,
                    });
                } else {
                    return Err(ParseError::ParseFailed(format!(
                        "unexpected token {:?} in function param definition",
                        t
                    )));
                }
            }

            Ok(params)
        } else {
            let mut params = Vec::new();
            for (oidx, cidx) in param_indices.iter() {
                match &tokens[(*oidx + 1)..*cidx] {
                    [Token::Identifier(name), Token::Colon, rest @ ..] => {
                        params.push(Identifier::FuncParam {
                            name: name.clone(),
                            typ: Some(Type::from_tokens(rest)?),
                        });
                    }
                    _ => {
                        return Err(ParseError::ParseFailed(format!(
                            "unrecognized token structure for function param in: {:?}",
                            tokens
                        )));
                    }
                }
            }

            Ok(params)
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
    fn from_tokens(tokens: &[Token], idents: &[Identifier]) -> Result<Self, ParseError> {
        if let Some(Token::Keyword(Keyword::Let)) = tokens.get(0) {
            let in_pos = tokens
                .iter()
                .position(|t| t == &Token::Keyword(Keyword::In))
                .ok_or(ParseError::ParseFailed(format!(
                    "no 'in' token after let declaration in: {:?}",
                    tokens
                )))?;

            let expression_vars = {
                let mut vars: Vec<Identifier> = Vec::new();
                let vars_tokens =
                    split_with_prefix(&tokens[..in_pos], &Token::Keyword(Keyword::Let));
                for vts in vars_tokens {
                    let var = Self::var_from_tokens(&vts[..], idents)?;
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
                _ => {
                    return Err(ParseError::ParseFailed(format!(
                        "unable to create expression from {:?}",
                        tokens
                    )));
                }
            };

            Ok(Self {
                local_vars: Vec::new(),
                expression_body,
                ret_type: typ,
            })
        } else if tokens.len() == 1 {
            let (expression_body, typ) = ExpressionBody::from_tokens(tokens, idents)?;
            Ok(Self {
                local_vars: Vec::new(),
                expression_body,
                ret_type: typ,
            })
        } else {
            return Err(ParseError::ParseFailed(format!(
                "{:?} is not an expression",
                tokens
            )));
        }
    }

    /// creates a variable definition from tokens representing an expression variable definition,
    /// this works on one definition at a time and can't have the 'in' token at the end
    fn var_from_tokens(tokens: &[Token], idents: &[Identifier]) -> Result<Identifier, ParseError> {
        match tokens {
            [
                Token::Keyword(Keyword::Let),
                Token::Identifier(name),
                Token::Operator(Operator::Eq),
                expression @ ..,
            ] => {
                let expr = Self::from_tokens(expression, idents)?;
                Ok(Identifier::VarDef {
                    name: name.clone(),
                    value: expr,
                })
            }

            _ => Err(ParseError::NotMatched),
        }
    }

    /// finds and creates multiple expressions as a list for when you have multiple expressions in
    /// a row in operations or function calls.
    /// must only include valid expressions seperated by spaces
    fn multiple_from_tokens(
        tokens: &[Token],
        idents: &[Identifier],
    ) -> Result<Vec<Self>, ParseError> {
        // finds the position of expression bodies
        let expressions = {
            let mut expressions = Vec::new();
            let mut current_expr = Vec::new();
            let mut parens = 0;
            for (i, t) in tokens.iter().enumerate() {
                match t {
                    Token::LParen => {
                        parens += 1;
                        current_expr.push(Token::LParen);
                    }
                    Token::RParen => {
                        if parens == 1 {
                            current_expr.push(Token::RParen);
                            parens -= 1;
                            let new_expr = match Expression::from_tokens(&current_expr, idents) {
                                Ok(e) => e,
                                Err(err) => return Err(err),
                            };
                            expressions.push(new_expr);
                            current_expr.clear();
                        } else if parens == 0 {
                            return Err(ParseError::ParseFailed(format!(
                                "parenthesis are not balanced in: {:?}",
                                tokens
                            )));
                        } else {
                            parens -= 1;
                            current_expr.push(Token::RParen);
                        }
                    }
                    t => {
                        if parens == 0 {
                            match Expression::from_tokens(&[t.clone()], idents) {
                                Ok(e) => expressions.push(e),
                                Err(err) => return Err(err),
                            }
                        } else {
                            current_expr.push(t.clone());
                        }
                    }
                }
            }

            expressions
        };

        Ok(expressions)
    }

    fn expression_until_stop_token(
        stop: &Token,
        tokens: &[Token],
        idents: &[Identifier],
    ) -> Result<(Expression, usize), ParseError> {
        match tokens {
            [Token::LParen, ..] | [Token::Keyword(Keyword::Let), ..] => {
                let mut parens = 0;
                let mut expr_tokens = Vec::new();
                let mut stop_pos = 0;

                for (i, t) in tokens.iter().enumerate() {
                    println!("token: {:?}", t);
                    println!("expr: {:?}", expr_tokens);
                    match t {
                        Token::LParen => {
                            parens += 1;
                            expr_tokens.push(t.clone());
                        }
                        Token::RParen => {
                            parens -= 1;
                            expr_tokens.push(t.clone());
                        }
                        t => {
                            if t == stop && parens == 0 {
                                stop_pos = i;
                                break;
                            } else {
                                expr_tokens.push(t.clone());
                            }
                        }
                    }
                }

                println!("idk: {:?}", expr_tokens);

                Ok((Expression::from_tokens(&expr_tokens[..], idents)?, stop_pos))
            }
            [t, b, ..] => {
                if b == stop {
                    Ok((Expression::from_tokens(&[t.clone()], idents)?, 1usize))
                } else {
                    return Err(ParseError::ParseFailed(format!(
                        "second token is not the stop token in: {:?}",
                        tokens
                    )));
                }
            }
            _ => {
                return Err(ParseError::ParseFailed(format!(
                    "unexpected start token for expression in: {:?}",
                    tokens,
                )));
            }
        }
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
    Conditional(Box<Conditional>),
    Extern(Type),
}

impl ExpressionBody {
    /// creates an expression body from tokens representing an expression body inside parenthesis
    /// but the parenthesis must not be included
    fn from_tokens(
        tokens: &[Token],
        idents: &[Identifier],
    ) -> Result<(ExpressionBody, Option<Type>), ParseError> {
        if tokens.len() == 1 {
            return Self::from_single_token(&tokens[0], idents);
        } else if let Ok(expr) = Expression::from_tokens(tokens, idents) {
            let typ = expr.ret_type.clone();
            return Ok((Self::Expression(Box::new(expr)), typ));
        } else if tokens.is_empty() {
            return Ok((
                Self::Literal(Box::new(Literal {
                    typ: Type::Unit,
                    value: TypeValue::Unit,
                })),
                Some(Type::Unit),
            ));
        }

        match Self::operation_from_tokens(tokens, idents) {
            Ok(r) => return Ok(r),
            Err(ParseError::NotMatched) => (),
            Err(ParseError::ParseFailed(why)) => return Err(ParseError::ParseFailed(why)),
        }

        match Self::list_from_tokens(tokens, idents) {
            Ok(r) => return Ok(r),
            Err(ParseError::NotMatched) => (),
            Err(ParseError::ParseFailed(why)) => return Err(ParseError::ParseFailed(why)),
        }

        match Self::func_from_tokens(tokens, idents) {
            Ok(r) => return Ok(r),
            Err(ParseError::NotMatched) => (),
            Err(ParseError::ParseFailed(why)) => return Err(ParseError::ParseFailed(why)),
        }

        match Self::conditional_from_tokens(tokens, idents) {
            Ok(r) => return Ok(r),
            Err(ParseError::NotMatched) => (),
            Err(ParseError::ParseFailed(why)) => return Err(ParseError::ParseFailed(why)),
        }

        match Self::func_call_from_tokens(tokens, idents) {
            Ok(r) => return Ok(r),
            Err(ParseError::NotMatched) => (),
            Err(ParseError::ParseFailed(why)) => return Err(ParseError::ParseFailed(why)),
        }

        match Self::extern_from_tokens(tokens, idents) {
            Ok((body, typ)) => return Ok((body, Some(typ))),
            Err(ParseError::NotMatched) => (),
            Err(ParseError::ParseFailed(why)) => return Err(ParseError::ParseFailed(why)),
        }

        Err(ParseError::ParseFailed(format!(
            "cannot create expression body from {:?}",
            tokens
        )))
    }

    fn extern_from_tokens(
        tokens: &[Token],
        _idents: &[Identifier],
    ) -> Result<(Self, Type), ParseError> {
        match tokens {
            [Token::Keyword(Keyword::Extern)] => Ok((Self::Extern(Type::Unit), Type::Unit)),
            [Token::Keyword(Keyword::Extern), rest @ ..] => {
                let typ = Type::from_tokens(rest)?;
                Ok((Self::Extern(typ.clone()), typ))
            }
            _ => Err(ParseError::NotMatched),
        }
    }

    fn operation_from_tokens(
        tokens: &[Token],
        idents: &[Identifier],
    ) -> Result<(Self, Option<Type>), ParseError> {
        let operation = Operation::from_tokens(tokens, idents)?;
        let typ = {
            match operation {
                Operation::Eq { lhs: _, rhs: _ }
                | Operation::Bigger { lhs: _, rhs: _ }
                | Operation::Smaller { lhs: _, rhs: _ }
                | Operation::BiggerEq { lhs: _, rhs: _ }
                | Operation::SmallerEq { lhs: _, rhs: _ } => Some(Type::Bool),
                Operation::Not { expr: _ } => Some(Type::Bool),

                Operation::Add { ref lhs, ref rhs }
                | Operation::Sub { ref lhs, ref rhs }
                | Operation::Mul { ref lhs, ref rhs }
                | Operation::Div { ref lhs, ref rhs }
                | Operation::Modulo { ref lhs, ref rhs } => {
                    match (lhs.ret_type.clone(), rhs.ret_type.clone()) {
                        (Some(Type::Int), Some(Type::Int)) => Some(Type::Int),
                        (Some(Type::Float), Some(Type::Float)) => Some(Type::Float),
                        (Some(Type::Float), Some(Type::Int)) => Some(Type::Float),
                        (Some(Type::Int), Some(Type::Float)) => Some(Type::Float),
                        (Some(Type::String), Some(Type::String)) => Some(Type::String),
                        (Some(Type::List(lhs_list_typ)), Some(Type::List(rhs_list_type))) => {
                            if lhs_list_typ == rhs_list_type {
                                Some(Type::List(rhs_list_type))
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                }
            }
        };

        Ok((Self::Operation(Box::new(operation)), typ))
    }

    fn from_single_token(
        token: &Token,
        idents: &[Identifier],
    ) -> Result<(Self, Option<Type>), ParseError> {
        match token {
            Token::Literal(literal) => {
                let lit = Literal::from_tokenizer_literal(literal);
                let typ = lit.typ.clone();
                Ok((Self::Literal(Box::new(lit)), Some(typ)))
            }
            Token::Keyword(Keyword::Extern) => Ok((Self::Extern(Type::Unit), Some(Type::Unit))),
            Token::Identifier(ident) => Ok({
                let refd_ident = idents.iter().find_map(|id| match id {
                    Identifier::FuncDef {
                        name,
                        value:
                            Func {
                                params,
                                body: _,
                                ret: _,
                            },
                    } => {
                        if name == ident && params.is_empty() {
                            Some(id)
                        } else {
                            None
                        }
                    }
                    Identifier::VarDef { name, value } => {
                        if name == ident {
                            Some(id)
                        } else {
                            None
                        }
                    }
                    Identifier::FuncParam { name, typ } => {
                        if name == ident {
                            Some(id)
                        } else {
                            None
                        }
                    }
                });

                match refd_ident {
                    Some(Identifier::FuncDef {
                        name,
                        value:
                            Func {
                                params: _,
                                body: _,
                                ret,
                            },
                    }) => (
                        Self::FuncCall(Box::new(FuncCall::IdentCall {
                            name: name.clone(),
                            params: Vec::new(),
                        })),
                        ret.clone(),
                    ),
                    Some(Identifier::VarDef { name, value }) => (
                        Self::VarRef(VarRef {
                            name: name.clone(),
                            typ: value.ret_type.clone(),
                        }),
                        value.ret_type.clone(),
                    ),
                    Some(Identifier::FuncParam { name, typ }) => (
                        Self::VarRef(VarRef {
                            name: name.clone(),
                            typ: typ.clone(),
                        }),
                        typ.clone(),
                    ),
                    None => (
                        Self::VarRef(VarRef {
                            name: ident.clone(),
                            typ: None,
                        }),
                        None,
                    ),
                }
            }),
            _ => Err(ParseError::ParseFailed(format!(
                "non- literal or identifier single token expression body: {:?}",
                token
            ))),
        }
    }

    fn func_call_from_tokens(
        tokens: &[Token],
        idents: &[Identifier],
    ) -> Result<(Self, Option<Type>), ParseError> {
        match tokens {
            [Token::Identifier(ident), rest @ ..] => {
                let param_expressions = Expression::multiple_from_tokens(rest, idents)?;

                if let Some(func) = idents.iter().find_map(|id| match id {
                    Identifier::FuncDef { name, value } => {
                        if name == ident {
                            Some(value.clone())
                        } else {
                            None
                        }
                    }
                    Identifier::VarDef {
                        name,
                        value:
                            Expression {
                                local_vars: _,
                                expression_body: ExpressionBody::Func(f),
                                ret_type,
                            },
                    } => {
                        if name == ident {
                            Some(*f.clone())
                        } else {
                            None
                        }
                    }
                    Identifier::FuncParam {
                        name,
                        typ: Some(Type::Func { params, ret }),
                    } => {
                        if name == ident {
                            Some(Func {
                                params: params
                                    .iter()
                                    .enumerate()
                                    .map(|(i, p)| Identifier::FuncParam {
                                        name: format!("param_{}", i),
                                        typ: Some(*p.clone()),
                                    })
                                    .collect(),
                                body: Expression {
                                    local_vars: vec![],
                                    expression_body: ExpressionBody::Literal(Box::new(Literal {
                                        typ: Type::Unit,
                                        value: TypeValue::Unit,
                                    })),
                                    ret_type: Some(Type::Func {
                                        params: params.clone(),
                                        ret: ret.clone(),
                                    }),
                                },
                                ret: Some(*ret.clone()),
                            })
                        } else {
                            None
                        }
                    }
                    _ => None,
                }) {
                    if func.params.len() != param_expressions.len() {
                        if func.params.len() < param_expressions.len() {
                            match func {
                                Func {
                                    params: _,
                                    body:
                                        Expression {
                                            local_vars: _,
                                            expression_body: _,
                                            ret_type:
                                                Some(Type::Func {
                                                    params: _,
                                                    ret: fc_ret,
                                                }),
                                        },
                                    ret,
                                } => {
                                    let outer_func_params =
                                        param_expressions[0..(func.params.len())].to_vec();
                                    let inner_func_params =
                                        param_expressions[(func.params.len())..].to_vec();

                                    let fc = FuncCall::AnonCall {
                                        params: inner_func_params,
                                        func: Expression {
                                            local_vars: Vec::new(),
                                            expression_body: ExpressionBody::FuncCall(Box::new(
                                                FuncCall::IdentCall {
                                                    name: ident.clone(),
                                                    params: outer_func_params,
                                                },
                                            )),
                                            ret_type: ret.clone(),
                                        },
                                    };

                                    Ok((Self::FuncCall(Box::new(fc)), Some(*fc_ret.clone())))
                                }
                                _ => Err(ParseError::ParseFailed(format!(
                                    "func return type is not fn in: {:?} ; {:?}",
                                    tokens, func,
                                ))),
                            }
                        } else {
                            Err(ParseError::ParseFailed(format!(
                                "function params don't match function in: {:?}",
                                tokens
                            )))
                        }
                    } else {
                        Ok((
                            Self::FuncCall(Box::new(FuncCall::IdentCall {
                                name: ident.clone(),
                                params: param_expressions,
                            })),
                            func.ret.clone(),
                        ))
                    }
                } else {
                    Ok((
                        Self::FuncCall(Box::new(FuncCall::IdentCall {
                            name: ident.clone(),
                            params: param_expressions,
                        })),
                        None,
                    ))
                }
            }
            _ => match Expression::multiple_from_tokens(tokens, idents) {
                Ok(exprs) => match &exprs[..] {
                    [
                        Expression {
                            local_vars: _,
                            expression_body: ExpressionBody::Func(f),
                            ret_type: Some(Type::Func { params, ret }),
                        },
                        rest @ ..,
                    ] => {
                        if rest.len() == f.params.len() {
                            Ok((
                                Self::FuncCall(Box::new(FuncCall::AnonCall {
                                    params: rest.to_vec(),
                                    func: Expression {
                                        local_vars: vec![],
                                        expression_body: ExpressionBody::Func(f.clone()),
                                        ret_type: Some(Type::Func {
                                            params: params.clone(),
                                            ret: ret.clone(),
                                        }),
                                    },
                                })),
                                Some(*ret.clone()),
                            ))
                        } else {
                            Err(ParseError::ParseFailed(format!(
                                "call params do not match func params in {:?}",
                                tokens
                            )))
                        }
                    }
                    _ => Err(ParseError::NotMatched),
                },
                Err(ParseError::NotMatched) => Err(ParseError::NotMatched),
                Err(ParseError::ParseFailed(why)) => Err(ParseError::ParseFailed(why)),
            },
        }
    }

    /// creates a list expression body from bracket enclosed sets of tokens representing
    /// expressions
    fn list_from_tokens(
        tokens: &[Token],
        idents: &[Identifier],
    ) -> Result<(Self, Option<Type>), ParseError> {
        match tokens {
            [Token::LBracket, middle @ .., Token::RBracket] => {
                let expressions = Expression::multiple_from_tokens(middle, idents)?;
                let list_type = {
                    if expressions.is_empty() {
                        Type::Unit
                    } else if let Some(expr) = expressions.get(0) {
                        let typ = expr.ret_type.clone();
                        for e in &expressions {
                            if e.ret_type != typ {
                                return Err(ParseError::ParseFailed(format!(
                                    "list has multiple expression types in: {:?}",
                                    tokens
                                )));
                            }
                        }
                        typ.unwrap_or(Type::Unit)
                    } else {
                        unreachable!("length checked")
                    }
                };

                Ok((
                    Self::List(expressions),
                    Some(Type::List(Box::new(list_type))),
                ))
            }
            _ => Err(ParseError::NotMatched),
        }
    }

    /// creates a fn expression body from tokens representing an anonymous function with some
    /// params and an expression
    fn func_from_tokens(
        tokens: &[Token],
        idents: &[Identifier],
    ) -> Result<(Self, Option<Type>), ParseError> {
        match tokens {
            [Token::Keyword(Keyword::Fn), rest @ ..] => {
                if let Some(arrow_pos) = rest.iter().position(|t| t == &Token::Arrow) {
                    let params = SyntaxTree::params_from_tokens(&rest[..arrow_pos])?;

                    let idents = {
                        let mut idents = idents.to_vec();
                        idents.append(&mut params.clone());
                        idents
                    };

                    let expr = Expression::from_tokens(&rest[(arrow_pos + 1)..], &idents)?;
                    let ret_typ = expr.ret_type.clone();

                    let self_typ = {
                        if let Ok(params_types) = params
                            .iter()
                            .map(|i| match i {
                                Identifier::FuncParam {
                                    name: _,
                                    typ: Some(t),
                                } => Ok(Box::new(t.clone())),
                                _ => Err(anyhow::anyhow!(
                                    "non- function param in function param identifiers"
                                )),
                            })
                            .collect::<anyhow::Result<Vec<Box<Type>>>>()
                        {
                            Some(Type::Func {
                                params: params_types,
                                ret: Box::new(expr.ret_type.clone().ok_or(
                                    ParseError::ParseFailed(format!(
                                        "no return type for func: {:?}",
                                        tokens
                                    )),
                                )?),
                            })
                        } else {
                            None
                        }
                    };

                    Ok((
                        Self::Func(Box::new(Func {
                            params,
                            body: expr,
                            ret: ret_typ,
                        })),
                        self_typ,
                    ))
                } else {
                    Err(ParseError::ParseFailed(format!(
                        "no arrow in fn definition: {:?}",
                        tokens
                    )))
                }
            }
            t => Err(ParseError::NotMatched),
        }
    }

    fn conditional_from_tokens(
        tokens: &[Token],
        idents: &[Identifier],
    ) -> Result<(Self, Option<Type>), ParseError> {
        match tokens {
            [Token::Keyword(Keyword::If), rest @ ..] => {
                let (if_expr, then_pos) = Expression::expression_until_stop_token(
                    &Token::Keyword(Keyword::Then),
                    rest,
                    idents,
                )?;
                println!("if_expr: {:?}", if_expr);
                let (then_expr, else_pos) = Expression::expression_until_stop_token(
                    &Token::Keyword(Keyword::Else),
                    &rest[(then_pos + 1)..],
                    idents,
                )?;
                println!("then_expr: {:?}", then_expr);

                println!("then_pos: {:?}, else_pos: {:?}", then_pos, else_pos);

                let else_expr =
                    Expression::from_tokens(&rest[(then_pos + else_pos + 2)..], idents)?;

                println!("else_expr: {:?}", else_expr);

                let then_type = then_expr.ret_type.clone();
                let else_type = else_expr.ret_type.clone();
                let typ = if then_type == else_type {
                    then_type
                } else if then_type == None && else_type != None {
                    else_type
                } else if else_type == None && then_type != None {
                    then_type
                } else {
                    return Err(ParseError::ParseFailed(format!(
                        "then and else expression return types do not match in: {:?}",
                        tokens
                    )));
                };

                Ok((
                    Self::Conditional(Box::new(Conditional {
                        cond: if_expr,
                        then: then_expr,
                        els: else_expr,
                    })),
                    typ,
                ))
            }
            _ => Err(ParseError::NotMatched),
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
pub enum FuncCall {
    IdentCall {
        name: String,
        params: Vec<Expression>,
    },
    AnonCall {
        params: Vec<Expression>,
        func: Expression,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct VarRef {
    pub(crate) name: String,
    pub(crate) typ: Option<Type>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Func {
    pub(crate) params: Vec<Identifier>,
    pub(crate) body: Expression,
    pub(crate) ret: Option<Type>,
}

impl Func {
    fn func_type(&self) -> Result<Type, ParseError> {
        let params = self
            .params
            .iter()
            .map(|ident| match ident {
                Identifier::FuncParam { name: _, typ } => Ok(Box::new(typ.clone().ok_or(
                    ParseError::ParseFailed(String::from("function param has None type")),
                )?)),
                _ => Err(ParseError::ParseFailed(String::from(
                    "function param has non-param identifier",
                ))),
            })
            .collect::<Result<Vec<Box<Type>>, ParseError>>()?;

        let ret = Box::new(
            self.ret
                .clone()
                .ok_or(ParseError::ParseFailed(String::from(
                    "function param has None return type",
                )))?,
        );

        Ok(Type::Func { params, ret })
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Conditional {
    pub(crate) cond: Expression,
    pub(crate) then: Expression,
    pub(crate) els: Expression,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Operation {
    Add { lhs: Expression, rhs: Expression },
    Sub { lhs: Expression, rhs: Expression },
    Mul { lhs: Expression, rhs: Expression },
    Div { lhs: Expression, rhs: Expression },
    Modulo { lhs: Expression, rhs: Expression },
    Eq { lhs: Expression, rhs: Expression },
    Not { expr: Expression },
    Bigger { lhs: Expression, rhs: Expression },
    Smaller { lhs: Expression, rhs: Expression },
    BiggerEq { lhs: Expression, rhs: Expression },
    SmallerEq { lhs: Expression, rhs: Expression },
}

impl Operation {
    fn from_tokens(tokens: &[Token], idents: &[Identifier]) -> Result<Self, ParseError> {
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

                match o {
                    Operator::Add => Ok(Self::Add {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    }),
                    Operator::Sub => Ok(Self::Sub {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    }),
                    Operator::Mul => Ok(Self::Mul {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    }),
                    Operator::Div => Ok(Self::Div {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    }),
                    Operator::Modulo => Ok(Self::Modulo {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    }),
                    Operator::Eq => Ok(Self::Eq {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    }),
                    Operator::Bigger => Ok(Self::Bigger {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    }),
                    Operator::Smaller => Ok(Self::Smaller {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    }),
                    Operator::BiggerEq => Ok(Self::BiggerEq {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    }),
                    Operator::SmallerEq => Ok(Self::SmallerEq {
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                    }),
                    Operator::Not => Err(ParseError::ParseFailed(format!(
                        "too many expression for not operator in: {:?}",
                        tokens
                    ))),
                }
            }
            [Token::Operator(o), rest @ ..] => {
                let expressions = Expression::multiple_from_tokens(rest, idents)?;
                if expressions.len() == 2 {
                    match o {
                        Operator::Add => Ok(Self::Add {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        }),
                        Operator::Sub => Ok(Self::Sub {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        }),
                        Operator::Mul => Ok(Self::Mul {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        }),
                        Operator::Div => Ok(Self::Div {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        }),
                        Operator::Modulo => Ok(Self::Modulo {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        }),
                        Operator::Eq => Ok(Self::Eq {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        }),
                        Operator::Bigger => Ok(Self::Bigger {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        }),
                        Operator::Smaller => Ok(Self::Smaller {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        }),
                        Operator::BiggerEq => Ok(Self::BiggerEq {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        }),
                        Operator::SmallerEq => Ok(Self::SmallerEq {
                            lhs: expressions[0].clone(),
                            rhs: expressions[1].clone(),
                        }),
                        Operator::Not => Err(ParseError::ParseFailed(format!(
                            "too many tokens for not operator in: {:?}",
                            tokens
                        ))),
                    }
                } else if expressions.len() == 1 {
                    match o {
                        Operator::Not => Ok(Self::Not {
                            expr: expressions[0].clone(),
                        }),
                        _ => Err(ParseError::ParseFailed(format!(
                            "too few expressions for operator in: {:?}",
                            tokens
                        ))),
                    }
                } else {
                    Err(ParseError::ParseFailed(format!(
                        "number of expressions does not match operator in: {:?}",
                        tokens
                    )))
                }
            }

            _ => Err(ParseError::NotMatched),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Type {
    Int,
    Float,
    Char,
    String,
    List(Box<Type>),
    Func {
        params: Vec<Box<Type>>,
        ret: Box<Type>,
    },
    Bool,
    Unit,
}

impl Type {
    fn from_tokens(tokens: &[Token]) -> Result<Self, ParseError> {
        println!("tokens: {:?}", tokens);
        match tokens {
            [Token::Identifier(name)] => match name.as_str() {
                "Int" => Ok(Self::Int),
                "Float" => Ok(Self::Float),
                "Char" => Ok(Self::Char),
                "Bool" => Ok(Self::Bool),
                "String" => Ok(Self::String),
                "Unit" => Ok(Self::Unit),
                _ => Err(ParseError::NotMatched),
            },
            [Token::LBracket, middle @ .., Token::RBracket] => {
                Ok(Self::List(Box::new(Self::from_tokens(middle)?)))
            }
            [
                Token::LParen,
                Token::LBracket,
                middle @ ..,
                Token::RBracket,
                Token::RParen,
            ] => Ok(Self::List(Box::new(Self::from_tokens(middle)?))),
            [Token::LParen, .., Token::RParen] => Self::func_type_from_tokens(tokens),
            _ => Err(ParseError::NotMatched),
        }
    }

    fn func_type_from_tokens(tokens: &[Token]) -> Result<Self, ParseError> {
        match tokens {
            [Token::LParen, middle @ .., Token::RParen] => {
                if let Some((in_tokens, arrow_pos)) =
                    paren_balanced_until_stop(middle, &Token::Arrow)
                {
                    let out_tokens = &middle[(arrow_pos + 1)..];
                    if let Some(param_type_tokens) = split_bodies(&in_tokens) {
                        let param_types = param_type_tokens
                            .iter()
                            .map(|pts| {
                                Ok(Box::new(match Self::from_tokens(&pts) {
                                    Ok(t) => t,
                                    Err(ParseError::NotMatched) => {
                                        return Err(ParseError::ParseFailed(String::from(
                                            format!("non-type in type definition: {:?}", pts),
                                        )));
                                    }
                                    Err(ParseError::ParseFailed(why)) => {
                                        return Err(ParseError::ParseFailed(why));
                                    }
                                }))
                            })
                            .collect::<Result<Vec<Box<Type>>, ParseError>>()?;

                        let ret_type = Self::from_tokens(out_tokens)?;

                        Ok(Self::Func {
                            params: param_types,
                            ret: Box::new(ret_type),
                        })
                    } else {
                        Err(ParseError::ParseFailed(String::from(
                            "parens not balanced when splitting func type params",
                        )))
                    }
                } else {
                    Err(ParseError::NotMatched)
                }
            }
            _ => Err(ParseError::NotMatched),
        }
    }
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
            tokenizer::Literal::Char(c) => Self {
                typ: Type::Char,
                value: TypeValue::Char(*c),
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
    Char(char),
    String(String),
    Bool(bool),
    Unit,
}

// errors //////////

#[derive(Clone, Debug, PartialEq)]
pub enum ParseError {
    NotMatched,
    ParseFailed(String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for ParseError {}

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

/// gathers all tokens until a stop token not enclosed by parens
fn paren_balanced_until_stop(tokens: &[Token], stop: &Token) -> Option<(Vec<Token>, usize)> {
    let mut parens = 0;
    let mut out_tokens = Vec::new();
    let mut stop_pos: i32 = -1;

    for (i, t) in tokens.iter().enumerate() {
        match t {
            Token::LParen => {
                parens += 1;
                out_tokens.push(t.clone());
            }
            Token::RParen => {
                parens -= 1;
                out_tokens.push(t.clone());
            }
            t => {
                if t == stop && parens == 0 {
                    stop_pos = i as i32;
                    break;
                } else {
                    out_tokens.push(t.clone());
                }
            }
        }
    }

    if stop_pos > -1 {
        Some((out_tokens, stop_pos as usize))
    } else {
        None
    }
}

/// splits tokens into bodies either enclosed by parens or single tokens outside parens
fn split_bodies(tokens: &[Token]) -> Option<Vec<Vec<Token>>> {
    let mut parens = 0;
    let mut brackets = 0;
    let mut bodies = Vec::new();
    let mut current_body = Vec::new();

    for t in tokens.iter() {
        match t {
            Token::LParen => {
                parens += 1;
                current_body.push(Token::LParen);
            }
            Token::RParen => {
                parens -= 1;
                current_body.push(Token::RParen);
                bodies.push(current_body.clone());
                current_body.clear();
                if parens < 0 {
                    return None;
                }
            }
            t => {
                if parens == 0 {
                    bodies.push(vec![t.clone()]);
                } else {
                    current_body.push(t.clone());
                }
            }
        }
    }

    Some(bodies)
}
