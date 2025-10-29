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
                _ => todo!(),
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
    Conditional(Box<Conditional>),
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

        match tokens {
            _ => Err(ParseError::ParseFailed(format!(
                "cannot create expression body from {:?}",
                tokens
            ))),
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
                | Operation::Div { ref lhs, ref rhs } => {
                    match (lhs.ret_type.clone(), rhs.ret_type.clone()) {
                        (Some(Type::Int), Some(Type::Int)) => Some(Type::Int),
                        (Some(Type::Float), Some(Type::Float)) => Some(Type::Float),
                        (Some(Type::Float), Some(Type::Int)) => Some(Type::Float),
                        (Some(Type::Int), Some(Type::Float)) => Some(Type::Float),
                        (Some(Type::String), Some(Type::String)) => Some(Type::String),
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
                            Some(value)
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
                                            expression_body,
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
            _ => Err(ParseError::NotMatched),
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
                let then_pos = rest
                    .iter()
                    .position(|t| t == &Token::Keyword(Keyword::Then))
                    .ok_or(ParseError::ParseFailed(format!(
                        "no then keyword in: {:?}",
                        tokens
                    )))?;
                let else_pos = rest
                    .iter()
                    .position(|t| t == &Token::Keyword(Keyword::Else))
                    .ok_or(ParseError::ParseFailed(format!("no else keyword")))?;

                let if_expr = Expression::from_tokens(&rest[..then_pos], idents)?;
                let then_expr = Expression::from_tokens(&rest[(then_pos + 1)..else_pos], idents)?;
                let else_expr = Expression::from_tokens(&rest[(else_pos + 1)..], idents)?;

                let then_type = then_expr.ret_type.clone();
                let else_type = else_expr.ret_type.clone();
                let typ = if then_type == else_type {
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
        match tokens {
            [Token::Identifier(name)] => match name.as_str() {
                "Int" => Ok(Self::Int),
                "Float" => Ok(Self::Float),
                "Bool" => Ok(Self::Bool),
                "String" => Ok(Self::String),
                "Unit" => Ok(Self::Unit),
                _ => Err(ParseError::NotMatched),
            },
            [Token::LBracket, middle @ .., Token::RBracket] => {
                Ok(Self::List(Box::new(Self::from_tokens(middle)?)))
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
