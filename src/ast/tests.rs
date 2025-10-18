#![cfg(test)]

use super::*;

#[test]
fn test_fn_expr() {
    let code = "(fn x -> (print (x)))".to_string();
    let tokens = tokenizer::TokenList::generate(code).unwrap();

    let func = Expression::from_tokens(&tokens.tokens()).unwrap();

    println!("{:#?}", func);

    assert_eq!(
        func,
        Expression {
            local_vars: vec![],
            expression_body: ExpressionBody::Func(Box::new(Func {
                params: vec![Identifier::FuncParam {
                    name: "x".into(),
                    typ: None,
                },],
                body: Expression {
                    local_vars: vec![],
                    expression_body: ExpressionBody::FuncCall(Box::new(FuncCall {
                        name: "print".into(),
                        params: vec![Expression {
                            local_vars: vec![],
                            expression_body: ExpressionBody::VarRef(VarRef { name: "x".into() },),
                        },],
                    })),
                },
                ret: None,
            })),
        }
    )
}

#[test]
fn test_func_declaration() {
    let tokens = tokenizer::TokenList::generate(
        r#"
        let meow s =
            let m = ("meow")
            in
            (print (m) (s))
        "#
        .to_string(),
    )
    .unwrap();

    let func = SyntaxTree::item_from_tokens(&tokens.tokens()).unwrap();

    println!("{:#?}", func);

    assert_eq!(
        func,
        Item::Declaration(Declaration::Func(Identifier::FuncDef {
            name: "meow".into(),
            value: Func {
                params: vec![Identifier::FuncParam {
                    name: "s".into(),
                    typ: None,
                }],
                body: Expression {
                    local_vars: vec![Identifier::VarDef {
                        name: "m".into(),
                        value: Expression {
                            local_vars: vec![],
                            expression_body: ExpressionBody::Literal(Box::new(Literal {
                                typ: Type::String,
                                value: TypeValue::String("meow".into()),
                            })),
                        },
                    }],
                    expression_body: ExpressionBody::FuncCall(Box::new(FuncCall {
                        name: "print".into(),
                        params: vec![
                            Expression {
                                local_vars: vec![],
                                expression_body: ExpressionBody::VarRef(VarRef {
                                    name: "m".into(),
                                }),
                            },
                            Expression {
                                local_vars: vec![],
                                expression_body: ExpressionBody::VarRef(VarRef {
                                    name: "s".into(),
                                }),
                            },
                        ],
                    })),
                },
                ret: None,
            },
        })),
    )
}

#[test]
fn test_operator_parsing() {
    let tokens = tokenizer::TokenList::generate("+ 2 4".to_string()).unwrap();
    let operation = Operation::from_tokens(&tokens.tokens()).unwrap();

    println!("{:#?}", operation);

    assert_eq!(
        operation,
        Operation::Add {
            lhs: Expression {
                local_vars: vec![],
                expression_body: ExpressionBody::Literal(Box::new(Literal {
                    typ: Type::Int,
                    value: TypeValue::Int(2)
                }))
            },
            rhs: Expression {
                local_vars: vec![],
                expression_body: ExpressionBody::Literal(Box::new(Literal {
                    typ: Type::Int,
                    value: TypeValue::Int(4)
                }))
            }
        }
    )
}

#[test]
fn test_func_call() {
    let tokens = tokenizer::TokenList::generate("(meow (\"nya\") (14))".to_string()).unwrap();
    let expr = Expression::from_tokens(&tokens.tokens()).unwrap();

    println!("{:#?}", expr);

    assert_eq!(
        expr,
        Expression {
            local_vars: vec![],
            expression_body: ExpressionBody::FuncCall(Box::new(FuncCall {
                name: "meow".to_string(),
                params: vec![
                    Expression {
                        local_vars: vec![],
                        expression_body: ExpressionBody::Literal(Box::new(Literal {
                            typ: Type::String,
                            value: TypeValue::String("nya".to_string()),
                        }),),
                    },
                    Expression {
                        local_vars: vec![],
                        expression_body: ExpressionBody::Literal(Box::new(Literal {
                            typ: Type::Int,
                            value: TypeValue::Int(14,),
                        }),),
                    },
                ],
            }),),
        }
    )
}

#[test]
fn test_var_from_tokens() {
    let tokens = crate::tokenizer::TokenList::generate("let meow = (\"meow\")".into()).unwrap();
    let var = crate::ast::Expression::var_from_tokens(&tokens.tokens()).unwrap();

    println!("{:#?}", var);

    assert_eq!(
        var,
        Identifier::VarDef {
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

#[test]
fn test_ident_expression() {
    let tokens =
        tokenizer::TokenList::generate("let meow = (14) let woof = (\"bork\") in (meow)".into())
            .unwrap();
    let expr = Expression::from_tokens(&tokens.tokens()).unwrap();

    println! {"{:#?}", expr};

    assert_eq!(
        expr,
        Expression {
            local_vars: vec![
                Identifier::VarDef {
                    name: String::from("meow"),
                    value: Expression {
                        local_vars: Vec::new(),
                        expression_body: ExpressionBody::Literal(Box::new(Literal {
                            typ: Type::Int,
                            value: TypeValue::Int(14)
                        }))
                    }
                },
                Identifier::VarDef {
                    name: String::from("woof"),
                    value: Expression {
                        local_vars: Vec::new(),
                        expression_body: ExpressionBody::Literal(Box::new(Literal {
                            typ: Type::String,
                            value: TypeValue::String("bork".into())
                        }))
                    }
                }
            ],
            expression_body: ExpressionBody::VarRef(VarRef {
                name: "meow".into()
            })
        }
    )
}
