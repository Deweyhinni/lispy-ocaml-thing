use std::collections::HashMap;

use crate::ast::{
    Declaration, Expression, ExpressionBody, Func, FuncCall, Identifier, Item, Literal, Operation,
    SyntaxTree, Type, TypeValue, VarRef,
};

pub struct RustGenerator {
    syntax: SyntaxTree,
    externs: HashMap<String, ExternFunc>,
}

impl RustGenerator {
    pub fn new(syntax: SyntaxTree) -> Self {
        let mut externs = HashMap::new();
        externs.insert(
            String::from("print"),
            ExternFunc {
                params: vec![None],
                implementation: None,
                ret: Type::Unit,
                call: String::from("println!(\"{:?}\", "),
            },
        );
        externs.insert(
            String::from("int_of_float"),
            ExternFunc {
                params: vec![Some(Type::Float)],
                implementation: Some(String::from(
                    "pub fn int_of_float(f: f64) -> i64 {f as i64}",
                )),
                ret: Type::Int,
                call: String::from("int_of_float("),
            },
        );
        externs.insert(
            String::from("float_of_int"),
            ExternFunc {
                params: vec![Some(Type::Float)],
                implementation: Some(String::from(
                    "pub fn float_of_int(i: i64) -> f64 {i as f64}",
                )),
                ret: Type::Float,
                call: String::from("float_of_int("),
            },
        );
        Self { syntax, externs }
    }

    pub fn generate(&self) -> anyhow::Result<String> {
        let prepend_stuff = String::from("#![allow(unused_braces)]\nuse std::rc::Rc;");

        let code = self
            .syntax
            .items
            .iter()
            .map(|item| match item {
                Item::Declaration(decl) => self.declaration(&decl),
            })
            .collect::<anyhow::Result<Vec<String>>>()?
            .join("\n");

        Ok(format!("{}\n{}", prepend_stuff, code))
    }

    fn declaration(&self, decl: &Declaration) -> anyhow::Result<String> {
        match decl {
            Declaration::Func(ident) => match ident {
                Identifier::FuncDef {
                    name,
                    value:
                        Func {
                            params: _,
                            body:
                                Expression {
                                    local_vars: _,
                                    expression_body: ExpressionBody::Extern(ext_typ),
                                    ret_type: _,
                                },
                            ret: _,
                        },
                } => {
                    if let Some(ext_func) = self.externs.get(name) {
                        if let Some(implementation) = &ext_func.implementation {
                            Ok(implementation.clone())
                        } else {
                            Ok(String::new())
                        }
                    } else {
                        Err(anyhow::anyhow!(
                            "extern function definition but no matching extern function"
                        ))
                    }
                }
                Identifier::FuncDef { name, value } => self.func(&value, &name),
                _ => Err(anyhow::anyhow!(
                    "non-function declaration in top level binding"
                )),
            },
        }
    }

    fn func(&self, func: &Func, name: &String) -> anyhow::Result<String> {
        let param_strs = func
            .params
            .iter()
            .map(|p| match p {
                Identifier::FuncParam { name, typ } => Ok(format!(
                    "{}: {}",
                    name,
                    Self::type_str(typ.as_ref().ok_or(anyhow::anyhow!("no type on param"))?)?
                )),
                _ => Err(anyhow::anyhow!("what should be a func param is not.")),
            })
            .collect::<anyhow::Result<Vec<String>>>()?;
        let body_str = self.expression(&func.body)?;
        let ret_type_str = Self::type_str(
            func.ret
                .as_ref()
                .ok_or(anyhow::anyhow!("no return type on function: {:#?}", func))?,
        )?;

        Ok(format!(
            "pub fn {}({}) -> {} {{ {} }}",
            if name == &String::from("unit") {
                &String::from("main")
            } else {
                name
            },
            param_strs.join(", "),
            ret_type_str,
            body_str
        ))
    }

    fn expression(&self, expr: &Expression) -> anyhow::Result<String> {
        let vars = expr
            .local_vars
            .iter()
            .map(|ident| match ident {
                Identifier::VarDef { name, value } => {
                    let val_expr = self.expression(value)?;
                    Ok(format!("let {} = {};", name, val_expr))
                }
                _ => Err(anyhow::anyhow!(
                    "an identifier that's not a VarDef found in expression var decleration"
                )),
            })
            .collect::<anyhow::Result<Vec<String>>>()?;

        let vars_amount = vars.len();

        let expr_body = self.expression_body(&expr.expression_body)?;

        let mut expr_string = if vars_amount > 0 {
            String::from("{")
        } else {
            String::new()
        };

        for var in vars {
            expr_string.push_str(format!("{}\n", var).as_str());
        }

        expr_string.push_str(&expr_body);

        if vars_amount > 0 {
            expr_string.push('}');
        }

        Ok(expr_string)
    }

    fn expression_body(&self, expr_body: &ExpressionBody) -> anyhow::Result<String> {
        match expr_body {
            ExpressionBody::Literal(l) => Ok(match &l.value {
                TypeValue::Int(i) => format!("{}_i64", i.to_string()),
                TypeValue::Float(f) => format!("{}_f64", f.to_string()),
                TypeValue::Char(c) => format!("'{}'", c.to_string()),
                TypeValue::String(s) => format!("String::from(\"{}\")", s),
                TypeValue::Bool(b) => format!("{}", b.to_string()),
                TypeValue::Unit => String::from("()"),
            }),
            ExpressionBody::FuncCall(fc) => match fc.as_ref() {
                FuncCall::IdentCall { name, params } => {
                    let fc_params = params
                        .iter()
                        .map(|p| self.expression(p))
                        .collect::<anyhow::Result<Vec<String>>>()?;

                    let param_types: Vec<Option<Type>> =
                        params.iter().map(|p| p.ret_type.clone()).collect();

                    if let Some(ext_func) = self.externs.get(name) {
                        if ext_func.params.len() == param_types.len() {
                            Ok(format!("{}{})", ext_func.call, fc_params.join(",")))
                        } else {
                            Err(anyhow::anyhow!(
                                "external function params do not match function call"
                            ))
                        }
                    } else {
                        Ok(format!("{}({})", name, fc_params.join(", ")))
                    }
                }
                FuncCall::AnonCall { params, func } => {
                    let fc_params = params
                        .iter()
                        .map(|p| self.expression(p))
                        .collect::<anyhow::Result<Vec<String>>>()?;

                    match func {
                        Expression {
                            local_vars,
                            expression_body,
                            ret_type: Some(Type::Func { params, ret }),
                        } => {
                            let expr_str = self.expression(&func)?;
                            Ok(format!("{{ {}({}) }}", expr_str, fc_params.join(", ")))
                        }
                        _ => Err(anyhow::anyhow!(
                            "anon call has expression with a non-func return type"
                        )),
                    }
                }
            },
            ExpressionBody::VarRef(VarRef { name, typ }) => match typ {
                Some(Type::Int) | Some(Type::Float) | Some(Type::Bool) | Some(Type::Char) => {
                    Ok(format!("{{ {} }}", name))
                }
                Some(Type::String) => Ok(format!("{{ {name}.clone() }}")),
                Some(Type::List(_list_type)) => Ok(format!("{{ Rc::clone(&{}) }}", name)),
                Some(Type::Func { params, ret }) => {
                    todo!()
                }
                Some(Type::Unit) => Ok(String::from("()")),
                None => {
                    todo!()
                }
            },
            ExpressionBody::Operation(op) => match op.as_ref() {
                Operation::Eq { lhs, rhs } => {
                    let lhs_str = self.expression(lhs)?;
                    let rhs_str = self.expression(rhs)?;
                    Ok(format!("{{ ({}) == ({}) }}", lhs_str, rhs_str))
                }
                Operation::Bigger { lhs, rhs } => {
                    let lhs_str = self.expression(lhs)?;
                    let rhs_str = self.expression(rhs)?;
                    Ok(format!("{{ ({}) > ({}) }}", lhs_str, rhs_str))
                }
                Operation::Smaller { lhs, rhs } => {
                    let lhs_str = self.expression(lhs)?;
                    let rhs_str = self.expression(rhs)?;
                    Ok(format!("{{ ({}) < ({}) }}", lhs_str, rhs_str))
                }
                Operation::BiggerEq { lhs, rhs } => {
                    let lhs_str = self.expression(lhs)?;
                    let rhs_str = self.expression(rhs)?;
                    Ok(format!("{{ ({}) >= ({}) }}", lhs_str, rhs_str))
                }
                Operation::SmallerEq { lhs, rhs } => {
                    let lhs_str = self.expression(lhs)?;
                    let rhs_str = self.expression(rhs)?;
                    Ok(format!("{{ ({}) <= ({}) }}", lhs_str, rhs_str))
                }
                Operation::Not { expr } => {
                    let expr_str = self.expression(expr)?;
                    Ok(format!("{{ !({}) }}", expr_str))
                }
                Operation::Add { lhs, rhs } => {
                    let lhs_str = self.expression(lhs)?;
                    let rhs_str = self.expression(rhs)?;
                    match (&lhs.ret_type, &rhs.ret_type) {
                        (Some(Type::String), Some(Type::String)) => {
                            Ok(format!("format!(\"{{}}{{}}\", {}, {})", lhs_str, rhs_str))
                        }
                        (Some(Type::String), Some(other)) => {
                            Ok(format!("format!(\"{{}}{{}}\", {}, {})", lhs_str, rhs_str))
                        }
                        (Some(other), Some(Type::String)) => {
                            Ok(format!("format!(\"{{}}{{}}\", {}, {})", lhs_str, rhs_str))
                        }
                        _ => Ok(format!("{{ ({}) + ({}) }}", lhs_str, rhs_str)),
                    }
                }
                Operation::Sub { lhs, rhs } => {
                    let lhs_str = self.expression(lhs)?;
                    let rhs_str = self.expression(rhs)?;
                    Ok(format!("{{ ({}) - ({}) }}", lhs_str, rhs_str))
                }
                Operation::Mul { lhs, rhs } => {
                    let lhs_str = self.expression(lhs)?;
                    let rhs_str = self.expression(rhs)?;
                    Ok(format!("{{ ({}) * ({}) }}", lhs_str, rhs_str))
                }
                Operation::Div { lhs, rhs } => {
                    let lhs_str = self.expression(lhs)?;
                    let rhs_str = self.expression(rhs)?;
                    Ok(format!("{{ ({}) / ({}) }}", lhs_str, rhs_str))
                }
            },
            ExpressionBody::Conditional(cd) => {
                let cond_str = self.expression(&cd.cond)?;
                let then_str = self.expression(&cd.then)?;
                let else_str = self.expression(&cd.els)?;
                Ok(format!(
                    "{{ if {} {{ {} }} else {{ {} }} }}",
                    cond_str, then_str, else_str
                ))
            }
            ExpressionBody::List(lst) => {
                let expr_strings = lst
                    .iter()
                    .map(|expr| self.expression(expr))
                    .collect::<anyhow::Result<Vec<String>>>()?;

                let exprs_str = expr_strings.join(",\n");

                Ok(format!("{{ Rc::new(vec![{}]) }}", exprs_str))
            }
            ExpressionBody::Func(func) => {
                let param_strs = func
                    .params
                    .iter()
                    .map(|p| match p {
                        Identifier::FuncParam { name, typ } => Ok(format!(
                            "{}: {}",
                            name,
                            Self::type_str(
                                typ.as_ref().ok_or(anyhow::anyhow!("no type on param"))?
                            )?
                        )),
                        _ => Err(anyhow::anyhow!("what should be a func param is not.")),
                    })
                    .collect::<anyhow::Result<Vec<String>>>()?;
                let body_str = self.expression(&func.body)?;
                let ret_type_str = Self::type_str(
                    func.ret
                        .as_ref()
                        .ok_or(anyhow::anyhow!("no return type on function"))?,
                )?;

                Ok(format!(
                    "Box::new(move |{}| -> {} {{ {} }})",
                    param_strs.join(", "),
                    ret_type_str,
                    body_str,
                ))
            }
            ExpressionBody::Extern(typ) => {
                todo!()
            }
            ExpressionBody::Expression(expr) => self.expression(expr),
        }
    }

    fn type_str(typ: &Type) -> anyhow::Result<String> {
        Ok(match typ {
            Type::Int => String::from("i64"),
            Type::Float => String::from("f64"),
            Type::Bool => String::from("bool"),
            Type::Char => String::from("char"),
            Type::String => String::from("String"),
            Type::Unit => String::from("()"),
            Type::List(t) => format!("Rc<Vec<{}>>", Self::type_str(t)?),
            Type::Func { params, ret } => {
                let params_strs: Vec<String> = params
                    .iter()
                    .map(|p| Self::type_str(p))
                    .collect::<anyhow::Result<Vec<String>>>()?;
                let ret_str = Self::type_str(ret)?;
                format!("Box<dyn Fn({}) -> {}>", params_strs.join(", "), ret_str)
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternFunc {
    pub(crate) params: Vec<Option<Type>>,
    pub(crate) implementation: Option<String>,
    pub(crate) ret: Type,
    pub(crate) call: String,
}
