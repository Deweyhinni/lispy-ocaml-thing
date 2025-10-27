use crate::ast::{
    Declaration, Expression, ExpressionBody, Func, Identifier, Item, Operation, SyntaxTree, Type,
    TypeValue, VarRef,
};

pub struct RustGenerator {
    syntax: SyntaxTree,
}

impl RustGenerator {
    pub fn new(syntax: SyntaxTree) -> Self {
        Self { syntax }
    }

    pub fn generate(&self) -> anyhow::Result<String> {
        let prepend_stuff = String::from(r#"use std::rc::Rc;"#);

        let code = self
            .syntax
            .items
            .iter()
            .map(|item| match item {
                Item::Declaration(decl) => Self::declaration(&decl),
            })
            .collect::<anyhow::Result<Vec<String>>>()?
            .join("\n");

        Ok(format!("{}\n{}", prepend_stuff, code))
    }

    fn declaration(decl: &Declaration) -> anyhow::Result<String> {
        match decl {
            Declaration::Func(ident) => match ident {
                Identifier::FuncDef { name, value } => Self::func(&value, &name),
                _ => Err(anyhow::anyhow!(
                    "non-function declaration in top level binding"
                )),
            },
        }
    }

    fn func(func: &Func, name: &String) -> anyhow::Result<String> {
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
        let body_str = Self::expression(&func.body)?;
        let ret_type_str = Self::type_str(
            func.ret
                .as_ref()
                .ok_or(anyhow::anyhow!("no return type on function"))?,
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

    fn expression(expr: &Expression) -> anyhow::Result<String> {
        let vars = expr
            .local_vars
            .iter()
            .map(|ident| match ident {
                Identifier::VarDef { name, value } => {
                    let val_expr = Self::expression(value)?;
                    Ok(format!("let {} = {};", name, val_expr))
                }
                _ => Err(anyhow::anyhow!(
                    "an identifier that's not a VarDef found in expression var decleration"
                )),
            })
            .collect::<anyhow::Result<Vec<String>>>()?;

        let expr_body = Self::expression_body(&expr.expression_body)?;

        let mut expr_string = String::from("{");

        for var in vars {
            expr_string.push_str(format!("{}\n", var).as_str());
        }

        expr_string.push_str(&expr_body);

        expr_string.push('}');

        Ok(expr_string)
    }

    fn expression_body(expr_body: &ExpressionBody) -> anyhow::Result<String> {
        match expr_body {
            ExpressionBody::Literal(l) => Ok(match &l.value {
                TypeValue::Int(i) => i.to_string(),
                TypeValue::Float(f) => f.to_string(),
                TypeValue::String(s) => format!("Rc::new(String::from(\"{}\"))", s),
                TypeValue::Bool(b) => b.to_string(),
                TypeValue::Unit => String::from("()"),
            }),
            ExpressionBody::FuncCall(fc) => {
                let mut params: String = String::new();
                for p in fc.params.iter() {
                    params.push_str(Self::expression(p)?.as_str());
                }

                match fc.name.as_str() {
                    "print" => Ok(format!("println!(\"{{:?}}\", ({}))", params)),
                    _ => Ok(format!("{}({})", fc.name, params)),
                }
            }
            ExpressionBody::VarRef(VarRef { name }) => Ok(format!("{{ Rc::clone(&{}) }}", name)),
            ExpressionBody::Operation(op) => match op.as_ref() {
                Operation::Eq { lhs, rhs } => {
                    let lhs_str = Self::expression(lhs)?;
                    let rhs_str = Self::expression(rhs)?;
                    Ok(format!("{{ {} == {} }}", lhs_str, rhs_str))
                }
                Operation::Add { lhs, rhs } => {
                    let lhs_str = Self::expression(lhs)?;
                    let rhs_str = Self::expression(rhs)?;
                    Ok(format!("{{ {} + {} }}", lhs_str, rhs_str))
                }
                Operation::Sub { lhs, rhs } => {
                    let lhs_str = Self::expression(lhs)?;
                    let rhs_str = Self::expression(rhs)?;
                    Ok(format!("{{ {} - {} }}", lhs_str, rhs_str))
                }
                Operation::Mul { lhs, rhs } => {
                    let lhs_str = Self::expression(lhs)?;
                    let rhs_str = Self::expression(rhs)?;
                    Ok(format!("{{ {} * {} }}", lhs_str, rhs_str))
                }
                Operation::Div { lhs, rhs } => {
                    let lhs_str = Self::expression(lhs)?;
                    let rhs_str = Self::expression(rhs)?;
                    Ok(format!("{{ {} / {} }}", lhs_str, rhs_str))
                }
            },
            ExpressionBody::Conditional(cd) => {
                let cond_str = Self::expression(&cd.cond)?;
                let then_str = Self::expression(&cd.then)?;
                let else_str = Self::expression(&cd.els)?;
                Ok(format!(
                    "{{ if {} {{ {} }} else {} }}",
                    cond_str, then_str, else_str
                ))
            }
            ExpressionBody::List(lst) => {
                let expr_strings = lst
                    .iter()
                    .map(|expr| Self::expression(expr))
                    .collect::<anyhow::Result<Vec<String>>>()?;

                let exprs_str = expr_strings.join(",\n");

                Ok(format!("{{ Rc::new(vec![{}]) }}", exprs_str))
            }
            ExpressionBody::Func(func) => {
                todo!()
            }
            ExpressionBody::Expression(expr) => Self::expression(expr),
        }
    }

    fn type_str(typ: &Type) -> anyhow::Result<String> {
        Ok(match typ {
            Type::Int => String::from("Rc<i64>"),
            Type::Float => String::from("Rc<f64>"),
            Type::Bool => String::from("Rc<bool>"),
            Type::String => String::from("Rc<String>"),
            Type::Unit => String::from("()"),
            Type::List(t) => format!("Rc<Vec<{}>>", Self::type_str(t)?),
            Type::Func { params, ret } => {
                todo!()
            }
        })
    }
}
