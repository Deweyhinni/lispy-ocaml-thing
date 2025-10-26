use crate::ast::{
    Declaration, Expression, ExpressionBody, Identifier, Operation, SyntaxTree, Type, TypeValue,
    VarRef,
};

pub struct RustGenerator {
    syntax: SyntaxTree,
}

impl RustGenerator {
    pub fn new(syntax: SyntaxTree) -> Self {
        Self { syntax }
    }

    pub fn generate(&self) -> String {
        todo!()
    }

    fn declaration(decl: Declaration) -> anyhow::Result<String> {
        todo!()
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
                TypeValue::String(s) => format!("String::from({})", s),
                TypeValue::Bool(b) => b.to_string(),
                TypeValue::Unit => String::from("()"),
            }),
            ExpressionBody::FuncCall(fc) => {
                let mut params: String = String::new();
                for p in fc.params.iter() {
                    params.push_str(Self::expression(p)?.as_str());
                }

                Ok(format!("{}({})", fc.name, params))
            }
            ExpressionBody::VarRef(VarRef { name }) => Ok(format!("{{ {} }}", name)),
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

                Ok(format!("{{ vec![{}] }}", exprs_str))
            }
            ExpressionBody::Func(func) => {
                todo!()
            }
            ExpressionBody::Expression(expr) => Self::expression(expr),
        }
    }

    fn type_str(typ: Type) -> anyhow::Result<String> {
        Ok(match typ {
            Type::Int => String::from("i64"),
            Type::Float => String::from("f64"),
            Type::Bool => String::from("bool"),
            Type::String => String::from("String"),
            Type::Unit => String::from("()"),
            Type::List(t) => format!("Vec<{}>", Self::type_str(*t.clone())?),
            Type::Func { params, ret } => {
                todo!()
            }
        })
    }
}
