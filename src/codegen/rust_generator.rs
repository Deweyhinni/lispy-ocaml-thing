use crate::ast::{Expression, ExpressionBody, SyntaxTree, TypeValue};

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

    fn expression(expr: &Expression) -> anyhow::Result<String> {
        let mut vars: Vec<String> = Vec::new();
        for ident in expr.local_vars.iter() {
            match ident {
                crate::ast::Identifier::VarDef { name, value } => {
                    let val_expr = Self::expression(value)?;
                    vars.push(format!("let {} = {};", name, val_expr));
                }
                _ => {
                    return Err(anyhow::anyhow!(
                        "an identifier that's not a VarDef found in expression var decleration"
                    ));
                }
            }
        }

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
            _ => todo!(),
        }
    }
}
